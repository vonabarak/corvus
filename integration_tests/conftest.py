"""Shared fixtures + pytest hooks for the Corvus integration suite.

Fixture lifecycle:

  session   — outer-Corvus reachable, nested-KVM enabled,
              integration-test image applied, host binary located.
              These are global preconditions; failure aborts the run.

  class     — `IntegrationTestCase` subclasses (see
              `corvus_test_harness.cases`) own a class-scoped Topology;
              the autouse fixture lives on the base class itself.

There are no function-scoped fixtures: every test method derives its
node(s) from the class fixture by inheriting from `SingleNodeCase`,
`TwoNodesCase`, or `ThreeNodesCase`.

The hooks below enforce the suite-wide behaviour:

The hooks below enforce the suite-wide behaviour:

  * Tests within a class run in source-line order (the pytest default,
    re-asserted defensively so xdist worker-side ordering can't drift).
  * Classes carrying `@pytest.mark.slow` float to the head of the
    collected-items queue. With `--dist=worksteal` xdist dispatches
    chunks of pending tests in items[] order at startup, so slow
    classes get assigned to workers first — classic LPT scheduling
    that keeps the run from finishing with one worker still chewing
    on a long class while the others idle.
  * The first failed/errored method in a class marks every subsequent
    method as skipped — the cascade hides the real bug less.
  * If the class fixture itself fails to set up, every method skips
    with a single coherent reason; the partially-booted topology is
    left alive for inspection.
  * Under `--dist=worksteal` (set in `pyproject.toml`) idle workers
    steal tests from busy workers' queues. A class whose tests get
    stolen mid-run will have its autouse Topology fixture re-run on
    the stealing worker — one extra outer test-node VM boot per
    steal, in exchange for not leaving 9 workers idle while 3 finish
    long classes.
"""

from __future__ import annotations

import os

import pytest
from corvus_test_harness import (
    Crv,
    HostBinary,
    ImageReady,
    InstallerImageReady,
    check_nested_kvm,
    check_outer_version,
)
from corvus_test_harness.cases import IntegrationTestCase, state_for

# ---------------------------------------------------------------------------
# Session-scoped preconditions
# ---------------------------------------------------------------------------


@pytest.fixture(scope="session")
def crv() -> Crv:
    """Autodetected `crv` CLI driver for the outer daemon.

    Picks the binary that's on the operator's '$PATH' so the
    outer-side CLI matches whichever Corvus install is actually
    running on the host. Override via '$CORVUS_CRV'. The
    freshly-built dev-tree 'crv' is intentionally not preferred
    here — that one ships with the inner daemons in test VMs.
    """
    return Crv.autodetect()


@pytest.fixture(scope="session")
def _nested_kvm_ok():
    status = check_nested_kvm()
    if not status.available:
        pytest.skip(
            f"nested KVM unavailable on this host: {status.reason}. "
            "Integration tests require it; see integration_tests/README.md."
        )


@pytest.fixture(scope="session")
def outer_version(crv: Crv, _nested_kvm_ok) -> dict:
    """Confirm the outer daemon is reachable and report its version."""
    return check_outer_version(crv)


@pytest.fixture(scope="session")
def host_binary() -> HostBinary:
    """Locate and validate the freshly-compiled inner Corvus binary."""
    return HostBinary.discover()


@pytest.fixture(scope="session")
def image_ready(crv: Crv, outer_version) -> ImageReady:
    """Ensure the integration-test image + template are applied."""
    return ImageReady.ensure(crv)


@pytest.fixture(scope="session")
def installer_image_ready(crv: Crv) -> InstallerImageReady:
    """Confirm the synthetic-installer ISO is registered with the outer daemon.

    Consumed only by `test_build_installer.py`. Session-scoped so a
    fresh checkout running `make integration-tests MATCH=
    test_build_installer` fails fast with a single ERROR message
    rather than once per test.
    """
    return InstallerImageReady.ensure(crv)


# ---------------------------------------------------------------------------
# Pytest hooks: ordering, skip-on-fail, class-affinity
# ---------------------------------------------------------------------------


def _is_class_based(item: pytest.Item) -> bool:
    """True if `item` belongs to an IntegrationTestCase subclass."""
    cls = getattr(item, "cls", None)
    return isinstance(cls, type) and issubclass(cls, IntegrationTestCase)


def _lineno(item: pytest.Item) -> int:
    obj = getattr(item, "function", None)
    if obj is None:
        return 0
    code = getattr(obj, "__code__", None)
    return getattr(code, "co_firstlineno", 0)


def _xdist_num_workers(config: pytest.Config) -> int | None:
    """Best-effort read of the configured xdist worker count.

    Two sources, in priority order:

    1. `$PYTEST_XDIST_WORKER_COUNT` — xdist sets this in every worker
       process at bootstrap (see `xdist.remote.remote_initconfig`).
       This is the *only* reliable signal inside a worker, because
       xdist deliberately clears `config.option.numprocesses` there
       (see `xdist.remote.setup_config`) so plugins don't try to
       re-shard from inside a worker.
    2. `config.option.numprocesses` — set on the master / single-
       process pytest invocations. Honours `-n N` from the CLI.
    """
    env = os.environ.get("PYTEST_XDIST_WORKER_COUNT")
    if env:
        try:
            v = int(env)
        except ValueError:
            pass
        else:
            if v > 0:
                return v
    n = getattr(config.option, "numprocesses", None)
    if isinstance(n, int) and n > 0:
        return n
    if isinstance(n, str):
        try:
            v = int(n)
        except ValueError:
            return None
        return v if v > 0 else None
    return None


def _xdist_chunk_boundaries(total: int, num_workers: int) -> list[int]:
    """Reproduce worksteal's initial chunk boundaries.

    Mirrors the loop in `xdist.scheduler.worksteal.check_schedule`:
    each iteration sends `pending // nodes_remaining` items, so chunk
    sizes are not uniform (e.g. for 141 items / 12 workers it's
    11,11,11,12,…,12). Returns the index where worker i's chunk
    starts, for i in [0, num_workers).
    """
    boundaries = []
    pos, remaining = 0, total
    for i in range(num_workers):
        boundaries.append(pos)
        nodes_remaining = num_workers - i
        if nodes_remaining == 0:
            break
        chunk = remaining // nodes_remaining
        pos += chunk
        remaining -= chunk
    return boundaries


@pytest.hookimpl(trylast=True)
def pytest_collection_modifyitems(
    config: pytest.Config, items: list[pytest.Item]
) -> None:
    """Source-line order within each class; slow classes pinned to
    worksteal chunk starts so they begin at t=0 on separate workers.

    Runs `trylast` so this is the final word on item order: any other
    plugin that reshuffles items (e.g. `--lf`'s last-failed-first) has
    already run, and we reapply both invariants on top.

    Under `--dist=worksteal` xdist sends each worker a contiguous
    prefix of items[] as its initial chunk (see
    `xdist.scheduler.worksteal._send_tests`). Simply putting every
    slow class at the head of the list bundles them all into
    worker 0's chunk and worker 0 only starts the second slow class
    once the first is done. To get every slow class running at t=0
    on a different worker we have to place each slow group at the
    *start* of a different worker's chunk and pad between them with
    fast items.
    """
    by_class: dict[type, list[pytest.Item]] = {}
    standalone: list[pytest.Item] = []
    for item in items:
        if _is_class_based(item):
            by_class.setdefault(item.cls, []).append(item)
        else:
            standalone.append(item)

    slow_groups: list[list[pytest.Item]] = []
    fast_groups: list[list[pytest.Item]] = []
    for group in by_class.values():
        group.sort(key=_lineno)
        # `pytestmark` at module scope propagates to every collected
        # item in the module, so checking the first item is enough.
        if group[0].get_closest_marker("slow") is not None:
            slow_groups.append(group)
        else:
            fast_groups.append(group)

    pool: list[pytest.Item] = [it for g in fast_groups for it in g] + standalone
    num_workers = _xdist_num_workers(config)

    # No worker count, single worker, or 0–1 slow classes → the
    # chunk-boundary trick is moot; just put slow classes first.
    if num_workers is None or num_workers <= 1 or len(slow_groups) <= 1:
        items[:] = [it for g in slow_groups for it in g] + pool
        return

    total = len(items)
    boundaries = _xdist_chunk_boundaries(total, num_workers)
    pool_iter = iter(pool)
    reordered: list[pytest.Item] = []
    for i, slow_group in enumerate(slow_groups):
        if i < len(boundaries):
            target = boundaries[i]
            while len(reordered) < target:
                try:
                    reordered.append(next(pool_iter))
                except StopIteration:
                    # Pool exhausted before reaching the target boundary.
                    # The remaining slow classes will land contiguously;
                    # nothing more we can do without inventing items.
                    break
        reordered.extend(slow_group)
    reordered.extend(pool_iter)
    items[:] = reordered


@pytest.hookimpl(hookwrapper=True)
def pytest_runtest_makereport(item: pytest.Item, call):
    """Record the first failing/erroring method on its class state.

    We watch all three phases (setup / call / teardown). Tests that
    `pytest.skip(...)` themselves don't flip the cascade — only true
    failures do (we check `report.failed`, not `outcome != 'passed'`).
    """
    outcome = yield
    if not _is_class_based(item):
        return
    report = outcome.get_result()
    if not report.failed:
        return
    state = state_for(item.cls)
    if state.first_failure is None:
        state.first_failure = item.name


def pytest_runtest_setup(item: pytest.Item) -> None:
    """Skip every class method after the first failure in the class.

    Also handles the case where the class-scoped autouse fixture itself
    failed: its handler in `IntegrationTestCase._class_topology` sets
    `state.setup_failed = True` and returns without raising; we surface
    that here as a uniform skip per method.
    """
    if not _is_class_based(item):
        return
    state = state_for(item.cls)
    if state.setup_failed:
        reason = f"class fixture setup failed for {item.cls.__qualname__}"
        if state.setup_error:
            reason = f"{reason}: {state.setup_error}"
        pytest.skip(reason)
    if state.first_failure is not None and state.first_failure != item.name:
        pytest.skip(
            f"previous test in class failed ({state.first_failure}); skipping the rest"
        )
