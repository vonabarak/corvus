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

  * Tests within a class run in source-line order (the pytest default,
    re-asserted defensively so xdist worker-side ordering can't drift).
  * The first failed/errored method in a class marks every subsequent
    method as skipped — the cascade hides the real bug less.
  * If the class fixture itself fails to set up, every method skips
    with a single coherent reason; the partially-booted topology is
    left alive for inspection.
  * With `pytest-xdist`'s `--dist=loadscope` (default in
    `pyproject.toml`), each class lands on a single worker for the
    full duration. Tests within a class never run in parallel.
"""

from __future__ import annotations

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


@pytest.hookimpl(tryfirst=True)
def pytest_collection_modifyitems(
    config: pytest.Config, items: list[pytest.Item]
) -> None:
    """Re-assert source-line order within each IntegrationTestCase class.

    Pytest's default *is* source-line order, but enforcing it here keeps
    the suite robust against external plugins (or xdist worker-side
    behaviour) that might otherwise reshuffle. We run with `tryfirst`
    so subsequent reorderings (e.g. `--lf`) win for non-class tests
    while class tests keep their declared sequence.
    """
    # Group items by their class while preserving the inter-class order
    # pytest already picked. Standalone (non-class) items stay where
    # they are.
    indexed: list[tuple[int, pytest.Item]] = list(enumerate(items))
    by_class: dict[type, list[tuple[int, pytest.Item]]] = {}
    for idx, item in indexed:
        if not _is_class_based(item):
            continue
        by_class.setdefault(item.cls, []).append((idx, item))

    for _cls, group in by_class.items():
        # Sort by source-line number of the underlying function. Items
        # already share a file (one class lives in one file), so name
        # alone would also work — but lineno is the property the user
        # actually wants.
        def _lineno(pair: tuple[int, pytest.Item]) -> int:
            _, it = pair
            obj = getattr(it, "function", None)
            if obj is None:
                return 0
            code = getattr(obj, "__code__", None)
            return getattr(code, "co_firstlineno", 0)

        group_sorted = sorted(group, key=_lineno)
        # Splice the sorted methods back into items[] at the original
        # positions, in their new order.
        positions = [orig_idx for orig_idx, _ in group]
        for pos, (_, new_item) in zip(positions, group_sorted, strict=False):
            items[pos] = new_item


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
