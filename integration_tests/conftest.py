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
  * Classes carrying `@pytest.mark.slow` float to the head of the
    collected-items queue. Under `--dist=loadscope` xdist dispatches
    each distinct scope (= class) to a worker in the order it first
    appears in items[], so slow classes get assigned to workers in
    the first dispatch batch — classic LPT scheduling that keeps the
    run from finishing with one worker still chewing on a long class
    while the others idle.
  * The first failed/errored method in a class marks every subsequent
    method as skipped — the cascade hides the real bug less.
  * If the class fixture itself fails to set up, every method skips
    with a single coherent reason; the partially-booted topology is
    left alive for inspection.
  * Under `--dist=loadscope` (set in `pyproject.toml`) every class is
    pinned to a single worker for its entire run, so the autouse
    Topology fixture boots exactly one outer test-node VM per class.
    `--dist=worksteal` was tried and rejected: it splits classes
    across workers and re-runs the class fixture on every stealer,
    multiplying test-node VM count beyond the number of classes.
  * xdist's loadscope GH-277 "second pass" gives each single-test
    worker a bonus scope from the queue head so the worker doesn't
    deadlock waiting for a nonexistent next-item. That bonus
    *traps* whatever class is at the queue head behind the slow
    single-test class on the same worker — even if other workers
    sit idle and could have run it. We override the second pass
    in `_LoadScopeShutdownSingleton`: single-test workers get a
    SHUTDOWN sentinel instead of a bonus, exit cleanly after their
    one test, and the queue head stays in the global queue for the
    next multi-test worker to chain through.
"""

from __future__ import annotations

import contextlib
import fcntl
import os
import secrets
import sys
import tempfile
from collections.abc import Iterator
from pathlib import Path

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
from xdist.scheduler.loadscope import LoadScopeScheduling
from xdist.workermanage import WorkerController

import yaml as _yaml

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


@pytest.fixture(scope="session")
def session_test_network(
    tmp_path_factory: pytest.TempPathFactory,
    worker_id: str,
    crv: Crv,
    outer_version: dict,
) -> Iterator[str]:
    """One Corvus managed network shared by every test-node across
    every xdist worker in a single pytest invocation.

    `tmp_path_factory.getbasetemp().parent` is the dir pytest-xdist
    creates for cross-worker coordination — same path for every
    worker in this run. We store the network's name there along
    with a refcount of how many worker sessions still hold the
    fixture; the last to leave deletes the network.

    Crashed workers leak the refcount; the leaked network itself
    is reaped by `make integration-tests-clean` on the next
    invocation. Functional correctness during a running session is
    unaffected — workers read the *name* out of the shared file,
    not the refcount.

    When pytest runs without xdist, `worker_id == "master"` and we
    take a trivial single-process create/delete path.
    """
    if worker_id == "master":
        name = _create_session_network(crv)
        try:
            yield name
        finally:
            _delete_session_network(crv, name)
        return

    root = tmp_path_factory.getbasetemp().parent
    name_file = root / "corvus_it_session_network.name"
    refcount_file = root / "corvus_it_session_network.refcount"
    lock_path = str(root / "corvus_it_session_network.lock")

    # Create-or-attach (refcount++)
    with _flock(lock_path):
        if name_file.exists():
            name = name_file.read_text().strip()
        else:
            name = _create_session_network(crv)
            name_file.write_text(name)
        n = int(refcount_file.read_text()) if refcount_file.exists() else 0
        refcount_file.write_text(str(n + 1))

    try:
        yield name
    finally:
        with _flock(lock_path):
            n = int(refcount_file.read_text()) - 1
            if n <= 0:
                _delete_session_network(crv, name)
                name_file.unlink(missing_ok=True)
                refcount_file.unlink(missing_ok=True)
            else:
                refcount_file.write_text(str(n))


def _pick_session_subnet(crv: Crv) -> str:
    """Choose the first 10.91.X.0/24 that no managed network on the
    outer daemon already claims.

    Each pytest invocation creates a fresh session network, and on
    a failed run the harness deliberately leaks the network for
    post-mortem inspection. If a follow-up run picked the same
    subnet, two bridges would carry overlapping CIDRs on the
    host's routing table and the per-network NAT rules would
    accumulate masquerade entries for the same source range —
    networking breaks in subtle ways. Pick a fresh /24 from the
    fixed harness pool instead.

    The pool stays inside 10.91.0.0/16 so operators recognise
    harness-owned address space at a glance (and so a per-host
    firewall rule can scope to it). Exhaustion (256 leaked
    sessions) raises with a pointer to the cleanup target.
    """
    existing = {n.get("subnet") for n in crv.network_list() if n.get("subnet")}
    for x in range(256):
        candidate = f"10.91.{x}.0/24"
        if candidate not in existing:
            return candidate
    raise RuntimeError(
        "All 256 /24 subnets under 10.91.0.0/16 are in use. "
        "Run `make integration-tests-clean` to release leaked "
        "session networks before retrying."
    )


def _create_session_network(crv: Crv) -> str:
    """Apply a one-shot ApplyConfig that creates the network, then
    explicitly start it. ``ApplyNetwork`` has no ``running`` field
    (only ``autostart``, which only fires on daemon boot), so the
    explicit start is the canonical way to bring the network up
    in the current session."""
    name = f"corvus-it-session-{secrets.token_hex(3)}"
    subnet = _pick_session_subnet(crv)
    # Visible in the captured stderr so a developer diffing two
    # failing runs can tell which leaked network is theirs.
    print(f"[harness] session network {name!r} on {subnet}", file=sys.stderr)
    apply_doc = {
        "ifExists": "skip",
        "networks": [
            {
                "name": name,
                "subnet": subnet,
                "dhcp": True,
                "nat": True,
                # Advertise public resolvers via DHCP option 6 so
                # test-nodes don't fall back to systemd-resolved's
                # hardcoded fallback list (1.0.0.1 / 8.8.8.8 /
                # …), which is unreachable on hosts whose egress
                # only permits 1.1.1.1.
                "dnsServers": ["1.1.1.1", "8.8.8.8"],
            }
        ],
    }
    with tempfile.NamedTemporaryFile(
        "w", suffix=".yml", prefix="corvus-it-session-", delete=False
    ) as tmp:
        _yaml.safe_dump(apply_doc, tmp)
        tmp_path = Path(tmp.name)
    try:
        crv.apply(tmp_path, skip_existing=True, wait=True)
    finally:
        try:
            tmp_path.unlink()
        except OSError:
            pass
    crv.network_start(name)
    return name


def _delete_session_network(crv: Crv, name: str) -> None:
    """Best-effort teardown: leaked VMs on the network make delete
    fail; `make integration-tests-clean` catches the orphan on the
    next invocation."""
    try:
        crv.network_stop(name, force=True)
    except Exception:
        pass
    try:
        crv.network_delete(name)
    except Exception:
        pass


@contextlib.contextmanager
def _flock(path: str) -> Iterator[None]:
    """Exclusive file lock on `path` using stdlib `fcntl.flock` —
    no third-party dep needed. Linux-only; integration tests are
    Linux-only (nested KVM)."""
    fd = os.open(path, os.O_CREAT | os.O_RDWR, 0o644)
    try:
        fcntl.flock(fd, fcntl.LOCK_EX)
        try:
            yield
        finally:
            fcntl.flock(fd, fcntl.LOCK_UN)
    finally:
        os.close(fd)


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


@pytest.hookimpl(trylast=True)
def pytest_collection_modifyitems(
    config: pytest.Config, items: list[pytest.Item]
) -> None:
    """Source-line order within each class; slow classes first overall.

    Runs `trylast` so this is the final word on item order: any other
    plugin that reshuffles items (e.g. `--lf`'s last-failed-first) has
    already run, and we reapply both invariants on top.

    Inter-class ordering matters because `--dist=loadscope` dispatches
    each distinct scope to a worker in the order it first appears in
    items[]; putting `@pytest.mark.slow` classes at the head ensures
    they land on workers in the first dispatch batch and start at t=0
    in parallel.
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

    # Sort slow classes by ascending test count so the single-test
    # slow class (TestWindows: one ~5 min method) lands at scope 0
    # ahead of multi-test slow classes — loadscope assigns scope 0
    # to the first-registered worker, so the genuinely-long single
    # test gets the earliest start.
    slow_groups.sort(key=len)

    # Sort fast classes by descending test count so the biggest fast
    # classes (TestDisk, TestNetdDeclarative, …) land in the initial
    # dispatch batch and the smallest classes end up at the queue
    # head. xdist's loadscope second pass (the GH-277 "ensure each
    # worker has ≥2 work units" loop) gives every single-test worker
    # a bonus scope from the queue head: with smallest-first at the
    # head, TestWindows's bonus is a 1-test class instead of an
    # 8-test class like TestQuickstart, so the bonus runs in
    # ~5 minutes after TestWindows rather than the 15+ minutes a
    # bigger class would cost. Without this sort we either deadlock
    # single-test workers (no second pass) or trap a heavy fast
    # class behind TestWindows (default queue order).
    fast_groups.sort(key=len, reverse=True)

    reordered: list[pytest.Item] = []
    for group in slow_groups:
        reordered.extend(group)
    for group in fast_groups:
        reordered.extend(group)
    reordered.extend(standalone)
    items[:] = reordered

    # Diagnostic: one-line snapshot of the resulting first-occurrence
    # scope order. Suppress on every xdist worker except gw0 so the
    # pytest header gets one line instead of one per worker (the hook
    # runs N times under --dist=loadscope, once on each worker).
    if os.environ.get("PYTEST_XDIST_WORKER", "gw0") == "gw0":
        seen: set[type] = set()
        head: list[str] = []
        for it in items[:25]:
            cls = getattr(it, "cls", None)
            if cls is not None and cls not in seen:
                seen.add(cls)
                head.append(f"{cls.__name__}({len(by_class[cls])})")
        sys.stderr.write("[corvus-sched] scope-order head: " + " → ".join(head) + "\n")
        sys.stderr.flush()


# ---------------------------------------------------------------------------
# Custom xdist scheduler — LoadScope minus the trap-fast-class-behind-slow.
# ---------------------------------------------------------------------------


class _LoadScopeShutdownSingleton(LoadScopeScheduling):
    """LoadScopeScheduling with on-demand scope dispatch.

    Two invariants the suite relies on:

    1. **Class atomicity.** Every method in a test class runs on the
       same worker (the loadscope default). Class-scoped fixtures
       (``IntegrationTestCase.topology`` — one outer test-node VM
       per class) depend on this.

    2. **On-demand dispatch.** A scope is assigned to a worker only
       when that worker is down to one pending test (the running
       one). Stock LoadScopeScheduling pre-assigns when pending ≤ 2
       — that extra lookahead drains the workqueue while a slow
       worker still has plenty of work, so the other workers see an
       empty queue when they finish their own scopes, shutdown, and
       the pre-assigned classes end up running sequentially behind
       the slow class on a single worker instead of in parallel.
       We override ``_reschedule`` with threshold "> 1" — the
       absolute minimum xdist's worker design permits without
       deadlocking on ``torun.get()``.

    Single-test scopes still need special handling at startup.
    xdist's worker main loop calls ``torun.get()`` a second time
    *before* running the current test, so it can pass ``nextitem``
    to the runtest hooks. A worker whose initial scope has only
    one test blocks forever on that second get() unless we send
    something. Stock loadscope handles this with the GH-277 second
    pass that hands every single-test worker a *bonus scope* —
    which traps the bonus on the same worker behind the slow test.
    We override the second pass instead: workers with exactly one
    pending test get a SHUTDOWN sentinel, exit cleanly after their
    one test, and the would-be-bonus stays in the global queue
    where the next free worker picks it up.
    """

    def _reschedule(self, node: WorkerController) -> None:
        """On-demand scope assignment with minimum pre-assignment.

        Threshold is "> 1" (vs stock LoadScopeScheduling's "> 2"):
        the next scope is only sent to a worker once that worker
        is down to one pending test — the test currently running.
        At that exact moment xdist's worker has already
        lookahead-fetched the last item from its queue, finished
        the second-to-last test's sendevent, looped back into
        ``run_one_test`` for the last item, and is about to call
        ``torun.get()`` for the test-after-that. We MUST send
        something (next scope's items or SHUTDOWN); without it
        the worker deadlocks waiting for an item that never
        arrives. Any tighter threshold (pending=0) would deadlock
        before the worker can even run its last test.

        Looser thresholds (xdist's stock "> 2") pre-assign the
        next scope when the worker still has 2 tests pending,
        trapping a fast scope on a possibly-slow worker even when
        other workers could have picked it up after they free up.
        """
        if node.shutting_down:
            return
        if not self.workqueue:
            node.shutdown()
            return
        if self._pending_of(self.assigned_work[node]) > 1:
            return
        self._assign_work_unit(node)

    def schedule(self) -> None:
        assert self.collection_is_completed
        if self.collection is not None:
            for node in self.nodes:
                self._reschedule(node)
            return
        if not self._check_nodes_have_same_collection():
            self.log("**Different tests collected, aborting run**")
            return
        self.collection = list(next(iter(self.registered_collections.values())))
        if not self.collection:
            return
        unsorted_workqueue: dict[str, dict[str, bool]] = {}
        for nodeid in self.collection:
            scope = self._split_scope(nodeid)
            unsorted_workqueue.setdefault(scope, {})[nodeid] = False
        if self.config.option.loadscopereorder:
            for scope, nodeids in sorted(
                unsorted_workqueue.items(), key=lambda item: -len(item[1])
            ):
                self.workqueue[scope] = nodeids
        else:
            for scope, nodeids in unsorted_workqueue.items():
                self.workqueue[scope] = nodeids
        extra_nodes = len(self.nodes) - len(self.workqueue)
        if extra_nodes > 0:
            self.log(f"Shutting down {extra_nodes} nodes")
            for _ in range(extra_nodes):
                unused_node, _ = self.assigned_work.popitem()
                self.log(f"Shutting down unused node {unused_node}")
                unused_node.shutdown()
        for node in self.nodes:
            self._assign_work_unit(node)
        # Second pass — but with our twist: shutdown single-test
        # workers instead of trapping a bonus scope behind them.
        for node in self.nodes:
            if self._pending_of(self.assigned_work[node]) == 1:
                node.shutdown()
            else:
                self._reschedule(node)


def pytest_xdist_make_scheduler(config, log):
    """Register our LoadScope variant for --dist=loadscope runs."""
    if config.getvalue("dist") == "loadscope":
        return _LoadScopeShutdownSingleton(config, log)
    return None


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
