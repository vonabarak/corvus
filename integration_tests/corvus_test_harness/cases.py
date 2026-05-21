"""Class-based integration test bases.

Real test classes inherit from `SingleNodeCase`, `TwoNodesCase`, or
`ThreeNodesCase` and get a class-scoped `Topology` for free. The
class fixture brings up one node per name in `NODES`, exposes each
as `self.node[_short_name]` and `self.client[_short_name]`, and
tears everything down at class scope. The pytest hooks in
`integration_tests/conftest.py` add ordering + skip-on-first-failure
+ xdist class-affinity on top.

Example:

    class TestVmLifecycle(SingleNodeCase):
        def test_reachable(self):
            assert self.client.status().protocol_version > 0

        def test_create_vm(self):
            vm = self.client.vms.create("nested", cpu_count=1, ram_mb=128)
            assert vm.show().name == "nested"
            vm.delete()

The state for a given class is held in a module-level dict keyed by the
concrete subclass type — not on the class body itself — so inheritance
between base/leaf classes never aliases a `_topology` attribute
accidentally. Hooks in conftest.py read and write that registry.
"""

from __future__ import annotations

import sys
import time
from dataclasses import dataclass
from pathlib import Path
from typing import TYPE_CHECKING, Optional

import pytest

from . import base_images as _base_images
from .ssh import HOST_ALPINE_KEY_PATH, VmShell
from .topology import NodeRole, Topology

if TYPE_CHECKING:
    from corvus_client import Client

    from .topology import TestNode


@dataclass
class _ClassState:
    """Per-class harness state. Looked up by the conftest hooks."""

    topology: Optional[Topology] = None
    first_failure: Optional[str] = None
    setup_failed: bool = False
    # Reason recorded when class-fixture setup failed. The
    # 'pytest_runtest_setup' hook surfaces this in the per-method
    # skip messages so the developer doesn't have to dig through
    # stderr to find what actually broke.
    setup_error: Optional[str] = None
    base_images_cache: Optional[dict[str, str]] = None


# Keyed by the concrete subclass `type`. Read by both `cases.py` (the
# property accessors below) and `conftest.py` (the pytest hooks).
_CLASS_STATE: dict[type, _ClassState] = {}


def state_for(cls: type) -> _ClassState:
    """Look up (creating if absent) the per-class state record."""
    s = _CLASS_STATE.get(cls)
    if s is None:
        s = _ClassState()
        _CLASS_STATE[cls] = s
    return s


class IntegrationTestCase:
    """Common machinery for class-scoped node tests.

    Subclasses set `NODES` to a tuple of short names. The class
    fixture boots one node per name in declaration order, mints a
    per-class CA (or per-node CAs, see :meth:`_ca_key_for`),
    deploys the cert trio onto each VM, and finally opens an
    mTLS-protected client for every FULL_STACK node.

    Subclasses get `self.nodes` (list[TestNode]) and `self.clients`
    (list[Client]); the concrete N-node bases below add named
    accessors on top.
    """

    # Tuple of short node names. Subclasses MUST override with a
    # non-empty tuple — `IntegrationTestCase` itself isn't a runnable
    # test class.
    NODES: tuple[str, ...] = ()

    # ---- TLS topology hooks ------------------------------------------------
    #
    # Two questions every case answers for the class fixture:
    # 1. "What software stack runs on each node?" (`_node_role`)
    # 2. "Which CA signs each node's cert?"        (`_ca_key_for`)
    #
    # SingleNodeCase + OneDaemonTwoNodesCase keep the default
    # answer for #2 (one shared CA); TwoDaemonsCase overrides
    # both to give every node its own isolated CA.

    def _node_role(self, short_name: str) -> NodeRole:
        """Default: every node runs the full stack. Override
        in subclasses (e.g. OneDaemonTwoNodesCase returns
        AGENTS_ONLY for ``"beta"``)."""

        return NodeRole.FULL_STACK

    def _ca_key_for(self, short_name: str) -> str:
        """Default: every node points at the one ``"shared"`` CA.
        Override in :class:`TwoDaemonsCase` to return *short_name*
        and get one CA per node."""

        return "shared"

    @pytest.fixture(scope="class", autouse=True)
    def _class_topology(self, request, crv, image_ready, host_binary):
        """Bring up the class-scoped Topology + nodes.

        Setup failure is recorded on the class state so the
        `pytest_runtest_setup` hook can skip every method with a
        consistent reason. The partially-booted Topology is intentionally
        kept alive on failure so a developer can `crv vm show <name>`
        and inspect the systemd journal.
        """
        cls = request.cls
        assert cls.NODES, (
            f"{cls.__name__} must set NODES to a non-empty tuple of "
            "short node names (or inherit from SingleNodeCase / "
            "TwoNodesCase / ThreeNodesCase)."
        )
        state = state_for(cls)
        topology: Optional[Topology] = None
        try:
            topology = Topology(
                crv, image_ready, host_binary, class_name=cls.__name__
            ).__enter__()
            # Build the per-class CA(s) first so add() can stash
            # the right CA key on each TestNode. Sorting de-duped
            # keys keeps the init order stable for log output.
            ca_keys = tuple(sorted({self._ca_key_for(name) for name in cls.NODES}))
            topology.init_cas(ca_keys)
            sys.stderr.write(
                f"[harness] booting class topology for "
                f"{cls.__qualname__} ({len(cls.NODES)} node(s), "
                f"{len(ca_keys)} CA(s))\n"
            )
            sys.stderr.flush()
            # Phase 1: bring every VM up. No cert deploys yet —
            # add() just stashes role + ca_key.
            for short_name in cls.NODES:
                topology.add(
                    short_name,
                    role=self._node_role(short_name),
                    ca_key=self._ca_key_for(short_name),
                )
            # Phase 2: mint + push cert trios, enable + start the
            # inner services. Each FULL_STACK node also lands a
            # host-side client cert dir on its TestNode so the
            # subsequent client() open dials over mTLS.
            topology.deploy_certs()
            # Phase 3: eagerly open clients for FULL_STACK nodes
            # so the first test method doesn't pay the
            # daemon-readiness probe in addition to its own
            # assertions. AGENTS_ONLY nodes have nothing to dial.
            for node in topology.nodes:
                if node.role is NodeRole.FULL_STACK:
                    node.client()
            state.topology = topology
        except BaseException as exc:
            import traceback

            state.setup_failed = True
            # Record the actual exception so 'pytest_runtest_setup'
            # can surface it in the per-method skip reason — the
            # legacy behaviour (silently swallow + 'see prior
            # stderr') hid the real problem when stderr was muxed
            # under pytest-xdist or the test was re-run in isolation.
            state.setup_error = f"{type(exc).__name__}: {exc}"
            # Also dump the full traceback to stderr so 'pytest -s'
            # operators see it without having to enable per-test
            # capture.
            sys.stderr.write(f"[harness] {cls.__qualname__} class fixture failed:\n")
            traceback.print_exc(file=sys.stderr)
            sys.stderr.flush()
            # Leak the partial topology for inspection. We can't `raise`
            # — that would surface as ERROR on every test method but
            # we'd lose the skip-on-prior-failure UX we just built in
            # conftest. Instead let the fixture finish; the hook in
            # `pytest_runtest_setup` will skip each method with a
            # clear reason.
            if topology is not None:
                try:
                    topology.finalize(leak_on_failure=True)
                except Exception:
                    pass
                state.topology = None
            yield
            return

        try:
            yield
        finally:
            t = state.topology
            state.topology = None
            if t is not None:
                leak = state.first_failure is not None or state.setup_failed
                try:
                    t.finalize(leak_on_failure=leak)
                except Exception as e:
                    # finalize() raising on the cleanup path is itself a
                    # failure worth surfacing; mark the class as failed
                    # so any subsequent class doesn't get a stale state.
                    sys.stderr.write(
                        f"[harness] finalize failed for {cls.__qualname__}: {e}\n"
                    )
                    sys.stderr.flush()
            # Drop the cached registration map so a fresh run starts clean.
            state.base_images_cache = None
            # Leave `first_failure` set: a subsequent re-collection of
            # the same class (rare; usually pytest creates a new class
            # object per session) would otherwise lose the failure
            # record. The state is reset on the next session anyway.

    # ---- Accessors ----------------------------------------------------------

    @property
    def topology(self) -> Topology:
        """The class-scoped Topology. Raises if setup hasn't run."""
        state = state_for(type(self))
        t = state.topology
        if t is None:
            reason = f"{type(self).__qualname__}: class topology not initialised"
            if state.setup_error:
                reason = f"{reason} (setup error: {state.setup_error})"
            elif state.setup_failed:
                reason = f"{reason} — class fixture failed (no recorded reason)"
            else:
                reason = f"{reason} — did the class fixture run?"
            raise RuntimeError(reason)
        return t

    @property
    def nodes(self) -> list["TestNode"]:
        return self.topology.nodes

    @property
    def clients(self) -> list["Client"]:
        """Clients for every FULL_STACK node. AGENTS_ONLY nodes
        are silently skipped — they have no daemon to dial."""
        return [
            node.client()
            for node in self.topology.nodes
            if node.role is NodeRole.FULL_STACK
        ]

    # ---- Convenience helpers -----------------------------------------------

    def register_base_images(self) -> dict[str, str]:
        """Register the host's pre-baked images with the FIRST node's
        inner daemon. Cached per-class — subsequent calls return the
        same dict without re-registering.

        For multi-node scenarios where every daemon needs the same
        catalogue, call `base_images.register_all` per node directly.
        """
        state = state_for(type(self))
        if state.base_images_cache is not None:
            return state.base_images_cache
        first = self.nodes[0]
        state.base_images_cache = _base_images.register_all(
            first.client(), self.topology.crv, first.name
        )
        return state.base_images_cache

    # ---- Task polling ------------------------------------------------------

    def wait_for_task(
        self,
        client,
        task_id: int,
        *,
        timeout_sec: float = 60.0,
        poll_sec: float = 0.5,
    ):
        """Block until a daemon task transitions out of `running`.

        Used by the async `vm.migrate` / `disks.copy` / `disks.move`
        RPCs (and any other taskId-returning verb). Polls
        ``client.tasks.get(tid).show()`` every ``poll_sec`` seconds
        and returns the final ``TaskInfo``. Raises ``AssertionError``
        if the task ends in ``error`` (the task's ``message`` field
        is surfaced verbatim) or if the wait exceeds ``timeout_sec``.
        """
        deadline = time.monotonic() + timeout_sec
        last_info = None
        while time.monotonic() < deadline:
            last_info = client.tasks.get(task_id).show()
            if last_info.result != "running":
                if last_info.result == "error":
                    raise AssertionError(
                        f"task {task_id} ({last_info.subsystem}/"
                        f"{last_info.command}) failed: "
                        f"{last_info.message or '(no message)'}"
                    )
                return last_info
            time.sleep(poll_sec)
        last_result = last_info.result if last_info else "(no snapshot)"
        raise AssertionError(
            f"task {task_id} did not finish within {timeout_sec}s "
            f"(last result: {last_result})"
        )

    # ---- VM-side shell -----------------------------------------------------

    def vm_shell(
        self,
        vm,
        *,
        node_index: int = 0,
        host_key_path: Path = HOST_ALPINE_KEY_PATH,
        user: str = "corvus",
        vm_tcp_port: Optional[int] = None,
    ) -> VmShell:
        """Build an SSH transport to a vm created by the inner daemon.

        `vm` is the pycapnp `Vm` capability returned by
        `client.vms.create` / `client.vms.get`. Two transports are
        supported:

        * **VSOCK** (default): the vm must already be started and have
          a VSOCK CID allocated. Raises if `vsock_cid` is None
          (usually means the node kernel is missing
          `/dev/vhost-vsock`; tests should catch and skip).
        * **node-side TCP hostfwd** (`vm_tcp_port=<port>`): the vm has
          a SLIRP user-mode NIC with `hostfwd=tcp:127.0.0.1:<port>-:22`
          and the inner SSH leg connects to `127.0.0.1:<port>` on the
          node. No vsock_cid required — useful for cloud-init images
          that don't ship a VSOCK sshd.

        Raises also if the host-side private key file is missing.
        The returned shell must be `close()`d or used as a context
        manager so the node-side ControlMaster is torn down.
        """
        if not host_key_path.exists():
            raise RuntimeError(
                f"SSH private key not found at {host_key_path} — "
                "run `make test-image-alpine` to generate it"
            )
        node = self.nodes[node_index]
        if vm_tcp_port is not None:
            return VmShell(
                node_cid=node.cid,
                vm_tcp_port=vm_tcp_port,
                host_key_path=host_key_path,
                user=user,
            )
        details = vm.show()
        if details.vsock_cid is None:
            raise RuntimeError(
                f"VM {details.name!r} has no vsock CID — node kernel "
                "may be missing /dev/vhost-vsock"
            )
        return VmShell(
            node_cid=node.cid,
            vm_cid=details.vsock_cid,
            host_key_path=host_key_path,
            user=user,
        )


class SingleNodeCase(IntegrationTestCase):
    """One node. Use `self.node` / `self.client`."""

    NODES = ("single",)

    @property
    def node(self) -> "TestNode":
        return self.nodes[0]

    @property
    def client(self) -> "Client":
        return self.node.client()


class OneDaemonTwoNodesCase(IntegrationTestCase):
    """Two nodes, **one** daemon, one CA.

    ``alpha`` runs the daemon + nodeagent + netd; ``beta`` runs
    only the agents. Both nodes are signed by the same CA so
    alpha's daemon can validate beta's ``corvus-node:beta`` /
    ``corvus-netd:beta`` certs on the cross-host dial.

    The test must register beta with alpha's daemon explicitly,
    typically via ``self.client_alpha.nodes.create("beta",
    self.node_beta.outer_ip)``. There is no ``client_beta`` —
    beta runs no daemon to dial.
    """

    NODES = ("alpha", "beta")

    def _node_role(self, short_name: str) -> NodeRole:
        return NodeRole.FULL_STACK if short_name == "alpha" else NodeRole.AGENTS_ONLY

    @property
    def node_alpha(self) -> "TestNode":
        return self.nodes[0]

    @property
    def node_beta(self) -> "TestNode":
        return self.nodes[1]

    @property
    def client_alpha(self) -> "Client":
        return self.node_alpha.client()


class TwoDaemonsCase(IntegrationTestCase):
    """Two nodes, **two** daemons, **two** isolated CAs.

    Each node runs the full daemon + agents stack signed by its
    own CA — alpha's daemon won't trust certs from beta's CA and
    vice-versa. Useful for tests that need to exercise CA
    isolation (e.g. a node from one cluster can't drive a daemon
    in another) or simply two parallel single-node universes.
    """

    NODES = ("alpha", "beta")

    def _ca_key_for(self, short_name: str) -> str:
        # One CA per node — name it after the node itself so the
        # admin store on disk is easy to inspect on a leaked run.
        return short_name

    @property
    def node_alpha(self) -> "TestNode":
        return self.nodes[0]

    @property
    def node_beta(self) -> "TestNode":
        return self.nodes[1]

    @property
    def client_alpha(self) -> "Client":
        return self.node_alpha.client()

    @property
    def client_beta(self) -> "Client":
        return self.node_beta.client()


class ThreeNodesCase(IntegrationTestCase):
    """Three nodes. Use `self.node_alpha`/`beta`/`gamma` and matching `client_*`."""

    NODES = ("alpha", "beta", "gamma")

    @property
    def node_alpha(self) -> "TestNode":
        return self.nodes[0]

    @property
    def node_beta(self) -> "TestNode":
        return self.nodes[1]

    @property
    def node_gamma(self) -> "TestNode":
        return self.nodes[2]

    @property
    def client_alpha(self) -> "Client":
        return self.node_alpha.client()

    @property
    def client_beta(self) -> "Client":
        return self.node_beta.client()

    @property
    def client_gamma(self) -> "Client":
        return self.node_gamma.client()
