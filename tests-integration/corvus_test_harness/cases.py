"""Class-based integration test bases.

Real test classes inherit from `SingleNodeCase`, `TwoNodesCase`, or
`ThreeNodesCase` and get a class-scoped `Topology` for free. The
class fixture brings up one node per name in `NODES`, exposes each
as `self.node[_short_name]` and `self.client[_short_name]`, and
tears everything down at class scope. The pytest hooks in
`tests-integration/conftest.py` add ordering + skip-on-first-failure
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

import subprocess
import sys
from dataclasses import dataclass, field
from pathlib import Path
from typing import TYPE_CHECKING, Optional

import pytest

from . import base_images as _base_images
from .ssh import HOST_ALPINE_KEY_PATH, NodeShell, VmShell
from .topology import Topology

if TYPE_CHECKING:
    from corvus_client import Client

    from .topology import TestNode


@dataclass
class _ClassState:
    """Per-class harness state. Looked up by the conftest hooks."""

    topology: Optional[Topology] = None
    first_failure: Optional[str] = None
    setup_failed: bool = False
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
    fixture boots one node per name in declaration order.
    Subclasses get `self.nodes` (list[TestNode]) and `self.clients`
    (list[Client]); the concrete N-node bases below add named
    accessors on top.
    """

    # Tuple of short node names. Subclasses MUST override with a
    # non-empty tuple — `IntegrationTestCase` itself isn't a runnable
    # test class.
    NODES: tuple[str, ...] = ()

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
            topology = Topology(crv, image_ready, host_binary).__enter__()
            for short_name in cls.NODES:
                topology.add(short_name)
            # Eagerly open clients so the first test method doesn't pay
            # the daemon-readiness probe in addition to its own
            # assertions. Status of every node lands in stderr — useful
            # when running with `pytest -s` and a class has multiple
            # nodes (so you can tell which one is slow).
            sys.stderr.write(
                f"[harness] booting class topology for "
                f"{cls.__qualname__} ({len(cls.NODES)} node(s))\n"
            )
            sys.stderr.flush()
            for node in topology.nodes:
                node.client()  # blocks until inner daemon is up
            state.topology = topology
        except BaseException:
            state.setup_failed = True
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
                        f"[harness] finalize failed for "
                        f"{cls.__qualname__}: {e}\n"
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
        t = state_for(type(self)).topology
        if t is None:
            raise RuntimeError(
                f"{type(self).__qualname__}: class topology not initialised — "
                "did the class fixture fail?"
            )
        return t

    @property
    def nodes(self) -> list["TestNode"]:
        return self.topology.nodes

    @property
    def clients(self) -> list["Client"]:
        return [node.client() for node in self.topology.nodes]

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

    # ---- Node-side shell ---------------------------------------------------

    def node_shell(
        self,
        node_index: int = 0,
        *,
        user: str = "corvus",
        host_key_path: Path = HOST_ALPINE_KEY_PATH,
    ) -> NodeShell:
        """Open a single-leg VSOCK SSH transport to one of the
        topology's nodes — i.e. one of the orchestrator Gentoo VMs
        that hosts an inner Corvus daemon.

        Each call returns a fresh `NodeShell` (a frozen dataclass).
        Reuse the returned object for multiple `.run(...)` calls in
        the same test; for one-shot use, prefer the `run_on_node`
        wrapper below.

          shell = self.node_shell()
          shell.run("mkdir -p /tmp/foo")
          shell.run("echo bar > /tmp/foo/baz")

        `node_index` selects the node by position in `self.NODES`;
        defaults to 0 (the first / only node). Multi-node cases
        (`TwoNodesCase`, `ThreeNodesCase`) pass 0/1/2 explicitly.
        """
        if not host_key_path.exists():
            raise RuntimeError(
                f"SSH private key not found at {host_key_path} — "
                "run `make test-image-key` to generate it"
            )
        node = self.nodes[node_index]
        return NodeShell(
            cid=node.cid,
            user=user,
            key_path=host_key_path,
        )

    def run_on_node(
        self,
        command: str,
        *,
        node_index: int = 0,
        user: str = "corvus",
        host_key_path: Path = HOST_ALPINE_KEY_PATH,
        timeout_sec: float = 60.0,
        check: bool = True,
    ) -> subprocess.CompletedProcess:
        """One-shot wrapper around `node_shell(...).run(...)`. Returns
        the raw `subprocess.CompletedProcess`; `stdout`/`stderr` are
        bytes (decode with `.stdout.decode()`). Set `check=False` to
        let non-zero exits return normally instead of raising.

          out = self.run_on_node("hostname").stdout.decode().strip()
        """
        return self.node_shell(
            node_index=node_index,
            user=user,
            host_key_path=host_key_path,
        ).run(command, timeout_sec=timeout_sec, check=check)

    # ---- VM-side shell -----------------------------------------------------

    def vm_shell(
        self,
        vm,
        *,
        node_index: int = 0,
        host_key_path: Path = HOST_ALPINE_KEY_PATH,
    ) -> VmShell:
        """Build an SSH transport to a vm created by the inner daemon.

        `vm` is the pycapnp `Vm` capability returned by
        `client.vms.create` / `client.vms.get`. It must already be
        started and have a VSOCK CID allocated. Raises if `vsock_cid`
        is None (usually means the node kernel is missing
        `/dev/vhost-vsock`; tests should catch and skip) or the
        host-side private key file is missing (presence on the host
        means the same file is virtiofs-mounted and available inside
        the node too).

        The returned shell must be `close()`d or used as a context
        manager so the node-side ControlMaster is torn down.
        """
        details = vm.show()
        if details.vsock_cid is None:
            raise RuntimeError(
                f"VM {details.name!r} has no vsock CID — node kernel "
                "may be missing /dev/vhost-vsock"
            )
        if not host_key_path.exists():
            raise RuntimeError(
                f"SSH private key not found at {host_key_path} — "
                "run `make test-image-alpine` to generate it"
            )
        node = self.nodes[node_index]
        return VmShell(
            node_cid=node.cid,
            vm_cid=details.vsock_cid,
            host_key_path=host_key_path,
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


class TwoNodesCase(IntegrationTestCase):
    """Two nodes. Use `self.node_alpha`/`self.node_beta` and matching `client_*`."""

    NODES = ("alpha", "beta")

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
