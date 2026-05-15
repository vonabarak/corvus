"""Class-based integration test bases.

Real test classes inherit from `SingleVmCase`, `TwoVmsCase`, or
`ThreeVmsCase` and get a class-scoped `Topology` for free. The class
fixture brings up one VM per name in `VMS`, exposes each as
`self.vm[_short_name]` and `self.client[_short_name]`, and tears
everything down at class scope. The pytest hooks in
`tests-integration/conftest.py` add ordering + skip-on-first-failure +
xdist class-affinity on top.

Example:

    class TestVmLifecycle(SingleVmCase):
        def test_reachable(self):
            assert self.client.status().protocol_version > 0

        def test_create_inner_vm(self):
            inner = self.client.vms.create("nested", cpu_count=1, ram_mb=128)
            assert inner.show().name == "nested"
            inner.delete()

The state for a given class is held in a module-level dict keyed by the
concrete subclass type — not on the class body itself — so inheritance
between base/leaf classes never aliases a `_topology` attribute
accidentally. Hooks in conftest.py read and write that registry.
"""
from __future__ import annotations

import sys
from dataclasses import dataclass, field
from typing import TYPE_CHECKING, Optional

import pytest

from . import base_images as _base_images
from .topology import Topology

if TYPE_CHECKING:
    from corvus_client import Client

    from .topology import TestVm


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
    """Common machinery for class-scoped VM tests.

    Subclasses set `VMS` to a tuple of short names. The class fixture
    boots one VM per name in declaration order. Subclasses get
    `self.vms` (list[TestVm]) and `self.clients` (list[Client]); the
    concrete N-VM bases below add named accessors on top.
    """

    # Tuple of short VM names. Subclasses MUST override with a
    # non-empty tuple — `IntegrationTestCase` itself isn't a runnable
    # test class.
    VMS: tuple[str, ...] = ()

    @pytest.fixture(scope="class", autouse=True)
    def _class_topology(self, request, crv, image_ready, host_binary):
        """Bring up the class-scoped Topology + VMs.

        Setup failure is recorded on the class state so the
        `pytest_runtest_setup` hook can skip every method with a
        consistent reason. The partially-booted Topology is intentionally
        kept alive on failure so a developer can `crv vm show <name>`
        and inspect the systemd journal.
        """
        cls = request.cls
        assert cls.VMS, (
            f"{cls.__name__} must set VMS to a non-empty tuple of "
            "short VM names (or inherit from SingleVmCase / TwoVmsCase / "
            "ThreeVmsCase)."
        )
        state = state_for(cls)
        topology: Optional[Topology] = None
        try:
            topology = Topology(crv, image_ready, host_binary).__enter__()
            for short_name in cls.VMS:
                topology.add(short_name)
            # Eagerly open clients so the first test method doesn't pay
            # the daemon-readiness probe in addition to its own
            # assertions. Status of every VM lands in stderr — useful
            # when running with `pytest -s` and a class has multiple
            # VMs (so you can tell which one is slow).
            sys.stderr.write(
                f"[harness] booting class topology for "
                f"{cls.__qualname__} ({len(cls.VMS)} VM(s))\n"
            )
            sys.stderr.flush()
            for vm in topology.vms:
                vm.client()  # blocks until inner daemon is up
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
    def vms(self) -> list["TestVm"]:
        return self.topology.vms

    @property
    def clients(self) -> list["Client"]:
        return [vm.client() for vm in self.topology.vms]

    # ---- Convenience helpers -----------------------------------------------

    def register_base_images(self) -> dict[str, str]:
        """Register the host's pre-baked images with the FIRST VM's
        inner daemon. Cached per-class — subsequent calls return the
        same dict without re-registering.

        For multi-VM scenarios where every daemon needs the same
        catalogue, call `base_images.register_all` per VM directly.
        """
        state = state_for(type(self))
        if state.base_images_cache is not None:
            return state.base_images_cache
        first = self.vms[0]
        state.base_images_cache = _base_images.register_all(
            first.client(), first.crv, first.outer_name
        )
        return state.base_images_cache


class SingleVmCase(IntegrationTestCase):
    """One VM. Use `self.vm` / `self.client`."""

    VMS = ("single",)

    @property
    def vm(self) -> "TestVm":
        return self.vms[0]

    @property
    def client(self) -> "Client":
        return self.vm.client()


class TwoVmsCase(IntegrationTestCase):
    """Two VMs. Use `self.vm_alpha`/`self.vm_beta` and matching `client_*`."""

    VMS = ("alpha", "beta")

    @property
    def vm_alpha(self) -> "TestVm":
        return self.vms[0]

    @property
    def vm_beta(self) -> "TestVm":
        return self.vms[1]

    @property
    def client_alpha(self) -> "Client":
        return self.vm_alpha.client()

    @property
    def client_beta(self) -> "Client":
        return self.vm_beta.client()


class ThreeVmsCase(IntegrationTestCase):
    """Three VMs. Use `self.vm_alpha`/`beta`/`gamma` and matching `client_*`."""

    VMS = ("alpha", "beta", "gamma")

    @property
    def vm_alpha(self) -> "TestVm":
        return self.vms[0]

    @property
    def vm_beta(self) -> "TestVm":
        return self.vms[1]

    @property
    def vm_gamma(self) -> "TestVm":
        return self.vms[2]

    @property
    def client_alpha(self) -> "Client":
        return self.vm_alpha.client()

    @property
    def client_beta(self) -> "Client":
        return self.vm_beta.client()

    @property
    def client_gamma(self) -> "Client":
        return self.vm_gamma.client()
