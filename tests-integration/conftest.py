"""Shared fixtures for the Corvus integration suite.

Fixture lifecycle:

  session   — outer-Corvus reachable, nested-KVM enabled,
              integration-test image applied, host binary located.
              These are global preconditions; failure aborts the run.

  function  — per-test `Topology` context manager. Default builds
              one VM; tests that need more call `topology.add(...)`
              explicitly. Teardown deletes all VMs + relays.

A pair of convenience fixtures wrap the common single-VM case:
  `single_vm`        — a single ready VM (with daemon up)
  `single_client`    — the pycapnp `Client` for the single VM
"""
from __future__ import annotations

import pytest

from corvus_test_harness import (
    Crv,
    HostBinary,
    ImageReady,
    Topology,
    check_nested_kvm,
    check_outer_version,
)


# ---------------------------------------------------------------------------
# Session-scoped preconditions
# ---------------------------------------------------------------------------


@pytest.fixture(scope="session")
def crv() -> Crv:
    """Autodetected `crv` CLI driver (prefers ~/.local/bin/crv)."""
    return Crv.autodetect()


@pytest.fixture(scope="session")
def _nested_kvm_ok():
    status = check_nested_kvm()
    if not status.available:
        pytest.skip(
            f"nested KVM unavailable on this host: {status.reason}. "
            "Integration tests require it; see tests-integration/README.md."
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


# ---------------------------------------------------------------------------
# Per-test topology
# ---------------------------------------------------------------------------


@pytest.fixture
def topology(crv: Crv, image_ready: ImageReady, host_binary: HostBinary):
    """A fresh `Topology` per test. Tests call `.add(...)` to spawn VMs."""
    with Topology(crv, image_ready, host_binary) as t:
        yield t


@pytest.fixture
def single_vm(topology: Topology):
    """One VM, named `single`. Daemon may not be ready until `.client()` is called."""
    return topology.add("single")


@pytest.fixture
def single_client(single_vm):
    """Pycapnp Client to the single VM's inner daemon. Blocks until ready."""
    return single_vm.client()
