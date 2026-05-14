"""End-to-end smoke test + VM lifecycle scenarios against the INNER daemon.

Each test in this file runs against a Corvus daemon executing inside
a nested VM (built from the freshly compiled binary on the host). The
outer Corvus only orchestrates the test VM; what we're really testing
is the inner daemon — which runs as root, has nested-KVM access, and
can exercise code paths the outer (unprivileged) deployment cannot.

The smoke test (`test_inner_daemon_reachable`) is the canary: if it
passes, the whole virtiofs + VSOCK transport chain is wired up
correctly and the rest of the suite is meaningful. If it fails, every
other test in this module is broken for an infrastructure reason, not
a Corvus regression.
"""
from __future__ import annotations

import pytest

from corvus_client import VmNotFound


pytestmark = pytest.mark.slow


def test_inner_daemon_reachable(single_client):
    """Smoke test: the inner daemon answers `status()` after first boot.

    Implicitly verifies every layer of the harness:
      - outer Corvus created the VM from our template
      - virtiofs mounted the host stack-install dir at /opt/corvus/bin
      - systemd's corvus-test.service ExecStarted the inner daemon
      - corvus-tcp-relay.service forwarded VSOCK ↔ TCP
      - the host's socat relay bridged the host TCP socket
      - pycapnp completed the bootstrap handshake against the inner
    """
    info = single_client.status()
    assert info.version, "inner daemon returned an empty version string"
    assert info.uptime_seconds >= 0
    assert info.protocol_version > 0


def test_inner_daemon_creates_inner_vm(single_client):
    """The inner daemon can create + list its own VMs.

    Sanity check that the inner Corvus's database (in-VM Postgres) is
    fully wired up and the daemon's mutation path works. We do not
    `start` the inner-of-inner VM here — that would exercise nested
    KVM, which has its own test module.
    """
    inner_vm = single_client.vms.create(
        "doubly-nested",
        cpu_count=1,
        ram_mb=128,
        headless=True,
    )
    details = inner_vm.show()
    assert details.name == "doubly-nested"
    assert details.cpu_count == 1
    assert details.ram_mb == 128
    inner_vm.delete()
    with pytest.raises(VmNotFound):
        single_client.vms.get("doubly-nested")


def test_inner_daemon_edit_persists(single_client):
    """Edits via the inner daemon round-trip through its Postgres."""
    vm = single_client.vms.create(
        "edit-target",
        cpu_count=1,
        ram_mb=256,
        headless=True,
    )
    vm.edit(cpu_count=2, ram_mb=512, description="from python integration tests")
    details = vm.show()
    assert details.cpu_count == 2
    assert details.ram_mb == 512
    assert details.description == "from python integration tests"
    vm.delete()
