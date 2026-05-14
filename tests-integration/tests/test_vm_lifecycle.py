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


def test_inner_daemon_two_status_calls(single_client):
    """Two `status()` calls in a row.

    Pinpoint test: if the first works and the second aborts (or
    raises), the bug is "any second RPC call on a TCP-transport
    Client breaks". If both work, the abort is specific to
    cap-returning calls (e.g. `vms.create`).
    """
    info1 = single_client.status()
    info2 = single_client.status()
    assert info1.version == info2.version
    assert info2.uptime_seconds >= info1.uptime_seconds


def test_inner_daemon_vms_list(single_client):
    """`vms.list()` exercises two cap calls but neither passes a
    struct param nor returns a capability:

      - `daemon.vms()` — returns the VmManager cap (no params).
      - `mgr.list()` — returns a List(VmInfo).

    If this works, the abort in `vms.create` is specifically about
    one of: (a) passing a struct param via kwargs, (b) the server
    returning a *new* capability (Vm). If it aborts too, the issue
    is with capability traversal itself.
    """
    vms = single_client.vms.list()
    # Fresh VM, no inner VMs yet.
    assert isinstance(vms, list)
    assert vms == []


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


def test_inner_daemon_edit_noop(single_client):
    """vm.edit() with no fields set.

    Sends an empty VmEditParams (all `hasX` flags False). Pinpoints
    whether `edit` aborts at the cap-method-with-struct-params level
    regardless of payload content, or only when there's actual data.
    """
    vm = single_client.vms.create(
        "edit-target-noop",
        cpu_count=1,
        ram_mb=256,
        headless=True,
    )
    vm.edit()
    details = vm.show()
    # Nothing changed.
    assert details.cpu_count == 1
    assert details.ram_mb == 256
    vm.delete()


def test_inner_daemon_edit_after_show(single_client):
    """vm.show() then vm.edit() — diagnostic: does a no-op cap call
    'warm up' the cap before a struct-parameter call against it?
    """
    vm = single_client.vms.create(
        "edit-after-show",
        cpu_count=1,
        ram_mb=256,
        headless=True,
    )
    _ = vm.show()
    vm.edit(ram_mb=512)
    details = vm.show()
    assert details.ram_mb == 512
    vm.delete()


def test_inner_daemon_edit_via_get(single_client):
    """Create VM, then `get` a fresh Vm cap by name, then edit on it.

    Diagnostic: maybe the abort is specific to the cap returned by
    `vms.create`. Importing the same VM via `vms.get(name)` yields a
    different cap; if edit works on that, the bug is in
    create's-returned-cap path.
    """
    created = single_client.vms.create(
        "edit-via-get",
        cpu_count=1,
        ram_mb=256,
        headless=True,
    )
    name = created.show().name  # use created cap once
    fresh = single_client.vms.get(name)
    fresh.edit(ram_mb=512)
    assert fresh.show().ram_mb == 512
    fresh.delete()


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
