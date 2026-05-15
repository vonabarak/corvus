"""End-to-end smoke test + VM lifecycle scenarios against the INNER daemon.

Each method in this class runs against the same Corvus daemon executing
inside a single nested VM (built from the freshly compiled binary on
the host). The outer Corvus only orchestrates the test VM; what we're
really testing is the inner daemon — which runs as root, has nested-KVM
access, and can exercise code paths the outer (unprivileged) deployment
cannot.

`test_inner_daemon_reachable` is the canary: if it passes, the whole
virtiofs + VSOCK transport chain is wired up correctly and the rest of
the methods in this class are meaningful. If it fails, the suite-wide
skip-on-first-failure machinery skips the rest of the class for a
single coherent reason instead of producing nine spurious failures.
"""
from __future__ import annotations

import pytest

from corvus_client import VmNotFound
from corvus_test_harness import SingleVmCase


pytestmark = pytest.mark.slow


class TestVmLifecycle(SingleVmCase):
    """All methods share one inner daemon. Each creates + deletes its
    own inner-side VMs under unique names, so the methods are mutually
    independent despite sharing the outer VM."""

    def test_inner_daemon_reachable(self):
        """Smoke test: the inner daemon answers `status()` after first boot.

        Implicitly verifies every layer of the harness:
          - outer Corvus created the VM from our template
          - virtiofs mounted the host stack-install dir at /opt/corvus/bin
          - systemd's corvus-test.service ExecStarted the inner daemon
          - corvus-tcp-relay.service forwarded VSOCK ↔ TCP
          - the host's socat relay bridged the host TCP socket
          - pycapnp completed the bootstrap handshake against the inner
        """
        info = self.client.status()
        assert info.version, "inner daemon returned an empty version string"
        assert info.uptime_seconds >= 0
        assert info.protocol_version > 0

    def test_inner_daemon_two_status_calls(self):
        """Two `status()` calls in a row.

        Pinpoint test: if the first works and the second aborts (or
        raises), the bug is "any second RPC call on the Client breaks".
        If both work, any later abort is specific to cap-returning
        calls (e.g. `vms.create`).
        """
        info1 = self.client.status()
        info2 = self.client.status()
        assert info1.version == info2.version
        assert info2.uptime_seconds >= info1.uptime_seconds

    def test_inner_daemon_vms_list(self):
        """`vms.list()` exercises two cap calls but neither passes a
        struct param nor returns a capability:

          - `daemon.vms()` — returns the VmManager cap (no params).
          - `mgr.list()` — returns a List(VmInfo).

        If this works, the abort in `vms.create` is specifically about
        one of: (a) passing a struct param via kwargs, (b) the server
        returning a *new* capability (Vm). If it aborts too, the issue
        is with capability traversal itself.
        """
        vms = self.client.vms.list()
        # Fresh VM, no inner VMs yet on first call. Subsequent methods
        # may leave their own VMs; the class fixture is per-CLASS, not
        # per-method, so the inner DB is shared. Every method below
        # creates + deletes its own under a unique name to avoid
        # interference.
        assert isinstance(vms, list)

    def test_inner_daemon_creates_inner_vm(self):
        """The inner daemon can create + list its own VMs.

        Sanity check that the inner Corvus's database (in-VM Postgres) is
        fully wired up and the daemon's mutation path works. We do not
        `start` the inner-of-inner VM here — that would exercise nested
        KVM, which has its own test module.
        """
        inner_vm = self.client.vms.create(
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
            self.client.vms.get("doubly-nested")

    def test_inner_daemon_edit_noop(self):
        """vm.edit() with no fields set.

        Sends an empty VmEditParams (all `hasX` flags False). Pinpoints
        whether `edit` aborts at the cap-method-with-struct-params level
        regardless of payload content, or only when there's actual data.
        """
        vm = self.client.vms.create(
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

    def test_inner_daemon_edit_after_show(self):
        """vm.show() then vm.edit() — diagnostic: does a no-op cap call
        'warm up' the cap before a struct-parameter call against it?
        """
        vm = self.client.vms.create(
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

    def test_inner_daemon_edit_via_get(self):
        """Create VM, then `get` a fresh Vm cap by name, then edit on it.

        Diagnostic: maybe the abort is specific to the cap returned by
        `vms.create`. Importing the same VM via `vms.get(name)` yields a
        different cap; if edit works on that, the bug is in
        create's-returned-cap path.
        """
        created = self.client.vms.create(
            "edit-via-get",
            cpu_count=1,
            ram_mb=256,
            headless=True,
        )
        name = created.show().name  # use created cap once
        fresh = self.client.vms.get(name)
        fresh.edit(ram_mb=512)
        assert fresh.show().ram_mb == 512
        fresh.delete()

    def test_inner_daemon_edit_persists(self):
        """Edits via the inner daemon round-trip through its Postgres."""
        vm = self.client.vms.create(
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

    def test_alpine_inner_vm_boots_runs_ssh_commands_and_shuts_down(self):
        """Full inner-VM lifecycle with SSH-driven assertions.

        Creates an inner VM whose disk is an overlay on the registered
        Alpine base image, starts it (nested KVM + qemu-guest-agent),
        waits for sshd, runs a few sanity commands over a multiplexed
        SSH connection, then gracefully shuts the VM down via
        `vm.stop()` and removes its disk.
        """
        images = self.register_base_images()
        alpine_disk = images.get("alpine")
        if alpine_disk is None:
            pytest.skip(
                "Alpine base image not registered — "
                "run `make test-image-alpine` to build it"
            )

        overlay_name = "alpine-ssh-target"
        self.client.disks.create_overlay(overlay_name, alpine_disk)
        try:
            vm = self.client.vms.create(
                overlay_name,
                cpu_count=1,
                ram_mb=512,
                headless=True,
                guest_agent=True,
            )
            try:
                # Attach the overlay after create. The `drives=` kwarg
                # on `vms.create` exists in the schema but no Python
                # test exercises it and the daemon's QEMU command
                # comes out empty — likely a pycapnp list-element
                # struct-assignment quirk in `_build_drive_attach`.
                vm.attach_disk(overlay_name, interface="virtio")
                vm.start(wait=True)

                try:
                    shell_ctx = self.inner_ssh(vm)
                except RuntimeError as e:
                    pytest.skip(str(e))

                with shell_ctx as shell:
                    shell.wait_ready(timeout_sec=90)

                    r = shell.run("uname -s")
                    assert r.exit_code == 0
                    assert r.stdout.strip() == "Linux"

                    r = shell.run("cat /etc/os-release")
                    assert "Alpine Linux" in r.stdout

                    r = shell.run("hostname")
                    assert "corvus-test" in r.stdout

                    # Multiplex sanity: many commands should reuse the
                    # ControlMaster connection and complete quickly.
                    for _ in range(5):
                        r = shell.run("true")
                        assert r.exit_code == 0

            finally:
                try:
                    vm.stop(wait=True)
                except Exception:
                    pass
                # delete_disks=True so the overlay we created is removed
                # alongside the VM record.
                vm.delete(delete_disks=True)
        finally:
            # Belt-and-suspenders: if vm.create raised before vm.delete
            # could clean up, the overlay is still in the inner daemon.
            try:
                self.client.disks.get(overlay_name).delete()
            except Exception:
                pass
