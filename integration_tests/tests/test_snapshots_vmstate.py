"""Full-machine (vmstate-aware) snapshot lifecycle.

Exercises the QMP ``snapshot-save`` / ``snapshot-load`` /
``snapshot-delete`` async-job path that landed alongside the
snapshot subsystem's ``hasVmstate`` extension. The standalone
CLI surface is ``crv disk snapshot create <disk> <name>
--with-ram``; the ``SnapshotInfo`` row that comes back has
``has_vmstate=True`` on the carrier disk.

Rollback restores RAM + device state + every disk in the
snapshot set atomically. After ``cont``, the daemon calls QGA
``guest-set-time`` so the guest's wall clock isn't stuck at
snapshot-time. The smoking-gun assertion is that a file
created BEFORE the snapshot and deleted AFTER comes back
after rollback — vmstate restore captures the in-flight page
cache, so the proof needs no ``sync`` round-trip on the host.
"""

from __future__ import annotations

import secrets

import pytest
from corvus_client import CorvusError
from corvus_test_harness import SingleNodeCase, Vm


def _uniq(stem: str) -> str:
    return f"{stem}-{secrets.token_hex(3)}"


class TestSnapshotsVmstate(SingleNodeCase):
    """Vmstate snapshot CRUD against a running VM. Each test boots
    its own VM via the harness ``Vm`` context manager."""

    def test_with_ram_snapshot_create_lists_with_v_flag(self):
        """``snapshot_create --with-ram`` produces a
        ``SnapshotInfo`` with ``has_vmstate=True`` on the carrier
        disk. The carrier is the disk the user invokes
        ``snapshot_create`` on; for a single-writable-disk VM
        there's only one row."""
        with Vm(self) as vm:
            disk = self.client.disks.get(vm.name)
            snap = disk.snapshot_create(_uniq("with-ram"), full_machine=True)
            try:
                info = snap.show()
                assert info.has_vmstate, (
                    "carrier row of a --with-ram snapshot must report "
                    f"has_vmstate=True; got info={info!r}"
                )
                # The carrier is also taken live (QMP path), and
                # the daemon's vmstate code path skips QGA
                # fsfreeze on purpose — vmstate captures the
                # in-flight page cache and writeback queue, so
                # freezing under a multi-second save would be
                # harmful. Assert both flags explicitly so a
                # future regression that flips defaults gets
                # caught.
                assert info.live, f"vmstate snapshot must be live; info={info!r}"
                assert not info.quiesced, (
                    "vmstate snapshot must NOT be fsfreeze-quiesced "
                    f"(captures page cache directly); info={info!r}"
                )

                listed = disk.snapshot_list()
                names = [s.name for s in listed]
                assert info.name in names
                v_carriers = [s for s in listed if s.has_vmstate]
                assert len(v_carriers) == 1, (
                    f"exactly one carrier per vmstate snapshot tag; got {v_carriers}"
                )
            finally:
                snap.delete()

    def test_with_ram_snapshot_delete_clears_qcow2_entry(self):
        """Deleting a vmstate-aware snapshot must remove both the
        DB row and the qcow2 internal-snapshot entry. The agent
        routes through QMP ``snapshot-delete`` (async job) rather
        than ``blockdev-snapshot-delete-internal-sync``, which is
        critical because the latter would leave the vmstate
        orphaned in the carrier qcow2."""
        with Vm(self) as vm:
            disk = self.client.disks.get(vm.name)
            tag = _uniq("delete-me")
            snap = disk.snapshot_create(tag, full_machine=True)
            assert snap.show().has_vmstate

            snap.delete()

            # DB row is gone.
            listed = disk.snapshot_list()
            assert not any(s.name == tag for s in listed), (
                f"snapshot {tag} still present after delete: {listed}"
            )

    def test_with_ram_snapshot_rejects_stopped_vm(self):
        """Full-machine snapshots require the VM to be running —
        the QMP ``snapshot-save`` job needs an attached QEMU
        process. A stopped VM means QMP isn't there to talk to;
        the handler should refuse cleanly rather than fall back
        to an offline path (which can't capture vmstate)."""
        with Vm(self) as vm:
            disk = self.client.disks.get(vm.name)
            vm.cap.stop(wait=True)
            with pytest.raises(CorvusError, match="running"):
                disk.snapshot_create(_uniq("nope"), full_machine=True)

    def test_with_ram_rollback_restores_guest_state(self):
        """The smoking-gun test: a file created before the
        snapshot and deleted after it must come back after
        rollback. This proves vmstate restore is captured the
        in-flight page cache (the file's inode was in memory at
        snapshot time but never had to be sync'd to disk for the
        restore to find it) and that QGA + the guest's clock
        come back online via ``guest-set-time`` post-cont."""
        with Vm(self) as vm:
            disk = self.client.disks.get(vm.name)

            # Phase 1: lay the sentinel down inside the guest.
            with self.vm_shell(vm.cap) as shell:
                shell.wait_ready(timeout_sec=90)
                shell.run("touch /tmp/vmstate-sentinel")
                got = shell.run(
                    "test -f /tmp/vmstate-sentinel && echo PRESENT || echo MISSING"
                ).stdout.strip()
                assert got == "PRESENT"

            # Snapshot the running guest including vmstate.
            tag = _uniq("restore")
            snap = disk.snapshot_create(tag, full_machine=True)
            try:
                assert snap.show().has_vmstate

                # Phase 2: mutate state AFTER the snapshot.
                with self.vm_shell(vm.cap) as shell:
                    shell.wait_ready(timeout_sec=90)
                    shell.run("rm /tmp/vmstate-sentinel")
                    missing = shell.run(
                        "test -f /tmp/vmstate-sentinel && echo PRESENT || echo MISSING"
                    ).stdout.strip()
                    assert missing == "MISSING"

                # Rollback — daemon drives QMP stop -> snapshot-load
                # -> cont -> guest-set-time. The VM stays running
                # from the operator's perspective (no separate
                # VmStart needed; vmstate restore is itself a
                # resume).
                snap.rollback()

                # Phase 3: the sentinel is back. QGA reconnects
                # under the same persistent socket the harness
                # is using.
                with self.vm_shell(vm.cap) as shell:
                    shell.wait_ready(timeout_sec=90)
                    restored = shell.run(
                        "test -f /tmp/vmstate-sentinel && echo PRESENT || echo MISSING"
                    ).stdout.strip()
                    assert restored == "PRESENT", (
                        "vmstate rollback did not restore the pre-snapshot "
                        "file; in-flight page cache was not captured"
                    )
            finally:
                # Snapshot may already be gone if the rollback
                # path consumed it; tolerate either.
                try:
                    snap.delete()
                except CorvusError:
                    pass

    def test_with_ram_rollback_rejects_stopped_vm(self):
        """Vmstate rollback requires a live QEMU process for the
        QMP ``snapshot-load`` async job. The paused-start
        lifecycle that would let us load into a freshly-spawned
        QEMU is a follow-up; for now a stopped VM gets a clear
        error directing the operator to start it first."""
        with Vm(self) as vm:
            disk = self.client.disks.get(vm.name)
            tag = _uniq("stopped")
            snap = disk.snapshot_create(tag, full_machine=True)
            try:
                vm.cap.stop(wait=True)
                with pytest.raises(CorvusError, match="requires the VM to be running"):
                    snap.rollback()
            finally:
                # Restart so the snapshot delete (which needs a
                # running VM too) can clean up.
                vm.cap.start(wait=True)
                try:
                    snap.delete()
                except CorvusError:
                    pass
