"""Disk snapshot lifecycle against the inner daemon.

Mirrors SnapshotIntegrationSpec (see doc/integration-tests-pre-capnp.md).
The new daemon exposes snapshot caps off the per-disk `Disk` cap
(create/list/get) and the per-snapshot `Snapshot` cap (rollback,
merge, delete); VM-scoped snapshot caps are still stubbed and not
needed here.

Most state-mutating tests bracket a stop / snapshot or stop / rollback
cycle inside one `Vm`, opening a fresh SSH session per boot — the
ControlMaster VmSsh keeps alive cannot span a stop/start.
"""

from __future__ import annotations

import secrets

import pytest
from corvus_client import CorvusError, VmMustBeStopped
from corvus_client.types import QuiesceMode
from corvus_test_harness import SingleNodeCase, Vm


def _uniq(stem: str) -> str:
    return f"{stem}-{secrets.token_hex(3)}"


class TestSnapshots(SingleNodeCase):
    """Disk-scoped snapshot CRUD + rollback + merge against the inner
    daemon. Each test creates and tears down its own resources."""

    # ---- pure DB-path tests (no VM boot) ------------------------------------

    def test_snapshot_create_list_delete(self):
        """Create → list → get → delete round-trip on a fresh qcow2."""
        name = _uniq("snap-crud")
        disk = self.client.disks.create(name, size_mb=8, format="qcow2")
        try:
            snap = disk.snapshot_create("only-one")
            assert snap.show().name == "only-one"
            listed = disk.snapshot_list()
            assert [s.name for s in listed] == ["only-one"]
            # Resolve by name and confirm it's the same record.
            via_get = disk.snapshot_get("only-one", by_name=True)
            assert via_get.show().id == snap.show().id
            snap.delete()
            assert disk.snapshot_list() == []
        finally:
            disk.delete()

    def test_duplicate_name_rejected(self):
        """The second `snapshot_create` with the same name on the same
        disk must fail (unique constraint on `(disk_id, name)`)."""
        name = _uniq("snap-dup")
        disk = self.client.disks.create(name, size_mb=8, format="qcow2")
        try:
            first = disk.snapshot_create("same-name")
            try:
                with pytest.raises(CorvusError):
                    disk.snapshot_create("same-name")
                # Only one row exists.
                listed = disk.snapshot_list()
                assert [s.name for s in listed] == ["same-name"]
                # A different name on the same disk still works.
                other = disk.snapshot_create("different-name")
                try:
                    names = {s.name for s in disk.snapshot_list()}
                    assert names == {"same-name", "different-name"}
                finally:
                    other.delete()
            finally:
                first.delete()
        finally:
            disk.delete()

    def test_rapid_sequential_creates(self):
        """Three snapshots in quick succession all land in the DB."""
        name = _uniq("snap-rapid")
        disk = self.client.disks.create(name, size_mb=8, format="qcow2")
        try:
            snaps = [disk.snapshot_create(f"concurrent-{i}") for i in (1, 2, 3)]
            try:
                listed_names = {s.name for s in disk.snapshot_list()}
                assert listed_names == {"concurrent-1", "concurrent-2", "concurrent-3"}
                # IDs are strictly positive and unique.
                ids = [s.show().id for s in snaps]
                assert all(i > 0 for i in ids)
                assert len(set(ids)) == 3
            finally:
                for s in snaps:
                    try:
                        s.delete()
                    except Exception:
                        pass
        finally:
            disk.delete()

    def test_snapshot_create_against_running_vm_uses_live_path(self):
        """`disk.snapshot_create` on a disk attached to a running VM
        now succeeds: the daemon routes the call through QMP
        (``blockdev-snapshot-internal-sync``) instead of refusing
        with `VmMustBeStopped`. The resulting `SnapshotInfo` is
        flagged ``live=True`` so operators can distinguish live
        snapshots from offline ones in `snapshot list`.

        Default quiesce mode is ``auto``: freeze guest filesystems
        via QGA when reachable, else skip. The test VM ships QGA,
        so ``quiesced`` should be True for this path. We assert the
        flag rather than booleans-or-None to catch a future
        regression where the daemon silently downgrades to offline.
        """
        with Vm(self) as vm:
            disk = self.client.disks.get(vm.name)
            snap = disk.snapshot_create("while-running")
            try:
                info = snap.show()
                assert info.name == "while-running"
                assert info.live, (
                    "snapshot taken against a running VM must be flagged "
                    f"live=True, got info={info!r}"
                )
                assert info.quiesced, (
                    "test VM has QGA, so auto-quiesce must have run; "
                    f"got quiesced=False on info={info!r}"
                )
            finally:
                snap.delete()

    def test_snapshot_delete_against_running_vm_uses_live_path(self):
        """`Snapshot.delete` on a snapshot whose disk is attached to a
        running VM now succeeds (QMP
        ``blockdev-snapshot-delete-internal-sync``); previously
        rejected with `VmMustBeStopped`."""
        with Vm(self) as vm:
            disk = self.client.disks.get(vm.name)
            snap = disk.snapshot_create("to-delete")
            # Live delete: the disk is still attached to the running VM.
            snap.delete()
            assert not any(s.name == "to-delete" for s in disk.snapshot_list())

    def test_rollback_rejects_running_vm_without_autostop(self):
        """Rollback has no QMP equivalent. Without ``--auto-stop`` the
        daemon refuses the call when an attached VM is running, with
        the same `VmMustBeStopped` it always raised."""
        with Vm(self) as vm:
            disk = self.client.disks.get(vm.name)
            vm.cap.stop(wait=True)
            snap = disk.snapshot_create("pre-rollback")
            vm.cap.start(wait=True)
            try:
                with pytest.raises(VmMustBeStopped):
                    snap.rollback()
            finally:
                vm.cap.stop(wait=True)
                snap.delete()

    def test_rollback_autostop_cycles_running_vm(self):
        """`Snapshot.rollback(auto_stop=True)` orchestrates a graceful
        VmStop + offline rollback + VmStart when the VM is running.
        End state: VM is running again on the snapshot's disk
        contents, NOT the post-snapshot writes."""
        token = secrets.token_hex(6)
        path = "/home/corvus/autostop-marker.txt"
        with Vm(self) as vm:
            disk = self.client.disks.get(vm.name)

            # Lay down the marker, snapshot the good state.
            with self.vm_shell(vm.cap) as shell:
                shell.wait_ready(timeout_sec=90)
                shell.run(f"echo {token} > {path}")
            vm.cap.stop(wait=True)
            disk.snapshot_create("good")
            vm.cap.start(wait=True)

            # Mutate the marker while running.
            with self.vm_shell(vm.cap) as shell:
                shell.wait_ready(timeout_sec=90)
                shell.run(f"echo MUTATED > {path}")

            # Auto-stop rollback: VM is RUNNING here on purpose; the
            # daemon must cycle it for us.
            disk.snapshot_get("good", by_name=True).rollback(auto_stop=True)

            # The daemon restarted the VM as part of the auto-stop
            # cycle, so the rollback restored the marker AND the VM
            # is up again.
            with self.vm_shell(vm.cap) as shell:
                shell.wait_ready(timeout_sec=90)
                restored = shell.run(f"cat {path}").stdout.strip()
                assert restored == token

    # ---- rollback / merge round-trips ---------------------------------------

    def test_rollback_restores_file(self):
        """Boot, write a marker, stop, snapshot, restart, delete the
        marker (confirm gone), stop, rollback, restart, confirm the
        marker is back. Standard "snapshot a known-good state" flow."""
        token = secrets.token_hex(6)
        path = "/home/corvus/snapshot-marker.txt"
        with Vm(self) as vm:
            disk = self.client.disks.get(vm.name)

            # Phase 1: write the marker.
            with self.vm_shell(vm.cap) as shell:
                shell.wait_ready(timeout_sec=90)
                shell.run(f"echo {token} > {path}")
                got = shell.run(f"cat {path}").stdout.strip()
                assert got == token

            vm.cap.stop(wait=True)
            disk.snapshot_create("good")
            vm.cap.start(wait=True)

            # Phase 2: delete the marker, confirm gone.
            with self.vm_shell(vm.cap) as shell:
                shell.wait_ready(timeout_sec=90)
                shell.run(f"rm {path}")
                missing = shell.run(
                    f"test -f {path} && echo PRESENT || echo MISSING"
                ).stdout.strip()
                assert missing == "MISSING"

            vm.cap.stop(wait=True)
            disk.snapshot_get("good", by_name=True).rollback()
            vm.cap.start(wait=True)

            # Phase 3: rollback restored it.
            with self.vm_shell(vm.cap) as shell:
                shell.wait_ready(timeout_sec=90)
                restored = shell.run(f"cat {path}").stdout.strip()
                assert restored == token

    def test_rollback_specific_snapshot_of_many(self):
        """Three writes interleaved with two snapshots; rollback to
        the FIRST snapshot must restore the value at that time, not
        the second."""
        path = "/home/corvus/snap-history.txt"
        with Vm(self) as vm:
            disk = self.client.disks.get(vm.name)

            with self.vm_shell(vm.cap) as shell:
                shell.wait_ready(timeout_sec=90)
                shell.run(f"echo 1 > {path}")

            vm.cap.stop(wait=True)
            disk.snapshot_create("state-1")
            vm.cap.start(wait=True)

            with self.vm_shell(vm.cap) as shell:
                shell.wait_ready(timeout_sec=90)
                shell.run(f"echo 2 > {path}")

            vm.cap.stop(wait=True)
            disk.snapshot_create("state-2")
            vm.cap.start(wait=True)

            with self.vm_shell(vm.cap) as shell:
                shell.wait_ready(timeout_sec=90)
                shell.run(f"echo 3 > {path}")

            vm.cap.stop(wait=True)
            disk.snapshot_get("state-1", by_name=True).rollback()
            vm.cap.start(wait=True)

            with self.vm_shell(vm.cap) as shell:
                shell.wait_ready(timeout_sec=90)
                got = shell.run(f"cat {path}").stdout.strip()
                assert got == "1", (
                    f"rollback to state-1 should have restored '1'; got {got!r}"
                )

    def test_multi_disk_vm_snapshot_targets_one_disk_only(self):
        """A VM with two attached disks where the operator snapshots
        only the boot disk: the data disk must NOT carry a snapshot
        with the same name. ``Disk.snapshot_create`` operates on a
        single image — there's no implicit fan-out to peer drives —
        but a future refactor that promoted the call to a
        VM-scoped sweep would silently snapshot ALL drives. Pin
        the contract."""
        data_name = _uniq("multi-snap-data")
        self.client.disks.create(data_name, size_mb=8, format="qcow2")
        try:
            with Vm(self) as vm:
                vm.cap.attach_disk(data_name, interface="virtio")
                # `vm.name` is the per-VM root overlay's disk name.
                boot_disk = self.client.disks.get(vm.name)
                data_disk = self.client.disks.get(data_name)

                # Snapshot the boot disk only.
                snap_name = "boot-only"
                snap = boot_disk.snapshot_create(snap_name)
                try:
                    boot_snaps = [s.name for s in boot_disk.snapshot_list()]
                    data_snaps = [s.name for s in data_disk.snapshot_list()]
                    assert snap_name in boot_snaps, boot_snaps
                    assert snap_name not in data_snaps, (
                        f"data disk picked up the snapshot {snap_name!r} "
                        f"even though we only asked the boot disk; "
                        f"data disk snapshots: {data_snaps!r}"
                    )
                finally:
                    snap.delete()
        finally:
            try:
                self.client.disks.get(data_name).delete()
            except Exception:
                pass

    def test_quiesce_require_records_quiesced_flag(self):
        """``snapshot_create(quiesce=QuiesceMode.REQUIRE)`` against a
        running QGA-equipped VM must invoke ``guest-fsfreeze-freeze``
        and surface ``info.quiesced=True``. The default ``auto`` path
        is already covered by
        :test:`test_snapshot_create_against_running_vm_uses_live_path`;
        this test pins the explicit ``REQUIRE`` mode so a refactor
        that quietly downgrades to ``auto`` (or skips fsfreeze on
        any path) flags here.

        ``dmesg | grep fsfreeze`` was considered as an in-guest
        confirmation but the kernel doesn't log ext4 fsfreeze at
        ``info`` level on Alpine's stock build; the daemon's own
        ``quiesced`` flag (set from QGA's ack) is the documented
        operator surface and the right thing to assert."""
        with Vm(self) as vm:
            disk = self.client.disks.get(vm.name)
            snap = disk.snapshot_create("freeze-required", quiesce=QuiesceMode.REQUIRE)
            try:
                info = snap.show()
                assert info.live, info
                assert info.quiesced, (
                    f"explicit quiesce=require did not freeze: info={info!r}"
                )
            finally:
                snap.delete()

    def test_snapshot_merge(self):
        """`Snapshot.merge` consolidates a snapshot into its parent.
        After merge the snapshot row is gone, and the disk's live data
        reflects writes made AFTER the snapshot was taken (the merged
        image is the parent + child deltas), and the snapshot count
        drops by exactly one."""
        path = "/home/corvus/snap-merge.txt"
        with Vm(self) as vm:
            disk = self.client.disks.get(vm.name)

            with self.vm_shell(vm.cap) as shell:
                shell.wait_ready(timeout_sec=90)
                shell.run(f"echo initial > {path}")

            vm.cap.stop(wait=True)
            snap = disk.snapshot_create("to-merge")
            count_before = len(disk.snapshot_list())
            vm.cap.start(wait=True)

            with self.vm_shell(vm.cap) as shell:
                shell.wait_ready(timeout_sec=90)
                shell.run(f"echo modified > {path}")

            vm.cap.stop(wait=True)
            snap.merge()
            count_after = len(disk.snapshot_list())
            assert count_after == count_before - 1, (
                f"snapshot count should drop by exactly 1, got "
                f"{count_before} → {count_after}"
            )
            # The merged snapshot is gone from the list.
            assert not any(s.name == "to-merge" for s in disk.snapshot_list())
            vm.cap.start(wait=True)

            with self.vm_shell(vm.cap) as shell:
                shell.wait_ready(timeout_sec=90)
                got = shell.run(f"cat {path}").stdout.strip()
                # Merge keeps the LATEST disk state — the writes after
                # the snapshot — not the snapshot's frozen content.
                assert got == "modified", (
                    f"merge should preserve post-snapshot writes; got {got!r}"
                )

    # ----------------------------------------------------------------------
    # VM-scoped full-machine snapshots (`crv vm snapshot ...`)
    # ----------------------------------------------------------------------

    def test_vm_snapshot_running_roundtrip(self):
        """``vm.snapshot_create`` / ``snapshot_rollback`` on a running
        VM: write marker → snapshot → overwrite marker → rollback →
        original marker is back.

        Exercises the running-VM rollback path
        (``handleVmstateRollback`` -> QMP ``stop`` -> ``snapshot-load``
        -> ``cont``). The marker is held only in the in-flight
        page cache + on-disk overlay; vmstate captures both, so
        the rollback restores both."""
        token = secrets.token_hex(6)
        path = "/home/corvus/vm-snap-running.txt"
        with Vm(self) as vm:
            with self.vm_shell(vm.cap) as shell:
                shell.wait_ready(timeout_sec=90)
                shell.run(f"echo {token} > {path}")

            info = vm.cap.snapshot_create("cp1")
            try:
                assert info.name == "cp1"
                assert info.disk_count >= 1
                # The carrier disk's name matches the VM's root
                # overlay (the lowest-DriveId disk).
                assert info.carrier_disk.name == vm.name

                # Mutate the marker while running.
                with self.vm_shell(vm.cap) as shell:
                    shell.wait_ready(timeout_sec=90)
                    shell.run(f"echo MUTATED > {path}")

                vm.cap.snapshot_rollback("cp1")

                # Rollback resumes the running guest at the captured
                # state — no separate start. The VM should be
                # running and the marker should be back.
                with self.vm_shell(vm.cap) as shell:
                    shell.wait_ready(timeout_sec=90)
                    got = shell.run(f"cat {path}").stdout.strip()
                    assert got == token, (
                        f"vm snapshot rollback should restore the marker; "
                        f"got {got!r}, expected {token!r}"
                    )
            finally:
                # Cleanup needs the VM running (QMP snapshot-delete).
                try:
                    vm.cap.snapshot_delete("cp1")
                except Exception:
                    pass

    def test_vm_snapshot_stopped_rollback(self):
        """``vm.snapshot_rollback`` on a stopped VM: launches QEMU
        paused, loads vmstate, then resumes — the VM ends running
        at the captured state. This is the paused-start follow-up
        the disk-scoped ``handleVmstateRollback`` previously
        deferred (it required the VM to already be running)."""
        token = secrets.token_hex(6)
        path = "/home/corvus/vm-snap-stopped.txt"
        with Vm(self) as vm:
            with self.vm_shell(vm.cap) as shell:
                shell.wait_ready(timeout_sec=90)
                shell.run(f"echo {token} > {path}")

            vm.cap.snapshot_create("cp1")
            try:
                # Stop the VM after the snapshot. With memory state
                # captured, a stopped-VM rollback should bring it
                # back up at the snapshot.
                vm.cap.stop(wait=True)

                vm.cap.snapshot_rollback("cp1")

                # The rollback left the VM running at the captured
                # state. Confirm via QGA + the marker.
                status = vm.cap.show().status
                assert str(status) == "running", (
                    f"stopped-VM rollback should leave the VM running; "
                    f"got status={status!r}"
                )
                with self.vm_shell(vm.cap) as shell:
                    shell.wait_ready(timeout_sec=90)
                    got = shell.run(f"cat {path}").stdout.strip()
                    assert got == token
            finally:
                try:
                    vm.cap.snapshot_delete("cp1")
                except Exception:
                    pass

    def test_vm_snapshot_uniqueness(self):
        """Creating a second snapshot under the same name must be
        rejected by the validation pre-flight — before any QMP
        traffic, so no orphaned sibling rows linger. The disk-scoped
        ``UniqueSnapshot diskImageId name`` constraint would catch
        the database-level conflict on disk 1 anyway, but the
        VM-scoped validation gives a clearer error message and
        avoids the partial-success-then-rollback shape that would
        otherwise need the agent to roll the QMP job back."""
        with Vm(self) as vm:
            vm.cap.snapshot_create("dup")
            try:
                with pytest.raises(CorvusError) as excinfo:
                    vm.cap.snapshot_create("dup")
                assert "already exists" in str(excinfo.value), excinfo.value
            finally:
                vm.cap.snapshot_delete("dup")

    def test_vm_snapshot_delete_clears_listing(self):
        """After ``snapshot_delete``, the VM-scoped list is empty AND
        the per-disk list shows no sibling rows under the deleted
        name. This catches a regression where vmstate delete fired
        QMP ``snapshot-delete`` successfully but the daemon forgot
        to delete the sibling DB rows — leaving ghost rows that
        would silently fail subsequent rollback attempts."""
        with Vm(self) as vm:
            vm.cap.snapshot_create("to-prune")
            assert any(s.name == "to-prune" for s in vm.cap.snapshot_list())

            vm.cap.snapshot_delete("to-prune")

            assert not any(s.name == "to-prune" for s in vm.cap.snapshot_list())
            # Disk-scoped view shows no row with that name either.
            disk = self.client.disks.get(vm.name)
            assert not any(s.name == "to-prune" for s in disk.snapshot_list())
