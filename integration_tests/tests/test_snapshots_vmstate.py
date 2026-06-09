"""Full-machine (vmstate-aware) snapshot lifecycle.

Exercises the QMP ``snapshot-save`` / ``snapshot-delete`` async-job
path that landed alongside the snapshot subsystem's
``hasVmstate`` extension. The standalone CLI surface is
``crv disk snapshot create <disk> <name> --with-ram``; the
``SnapshotInfo`` row that comes back has ``has_vmstate=True`` on
the carrier disk.

Standalone vmstate ROLLBACK is intentionally gated to a clear
error in this layer — the only consumer that drives
``snapshot-load`` is the build cache in ``cacheMode: memory``,
which has its own bake-VM lifecycle code. This test covers
create + delete; it asserts rollback errors cleanly so a future
regression that silently calls the offline ``qemu-img snapshot
-a`` (which would corrupt a vmstate-aware qcow2) gets caught.
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

    def test_with_ram_rollback_returns_clear_error(self):
        """Standalone rollback of a vmstate-aware snapshot is
        deliberately not wired up in this layer — the build cache
        will drive ``snapshot-load`` through its own bake-VM
        lifecycle code. The handler must return a clear error
        directing the operator at the build-cache path rather
        than silently calling the offline rollback (which would
        corrupt the carrier qcow2 — vmstate references RAM/CPU
        state that no longer matches the running QEMU process)."""
        with Vm(self) as vm:
            disk = self.client.disks.get(vm.name)
            snap = disk.snapshot_create(_uniq("no-rollback"), full_machine=True)
            try:
                with pytest.raises(CorvusError, match="not yet implemented"):
                    snap.rollback()
            finally:
                snap.delete()
