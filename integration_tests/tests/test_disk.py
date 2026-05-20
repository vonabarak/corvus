"""Disk subsystem against the inner daemon.

Mirrors DiskIntegrationSpec (see doc/integration-tests-pre-capnp.md).
The inner daemon's `basePath` is private to the node's filesystem,
so each test creates and deletes its own qcow2s without any host-side
filesystem hacks (the pre-capnp `withSystemTempDirectory` + `HOME`
swap is gone).
"""
from __future__ import annotations

import secrets

import pytest

from corvus_client import VmMustBeStopped
from corvus_test_harness import Vm, VmSsh, SingleNodeCase


pytestmark = pytest.mark.slow


def _uniq(stem: str) -> str:
    """6-hex-char suffix to keep concurrent runs / leak hunts trivial."""
    return f"{stem}-{secrets.token_hex(3)}"


class TestDisk(SingleNodeCase):
    """Disk CRUD + clone + overlay + rebase + import + hot-plug + resize.

    All tests share one node + one inner daemon via
    `SingleNodeCase`. Each test creates its own disks under a unique
    name; teardown deletes them (best-effort) so a class run leaves
    no orphans even on intermediate failures.
    """

    def _delete_silent(self, name: str) -> None:
        try:
            self.client.disks.get(name).delete()
        except Exception:
            pass

    # ---- create / show / delete --------------------------------------------

    def test_disk_create_and_delete(self):
        name = _uniq("crud")
        disk = self.client.disks.create(name, size_mb=16, format="qcow2")
        try:
            info = disk.show()
            assert info.name == name
            assert info.format == "qcow2"
            assert info.size_mb == 16
            assert info.backing_image_id is None
            # Listed by the manager.
            assert any(d.name == name for d in self.client.disks.list())
        finally:
            disk.delete()
        # After delete, the disk is gone.
        names = {d.name for d in self.client.disks.list()}
        assert name not in names

    # ---- clone --------------------------------------------------------------

    def test_clone_preserves_size_and_format(self):
        src_name = _uniq("clone-src")
        clone_name = _uniq("clone-dst")
        self.client.disks.create(src_name, size_mb=8, format="qcow2")
        try:
            clone = self.client.disks.clone(src_name, clone_name)
            try:
                info = clone.show()
                assert info.name == clone_name
                assert info.format == "qcow2"
                # qcow2 is sparse, so the daemon reports the virtual
                # size — exactly equal to the source's.
                assert info.size_mb == 8
                assert info.backing_image_id is None
            finally:
                clone.delete()
        finally:
            self._delete_silent(src_name)

    def test_clone_preserves_snapshots(self):
        src_name = _uniq("clone-snap-src")
        clone_name = _uniq("clone-snap-dst")
        src = self.client.disks.create(src_name, size_mb=8, format="qcow2")
        try:
            src.snapshot_create("snap1")
            clone = self.client.disks.clone(src_name, clone_name)
            try:
                snaps = clone.snapshot_list()
                assert [s.name for s in snaps] == ["snap1"]
            finally:
                clone.delete()
        finally:
            self._delete_silent(src_name)

    def test_clone_rejects_running_vm(self):
        """Cloning a disk attached to a running VM must fail with
        `VmMustBeStopped`. The disk is the Alpine vm's overlay,
        which is attached and busy for the lifetime of the `with`."""
        with Vm(self) as vm:
            with pytest.raises(VmMustBeStopped):
                self.client.disks.clone(vm.name, _uniq("clone-while-running"))

    def test_clone_to_custom_path(self):
        """`disks.clone(..., path=...)` writes the clone to an
        explicit filesystem location. The path is in the daemon's
        view (i.e. inside the node); we don't ssh in to
        stat the file from the host. Instead we verify the daemon
        records the custom path in `file_path` on the new disk."""
        src_name = _uniq("clone-cp-src")
        clone_name = _uniq("clone-cp-dst")
        # Relative path → resolved against the inner daemon's
        # basePath. Pre-capnp used absolute /tmp paths; relative
        # keeps us in the daemon's namespace and avoids leaking
        # state into the VM's /tmp.
        custom_rel = f"{clone_name}-at-custom.qcow2"
        self.client.disks.create(src_name, size_mb=8, format="qcow2")
        try:
            clone = self.client.disks.clone(
                src_name, clone_name, path=custom_rel
            )
            try:
                info = clone.show()
                assert info.name == clone_name
                # The daemon records the actual on-disk path on
                # each node the image lives on (Phase 3 moved
                # 'file_path' off DiskImageInfo and onto a per-node
                # 'DiskImagePlacement' list). For a fresh clone
                # there's exactly one placement on the bake VM's
                # node; that placement's 'file_path' must end in
                # our custom suffix — NOT the default
                # '<basePath>/<clone_name>.qcow2'.
                assert info.placements, (
                    f"clone has no recorded placement: {info!r}"
                )
                file_paths = [p.file_path for p in info.placements]
                assert any(fp.endswith(custom_rel) for fp in file_paths), (
                    f"clone landed at default location, not custom path: "
                    f"{file_paths!r}"
                )
                # Sanity: the path is distinct from what the
                # default would have been.
                default_suffix = f"/{clone_name}.qcow2"
                assert not any(fp.endswith(default_suffix) for fp in file_paths), (
                    f"file_path matches the default location, not the custom one: "
                    f"{file_paths!r}"
                )
            finally:
                clone.delete()
        finally:
            self._delete_silent(src_name)

    # ---- overlay ------------------------------------------------------------

    def test_create_overlay_has_backing(self):
        base_name = _uniq("ov-base")
        overlay_name = _uniq("ov-top")
        base = self.client.disks.create(base_name, size_mb=8, format="qcow2")
        try:
            overlay = self.client.disks.create_overlay(overlay_name, base_name)
            try:
                info = overlay.show()
                assert info.name == overlay_name
                assert info.backing_image_id == base.show().id
                assert info.backing_image_name == base_name
                # Overlay inherits the base's virtual size.
                assert info.size_mb == 8
            finally:
                overlay.delete()
        finally:
            self._delete_silent(base_name)

    # ---- rebase -------------------------------------------------------------

    def test_rebase_to_other_base(self):
        """Rebase an overlay from baseA to baseB and confirm the
        backing pointer follows. Both bases are independent 8 MB qcow2s
        so the rebase is `qemu-img rebase`-fast."""
        base_a = _uniq("rebase-a")
        base_b = _uniq("rebase-b")
        overlay = _uniq("rebase-top")
        a = self.client.disks.create(base_a, size_mb=8, format="qcow2")
        b = self.client.disks.create(base_b, size_mb=8, format="qcow2")
        try:
            ov = self.client.disks.create_overlay(overlay, base_a)
            try:
                assert ov.show().backing_image_name == base_a
                self.client.disks.rebase(overlay, base_b)
                assert ov.show().backing_image_name == base_b
                assert ov.show().backing_image_id == b.show().id
            finally:
                ov.delete()
        finally:
            self._delete_silent(base_a)
            self._delete_silent(base_b)

    def test_rebase_flatten_drops_backing(self):
        """`disks.flatten(overlay)` consolidates the overlay's delta
        with its backing image(s) into a single standalone qcow2.
        Afterwards `disk.show()` must report no backing image."""
        base_name = _uniq("flatten-base")
        overlay_name = _uniq("flatten-top")
        base = self.client.disks.create(base_name, size_mb=8, format="qcow2")
        try:
            overlay = self.client.disks.create_overlay(overlay_name, base_name)
            try:
                # Sanity: overlay starts with a backing pointer.
                info = overlay.show()
                assert info.backing_image_id == base.show().id
                assert info.backing_image_name == base_name

                self.client.disks.flatten(overlay_name)

                info = overlay.show()
                assert info.backing_image_id is None, (
                    f"flatten left a backing pointer: {info!r}"
                )
                assert info.backing_image_name is None, (
                    f"flatten left a backing name: {info!r}"
                )
                # Same disk id, same name — only the backing fields changed.
                assert info.name == overlay_name
            finally:
                overlay.delete()
        finally:
            self._delete_silent(base_name)

    # ---- import -------------------------------------------------------------

    def test_import_local_file_copies(self):
        """Register a daemon-owned file, then `import_` it under a new
        name pointing at the registered file's on-disk path. The import
        copies (canonicalised dest != src), and we get a fresh disk
        record with a distinct file."""
        src_name = _uniq("import-src")
        copy_name = _uniq("import-copy")
        src = self.client.disks.create(src_name, size_mb=4, format="qcow2")
        try:
            src_path = src.show().file_path
            copy = self.client.disks.import_(copy_name, src_path, format="qcow2")
            try:
                info = copy.show()
                assert info.name == copy_name
                assert info.format == "qcow2"
                # New disk lives at a different path than the source.
                assert info.file_path != src_path
            finally:
                copy.delete()
        finally:
            self._delete_silent(src_name)

    def test_import_same_path_rejected(self):
        """If the import name resolves to the same canonical path as
        the source file, the daemon refuses with a 'Source and
        destination paths are the same' error (Handlers/Disk/Import.hs:145)."""
        name = _uniq("import-collide")
        disk = self.client.disks.create(name, size_mb=4, format="qcow2")
        try:
            src_path = disk.show().file_path
            with pytest.raises(Exception, match="(?i)same"):
                # Re-importing under the same name targets the same
                # `<basePath>/<name>.qcow2` destination — canonicalised
                # source and dest collide.
                self.client.disks.import_(name, src_path, format="qcow2")
        finally:
            disk.delete()

    # ---- hot-plug attach / detach -------------------------------------------

    def test_hot_attach_detach_reattach(self):
        """Boot an Alpine guest, create a fresh data disk, attach it
        over virtio, see the kernel pick up `/dev/vd*`, detach,
        re-attach (regression check that the qcow2 lock is released
        cleanly), and finally clean up.

        The bookkeeping the daemon does is asserted via `vm.show()`'s
        `drives` list; the guest-visible effect is asserted via SSH.
        """
        data_disk = _uniq("hotplug-data")
        self.client.disks.create(data_disk, size_mb=32, format="qcow2")
        try:
            with VmSsh(self) as vm:
                before = self._guest_vd_count(vm)
                drive_id = vm.cap.attach_disk(data_disk, interface="virtio")
                try:
                    # Daemon records the drive.
                    drives = vm.cap.show().drives
                    assert any(d.id == drive_id for d in drives), drives
                    # Guest kernel sees a new /dev/vd*.
                    self._wait_vd_count(vm, before + 1)

                    vm.cap.detach_disk(drive_id)
                    self._wait_vd_count(vm, before)
                    # Daemon updated its drive list.
                    drives = vm.cap.show().drives
                    assert not any(d.id == drive_id for d in drives), drives

                    # Re-attach: succeeds → the qcow2 lock was released
                    # by the detach. Drive ID is fresh.
                    new_drive_id = vm.cap.attach_disk(
                        data_disk, interface="virtio"
                    )
                    assert new_drive_id != drive_id
                    self._wait_vd_count(vm, before + 1)
                    vm.cap.detach_disk(new_drive_id)
                finally:
                    # Defence-in-depth: if assertions raised mid-flight,
                    # peel the drive off so the disk delete below
                    # doesn't trip 'Disk in use'.
                    try:
                        vm.cap.detach_disk_by_name(data_disk)
                    except Exception:
                        pass
        finally:
            self._delete_silent(data_disk)

    def test_hot_attach_read_only(self):
        """`attach_disk(..., read_only=True)` surfaces as `read_only=True`
        on the matching drive in `vm.show()`."""
        data_disk = _uniq("hotplug-ro")
        self.client.disks.create(data_disk, size_mb=16, format="qcow2")
        try:
            with VmSsh(self) as vm:
                drive_id = vm.cap.attach_disk(
                    data_disk, interface="virtio", read_only=True
                )
                try:
                    drives = vm.cap.show().drives
                    matching = [d for d in drives if d.id == drive_id]
                    assert matching, drives
                    assert matching[0].read_only is True
                finally:
                    try:
                        vm.cap.detach_disk(drive_id)
                    except Exception:
                        pass
        finally:
            self._delete_silent(data_disk)

    # ---- resize -------------------------------------------------------------

    def test_resize_grows_disk(self):
        name = _uniq("resize")
        disk = self.client.disks.create(name, size_mb=8, format="qcow2")
        try:
            assert disk.show().size_mb == 8
            disk.resize(16)
            # `resize` only widens; show should reflect the new virtual
            # size synchronously after the cap returns.
            assert disk.show().size_mb == 16
        finally:
            disk.delete()

    # ---- helpers -----------------------------------------------------------

    def _guest_vd_count(self, vm: "VmSsh") -> int:
        """Count of /dev/vd* block devices the guest kernel exposes."""
        out = vm.run("ls /dev/vd* 2>/dev/null | wc -l").stdout.strip()
        return int(out or "0")

    def _wait_vd_count(
        self,
        vm: "VmSsh",
        target: int,
        *,
        timeout_sec: float = 10.0,
    ) -> None:
        """Block briefly for udev to settle after a hot-(de)attach;
        the kernel surfaces the new virtio-blk device within a second
        or two but we leave slack for busy nested-KVM hosts."""
        import time
        deadline = time.monotonic() + timeout_sec
        last = -1
        while time.monotonic() < deadline:
            last = self._guest_vd_count(vm)
            if last == target:
                return
            time.sleep(0.5)
        raise AssertionError(
            f"guest /dev/vd* count stuck at {last}, expected {target}"
        )
