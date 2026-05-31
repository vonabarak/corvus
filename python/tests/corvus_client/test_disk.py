"""Disk CRUD + overlay + clone + snapshot end-to-end against a real daemon."""

from __future__ import annotations

import pytest
from corvus_client import DiskNotFound

from ._helpers import with_client


def test_disk_create_show_delete(daemon_socket):
    run = with_client(daemon_socket)

    async def go(c):
        disk = await c.disks.create("py-disk-1", size_mb=64)
        info = await disk.show()
        assert info.name == "py-disk-1"
        assert info.size_mb == 64
        # disks.get by name and by id both find it
        by_name = await c.disks.get("py-disk-1")
        by_id = await c.disks.get(info.id)
        assert (await by_name.show()).id == info.id
        assert (await by_id.show()).name == "py-disk-1"
        # listing shows our disk
        all_disks = await c.disks.list()
        assert any(d.id == info.id for d in all_disks)
        await disk.delete()
        # After deletion, get raises the typed DiskNotFound exception
        # (translated from the daemon's "Disk not found" message).
        with pytest.raises(DiskNotFound):
            await c.disks.get("py-disk-1")

    run(go)


def test_disk_overlay_and_clone(daemon_socket):
    run = with_client(daemon_socket)

    async def go(c):
        base = await c.disks.create("py-base", size_mb=64)
        base_info = await base.show()

        overlay = await c.disks.create_overlay("py-ovl", backing_disk_ref=base_info.id)
        ovl_info = await overlay.show()
        assert ovl_info.backing_image is not None
        assert ovl_info.backing_image.id == base_info.id

        cloned = await c.disks.clone(source_ref="py-base", new_name="py-clone")
        cloned_info = await cloned.show()
        assert cloned_info.name == "py-clone"

        for d in (overlay, cloned):
            await d.delete()
        await base.delete()

    run(go)


def test_snapshot_create_and_delete(daemon_socket):
    run = with_client(daemon_socket)

    async def go(c):
        disk = await c.disks.create("py-snap-base", size_mb=64)
        s1 = await disk.snapshot_create("first")
        s1_info = await s1.show()
        assert s1_info.name == "first"
        snaps = await disk.snapshot_list()
        assert any(s.name == "first" for s in snaps)
        await s1.delete()
        # Deletion drops it from list
        snaps_after = await disk.snapshot_list()
        assert all(s.name != "first" for s in snaps_after)
        await disk.delete()

    run(go)
