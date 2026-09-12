"""CD-ROM media eject/change against a real daemon (stopped-VM paths).

The conftest daemon fixture has no KVM, so only the stopped-VM
(DB-only) paths are exercised here — live QMP eject/change is covered
by the integration tests.
"""

from __future__ import annotations

import pytest
from corvus_client import CorvusError

from ._helpers import with_client


def _drive(details, drive_id):
    for d in details.drives:
        if d.id == drive_id:
            return d
    raise AssertionError(f"drive {drive_id} not in vm.show().drives")


def test_media_eject_and_change_stopped_vm(daemon_socket):
    run = with_client(daemon_socket)

    async def go(c):
        disk_a = await c.disks.create("py-cd-a", size_mb=16)
        disk_b = await c.disks.create("py-cd-b", size_mb=16)
        vm = await c.vms.create("py-vm-media", cpu_count=1, ram_mb=256, headless=True)
        try:
            a = await disk_a.show()
            b = await disk_b.show()
            drive_id = await vm.attach_disk(a.id, media="cdrom")

            d = _drive(await vm.show(), drive_id)
            assert d.media == "cdrom"
            assert d.disk_image is not None
            assert d.disk_image.id == a.id

            # Eject: tray empties, the drive row's disk_image clears.
            await c.disks.eject_media(drive_id)
            d2 = _drive(await vm.show(), drive_id)
            assert d2.disk_image is None

            # Change from an empty tray repoints the drive at the new
            # disk; the tray stays a CD-ROM.
            await c.disks.change_media(drive_id, b.id)
            d3 = _drive(await vm.show(), drive_id)
            assert d3.media == "cdrom"
            assert d3.disk_image is not None
            assert d3.disk_image.id == b.id

            await vm.detach_disk(drive_id)
        finally:
            await vm.delete()
            await disk_a.delete()
            await disk_b.delete()

    run(go)


def test_media_eject_rejected_on_plain_disk(daemon_socket):
    run = with_client(daemon_socket)

    async def go(c):
        disk = await c.disks.create("py-plain-disk", size_mb=16)
        vm = await c.vms.create(
            "py-vm-media-reject", cpu_count=1, ram_mb=256, headless=True
        )
        try:
            info = await disk.show()
            drive_id = await vm.attach_disk(info.id)
            with pytest.raises(CorvusError):
                await c.disks.eject_media(drive_id)
            with pytest.raises(CorvusError):
                await c.disks.change_media(drive_id, info.id)
            await vm.detach_disk(drive_id)
        finally:
            await vm.delete()
            await disk.delete()

    run(go)
