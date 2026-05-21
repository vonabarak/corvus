"""VM manager + Vm cap exercise.

Stays at create/edit/show/delete level. Boot is exercised in the
integration tests (which would require KVM and an OS image — separate
concern; the conftest daemon fixture doesn't depend on that).
"""

from __future__ import annotations

import pytest

from corvus_client import VmNotFound

from ._helpers import with_client


def test_vm_create_show_edit_delete(daemon_socket):
    run = with_client(daemon_socket)

    async def go(c):
        vm = await c.vms.create(
            "py-vm-1",
            cpu_count=2,
            ram_mb=512,
            description="from python",
            headless=True,
            autostart=True,
        )
        details = await vm.show()
        assert details.name == "py-vm-1"
        assert details.cpu_count == 2
        assert details.ram_mb == 512
        assert details.headless is True
        assert details.autostart is True
        assert details.description == "from python"

        await vm.edit(cpu_count=4, ram_mb=1024, description=None)
        details2 = await vm.show()
        assert details2.cpu_count == 4
        assert details2.ram_mb == 1024
        # description=None on edit means "do not change"; description stays
        assert details2.description == "from python"

        listed = await c.vms.list()
        assert any(v.id == details.id for v in listed)

        await vm.delete()
        with pytest.raises(VmNotFound):
            await c.vms.get("py-vm-1")

    run(go)


def test_vm_attach_detach_disk(daemon_socket):
    run = with_client(daemon_socket)

    async def go(c):
        disk = await c.disks.create("py-attach-disk", size_mb=64)
        vm = await c.vms.create("py-vm-attach", cpu_count=1, ram_mb=256, headless=True)
        info = await disk.show()
        drive_id = await vm.attach_disk(info.id)
        details = await vm.show()
        assert any(d.id == drive_id for d in details.drives)
        await vm.detach_disk(drive_id)
        details2 = await vm.show()
        assert all(d.id != drive_id for d in details2.drives)
        await vm.delete()
        await disk.delete()

    run(go)
