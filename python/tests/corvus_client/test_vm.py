"""VM manager + Vm cap exercise.

Stays at create/edit/show/delete level. Boot is exercised in the
integration tests (which would require KVM and an OS image — separate
concern; the conftest daemon fixture doesn't depend on that).
"""

from __future__ import annotations

from pathlib import Path
from typing import cast

import pytest
from corvus_client import AsyncClient, VmNotFound, VmNotRunning

from ._helpers import with_client


def test_vm_create_show_edit_delete(daemon_socket: Path) -> None:
    run = with_client(daemon_socket)

    async def go(c: AsyncClient) -> None:
        vm = await c.vms.create(
            "py-vm-1",
            cpu_count=2,
            ram=536870912,
            description="from python",
            headless=True,
            autostart=True,
            graphics_adapter="qxl-vga",
        )
        details = await vm.show()
        assert details.name == "py-vm-1"
        assert details.cpu_count == 2
        assert details.ram == 536870912
        assert details.headless is True
        assert details.tpm is False
        assert details.autostart is True
        assert details.description == "from python"
        assert details.graphics_adapter == "qxl-vga"

        await vm.edit(
            cpu_count=4, ram=1073741824, description=None, graphics_adapter="virtio-vga"
        )
        details2 = await vm.show()
        assert details2.cpu_count == 4
        assert details2.ram == 1073741824
        assert details2.graphics_adapter == "virtio-vga"
        # description=None on edit means "do not change"; description stays
        assert details2.description == "from python"

        listed = await c.vms.list()
        assert any(v.id == details.id for v in listed)
        assert next(v for v in listed if v.id == details.id).tpm is False
        assert (
            next(v for v in listed if v.id == details.id).graphics_adapter
            == "virtio-vga"
        )

        await vm.delete()
        with pytest.raises(VmNotFound):
            await c.vms.get("py-vm-1")

    run(go)


def test_vm_attach_detach_disk(daemon_socket: Path) -> None:
    run = with_client(daemon_socket)

    async def go(c: AsyncClient) -> None:
        disk = await c.disks.create("py-attach-disk", size=67108864)
        vm = await c.vms.create(
            "py-vm-attach", cpu_count=1, ram=268435456, headless=True
        )
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


def test_balloon_validation(daemon_socket: Path) -> None:
    async def go(c: AsyncClient) -> None:
        vm = await c.vms.create(
            "balloon-validation", cpu_count=1, ram=268435456, headless=True
        )
        for target in (0, -1, 2**64):
            with pytest.raises(ValueError):
                await vm.set_balloon(target_bytes=target)
        for invalid_type in (True, 1.5, "128M"):
            with pytest.raises(TypeError):
                await vm.set_balloon(target_bytes=cast(int, invalid_type))
        with pytest.raises(VmNotRunning):
            await vm.set_balloon(target_bytes=128 * 1024**2)
        assert (await vm.show()).ram == 268435456
        await c.ping()
        await vm.delete()

    with_client(daemon_socket)(go)
