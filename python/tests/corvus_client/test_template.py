"""Template CRUD + instantiate."""

from __future__ import annotations

from pathlib import Path

from corvus_client import AsyncClient

from ._helpers import with_client

TEMPLATE_YAML = """\
name: py-tpl
cpuCount: 2
ram: 512M
headless: true
guestAgent: false
tpm: false
description: minimal template from python tests
drives: []
networkInterfaces: []
sshKeys: []
"""


def test_template_create_show_delete(daemon_socket: Path) -> None:
    run = with_client(daemon_socket)

    async def go(c: AsyncClient) -> None:
        tpl = await c.templates.create(TEMPLATE_YAML)
        details = await tpl.show()
        assert details.name == "py-tpl"
        assert details.cpu_count == 2
        assert details.ram == 536870912
        assert details.headless is True
        assert details.tpm is False
        listed = await c.templates.list()
        assert any(t.id == details.id for t in listed)
        await tpl.delete()

    run(go)


def test_template_instantiate(daemon_socket: Path) -> None:
    run = with_client(daemon_socket)

    async def go(c: AsyncClient) -> None:
        tpl = await c.templates.create(TEMPLATE_YAML)
        vm = await tpl.instantiate("py-tpl-vm")
        details = await vm.show()
        assert details.name == "py-tpl-vm"
        assert details.cpu_count == 2
        assert details.tpm is False
        await vm.delete()
        await tpl.delete()

    run(go)


def test_floating_tags_and_pinned_ids(daemon_socket: Path) -> None:
    run = with_client(daemon_socket)

    async def go(c: AsyncClient) -> None:
        old = await c.disks.create("template-source:v1", size=1048576)
        old_id = (await old.show()).id
        templates = []
        vms = []
        new = None
        try:
            for label, selector in [
                ("floating", "template-source"),
                ("tagged", "template-source:v1"),
                ("pinned", old_id),
            ]:
                templates.append(
                    await c.templates.create(
                        f"name: {label}\ncpuCount: 1\nram: 128M\ndrives:\n  - diskImage: {selector}\n    strategy: overlay\n    interface: virtio\n"
                    )
                )
            before = await templates[0].instantiate("before-update")
            vms.append(before)
            new = await c.disks.create("template-source:v2", size=2097152)
            new_id = (await new.show()).id
            for template, expected in zip(
                templates, [new_id, old_id, old_id], strict=True
            ):
                details = await template.show()
                assert details.drives[0].disk_image is not None
                assert details.drives[0].disk_image.id == expected
                vm = await template.instantiate(details.name + "-vm")
                vms.append(vm)
                drive = (await vm.show()).drives[0]
                assert drive.disk_image is not None
                overlay = await c.disks.get(drive.disk_image.id)
                backing = (await overlay.show()).backing_image
                assert backing is not None and backing.id == expected
            first_drive = (await before.show()).drives[0]
            assert first_drive.disk_image is not None
            first_image = await c.disks.get(first_drive.disk_image.id)
            first_backing = (await first_image.show()).backing_image
            assert first_backing is not None and first_backing.id == old_id
            assert (await templates[0].show()).drives[
                0
            ].disk_selector == "template-source:latest"
            assert (await templates[2].show()).drives[0].disk_selector == old_id
        finally:
            for vm in reversed(vms):
                await vm.delete()
            for template in templates:
                await template.delete()
            if new is not None:
                await new.delete()
            await old.delete()

    run(go)
