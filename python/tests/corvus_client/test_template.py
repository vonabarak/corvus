"""Template CRUD + instantiate."""

from __future__ import annotations

from ._helpers import with_client


TEMPLATE_YAML = """\
name: py-tpl
cpuCount: 2
ramMb: 512
headless: true
guestAgent: false
description: minimal template from python tests
drives: []
networkInterfaces: []
sshKeys: []
"""


def test_template_create_show_delete(daemon_socket):
    run = with_client(daemon_socket)

    async def go(c):
        tpl = await c.templates.create(TEMPLATE_YAML)
        details = await tpl.show()
        assert details.name == "py-tpl"
        assert details.cpu_count == 2
        assert details.ram_mb == 512
        assert details.headless is True
        listed = await c.templates.list()
        assert any(t.id == details.id for t in listed)
        await tpl.delete()

    run(go)


def test_template_instantiate(daemon_socket):
    run = with_client(daemon_socket)

    async def go(c):
        tpl = await c.templates.create(TEMPLATE_YAML)
        vm = await tpl.instantiate("py-tpl-vm")
        details = await vm.show()
        assert details.name == "py-tpl-vm"
        assert details.cpu_count == 2
        await vm.delete()
        await tpl.delete()

    run(go)
