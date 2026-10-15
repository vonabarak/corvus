"""SSH key CRUD."""

from __future__ import annotations

from pathlib import Path

import pytest
from corvus_client import AsyncClient, SshKeyNotFound

from ._helpers import with_client

SAMPLE_PUBKEY = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIClvxsxlcOg+G5d9G2qZIDPyAVlcgVuJqJg corvus-test"


def test_ssh_key_create_show_delete(daemon_socket: Path) -> None:
    run = with_client(daemon_socket)

    async def go(c: AsyncClient) -> None:
        key = await c.ssh_keys.create("py-key-1", SAMPLE_PUBKEY)
        info = await key.show()
        assert info.name == "py-key-1"
        assert info.public_key == SAMPLE_PUBKEY
        all_keys = await c.ssh_keys.list()
        assert any(k.id == info.id for k in all_keys)
        await key.delete()
        with pytest.raises(SshKeyNotFound):
            await c.ssh_keys.get("py-key-1")

    run(go)


def test_inject_stopped_vm_and_invalid_username(daemon_socket: Path) -> None:
    from corvus_client import ServerError, VmNotFound, VmNotRunning

    async def go(c: AsyncClient) -> None:
        vm = await c.vms.create("py-inject-stopped", cpu_count=1, ram=67108864)
        info = await vm.show()
        key = await c.ssh_keys.create("py-inject-key", SAMPLE_PUBKEY)
        await vm.attach_ssh_key("py-inject-key")
        assert [item.name for item in await vm.list_ssh_keys()] == ["py-inject-key"]
        refs: list[int | str] = [info.id, str(info.id), info.name]
        for ref in refs:
            with pytest.raises(VmNotRunning):
                await c.ssh_keys.inject(ref, "root")
        with pytest.raises(VmNotRunning):
            await c.ssh_keys.inject(info.name, "root", reset=True, by_name=True)
        with pytest.raises(ServerError, match="Guest username"):
            await c.ssh_keys.inject(info.id, "")
        with pytest.raises(VmNotFound):
            await c.ssh_keys.inject("missing-injection-vm", "root")
        await vm.detach_ssh_key("py-inject-key")
        await key.delete()
        await vm.delete()

    with_client(daemon_socket)(go)
