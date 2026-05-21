"""SSH key CRUD."""

from __future__ import annotations

import pytest

from corvus_client import SshKeyNotFound

from ._helpers import with_client


SAMPLE_PUBKEY = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIClvxsxlcOg+G5d9G2qZIDPyAVlcgVuJqJg corvus-test"


def test_ssh_key_create_show_delete(daemon_socket):
    run = with_client(daemon_socket)

    async def go(c):
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
