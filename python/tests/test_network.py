"""Network CRUD."""
from __future__ import annotations

import pytest

from corvus_client import NetworkNotFound

from ._helpers import with_client


def test_network_create_edit_delete(daemon_socket):
    run = with_client(daemon_socket)

    async def go(c):
        net = await c.networks.create("py-net-1", subnet="10.66.0.0/24", dhcp=True)
        info = await net.show()
        assert info.name == "py-net-1"
        assert info.subnet == "10.66.0.0/24"
        assert info.dhcp is True
        await net.edit(nat=True)
        info2 = await net.show()
        assert info2.nat is True
        await net.delete()
        with pytest.raises(NetworkNotFound):
            await c.networks.get("py-net-1")

    run(go)
