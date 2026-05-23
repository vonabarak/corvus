"""Async Network manager + Network wrappers."""

from __future__ import annotations

from .. import _schema
from .._entityref import entity_ref
from ..exceptions import translate_errors
from . import _convert as conv


@translate_errors
class AsyncNetworkManager:
    def __init__(self, daemon):
        self._daemon = daemon
        self._mgr = None

    async def _ensure(self):
        if self._mgr is None:
            self._mgr = (await self._daemon.networks()).mgr
        return self._mgr

    async def list(self):
        mgr = await self._ensure()
        resp = await mgr.list()
        return [conv.network_info(n) for n in resp.networks]

    async def get(self, ref: int | str, *, by_name: bool = False) -> AsyncNetwork:
        mgr = await self._ensure()
        resp = await mgr.get(ref=entity_ref(ref, by_name=by_name))
        return AsyncNetwork(resp.network)

    async def create(
        self,
        name: str,
        subnet: str,
        *,
        node: str | None = None,
        dhcp: bool = False,
        nat: bool = False,
        autostart: bool = False,
    ) -> AsyncNetwork:
        """Create a virtual network.

        Networks are per-node; pass `node=` to pin to a specific
        node by name or id, or omit to let the daemon's scheduler
        place it.
        """
        mgr = await self._ensure()
        params = _schema.network.NetworkCreateParams.new_message()
        params.name = name
        if node is not None:
            params.node = entity_ref(node)
        params.subnet = subnet
        params.dhcp = dhcp
        params.nat = nat
        params.autostart = autostart
        resp = await mgr.create(params=params)
        return AsyncNetwork(resp.network)


@translate_errors
class AsyncNetwork:
    def __init__(self, cap):
        self._cap = cap

    async def show(self):
        resp = await self._cap.show()
        return conv.network_info(resp.info)

    async def start(self) -> None:
        await self._cap.start()

    async def stop(self, *, force: bool = False) -> None:
        await self._cap.stop(force=force)

    async def edit(
        self,
        *,
        name: str | None = None,
        subnet: str | None = None,
        dhcp: bool | None = None,
        nat: bool | None = None,
        autostart: bool | None = None,
    ) -> None:
        params = _schema.network.NetworkEditParams.new_message()
        if name is not None:
            params.hasName = True
            params.name = name
        if subnet is not None:
            params.hasSubnet = True
            params.subnet = subnet
        if dhcp is not None:
            params.hasDhcp = True
            params.dhcp = dhcp
        if nat is not None:
            params.hasNat = True
            params.nat = nat
        if autostart is not None:
            params.hasAutostart = True
            params.autostart = autostart
        await self._cap.edit(params=params)

    async def delete(self) -> None:
        await self._cap.delete()

    async def attach_node(self, node: int | str) -> None:
        """Add a peer node to a multi-node overlay network."""
        params = _schema.network.NetworkPeerParams.new_message()
        params.node = entity_ref(node)
        await self._cap.attachNode(params=params)

    async def detach_node(self, node: int | str) -> None:
        """Remove a peer node from a multi-node overlay network."""
        params = _schema.network.NetworkPeerParams.new_message()
        params.node = entity_ref(node)
        await self._cap.detachNode(params=params)
