"""Async Network manager + Network wrappers."""

from __future__ import annotations

from collections.abc import Iterable

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
        dns_servers: Iterable[str] = (),
        domain: str = "",
        host_dns: bool = True,
    ) -> AsyncNetwork:
        """Create a virtual network.

        Networks are per-node; pass `node=` to pin to a specific
        node by name or id, or omit to let the daemon's scheduler
        place it.

        `dns_servers` (when non-empty) lands as
        `--dhcp-option=option:dns-server,IP1,IP2,...` on the
        dnsmasq the agent spawns, so DHCP clients pick up DNS
        without falling back to whatever their stack defaults to.

        `domain` is the DNS suffix dnsmasq is authoritative for; an
        empty string defaults to the network's name. `host_dns`
        controls whether the agent installs a systemd-resolved
        drop-in on the owner host pointing `*.<domain>` queries at
        the bridge IP.
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
        params.dnsServers = list(dns_servers)
        params.domain = domain
        params.hostDns = host_dns
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
        dns_servers: Iterable[str] | None = None,
        domain: str | None = None,
        host_dns: bool | None = None,
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
        if dns_servers is not None:
            params.hasDnsServers = True
            params.dnsServers = list(dns_servers)
        if domain is not None:
            params.hasDomain = True
            params.domain = domain
        if host_dns is not None:
            params.hasHostDns = True
            params.hostDns = host_dns
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
