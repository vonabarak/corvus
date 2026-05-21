"""Async Node manager + Node wrappers.

Mirrors the @crv node@ subcommand surface. A registered ``Node`` is
the daemon's record of one host it orchestrates; per-node agent
endpoints plus the agent-pushed capacity snapshot live on each row.
"""

from __future__ import annotations

from .. import _schema
from .._entityref import entity_ref
from ..exceptions import translate_errors
from . import _convert as conv


@translate_errors
class AsyncNodeManager:
    def __init__(self, daemon):
        self._daemon = daemon
        self._mgr = None

    async def _ensure(self):
        if self._mgr is None:
            self._mgr = (await self._daemon.nodes()).mgr
        return self._mgr

    async def list(self):
        """List every registered node."""
        mgr = await self._ensure()
        resp = await mgr.list()
        return [conv.node_info(n) for n in resp.nodes]

    async def get(self, ref: int | str, *, by_name: bool = False) -> AsyncNode:
        mgr = await self._ensure()
        resp = await mgr.get(ref=entity_ref(ref, by_name=by_name))
        return AsyncNode(resp.node)

    async def create(
        self,
        name: str,
        host: str,
        *,
        node_agent_port: int = 9878,
        net_agent_port: int = 9877,
        base_path: str = "/home/corvus/VMs",
        description: str | None = None,
        admin_state: str = "online",
    ) -> AsyncNode:
        """Register a new node.

        ``host`` is the IPv4 / IPv6 / hostname the daemon will dial.
        ``node_agent_port`` defaults to 9878 (corvus-nodeagent);
        ``net_agent_port`` defaults to 9877 (corvus-netd). The
        daemon spawns the per-node reconnect supervisor as soon as
        this RPC returns successfully — the new node is live without
        a daemon restart.
        """
        mgr = await self._ensure()
        params = _schema.node.NodeAddParams.new_message()
        params.name = name
        params.host = host
        params.nodeAgentPort = node_agent_port
        params.netAgentPort = net_agent_port
        params.basePath = base_path
        if description is not None:
            params.description = description
        params.adminState = admin_state
        resp = await mgr.create(params=params)
        return AsyncNode(resp.node)


@translate_errors
class AsyncNode:
    def __init__(self, cap):
        self._cap = cap

    async def show(self):
        resp = await self._cap.show()
        return conv.node_details(resp.details)

    async def edit(
        self,
        *,
        name: str | None = None,
        host: str | None = None,
        node_agent_port: int | None = None,
        net_agent_port: int | None = None,
        base_path: str | None = None,
        description: str | None = None,
        admin_state: str | None = None,
    ) -> None:
        """Mutate a subset of fields. Pass ``None`` (the default) to
        leave a field unchanged. Pass an empty string for
        ``description`` to clear it. Changing ``host`` /
        ``node_agent_port`` / ``net_agent_port`` triggers a daemon-side
        supervisor respawn against the new endpoint.
        """
        params = _schema.node.NodeEditParams.new_message()
        if name is not None:
            params.hasName = True
            params.name = name
        if host is not None:
            params.hasHost = True
            params.host = host
        if node_agent_port is not None:
            params.hasNodeAgentPort = True
            params.nodeAgentPort = node_agent_port
        if net_agent_port is not None:
            params.hasNetAgentPort = True
            params.netAgentPort = net_agent_port
        if base_path is not None:
            params.hasBasePath = True
            params.basePath = base_path
        if description is not None:
            params.hasDescription = True
            params.description = description
        if admin_state is not None:
            params.hasAdminState = True
            params.adminState = admin_state
        await self._cap.edit(params=params)

    async def drain(self) -> None:
        """Shortcut for ``edit(admin_state='draining')``."""
        await self._cap.drain()

    async def delete(self) -> None:
        """Remove the node. Refuses while any VM, network, or
        disk-image placement still references it.
        """
        await self._cap.delete()
