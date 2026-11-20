"""Async Node manager + Node wrappers.

Mirrors the @crv node@ subcommand surface. A registered ``Node`` is
the daemon's record of one host it orchestrates; per-node agent
endpoints plus the agent-pushed capacity snapshot live on each row.
"""

from __future__ import annotations

import capnp

from .. import _schema
from .. import types as t
from .._entityref import entity_ref
from ..exceptions import translate_errors
from . import _convert as conv


@translate_errors
class AsyncNodeManager:
    def __init__(self, daemon: capnp.lib.capnp._DynamicCapabilityClient) -> None:
        self._daemon = daemon
        self._mgr = None

    async def _ensure(self) -> capnp.lib.capnp._DynamicCapabilityClient:
        if self._mgr is None:
            self._mgr = (await self._daemon.nodes()).mgr
        return self._mgr

    async def list(self) -> list[t.NodeInfo]:
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
        netd_disabled: bool = False,
    ) -> AsyncNode:
        """Register a new node.

        ``host`` is the IPv4 / IPv6 / hostname the daemon will dial.
        ``node_agent_port`` defaults to 9878 (corvus-nodeagent);
        ``net_agent_port`` defaults to 9877 (corvus-netd). The
        daemon starts a reconnect supervisor for an online or
        maintenance node. An offline node requires a base path and
        starts connecting when its state changes.
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
        params.netdDisabled = netd_disabled
        resp = await mgr.create(params=params)
        return AsyncNode(resp.node)


@translate_errors
class AsyncNode:
    def __init__(self, cap: capnp.lib.capnp._DynamicCapabilityClient) -> None:
        self._cap = cap

    async def ping(self) -> t.NodePingResult:
        """Probe the existing supervisor session without changing resources."""
        resp = await self._cap.ping()
        return t.NodePingResult(reachable=resp.reachable, message=resp.message)

    async def show(self) -> t.NodeDetails:
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
        netd_disabled: bool | None = None,
    ) -> None:
        """Mutate a subset of fields. Pass ``None`` (the default) to
        leave a field unchanged. Pass an empty string for
        ``description`` to clear it. Changing ``host`` /
        ``node_agent_port`` / ``net_agent_port`` triggers a daemon-side
        supervisor respawn against the new endpoint. Flipping
        ``netd_disabled`` from ``False`` to ``True`` is refused while
        the node still owns managed networks or has VMs with
        netd-dependent NICs.
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
        if netd_disabled is not None:
            params.hasNetdDisabled = True
            params.netdDisabled = netd_disabled
        await self._cap.edit(params=params)

    async def set_state(self, state: str) -> None:
        """Set the administrative state to online, maintenance, or offline."""
        await self._cap.setState(hasState=True, state=state)

    async def delete(self) -> None:
        """Remove the node. Refuses while any VM, network, or
        disk-image placement still references it.
        """
        await self._cap.delete()

    async def forget_preview(self) -> t.NodeForgetImpact:
        """Preview the records affected by forgetting this node."""
        return _forget_impact((await self._cap.forgetPreview()).impact)

    async def forget(self, *, confirmed_fenced: bool) -> t.NodeForgetResult:
        """Remove a permanently unavailable node after fencing the host."""
        result = await self._cap.forget(confirmedFenced=confirmed_fenced)
        return t.NodeForgetResult(_forget_impact(result.impact), list(result.warnings))


def _forget_impact(impact: capnp.lib.capnp._DynamicStructReader) -> t.NodeForgetImpact:
    return t.NodeForgetImpact(
        vms=[conv.named_ref(r) for r in impact.vms],
        owned_networks=[conv.named_ref(r) for r in impact.ownedNetworks],
        peer_networks=[conv.named_ref(r) for r in impact.peerNetworks],
        disk_images=[conv.named_ref(r) for r in impact.diskImages],
        zero_copy_images=[conv.named_ref(r) for r in impact.zeroCopyImages],
        blocking_vms=[conv.named_ref(r) for r in impact.blockingVms],
    )
