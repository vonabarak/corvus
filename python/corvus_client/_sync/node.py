"""Sync mirrors for the async Node wrappers."""

from __future__ import annotations

from .. import types as t
from .._async.node import AsyncNode, AsyncNodeManager
from .._runloop import SyncRunloop
from ._resource import LoopBoundResource


class SyncNodeManager:
    def __init__(self, async_mgr: AsyncNodeManager, runloop: SyncRunloop) -> None:
        self._a = async_mgr
        self._rl = runloop

    def list(self) -> list[t.NodeInfo]:
        return self._rl.run(self._a.list())

    def get(self, ref: int | str, *, by_name: bool = False) -> SyncNode:
        return SyncNode(self._rl.run(self._a.get(ref, by_name=by_name)), self._rl)

    def create(
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
    ) -> SyncNode:
        return SyncNode(
            self._rl.run(
                self._a.create(
                    name,
                    host,
                    node_agent_port=node_agent_port,
                    net_agent_port=net_agent_port,
                    base_path=base_path,
                    description=description,
                    admin_state=admin_state,
                    netd_disabled=netd_disabled,
                )
            ),
            self._rl,
        )


class SyncNode(LoopBoundResource):
    def __init__(self, async_node: AsyncNode, runloop: SyncRunloop) -> None:
        self._a = async_node
        self._rl = runloop

    def show(self) -> t.NodeDetails:
        return self._rl.run(self._a.show())

    def edit(
        self,
        *,
        name: str | None = None,
        host: str | None = None,
        node_agent_port: int | None = None,
        net_agent_port: int | None = None,
        base_path: str | None = None,
        description: str | None = None,
        admin_state: str | None = None,
        netd_disabled: bool | None = None,
    ) -> None:
        return self._rl.run(
            self._a.edit(
                name=name,
                host=host,
                node_agent_port=node_agent_port,
                net_agent_port=net_agent_port,
                base_path=base_path,
                description=description,
                admin_state=admin_state,
                netd_disabled=netd_disabled,
            )
        )

    def drain(self) -> None:
        return self._rl.run(self._a.drain())

    def delete(self) -> None:
        return self._rl.run(self._a.delete())
