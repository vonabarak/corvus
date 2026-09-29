"""Sync mirrors for the async Network wrappers."""

from __future__ import annotations

from collections.abc import Iterable

from .. import types as t
from .._async.network import AsyncNetwork, AsyncNetworkManager
from .._runloop import SyncRunloop
from ._resource import LoopBoundResource


class SyncNetworkManager:
    def __init__(self, async_mgr: AsyncNetworkManager, runloop: SyncRunloop) -> None:
        self._a = async_mgr
        self._rl = runloop

    def list(self) -> list[t.NetworkInfo]:
        return self._rl.run(self._a.list())

    def get(self, ref: int | str, *, by_name: bool = False) -> SyncNetwork:
        return SyncNetwork(self._rl.run(self._a.get(ref, by_name=by_name)), self._rl)

    def create(
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
    ) -> SyncNetwork:
        return SyncNetwork(
            self._rl.run(
                self._a.create(
                    name,
                    subnet,
                    node=node,
                    dhcp=dhcp,
                    nat=nat,
                    autostart=autostart,
                    dns_servers=dns_servers,
                    domain=domain,
                    host_dns=host_dns,
                )
            ),
            self._rl,
        )


class SyncNetwork(LoopBoundResource):
    def __init__(self, async_net: AsyncNetwork, runloop: SyncRunloop) -> None:
        self._a = async_net
        self._rl = runloop

    def show(self) -> t.NetworkInfo:
        return self._rl.run(self._a.show())

    def start(self) -> None:
        return self._rl.run(self._a.start())

    def stop(self, *, force: bool = False) -> None:
        return self._rl.run(self._a.stop(force=force))

    def edit(
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
        return self._rl.run(
            self._a.edit(
                name=name,
                subnet=subnet,
                dhcp=dhcp,
                nat=nat,
                autostart=autostart,
                dns_servers=dns_servers,
                domain=domain,
                host_dns=host_dns,
            )
        )

    def delete(self) -> None:
        return self._rl.run(self._a.delete())

    def attach_node(self, node: int | str) -> None:
        return self._rl.run(self._a.attach_node(node))

    def detach_node(self, node: int | str) -> None:
        return self._rl.run(self._a.detach_node(node))
