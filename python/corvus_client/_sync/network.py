"""Sync mirrors for the async Network wrappers."""
from __future__ import annotations

from typing import Union

from ._resource import LoopBoundResource


class SyncNetworkManager:
    def __init__(self, async_mgr, runloop):
        self._a = async_mgr
        self._rl = runloop

    def list(self):
        return self._rl.run(self._a.list())

    def get(self, ref: Union[int, str], *, by_name: bool = False):
        return SyncNetwork(self._rl.run(self._a.get(ref, by_name=by_name)), self._rl)

    def create(self, name: str, subnet: str, **kwargs):
        return SyncNetwork(self._rl.run(self._a.create(name, subnet, **kwargs)), self._rl)


class SyncNetwork(LoopBoundResource):
    def __init__(self, async_net, runloop):
        self._a = async_net
        self._rl = runloop

    def show(self):
        return self._rl.run(self._a.show())

    def start(self):
        return self._rl.run(self._a.start())

    def stop(self, *, force: bool = False):
        return self._rl.run(self._a.stop(force=force))

    def edit(self, **kwargs):
        return self._rl.run(self._a.edit(**kwargs))

    def delete(self):
        return self._rl.run(self._a.delete())
