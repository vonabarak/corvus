"""Sync mirrors for the async Node wrappers."""
from __future__ import annotations

from typing import Union

from ._resource import LoopBoundResource


class SyncNodeManager:
    def __init__(self, async_mgr, runloop):
        self._a = async_mgr
        self._rl = runloop

    def list(self):
        return self._rl.run(self._a.list())

    def get(self, ref: Union[int, str], *, by_name: bool = False):
        return SyncNode(self._rl.run(self._a.get(ref, by_name=by_name)), self._rl)

    def create(self, name: str, host: str, **kwargs):
        return SyncNode(
            self._rl.run(self._a.create(name, host, **kwargs)), self._rl
        )


class SyncNode(LoopBoundResource):
    def __init__(self, async_node, runloop):
        self._a = async_node
        self._rl = runloop

    def show(self):
        return self._rl.run(self._a.show())

    def edit(self, **kwargs):
        return self._rl.run(self._a.edit(**kwargs))

    def drain(self):
        return self._rl.run(self._a.drain())

    def delete(self):
        return self._rl.run(self._a.delete())
