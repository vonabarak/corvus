"""Sync mirrors for the async SshKey wrappers."""

from __future__ import annotations

from ._resource import LoopBoundResource


class SyncSshKeyManager:
    def __init__(self, async_mgr, runloop):
        self._a = async_mgr
        self._rl = runloop

    def list(self):
        return self._rl.run(self._a.list())

    def get(self, ref: int | str, *, by_name: bool = False):
        return SyncSshKey(self._rl.run(self._a.get(ref, by_name=by_name)), self._rl)

    def create(self, name: str, public_key: str):
        return SyncSshKey(self._rl.run(self._a.create(name, public_key)), self._rl)


class SyncSshKey(LoopBoundResource):
    def __init__(self, async_key, runloop):
        self._a = async_key
        self._rl = runloop

    def show(self):
        return self._rl.run(self._a.show())

    def delete(self):
        return self._rl.run(self._a.delete())
