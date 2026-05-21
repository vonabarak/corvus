"""Sync mirrors for the async Template wrappers."""

from __future__ import annotations

from typing import Union

from ._resource import LoopBoundResource


class SyncTemplateManager:
    def __init__(self, async_mgr, runloop):
        self._a = async_mgr
        self._rl = runloop

    def list(self):
        return self._rl.run(self._a.list())

    def get(self, ref: Union[int, str], *, by_name: bool = False):
        return SyncTemplate(self._rl.run(self._a.get(ref, by_name=by_name)), self._rl)

    def create(self, yaml: str):
        return SyncTemplate(self._rl.run(self._a.create(yaml)), self._rl)


class SyncTemplate(LoopBoundResource):
    def __init__(self, async_tpl, runloop):
        self._a = async_tpl
        self._rl = runloop

    def show(self):
        return self._rl.run(self._a.show())

    def delete(self):
        return self._rl.run(self._a.delete())

    def instantiate(self, name: str, **kwargs):
        from .vm import SyncVm

        return SyncVm(self._rl.run(self._a.instantiate(name, **kwargs)), self._rl)

    def update(self, yaml: str):
        return self._rl.run(self._a.update(yaml))
