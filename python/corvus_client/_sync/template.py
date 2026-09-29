"""Sync mirrors for the async Template wrappers."""

from __future__ import annotations

from typing import TYPE_CHECKING

from .. import types as t
from .._async.template import AsyncTemplate, AsyncTemplateManager
from .._runloop import SyncRunloop
from ._resource import LoopBoundResource

if TYPE_CHECKING:
    from .vm import SyncVm


class SyncTemplateManager:
    def __init__(self, async_mgr: AsyncTemplateManager, runloop: SyncRunloop) -> None:
        self._a = async_mgr
        self._rl = runloop

    def list(self) -> list[t.TemplateVmInfo]:
        return self._rl.run(self._a.list())

    def get(self, ref: int | str, *, by_name: bool = False) -> SyncTemplate:
        return SyncTemplate(self._rl.run(self._a.get(ref, by_name=by_name)), self._rl)

    def create(self, yaml: str) -> SyncTemplate:
        return SyncTemplate(self._rl.run(self._a.create(yaml)), self._rl)


class SyncTemplate(LoopBoundResource):
    def __init__(self, async_tpl: AsyncTemplate, runloop: SyncRunloop) -> None:
        self._a = async_tpl
        self._rl = runloop

    def show(self) -> t.TemplateDetails:
        return self._rl.run(self._a.show())

    def delete(self) -> None:
        return self._rl.run(self._a.delete())

    def instantiate(self, name: str, *, node: str | None = None) -> SyncVm:
        from .vm import SyncVm

        return SyncVm(self._rl.run(self._a.instantiate(name, node=node)), self._rl)

    def update(self, yaml: str) -> None:
        return self._rl.run(self._a.update(yaml))
