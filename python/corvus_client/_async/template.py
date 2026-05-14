"""Async Template manager + Template wrappers."""
from __future__ import annotations

from typing import TYPE_CHECKING, Union

from .. import _schema
from .._entityref import entity_ref
from ..exceptions import translate_errors
from . import _convert as conv

if TYPE_CHECKING:
    from .vm import AsyncVm


@translate_errors
class AsyncTemplateManager:
    def __init__(self, daemon):
        self._daemon = daemon
        self._mgr = None

    async def _ensure(self):
        if self._mgr is None:
            self._mgr = (await self._daemon.templates()).mgr
        return self._mgr

    async def list(self):
        mgr = await self._ensure()
        resp = await mgr.list()
        return [conv.template_vm_info(t) for t in resp.templates]

    async def get(self, ref: Union[int, str], *, by_name: bool = False) -> "AsyncTemplate":
        mgr = await self._ensure()
        resp = await mgr.get(ref=entity_ref(ref, by_name=by_name))
        return AsyncTemplate(resp.template)

    async def create(self, yaml: str) -> "AsyncTemplate":
        mgr = await self._ensure()
        resp = await mgr.create(yaml=yaml)
        return AsyncTemplate(resp.template)


@translate_errors
class AsyncTemplate:
    def __init__(self, cap):
        self._cap = cap

    async def show(self):
        resp = await self._cap.show()
        return conv.template_details(resp.details)

    async def delete(self) -> None:
        await self._cap.delete()

    async def instantiate(self, name: str) -> "AsyncVm":
        from .vm import AsyncVm

        req = self._cap.instantiate_request()
        req.name = name
        resp = await req.send()
        return AsyncVm(resp.vm)

    async def update(self, yaml: str) -> None:
        await self._cap.update(yaml=yaml)
