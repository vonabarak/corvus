"""Async SshKey manager + SshKey wrappers."""

from __future__ import annotations

from .. import _schema
from .._entityref import entity_ref
from ..exceptions import translate_errors
from . import _convert as conv


@translate_errors
class AsyncSshKeyManager:
    def __init__(self, daemon):
        self._daemon = daemon
        self._mgr = None

    async def _ensure(self):
        if self._mgr is None:
            self._mgr = (await self._daemon.sshKeys()).mgr
        return self._mgr

    async def list(self):
        mgr = await self._ensure()
        resp = await mgr.list()
        return [conv.ssh_key_info(k) for k in resp.keys]

    async def get(self, ref: int | str, *, by_name: bool = False) -> AsyncSshKey:
        mgr = await self._ensure()
        resp = await mgr.get(ref=entity_ref(ref, by_name=by_name))
        return AsyncSshKey(resp.key)

    async def create(self, name: str, public_key: str) -> AsyncSshKey:
        mgr = await self._ensure()
        params = _schema.sshkey.SshKeyCreateParams.new_message()
        params.name = name
        params.publicKey = public_key
        resp = await mgr.create(params=params)
        return AsyncSshKey(resp.key)


@translate_errors
class AsyncSshKey:
    def __init__(self, cap):
        self._cap = cap

    async def show(self):
        resp = await self._cap.show()
        return conv.ssh_key_info(resp.info)

    async def delete(self) -> None:
        await self._cap.delete()
