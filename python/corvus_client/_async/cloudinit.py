"""Async CloudInit manager wrapper.

Cloud-init configs are owned by their VM; the schema exposes a
manager cap only (no per-config resource cap).
"""

from __future__ import annotations

from .. import _schema
from .._entityref import entity_ref
from ..exceptions import translate_errors
from . import _convert as conv


def _build_cloud_init_info(
    *,
    user_data: str | None,
    network_config: str | None,
    inject_ssh_keys: bool,
):
    info = _schema.cloudinit.CloudInitInfo.new_message()
    if user_data is not None:
        info.hasUserData = True
        info.userData = user_data
    if network_config is not None:
        info.hasNetworkConfig = True
        info.networkConfig = network_config
    info.injectSshKeys = inject_ssh_keys
    return info


@translate_errors
class AsyncCloudInitManager:
    def __init__(self, daemon):
        self._daemon = daemon
        self._mgr = None

    async def _ensure(self):
        if self._mgr is None:
            self._mgr = (await self._daemon.cloudInit()).mgr
        return self._mgr

    async def set(
        self,
        vm_ref: int | str,
        *,
        user_data: str | None = None,
        network_config: str | None = None,
        inject_ssh_keys: bool = False,
    ) -> None:
        mgr = await self._ensure()
        params = _schema.cloudinit.CloudInitSetParams.new_message()
        params.vmRef = entity_ref(vm_ref)
        params.config = _build_cloud_init_info(
            user_data=user_data,
            network_config=network_config,
            inject_ssh_keys=inject_ssh_keys,
        )
        await mgr.set(params=params)

    async def get(self, vm_ref: int | str):
        mgr = await self._ensure()
        resp = await mgr.get(vmRef=entity_ref(vm_ref))
        return conv.cloud_init_info(resp.config)

    async def delete(self, vm_ref: int | str) -> None:
        mgr = await self._ensure()
        await mgr.delete(vmRef=entity_ref(vm_ref))
