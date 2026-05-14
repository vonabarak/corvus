"""Sync mirror for the async CloudInit manager."""
from __future__ import annotations

from typing import Optional, Union


class SyncCloudInitManager:
    def __init__(self, async_mgr, runloop):
        self._a = async_mgr
        self._rl = runloop

    def set(
        self,
        vm_ref: Union[int, str],
        *,
        user_data: Optional[str] = None,
        network_config: Optional[str] = None,
        inject_ssh_keys: bool = False,
    ) -> None:
        return self._rl.run(
            self._a.set(
                vm_ref,
                user_data=user_data,
                network_config=network_config,
                inject_ssh_keys=inject_ssh_keys,
            )
        )

    def get(self, vm_ref: Union[int, str]):
        return self._rl.run(self._a.get(vm_ref))

    def delete(self, vm_ref: Union[int, str]) -> None:
        return self._rl.run(self._a.delete(vm_ref))
