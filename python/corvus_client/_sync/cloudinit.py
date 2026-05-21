"""Sync mirror for the async CloudInit manager."""

from __future__ import annotations


class SyncCloudInitManager:
    def __init__(self, async_mgr, runloop):
        self._a = async_mgr
        self._rl = runloop

    def set(
        self,
        vm_ref: int | str,
        *,
        user_data: str | None = None,
        network_config: str | None = None,
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

    def get(self, vm_ref: int | str):
        return self._rl.run(self._a.get(vm_ref))

    def delete(self, vm_ref: int | str) -> None:
        return self._rl.run(self._a.delete(vm_ref))
