"""Sync API tests — same coverage as the async tests but via Client.

Verifies the sync wrapper threads correctly and the background runloop
is reentrant: multiple calls in the same Client instance share a loop.
"""

from __future__ import annotations

import asyncio
from collections.abc import Awaitable, Callable, Coroutine
from pathlib import Path
from typing import TypeVar, cast

import pytest
from corvus_client import Client, DiskNotFound, VmNotRunning
from corvus_client._async.task import AsyncTaskManager
from corvus_client._runloop import SyncRunloop
from corvus_client._sync.task import SyncTaskManager
from corvus_client.types import TaskProgressEvent, TaskProgressProgress

T = TypeVar("T")


def test_sync_status_and_ping(daemon_socket: Path) -> None:
    with Client(unix_socket=str(daemon_socket)) as c:
        info = c.status()
        assert info.version
        assert info.uptime_seconds >= 0
        assert info.database_backend
        assert info.database_version
        c.ping()


def test_sync_disk_lifecycle(daemon_socket: Path) -> None:
    with Client(unix_socket=str(daemon_socket)) as c:
        d = c.disks.create("sync-disk", size=67108864)
        info = d.show()
        assert info.name == "sync-disk"
        assert info.size == 67108864
        d.delete()
        with pytest.raises(DiskNotFound):
            c.disks.get("sync-disk")


def test_sync_vm_create_edit_delete(daemon_socket: Path) -> None:
    with Client(unix_socket=str(daemon_socket)) as c:
        v = c.vms.create("sync-vm", cpu_count=1, ram=268435456, headless=True)
        details = v.show()
        assert details.name == "sync-vm"
        v.edit(ram=536870912)
        assert v.show().ram == 536870912
        v.delete()


def test_sync_multiple_clients_isolated(daemon_socket: Path) -> None:
    """Two Client instances must not share state, even on the same daemon."""
    with (
        Client(unix_socket=str(daemon_socket)) as c1,
        Client(unix_socket=str(daemon_socket)) as c2,
    ):
        c1.ping()
        c2.ping()
        # Manager caps fetched independently per client.
        d1 = c1.disks.create("sync-iso-1", size=33554432)
        d2 = c2.disks.create("sync-iso-2", size=33554432)
        names = [info.name for info in c1.disks.list()]
        assert "sync-iso-1" in names
        assert "sync-iso-2" in names
        d1.delete()
        d2.delete()


def test_sync_close_is_idempotent(daemon_socket: Path) -> None:
    c = Client(unix_socket=str(daemon_socket))
    c.ping()
    c.close()
    c.close()  # second close is a no-op


def test_sync_task_subscription_bridges_callback_and_closes() -> None:
    """The sync task facade bridges callbacks and releases its async owner."""

    class AsyncSubscription:
        def __init__(self) -> None:
            self.close_calls = 0

        async def close(self) -> None:
            self.close_calls += 1

    class AsyncManager:
        def __init__(self) -> None:
            self.cancelled: list[int] = []
            self.subscription = AsyncSubscription()

        async def cancel(self, task_id: int) -> None:
            self.cancelled.append(task_id)

        async def subscribe(
            self,
            task_id: int,
            callback: Callable[[TaskProgressEvent], Awaitable[None]],
        ) -> AsyncSubscription:
            await callback(TaskProgressProgress(task_id=task_id, completed=1))
            return self.subscription

    class Runloop:
        def run(self, coroutine: Coroutine[object, object, T]) -> T:
            return asyncio.run(coroutine)

    async_manager = AsyncManager()
    manager = SyncTaskManager(
        cast(AsyncTaskManager, async_manager), cast(SyncRunloop, Runloop())
    )
    received: list[TaskProgressEvent] = []
    sub = manager.subscribe(42, received.append)
    assert received == [TaskProgressProgress(task_id=42, completed=1)]
    manager.cancel(42)
    assert async_manager.cancelled == [42]
    sub.close()
    sub.close()
    assert async_manager.subscription.close_calls == 1


def test_sync_balloon_validation(daemon_socket: Path) -> None:
    with Client(unix_socket=str(daemon_socket)) as c:
        vm = c.vms.create(
            "balloon-validation", cpu_count=1, ram=268435456, headless=True
        )
        for target in (0, -1, 2**64):
            with pytest.raises(ValueError):
                vm.set_balloon(target_bytes=target)
        for invalid_type in (True, 1.5, "128M"):
            with pytest.raises(TypeError):
                vm.set_balloon(target_bytes=cast(int, invalid_type))
        with pytest.raises(VmNotRunning):
            vm.set_balloon(target_bytes=128 * 1024**2)
        assert (vm.show()).ram == 268435456
        c.ping()
        vm.delete()
