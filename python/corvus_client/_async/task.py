"""Async Task manager + Task wrappers."""

from __future__ import annotations

import builtins
from collections.abc import Awaitable, Callable
from typing import TYPE_CHECKING

import capnp

from .. import _schema
from .. import types as t
from ..exceptions import translate_errors
from . import _convert as conv

if TYPE_CHECKING:
    from .streams import TaskProgressSubscription


@translate_errors
class AsyncTaskManager:
    def __init__(self, daemon: capnp.lib.capnp._DynamicCapabilityClient) -> None:
        self._daemon = daemon
        self._mgr = None

    async def _ensure(self) -> capnp.lib.capnp._DynamicCapabilityClient:
        if self._mgr is None:
            self._mgr = (await self._daemon.tasks()).mgr
        return self._mgr

    async def list(
        self,
        *,
        limit: int | None = None,
        subsystem: str | None = None,
        entity_id: int | None = None,
        result: str | None = None,
        include_subtasks: bool = False,
    ) -> list[t.TaskInfo]:
        mgr = await self._ensure()
        params = _schema.task.TaskListParams.new_message()
        if limit is not None:
            params.limit = limit
        if subsystem is not None:
            params.hasSubsystem = True
            params.subsystem = subsystem
        if entity_id is not None:
            params.entityId = entity_id
        if result is not None:
            params.hasResult = True
            params.result = result
        params.includeSubtasks = include_subtasks
        resp = await mgr.list(params=params)
        return [conv.task_info(t) for t in resp.tasks]

    async def get(self, task_id: int) -> AsyncTask:
        mgr = await self._ensure()
        resp = await mgr.get(taskId=task_id)
        return AsyncTask(resp.task)

    async def list_children(self, parent_id: int) -> builtins.list[t.TaskInfo]:
        mgr = await self._ensure()
        resp = await mgr.listChildren(parentId=parent_id)
        return [conv.task_info(t) for t in resp.tasks]

    async def cancel(self, task_id: int) -> None:
        """Request best-effort cancellation of a running task.

        This records the cancellation request; it does not wait for the
        worker (or its active child) to reach a terminal state.
        """
        mgr = await self._ensure()
        await mgr.cancel(taskId=task_id)

    async def subscribe(
        self,
        task_id: int,
        on_event: Callable[[t.TaskProgressEvent], Awaitable[None]],
    ) -> TaskProgressSubscription:
        """Subscribe to live progress events for the given task.

        `on_event` is an async callable invoked with each
        `TaskProgressEvent` payload. Returns a `TaskProgressSubscription`
        — drop it to unsubscribe.
        """
        from .streams import subscribe_task_progress

        mgr = await self._ensure()
        return await subscribe_task_progress(mgr, task_id, on_event)


@translate_errors
class AsyncTask:
    def __init__(self, cap: capnp.lib.capnp._DynamicCapabilityClient) -> None:
        self._cap = cap

    async def show(self) -> t.TaskInfo:
        resp = await self._cap.show()
        return conv.task_info(resp.info)
