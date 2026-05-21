"""Async Task manager + Task wrappers."""

from __future__ import annotations

from .. import _schema
from ..exceptions import translate_errors
from . import _convert as conv


@translate_errors
class AsyncTaskManager:
    def __init__(self, daemon):
        self._daemon = daemon
        self._mgr = None

    async def _ensure(self):
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
    ):
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
        resp = await mgr.list(params=params)
        return [conv.task_info(t) for t in resp.tasks]

    async def get(self, task_id: int) -> AsyncTask:
        mgr = await self._ensure()
        resp = await mgr.get(taskId=task_id)
        return AsyncTask(resp.task)

    async def list_children(self, parent_id: int):
        mgr = await self._ensure()
        resp = await mgr.listChildren(parentId=parent_id)
        return [conv.task_info(t) for t in resp.tasks]

    async def subscribe(self, task_id: int, on_event):
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
    def __init__(self, cap):
        self._cap = cap

    async def show(self):
        resp = await self._cap.show()
        return conv.task_info(resp.info)
