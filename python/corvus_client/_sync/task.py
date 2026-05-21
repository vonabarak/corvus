"""Sync mirrors for the async Task wrappers."""

from __future__ import annotations

from ._resource import LoopBoundResource


class SyncTaskManager:
    def __init__(self, async_mgr, runloop):
        self._a = async_mgr
        self._rl = runloop

    def list(
        self,
        *,
        limit: int | None = None,
        subsystem: str | None = None,
        entity_id: int | None = None,
        result: str | None = None,
    ):
        return self._rl.run(
            self._a.list(
                limit=limit, subsystem=subsystem, entity_id=entity_id, result=result
            )
        )

    def get(self, task_id: int):
        return SyncTask(self._rl.run(self._a.get(task_id)), self._rl)

    def list_children(self, parent_id: int):
        return self._rl.run(self._a.list_children(parent_id))


class SyncTask(LoopBoundResource):
    def __init__(self, async_task, runloop):
        self._a = async_task
        self._rl = runloop

    def show(self):
        return self._rl.run(self._a.show())
