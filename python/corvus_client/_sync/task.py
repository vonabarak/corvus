"""Sync mirrors for the async Task wrappers."""

from __future__ import annotations

from ._resource import LoopBoundResource


class SyncTaskProgressSubscription:
    """Synchronous owner for a live task-progress subscription.

    The callback executes on the client's runloop thread. Use thread-safe
    primitives when sharing callback state with the calling thread, then call
    :meth:`close` (or use a context manager) to unsubscribe.
    """

    def __init__(self, async_sub, runloop):
        self._a = async_sub
        self._rl = runloop

    def close(self) -> None:
        if self._a is not None:
            self._rl.run(self._a.close())
            self._a = None

    def __enter__(self):
        return self

    def __exit__(self, exc_type, exc, tb) -> None:
        self.close()


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
        include_subtasks: bool = False,
    ):
        return self._rl.run(
            self._a.list(
                limit=limit,
                subsystem=subsystem,
                entity_id=entity_id,
                result=result,
                include_subtasks=include_subtasks,
            )
        )

    def get(self, task_id: int):
        return SyncTask(self._rl.run(self._a.get(task_id)), self._rl)

    def list_children(self, parent_id: int):
        return self._rl.run(self._a.list_children(parent_id))

    def cancel(self, task_id: int) -> None:
        """Request best-effort cancellation without waiting for completion."""
        return self._rl.run(self._a.cancel(task_id))

    def subscribe(self, task_id: int, on_event):
        """Subscribe to task progress using a synchronous callback.

        Callbacks run on the background runloop thread. The returned
        subscription's :meth:`close` method has the same lifecycle as VM
        guest-agent and stats subscriptions.
        """

        async def _bridge(event):
            on_event(event)

        async_sub = self._rl.run(self._a.subscribe(task_id, _bridge))
        return SyncTaskProgressSubscription(async_sub, self._rl)


class SyncTask(LoopBoundResource):
    def __init__(self, async_task, runloop):
        self._a = async_task
        self._rl = runloop

    def show(self):
        return self._rl.run(self._a.show())
