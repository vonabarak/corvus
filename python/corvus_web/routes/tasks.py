"""Task history endpoints: list (with filters), detail, child tasks,
plus a live-progress WebSocket bridge.

The WS endpoint subscribes to the daemon's ``TaskProgressSink`` via
``corvus_client.AsyncClient.tasks.subscribe`` and forwards each event
as one JSON frame. Frame shape mirrors the ``TaskProgressEvent`` union
in ``schema/streams.capnp`` — one of:

    {"type": "started",  "task_id": 42, "command": "...", "subsystem": "..."}
    {"type": "progress", "task_id": 42, "completed": 3, "total": 10, "label": "..."}
    {"type": "finished", "task_id": 42, "result": "success", "message": null}
"""

from __future__ import annotations

import asyncio
import logging
from contextlib import suppress
from dataclasses import asdict, is_dataclass
from typing import TYPE_CHECKING, Annotated, Any

from corvus_client.exceptions import CorvusError, TaskNotFound
from corvus_client.types import (
    TaskProgressFinished,
    TaskProgressProgress,
    TaskProgressStarted,
)
from fastapi import (
    APIRouter,
    Depends,
    HTTPException,
    Query,
    WebSocket,
    WebSocketDisconnect,
)

from ..deps import get_client

if TYPE_CHECKING:
    from corvus_client import AsyncClient

router = APIRouter(prefix="/tasks", tags=["tasks"])

ClientDep = Annotated["AsyncClient", Depends(get_client)]

logger = logging.getLogger(__name__)


def _as_dict(obj: Any) -> Any:
    if is_dataclass(obj) and not isinstance(obj, type):
        return {k: _as_dict(v) for k, v in asdict(obj).items()}
    if isinstance(obj, list | tuple):
        return [_as_dict(v) for v in obj]
    return obj


@router.get("")
async def list_tasks(
    client: ClientDep,
    limit: int = Query(50, ge=1, le=500),
    subsystem: str | None = None,
    entity_id: int | None = None,
    result: str | None = None,
) -> list[dict[str, Any]]:
    """List tasks newest-first. Filter knobs match
    ``crv task list``: subsystem (vm/disk/network/ssh-key/template/
    shared-dir/snapshot/system/apply), entity_id (the resource the
    task acted on), result (running/success/error)."""
    tasks = await client.tasks.list(
        limit=limit,
        subsystem=subsystem,
        entity_id=entity_id,
        result=result,
    )
    return [_as_dict(t) for t in tasks]


@router.get("/{task_id}")
async def get_task(task_id: int, client: ClientDep) -> dict[str, Any]:
    """Full task record."""
    try:
        task = await client.tasks.get(task_id)
    except TaskNotFound as exc:
        raise HTTPException(status_code=404, detail=str(exc)) from exc
    return _as_dict(await task.show())


@router.get("/{task_id}/children")
async def list_task_children(task_id: int, client: ClientDep) -> list[dict[str, Any]]:
    """Sub-tasks spawned by this task. Apply and build flows record a
    parent task and one child per resource they touch."""
    children = await client.tasks.list_children(task_id)
    return [_as_dict(t) for t in children]


def _task_progress_event_to_dict(event: Any) -> dict[str, Any]:
    """Tag a ``TaskProgressEvent`` dataclass with its variant name and
    flatten to a JSON-friendly dict. The variant tag mirrors the
    Cap'n Proto union discriminator (started / progress / finished)."""
    if isinstance(event, TaskProgressStarted):
        return {"type": "started", **asdict(event)}
    if isinstance(event, TaskProgressProgress):
        return {"type": "progress", **asdict(event)}
    if isinstance(event, TaskProgressFinished):
        return {"type": "finished", **asdict(event)}
    # Unknown variant — surface raw fields so the client at least
    # sees something. Should not happen in practice.
    return {"type": "unknown", "payload": str(event)}


@router.websocket("/{task_id}/ws")
async def task_progress_ws(ws: WebSocket, task_id: int) -> None:
    """Subscribe to a task's progress stream and forward each event as
    a JSON frame.

    The daemon emits ``started`` on join, zero or more ``progress``
    events, and a single ``finished`` event. After ``finished`` the
    daemon closes the subscription handle; we drain any pending events
    and close the WebSocket with a normal 1000.

    Dependency injection: pulls the client off ``ws.app.state`` for
    the same reason ``serial_console_ws`` does (avoids a parallel
    Annotated dep for WS endpoints)."""
    client = ws.app.state.client
    await ws.accept()

    queue: asyncio.Queue[Any] = asyncio.Queue()

    async def on_event(event: Any) -> None:
        await queue.put(event)

    try:
        subscription = await client.tasks.subscribe(task_id, on_event)
    except TaskNotFound as exc:
        await ws.close(code=1008, reason=str(exc))
        return
    except CorvusError as exc:
        logger.warning("task progress WS: subscribe failed for %d: %s", task_id, exc)
        await ws.close(code=1011, reason=str(exc))
        return

    async def queue_to_ws() -> None:
        while True:
            event = await queue.get()
            payload = _task_progress_event_to_dict(event)
            try:
                await ws.send_json(payload)
            except (WebSocketDisconnect, RuntimeError):
                return
            if payload["type"] == "finished":
                # Daemon won't send anything else; close cleanly so
                # the client sees a normal end (not a 1006 abort).
                with suppress(Exception):
                    await ws.close()
                return

    async def watch_disconnect() -> None:
        # ``receive`` blocks until the client sends a message OR
        # disconnects. We don't expect inbound traffic on this stream,
        # so the only realistic return path is disconnect.
        while True:
            try:
                msg = await ws.receive()
            except WebSocketDisconnect:
                return
            if msg["type"] == "websocket.disconnect":
                return
            # Any other message type (text/binary frames from the
            # client) is unexpected — silently discard.

    tasks_ = [
        asyncio.create_task(queue_to_ws(), name=f"task-progress-{task_id}"),
        asyncio.create_task(watch_disconnect(), name=f"task-progress-watch-{task_id}"),
    ]
    try:
        _, pending = await asyncio.wait(tasks_, return_when=asyncio.FIRST_COMPLETED)
        for t in pending:
            t.cancel()
            with suppress(asyncio.CancelledError, Exception):
                await t
    finally:
        # Dropping the subscription handle tells the daemon to stop
        # pushing — its subscriber list gets pruned on the next event.
        with suppress(Exception):
            await subscription.close()
        with suppress(Exception):
            await ws.close()
