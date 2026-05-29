"""Task history endpoints: list (with filters), detail, child tasks.

Live progress via WebSocket lands in a dedicated streams slice — the
pycapnp ``TaskProgressSink.Server`` subclass + queue → websocket bridge
is non-trivial and deserves its own surface. For v1, the existing
5-second polling on detail + list is enough.
"""

from __future__ import annotations

from dataclasses import asdict, is_dataclass
from typing import TYPE_CHECKING, Annotated, Any

from corvus_client.exceptions import TaskNotFound
from fastapi import APIRouter, Depends, HTTPException, Query

from ..deps import get_client

if TYPE_CHECKING:
    from corvus_client import AsyncClient

router = APIRouter(prefix="/tasks", tags=["tasks"])

ClientDep = Annotated["AsyncClient", Depends(get_client)]


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
