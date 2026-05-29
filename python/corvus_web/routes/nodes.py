"""Node endpoints: list + detail (read-only).

Full node CRUD (create/edit/drain/delete) lives in ``corvus-admin``
today — adding nodes is a one-time setup step, not a recurring
operator workflow. The web UI exposes the read path so operators can
see capacity, agent connection state, and pick a node from the
VM-create dropdown."""

from __future__ import annotations

from dataclasses import asdict, is_dataclass
from typing import TYPE_CHECKING, Annotated, Any

from corvus_client.exceptions import NodeNotFound
from fastapi import APIRouter, Depends, HTTPException

from ..deps import get_client

if TYPE_CHECKING:
    from corvus_client import AsyncClient

router = APIRouter(prefix="/nodes", tags=["nodes"])

ClientDep = Annotated["AsyncClient", Depends(get_client)]


def _as_dict(obj: Any) -> Any:
    if is_dataclass(obj) and not isinstance(obj, type):
        return {k: _as_dict(v) for k, v in asdict(obj).items()}
    if isinstance(obj, list | tuple):
        return [_as_dict(v) for v in obj]
    return obj


@router.get("")
async def list_nodes(client: ClientDep) -> list[dict[str, Any]]:
    """Mirrors ``crv node list``. Returns the full NodeInfo set: name,
    host, admin state, capacity snapshot (CPU / RAM / storage / load),
    netd status."""
    return [_as_dict(n) for n in await client.nodes.list()]


@router.get("/{node_id}")
async def get_node(node_id: int, client: ClientDep) -> dict[str, Any]:
    """Node detail. Adds kernel_release, agent_version, full load
    average vector, and the base_path on top of the list view."""
    try:
        node = await client.nodes.get(node_id)
    except NodeNotFound as exc:
        raise HTTPException(status_code=404, detail=str(exc)) from exc
    return _as_dict(await node.show())
