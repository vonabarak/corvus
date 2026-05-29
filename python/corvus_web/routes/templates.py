"""Template endpoints: list, detail, instantiate, delete.

Template create + update both take raw YAML and want a Monaco editor
on the frontend; those endpoints come along with the Apply slice.
"""

from __future__ import annotations

from dataclasses import asdict, is_dataclass
from typing import TYPE_CHECKING, Annotated, Any

from corvus_client.exceptions import TemplateNotFound
from fastapi import APIRouter, Depends, HTTPException
from pydantic import BaseModel, Field

from ..deps import get_client

if TYPE_CHECKING:
    from corvus_client import AsyncClient

router = APIRouter(prefix="/templates", tags=["templates"])

ClientDep = Annotated["AsyncClient", Depends(get_client)]


def _as_dict(obj: Any) -> Any:
    if is_dataclass(obj) and not isinstance(obj, type):
        return {k: _as_dict(v) for k, v in asdict(obj).items()}
    if isinstance(obj, list | tuple):
        return [_as_dict(v) for v in obj]
    return obj


class InstantiateBody(BaseModel):
    name: str = Field(..., min_length=1, description="Name for the new VM.")
    node: str | None = Field(
        None, description="Optional node name or id to pin placement to."
    )


@router.get("")
async def list_templates(client: ClientDep) -> list[dict[str, Any]]:
    return [_as_dict(t) for t in await client.templates.list()]


@router.get("/{template_id}")
async def get_template(template_id: int, client: ClientDep) -> dict[str, Any]:
    try:
        tmpl = await client.templates.get(template_id)
    except TemplateNotFound as exc:
        raise HTTPException(status_code=404, detail=str(exc)) from exc
    return _as_dict(await tmpl.show())


@router.post("/{template_id}/instantiate")
async def instantiate_template(
    template_id: int, body: InstantiateBody, client: ClientDep
) -> dict[str, Any]:
    """Create a new VM from the template. Returns the resulting VM's
    detail payload so the frontend can route straight to it."""
    try:
        tmpl = await client.templates.get(template_id)
    except TemplateNotFound as exc:
        raise HTTPException(status_code=404, detail=str(exc)) from exc
    vm = await tmpl.instantiate(body.name, node=body.node)
    details = await vm.show()
    return _as_dict(details)


@router.delete("/{template_id}")
async def delete_template(template_id: int, client: ClientDep) -> dict[str, str]:
    try:
        tmpl = await client.templates.get(template_id)
    except TemplateNotFound as exc:
        raise HTTPException(status_code=404, detail=str(exc)) from exc
    await tmpl.delete()
    return {"status": "deleted"}
