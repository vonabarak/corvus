"""Declarative environment endpoint.

POSTs a YAML pipeline to the daemon; returns the per-kind summary of
created resources and the task id for follow-up. Mirrors ``crv apply``.

The request blocks on the daemon side via ``wait=True`` — the gateway
holds the HTTP connection open until the apply completes. For large
pipelines the frontend should pair this with a generous fetch timeout
or move to a background-task-id pattern; v1 keeps the synchronous
shape because the typical apply is small (a handful of resources).
"""

from __future__ import annotations

from dataclasses import asdict, is_dataclass
from typing import TYPE_CHECKING, Annotated, Any

from corvus_client.exceptions import CorvusError
from fastapi import APIRouter, Depends, HTTPException
from pydantic import BaseModel, Field

from ..deps import get_client

if TYPE_CHECKING:
    from corvus_client import AsyncClient

router = APIRouter(tags=["apply"])

ClientDep = Annotated["AsyncClient", Depends(get_client)]


def _as_dict(obj: Any) -> Any:
    if is_dataclass(obj) and not isinstance(obj, type):
        return {k: _as_dict(v) for k, v in asdict(obj).items()}
    if isinstance(obj, list | tuple):
        return [_as_dict(v) for v in obj]
    return obj


class ApplyBody(BaseModel):
    yaml: str = Field(..., min_length=1, description="The pipeline document.")
    skip_existing: bool = Field(
        False,
        description=(
            "Reuse existing resources by name instead of erroring on a "
            "duplicate. Same as ``crv apply --skip-existing`` / per-resource "
            "``ifExists: skip``."
        ),
    )


@router.post("/apply")
async def run_apply(body: ApplyBody, client: ClientDep) -> dict[str, Any]:
    """Run an apply pipeline. Returns
    ``{result: ApplyResult, task_id: int}``. The daemon's ``CorvusError``
    subclasses (BadEnvelope, ProtocolError, ServerError, …) translate to
    a 400 with the daemon's message verbatim — the operator usually just
    needs to fix the YAML and retry."""
    try:
        result, task_id = await client.apply(
            body.yaml, skip_existing=body.skip_existing, wait=True
        )
    except CorvusError as exc:
        raise HTTPException(status_code=400, detail=str(exc)) from exc
    return {"result": _as_dict(result), "task_id": task_id}
