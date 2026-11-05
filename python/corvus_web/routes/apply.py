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

from typing import TYPE_CHECKING, Annotated

from corvus_client.exceptions import ConnectError, CorvusError
from fastapi import APIRouter, Depends, HTTPException
from pydantic import BaseModel, ConfigDict, Field

from ..deps import get_client
from ..lib import JsonObject, to_dict

if TYPE_CHECKING:
    from corvus_client import AsyncClient

router = APIRouter(tags=["apply"])

ClientDep = Annotated["AsyncClient", Depends(get_client)]


class ApplyBody(BaseModel):  # type: ignore[explicit-any]
    yaml: str = Field(..., min_length=1, description="The pipeline document.")
    model_config = ConfigDict(extra="forbid")


@router.post("/apply")
async def run_apply(body: ApplyBody, client: ClientDep) -> JsonObject:
    """Run an apply pipeline. Returns
    ``{result: ApplyResult, task_id: int}``. The daemon's ``CorvusError``
    subclasses (BadEnvelope, ProtocolError, ServerError, …) translate to
    a 400 with the daemon's message verbatim — the operator usually just
    needs to fix the YAML and retry."""
    try:
        result, task_id = await client.apply(body.yaml, wait=True)
    except ConnectError:
        raise
    except CorvusError as exc:
        raise HTTPException(status_code=400, detail=str(exc)) from exc
    return {"result": to_dict(result), "task_id": task_id}
