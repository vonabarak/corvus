"""SSH key endpoints: list, detail, create, delete.

Attach/detach is per-VM (see ``vm.attach_ssh_key`` /
``vm.detach_ssh_key`` in ``corvus_client._async.vm``); we'll surface
it from the VM page in a later slice. For now, callers see *which*
VMs a key is attached to via the ``attached_vms`` field.
"""

from __future__ import annotations

from dataclasses import asdict, is_dataclass
from typing import TYPE_CHECKING, Annotated, Any

from corvus_client.exceptions import SshKeyNotFound
from fastapi import APIRouter, Depends, HTTPException
from pydantic import BaseModel, Field

from ..deps import get_client

if TYPE_CHECKING:
    from corvus_client import AsyncClient

router = APIRouter(prefix="/ssh-keys", tags=["ssh-keys"])

ClientDep = Annotated["AsyncClient", Depends(get_client)]


def _as_dict(obj: Any) -> Any:
    if is_dataclass(obj) and not isinstance(obj, type):
        return {k: _as_dict(v) for k, v in asdict(obj).items()}
    if isinstance(obj, list | tuple):
        return [_as_dict(v) for v in obj]
    return obj


class SshKeyCreateBody(BaseModel):
    name: str = Field(..., min_length=1, description="Friendly name (unique).")
    public_key: str = Field(..., min_length=1, description="OpenSSH public-key text.")


@router.get("")
async def list_keys(client: ClientDep) -> list[dict[str, Any]]:
    return [_as_dict(k) for k in await client.ssh_keys.list()]


@router.get("/{key_id}")
async def get_key(key_id: int, client: ClientDep) -> dict[str, Any]:
    try:
        key = await client.ssh_keys.get(key_id)
    except SshKeyNotFound as exc:
        raise HTTPException(status_code=404, detail=str(exc)) from exc
    return _as_dict(await key.show())


@router.post("")
async def create_key(body: SshKeyCreateBody, client: ClientDep) -> dict[str, Any]:
    key = await client.ssh_keys.create(body.name, body.public_key)
    return _as_dict(await key.show())


@router.delete("/{key_id}")
async def delete_key(key_id: int, client: ClientDep) -> dict[str, str]:
    try:
        key = await client.ssh_keys.get(key_id)
    except SshKeyNotFound as exc:
        raise HTTPException(status_code=404, detail=str(exc)) from exc
    await key.delete()
    return {"status": "deleted"}
