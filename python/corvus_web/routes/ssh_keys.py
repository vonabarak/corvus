"""SSH key endpoints: list, detail, create, delete.

Attach/detach is per-VM (see ``vm.attach_ssh_key`` /
``vm.detach_ssh_key`` in ``corvus_client._async.vm``); we'll surface
it from the VM page in a later slice. For now, callers see *which*
VMs a key is attached to via the ``attached_vms`` field.
"""

from __future__ import annotations

from typing import TYPE_CHECKING, Annotated

from corvus_client.exceptions import SshKeyNotFound
from fastapi import APIRouter, Depends, HTTPException
from pydantic import BaseModel, Field

from ..deps import get_client
from ..lib import JsonObject, to_dict

if TYPE_CHECKING:
    from corvus_client import AsyncClient

router = APIRouter(prefix="/ssh-keys", tags=["ssh-keys"])

ClientDep = Annotated["AsyncClient", Depends(get_client)]


class SshKeyCreateBody(BaseModel):  # type: ignore[explicit-any]
    name: str = Field(..., min_length=1, description="Friendly name (unique).")
    public_key: str = Field(..., min_length=1, description="OpenSSH public-key text.")


@router.get("")
async def list_keys(client: ClientDep) -> list[JsonObject]:
    return [to_dict(k) for k in await client.ssh_keys.list()]


@router.get("/{key_id}")
async def get_key(key_id: int, client: ClientDep) -> JsonObject:
    try:
        key = await client.ssh_keys.get(key_id)
    except SshKeyNotFound as exc:
        raise HTTPException(status_code=404, detail=str(exc)) from exc
    return to_dict(await key.show())


@router.post("")
async def create_key(body: SshKeyCreateBody, client: ClientDep) -> JsonObject:
    key = await client.ssh_keys.create(body.name, body.public_key)
    return to_dict(await key.show())


@router.delete("/{key_id}")
async def delete_key(key_id: int, client: ClientDep) -> dict[str, str]:
    try:
        key = await client.ssh_keys.get(key_id)
    except SshKeyNotFound as exc:
        raise HTTPException(status_code=404, detail=str(exc)) from exc
    await key.delete()
    return {"status": "deleted"}
