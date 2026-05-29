"""Network endpoints: list, detail, start/stop, delete.

Thin shell over ``corvus_client.AsyncClient.networks``. Create and
edit flows need form UI; peer attach/detach belong with the
multi-node UI. Both stay out of this slice.
"""

from __future__ import annotations

from dataclasses import asdict, is_dataclass
from typing import TYPE_CHECKING, Annotated, Any

from corvus_client.exceptions import NetworkNotFound
from fastapi import APIRouter, Depends, HTTPException

from ..deps import get_client

if TYPE_CHECKING:
    from corvus_client import AsyncClient

router = APIRouter(prefix="/networks", tags=["networks"])

ClientDep = Annotated["AsyncClient", Depends(get_client)]


def _as_dict(obj: Any) -> Any:
    if is_dataclass(obj) and not isinstance(obj, type):
        d = {k: _as_dict(v) for k, v in asdict(obj).items()}
        # NetworkInfo.peer_node_ids is a tuple in the dataclass; flatten to
        # list so the JSON encoder treats it as an array.
        return d
    if isinstance(obj, list | tuple):
        return [_as_dict(v) for v in obj]
    return obj


@router.get("")
async def list_networks(client: ClientDep) -> list[dict[str, Any]]:
    """Mirrors ``crv network list``."""
    return [_as_dict(n) for n in await client.networks.list()]


@router.get("/{network_id}")
async def get_network(network_id: int, client: ClientDep) -> dict[str, Any]:
    """Network detail — same fields as the list entry."""
    try:
        net = await client.networks.get(network_id)
    except NetworkNotFound as exc:
        raise HTTPException(status_code=404, detail=str(exc)) from exc
    return _as_dict(await net.show())


@router.post("/{network_id}/start")
async def start_network(network_id: int, client: ClientDep) -> dict[str, str]:
    """Bring the bridge up and start the dnsmasq instance if DHCP/NAT
    are enabled. The daemon delegates to corvus-netd."""
    try:
        net = await client.networks.get(network_id)
    except NetworkNotFound as exc:
        raise HTTPException(status_code=404, detail=str(exc)) from exc
    await net.start()
    return {"status": "started"}


@router.post("/{network_id}/stop")
async def stop_network(
    network_id: int, client: ClientDep, force: bool = False
) -> dict[str, str]:
    """Tear down the bridge + dnsmasq. With ``?force=true`` the
    daemon detaches in-flight TAPs first — use only when a guest is
    holding the bridge open and you need to recover."""
    try:
        net = await client.networks.get(network_id)
    except NetworkNotFound as exc:
        raise HTTPException(status_code=404, detail=str(exc)) from exc
    await net.stop(force=force)
    return {"status": "stopped"}


@router.delete("/{network_id}")
async def delete_network(network_id: int, client: ClientDep) -> dict[str, str]:
    """Delete a stopped network. The daemon refuses if any net-if is
    still attached."""
    try:
        net = await client.networks.get(network_id)
    except NetworkNotFound as exc:
        raise HTTPException(status_code=404, detail=str(exc)) from exc
    await net.delete()
    return {"status": "deleted"}
