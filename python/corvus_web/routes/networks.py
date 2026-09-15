"""Network endpoints: list, detail, create, start/stop, delete.

Thin shell over ``corvus_client.AsyncClient.networks``. Edit (rename,
re-CIDR, toggle DHCP/NAT/autostart) and peer attach/detach for
multi-node overlays are still TODO.
"""

from __future__ import annotations

from typing import TYPE_CHECKING, Annotated, Any

from corvus_client.exceptions import CorvusError, NetworkNotFound
from fastapi import APIRouter, Depends, HTTPException
from pydantic import BaseModel, Field

from ..deps import get_client
from ..lib import to_dict

if TYPE_CHECKING:
    from corvus_client import AsyncClient

router = APIRouter(prefix="/networks", tags=["networks"])

ClientDep = Annotated["AsyncClient", Depends(get_client)]


@router.get("")
async def list_networks(client: ClientDep) -> list[dict[str, Any]]:
    """Mirrors ``crv network list``."""
    return [to_dict(n) for n in await client.networks.list()]


class NetworkCreateBody(BaseModel):
    """Mirrors corvus_client.AsyncNetworkManager.create kwargs.

    Networks are per-node (the bridge + dnsmasq instance live on one
    host's root netns). Pass ``node`` to pin, or leave it null to let
    the daemon's scheduler pick.
    """

    name: str = Field(..., min_length=1, description="Unique network name.")
    subnet: str = Field(
        ...,
        min_length=1,
        description=(
            "CIDR for the bridge's gateway IP and the DHCP pool, e.g. ``10.10.0.0/24``."
        ),
    )
    node: str | None = Field(
        None, description="Node name or id; null = scheduler picks."
    )
    dhcp: bool = Field(
        False,
        description="Start a dnsmasq DHCP server bound to the bridge.",
    )
    nat: bool = Field(
        False,
        description="Add an nftables masquerade for traffic leaving the bridge.",
    )
    autostart: bool = Field(
        False,
        description="Start the bridge whenever the daemon comes up.",
    )


@router.post("")
async def create_network(body: NetworkCreateBody, client: ClientDep) -> dict[str, Any]:
    """Create a virtual network record. Returns the new NetworkInfo
    so the frontend can route straight to the detail page. The bridge
    is not started here — call POST /api/networks/{id}/start (or set
    autostart=true at create time) to bring it up."""
    try:
        net = await client.networks.create(
            body.name,
            body.subnet,
            node=body.node or None,
            dhcp=body.dhcp,
            nat=body.nat,
            autostart=body.autostart,
        )
    except CorvusError as exc:
        raise HTTPException(status_code=400, detail=str(exc)) from exc
    return to_dict(await net.show())


@router.get("/{network_id}")
async def get_network(network_id: int, client: ClientDep) -> dict[str, Any]:
    """Network detail — same fields as the list entry."""
    try:
        net = await client.networks.get(network_id)
    except NetworkNotFound as exc:
        raise HTTPException(status_code=404, detail=str(exc)) from exc
    return to_dict(await net.show())


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
