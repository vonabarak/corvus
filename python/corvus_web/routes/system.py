"""System-level endpoints: status and ping.

These hit the Daemon's top-level methods (no manager cap traversal),
making them the cheapest possible round-trip — useful for the
dashboard's "is the daemon reachable" indicator and for liveness
probes in front of the gateway.
"""

from __future__ import annotations

from typing import TYPE_CHECKING, Annotated, Any

from fastapi import APIRouter, Depends

from ..deps import get_client

if TYPE_CHECKING:
    from corvus_client import AsyncClient

router = APIRouter(tags=["system"])

# `Annotated[..., Depends(...)]` is the FastAPI-recommended pattern for
# dependency injection in modern code; it avoids the lint pitfall of
# calling Depends() inside a default-argument position.
ClientDep = Annotated["AsyncClient", Depends(get_client)]


@router.get("/ping")
async def ping(client: ClientDep) -> dict[str, str]:
    """Round-trip to the daemon. Returns {"status": "ok"} on success;
    the corvus_client exception translator surfaces ConnectError /
    ProtocolError as a FastAPI 500 by default."""
    await client.ping()
    return {"status": "ok"}


@router.get("/status")
async def status(client: ClientDep) -> dict[str, Any]:
    """Daemon uptime, connection count, version, protocol version.

    Mirrors ``crv status`` (see src/Corvus/Client/Commands/*.hs)."""
    info = await client.status()
    return {
        "uptime_seconds": info.uptime_seconds,
        "connections": info.connections,
        "version": info.version,
        "protocol_version": info.protocol_version,
    }
