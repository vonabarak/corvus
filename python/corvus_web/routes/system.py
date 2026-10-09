"""System-level endpoints: status and ping.

These hit the Daemon's top-level methods (no manager cap traversal),
making them the cheapest possible round-trip — useful for the
dashboard's "is the daemon reachable" indicator and for liveness
probes in front of the gateway.
"""

from __future__ import annotations

from typing import TYPE_CHECKING, Annotated

from fastapi import APIRouter, Depends

from ..deps import get_client
from ..lib import JsonObject

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
    a missing or disconnected daemon session returns HTTP 502."""
    await client.ping()
    return {"status": "ok"}


@router.get("/status")
async def status(client: ClientDep) -> JsonObject:
    """Daemon uptime, connection count, version, protocol, and database info.

    Mirrors ``crv status`` (see src/Corvus/Client/Commands/*.hs)."""
    info = await client.status()
    return {
        "uptime_seconds": info.uptime_seconds,
        "connections": info.connections,
        "version": info.version,
        "protocol_version": info.protocol_version,
        "database_backend": info.database_backend,
        "database_version": info.database_version,
    }
