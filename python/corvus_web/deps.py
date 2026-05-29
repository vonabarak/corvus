"""Shared FastAPI dependencies.

The gateway holds exactly one ``AsyncClient`` per process — opened in
the app lifespan, stashed on ``app.state``. Routes pull it through the
:func:`get_client` dependency so unit tests can override it with a
fake client via the standard FastAPI ``dependency_overrides`` mechanism.
"""

from __future__ import annotations

from typing import TYPE_CHECKING

from fastapi import Request

if TYPE_CHECKING:
    from corvus_client import AsyncClient


def get_client(request: Request) -> AsyncClient:
    """Return the live AsyncClient for the current request.

    The lifespan in :func:`corvus_web.app.create_app` is responsible
    for opening it; if it's missing here the app was constructed
    without going through the factory."""
    client = getattr(request.app.state, "client", None)
    if client is None:
        raise RuntimeError(
            "corvus-web: AsyncClient not on app.state — "
            "the lifespan did not initialise correctly"
        )
    return client
