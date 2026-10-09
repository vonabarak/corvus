"""Shared FastAPI dependencies.

The gateway owns one replaceable daemon session. Routes pull its client through the
:func:`get_client` dependency so unit tests can override it with a
fake client via the standard FastAPI ``dependency_overrides`` mechanism.
"""

from __future__ import annotations

from typing import TYPE_CHECKING, cast

from fastapi import Request

if TYPE_CHECKING:
    from corvus_client import AsyncClient

    from .connection import DaemonConnection


def get_client(request: Request) -> AsyncClient:
    """Return the live AsyncClient for the current request.

    A daemon outage raises ConnectError, mapped to HTTP 502 by the app."""
    return cast("DaemonConnection", request.app.state.connection).get_session().client
