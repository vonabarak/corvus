"""Tie daemon-backed WebSockets to the session that owns their capabilities."""

from __future__ import annotations

import asyncio
from collections.abc import Awaitable, Callable
from contextlib import suppress
from typing import cast

from corvus_client import AsyncClient
from corvus_client.exceptions import ConnectError
from fastapi import WebSocket

from .connection import DaemonConnection


async def run_daemon_websocket(
    ws: WebSocket, handler: Callable[[AsyncClient], Awaitable[None]]
) -> None:
    """Close even an idle subscription when its daemon session disconnects."""
    await ws.accept()
    connection = cast(DaemonConnection, ws.app.state.connection)
    try:
        session = connection.get_session()
    except ConnectError:
        await ws.close(code=1013, reason="Corvus daemon is unavailable")
        return

    async def forward() -> None:
        await handler(session.client)

    stream = asyncio.create_task(forward())
    disconnected = asyncio.create_task(session.disconnected.wait())
    try:
        await asyncio.wait((stream, disconnected), return_when=asyncio.FIRST_COMPLETED)
        if session.disconnected.is_set():
            with suppress(RuntimeError):
                await ws.close(code=1013, reason="Corvus daemon disconnected")
        else:
            try:
                await stream
            except ConnectError:
                with suppress(RuntimeError):
                    await ws.close(code=1013, reason="Corvus daemon disconnected")
    finally:
        stream.cancel()
        disconnected.cancel()
        await asyncio.gather(stream, disconnected, return_exceptions=True)
        with suppress(RuntimeError):
            await ws.close()
