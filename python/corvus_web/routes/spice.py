"""In-browser SPICE console — REST grant + WebSocket TCP bridge.

Two endpoints sit behind the spice-html5 client embedded in the SPA.
See doc/web-interface.md (Graphical console) for the operator-facing
story, and routes/vms.py serial_console_ws for the parallel serial
bridge that this module mirrors.

Why two endpoints (and not "everything in the WS")
--------------------------------------------------
spice-html5's ``SpiceMainConn`` opens its own WebSocket given a ``uri``
config. It doesn't accept a pre-opened socket, and the password it
ships in the SPICE handshake comes from JS config — not from anything
it reads off the wire. So the gateway has to give the browser the
password before the WS opens.

The naive flow ("REST returns password, WS does its own grant") has
a subtle race: each ``viewGrant`` call overwrites QEMU's stored
ticket, so calling it from both endpoints would mean the password the
browser holds is no longer the one QEMU expects. Hence the session
table: one grant per console session, password to the browser, host +
port stashed server-side, opaque token threads the two together.

Sessions are **not** one-shot: SPICE multiplexes channels (main,
display, inputs, cursor, playback, …) over independent connections,
and spice-html5 opens a fresh WebSocket to the same URI for each one.
The token therefore stays valid for the lifetime of the operator's
console tab, gated only by a TTL (well past the daemon's 120-s grant
expiry — once the SPICE handshake on the main channel succeeds, the
ticket has done its job and QEMU lets the other channels in by
session-id). The dict lives in process memory; restart kills
outstanding sessions but the browser just makes a new one on the
next user action.
"""

from __future__ import annotations

import asyncio
import logging
import secrets
import time
from contextlib import suppress
from dataclasses import dataclass
from typing import TYPE_CHECKING, Annotated

from corvus_client.exceptions import CorvusError, VmNotFound
from fastapi import APIRouter, Depends, HTTPException, WebSocket, WebSocketDisconnect

from ..deps import get_client

if TYPE_CHECKING:
    from corvus_client import AsyncClient

router = APIRouter(prefix="/vms", tags=["spice"])

ClientDep = Annotated["AsyncClient", Depends(get_client)]

logger = logging.getLogger(__name__)


# ---- session table -------------------------------------------------------


@dataclass(frozen=True)
class _Session:
    vm_id: int
    host: str
    port: int
    expires_at: float  # monotonic seconds, like time.monotonic()


# Bare dict is fine — gateway is single-process, accesses are short, and
# Python's GIL makes dict assignment + lookup atomic. The asyncio.Lock
# only matters because consume() does a read-then-delete that a second
# coroutine could otherwise interleave with.
_SESSIONS: dict[str, _Session] = {}
_LOCK = asyncio.Lock()


def _reap_expired(now: float) -> None:
    """Drop any session whose grant has already expired daemon-side.

    Called inside _LOCK from consume(); a separate background reaper
    would be overkill given the volume (one session per console-open).
    """
    stale = [k for k, s in _SESSIONS.items() if s.expires_at <= now]
    for k in stale:
        _SESSIONS.pop(k, None)


async def _put_session(session: _Session) -> str:
    """Mint a fresh session id and store the grant under it.

    secrets.token_urlsafe(32) gives ~256 bits of entropy — plenty for a
    short-lived single-process identifier."""
    session_id = secrets.token_urlsafe(32)
    async with _LOCK:
        _SESSIONS[session_id] = session
    return session_id


async def _lookup(session_id: str) -> _Session | None:
    """Look up the session without removing it. SPICE opens one WS per
    channel (main / display / inputs / cursor / playback), so the same
    token has to authorise multiple connections for the lifetime of
    the operator's console tab. Stale entries are reaped on each
    lookup; nothing else expires them."""
    now = time.monotonic()
    async with _LOCK:
        _reap_expired(now)
        return _SESSIONS.get(session_id)


# ---- REST: mint a grant + session ----------------------------------------


@router.post("/{vm_id}/spice")
async def create_spice_session(vm_id: int, client: ClientDep) -> dict[str, object]:
    """Issue a single-use SPICE session: call viewGrant on the VM, push
    the password to QEMU via QMP, hand the password back to the
    browser, stash the host:port server-side under an opaque token.

    The browser then opens
    ``/api/vms/{vm_id}/spice/ws?session=<token>`` — see the WS handler
    below.
    """
    try:
        vm = await client.vms.get(vm_id)
    except VmNotFound as exc:
        raise HTTPException(status_code=404, detail=str(exc)) from exc
    try:
        grant = await vm.view_grant()
    except CorvusError as exc:
        # Most common cause: VM not running, or headless VM with no
        # SPICE device. Daemon's message is already user-friendly.
        raise HTTPException(status_code=400, detail=str(exc)) from exc

    # Gateway-side TTL outlives the daemon's 120-s grant expiry: once
    # the SPICE main channel finishes its password-authenticated
    # handshake, QEMU lets the sibling channels join by session-id, so
    # we only need the token valid long enough for the operator's tab
    # to stay open. 1 hour is generous and still lets stale entries
    # get reaped before the dict balloons.
    expires_at = time.monotonic() + 3600
    session_id = await _put_session(
        _Session(vm_id=vm_id, host=grant.host, port=grant.port, expires_at=expires_at)
    )
    return {
        "session_id": session_id,
        "password": grant.password,
        "ttl_seconds": grant.ttl_seconds,
    }


# ---- WebSocket: bridge to QEMU's SPICE TCP listener ----------------------


@router.websocket("/{vm_id}/spice/ws")
async def spice_console_ws(ws: WebSocket, vm_id: int) -> None:
    """Pipe binary frames between the browser's WebSocket and the
    daemon-allocated SPICE TCP port. Mirrors serial_console_ws in
    routes/vms.py — same two-task FIRST_COMPLETED bridge, different
    daemon-side source (asyncio TCP, not Cap'n Proto sink).
    """
    session_id = ws.query_params.get("session", "")
    if not session_id:
        await ws.close(code=1008, reason="missing session token")
        return

    session = await _lookup(session_id)
    if session is None:
        await ws.close(code=1008, reason="unknown or expired session")
        return
    if session.vm_id != vm_id:
        # Token issued for a different VM — defence-in-depth against a
        # browser bug that confuses VM ids.
        await ws.close(code=1008, reason="session/vm mismatch")
        return

    await ws.accept()

    try:
        reader, writer = await asyncio.open_connection(session.host, session.port)
    except OSError as exc:
        logger.warning(
            "SPICE WS: open_connection(%s, %s) failed for vm %d: %s",
            session.host,
            session.port,
            vm_id,
            exc,
        )
        # 1011: server abort. Operator typically sees "VM stopped
        # before console connected" in this path.
        await ws.close(code=1011, reason=f"connect failed: {exc}")
        return

    async def ws_to_tcp() -> None:
        while True:
            try:
                msg = await ws.receive()
            except WebSocketDisconnect:
                return
            if msg["type"] == "websocket.disconnect":
                return
            data = msg.get("bytes")
            if data:
                writer.write(data)
                with suppress(Exception):
                    await writer.drain()
            # Text frames are ignored — spice-html5 only sends binary.

    async def tcp_to_ws() -> None:
        # 65 KiB matches what wsproto / starlette pick as a typical
        # per-frame ceiling; smaller would just add WS framing
        # overhead, larger doesn't help with SPICE's many small messages.
        while True:
            try:
                chunk = await reader.read(65536)
            except (ConnectionResetError, BrokenPipeError):
                return
            if not chunk:
                return  # daemon EOF
            try:
                await ws.send_bytes(chunk)
            except (WebSocketDisconnect, RuntimeError):
                return

    tasks = [
        asyncio.create_task(ws_to_tcp(), name=f"spice-w2t-{vm_id}"),
        asyncio.create_task(tcp_to_ws(), name=f"spice-t2w-{vm_id}"),
    ]
    try:
        _, pending = await asyncio.wait(tasks, return_when=asyncio.FIRST_COMPLETED)
        for t in pending:
            t.cancel()
            with suppress(asyncio.CancelledError, Exception):
                await t
    finally:
        # Close TCP first — the daemon side then surfaces EOF on its
        # half, which is the QMP-aware shutdown path.
        with suppress(Exception):
            writer.close()
            await writer.wait_closed()
        with suppress(Exception):
            await ws.close()
