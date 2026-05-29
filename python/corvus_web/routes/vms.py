"""VM endpoints: list, detail, lifecycle (start/stop/pause/reset/save), delete.

Routes are a thin shell over ``corvus_client.AsyncClient.vms``. Each
mutating endpoint returns ``{"task_id": ...}`` (when the daemon emits
one) or just the final status string, so the frontend can subscribe to
``/api/tasks/{id}/ws`` for live progress in a future slice.
"""

from __future__ import annotations

import asyncio
import logging
from contextlib import suppress
from dataclasses import asdict, is_dataclass
from typing import TYPE_CHECKING, Annotated, Any

from corvus_client.exceptions import CorvusError, VmNotFound
from fastapi import APIRouter, Depends, HTTPException, WebSocket, WebSocketDisconnect

from ..deps import get_client

if TYPE_CHECKING:
    from corvus_client import AsyncClient

router = APIRouter(prefix="/vms", tags=["vms"])

ClientDep = Annotated["AsyncClient", Depends(get_client)]

logger = logging.getLogger(__name__)


def _as_dict(obj: Any) -> Any:
    """Recursively convert a frozen dataclass tree (incl. datetimes,
    nested lists) into JSON-friendly primitives. FastAPI's default
    JSON encoder handles datetime, but explicit conversion gives us
    deterministic field naming and lets us strip ``None`` if we ever
    want to."""
    if is_dataclass(obj) and not isinstance(obj, type):
        return {k: _as_dict(v) for k, v in asdict(obj).items()}
    if isinstance(obj, list):
        return [_as_dict(v) for v in obj]
    return obj


@router.get("")
async def list_vms(client: ClientDep) -> list[dict[str, Any]]:
    """List every VM. Mirrors ``crv vm list``."""
    vms = await client.vms.list()
    return [_as_dict(v) for v in vms]


@router.get("/{vm_id}")
async def get_vm(vm_id: int, client: ClientDep) -> dict[str, Any]:
    """Full detail view. Mirrors ``crv vm show <id>``."""
    try:
        vm = await client.vms.get(vm_id)
    except VmNotFound as exc:
        raise HTTPException(status_code=404, detail=str(exc)) from exc
    details = await vm.show()
    return _as_dict(details)


@router.post("/{vm_id}/start")
async def start_vm(vm_id: int, client: ClientDep) -> dict[str, str]:
    """Start a stopped VM. Non-blocking — the daemon enqueues the task
    and returns the next state immediately."""
    try:
        vm = await client.vms.get(vm_id)
    except VmNotFound as exc:
        raise HTTPException(status_code=404, detail=str(exc)) from exc
    status = await vm.start(wait=False)
    return {"status": status}


@router.post("/{vm_id}/stop")
async def stop_vm(vm_id: int, client: ClientDep) -> dict[str, str]:
    """Graceful ACPI shutdown. Returns once the QMP request is sent;
    the guest's actual shutdown happens asynchronously."""
    try:
        vm = await client.vms.get(vm_id)
    except VmNotFound as exc:
        raise HTTPException(status_code=404, detail=str(exc)) from exc
    status = await vm.stop(wait=False)
    return {"status": status}


@router.post("/{vm_id}/pause")
async def pause_vm(vm_id: int, client: ClientDep) -> dict[str, str]:
    """Pause vCPU execution (QMP ``stop``)."""
    try:
        vm = await client.vms.get(vm_id)
    except VmNotFound as exc:
        raise HTTPException(status_code=404, detail=str(exc)) from exc
    status = await vm.pause()
    return {"status": status}


@router.post("/{vm_id}/reset")
async def reset_vm(vm_id: int, client: ClientDep) -> dict[str, str]:
    """Hard reset — equivalent to a power-cycle. Always returns to
    ``stopped`` so the operator can decide whether to start it again."""
    try:
        vm = await client.vms.get(vm_id)
    except VmNotFound as exc:
        raise HTTPException(status_code=404, detail=str(exc)) from exc
    status = await vm.reset()
    return {"status": status}


@router.post("/{vm_id}/save")
async def save_vm(vm_id: int, client: ClientDep) -> dict[str, str]:
    """Save VM state to disk (managedsave/migrate-to-file). The VM
    transitions to ``stopped`` after the state file is flushed."""
    try:
        vm = await client.vms.get(vm_id)
    except VmNotFound as exc:
        raise HTTPException(status_code=404, detail=str(exc)) from exc
    status = await vm.save()
    return {"status": status}


@router.post("/{vm_id}/send-ctrl-alt-del")
async def send_ctrl_alt_del(vm_id: int, client: ClientDep) -> dict[str, str]:
    """Send Ctrl+Alt+Del via QMP — handy for stuck Windows guests."""
    try:
        vm = await client.vms.get(vm_id)
    except VmNotFound as exc:
        raise HTTPException(status_code=404, detail=str(exc)) from exc
    await vm.send_ctrl_alt_del()
    return {"status": "sent"}


@router.delete("/{vm_id}")
async def delete_vm(
    vm_id: int,
    client: ClientDep,
    keep_disks: bool = False,
) -> dict[str, str]:
    """Delete a stopped VM. By default reaps ephemeral disks
    (cloud-init ISOs, template-instantiated overlays); pass
    ``?keep_disks=true`` to leave them behind."""
    try:
        vm = await client.vms.get(vm_id)
    except VmNotFound as exc:
        raise HTTPException(status_code=404, detail=str(exc)) from exc
    await vm.delete(keep_disks=keep_disks)
    return {"status": "deleted"}


# ---- per-VM cloud-init ---------------------------------------------------


@router.get("/{vm_id}/cloud-init")
async def get_cloud_init(vm_id: int, client: ClientDep) -> dict[str, Any]:
    """Read the VM's effective cloud-init config (user-data,
    network-config, inject_ssh_keys flag). Mirrors ``crv cloud-init
    show <vm>``. Editing lands with the YAML-editor slice."""
    try:
        vm = await client.vms.get(vm_id)
    except VmNotFound as exc:
        raise HTTPException(status_code=404, detail=str(exc)) from exc
    info = await vm.cloud_init()
    return _as_dict(info)


# ---- serial console WebSocket --------------------------------------------


@router.websocket("/{vm_id}/serial/ws")
async def serial_console_ws(ws: WebSocket, vm_id: int) -> None:
    """Bidirectional serial console over WebSocket.

    Wire shape:
      * Server → client: binary frames of raw bytes from the daemon
        (serial output: kernel boot, login prompt, shell stdout, …).
        Includes the ring-buffer replay on connect — the daemon
        emits the last ~1 MB of console history so reload doesn't
        lose context.
      * Client → server: binary frames of keystrokes (xterm.js
        emits these via ``onData`` after UTF-8-encoding).
      * Text frames in either direction are ignored — keeps the
        wire shape unambiguous and matches the daemon's ``ByteSink``.

    The two directions run as concurrent tasks; whichever ends first
    cancels the other. The daemon's ``ByteStream`` is closed in the
    ``finally`` so the daemon's ring-buffer subscriber count is
    correctly decremented even on abrupt client disconnects.

    Dependency injection: we read the client off ``ws.app.state``
    directly. The ``ClientDep`` shape is HTTP-Request-shaped; WS
    endpoints would need a parallel dep that takes ``WebSocket``,
    which is overkill for a single endpoint.
    """
    client = ws.app.state.client

    await ws.accept()

    try:
        vm = await client.vms.get(vm_id)
    except VmNotFound:
        await ws.close(code=1008, reason="VM not found")
        return
    except CorvusError as exc:
        logger.warning("serial WS: vm lookup failed: %s", exc)
        await ws.close(code=1011, reason=str(exc))
        return

    try:
        stream = await vm.serial_console()
    except CorvusError as exc:
        logger.warning("serial WS: open failed for vm %d: %s", vm_id, exc)
        # 1008 (policy violation) is the closest standard close code
        # for "the daemon refused (e.g. VM not running)".
        await ws.close(code=1008, reason=str(exc))
        return

    async def daemon_to_ws() -> None:
        while True:
            chunk = await stream.read()
            if chunk is None:
                return  # daemon closed the stream
            try:
                await ws.send_bytes(chunk)
            except (WebSocketDisconnect, RuntimeError):
                return  # client gone

    async def ws_to_daemon() -> None:
        while True:
            try:
                msg = await ws.receive()
            except WebSocketDisconnect:
                return
            if msg["type"] == "websocket.disconnect":
                return
            data = msg.get("bytes")
            if data:
                with suppress(CorvusError):
                    await stream.write(data)
                continue
            # Text frames are ignored; xterm.js sends binary by
            # default after we encode keystrokes ourselves.

    tasks = [
        asyncio.create_task(daemon_to_ws(), name=f"serial-d2c-{vm_id}"),
        asyncio.create_task(ws_to_daemon(), name=f"serial-c2d-{vm_id}"),
    ]
    try:
        # Whichever side finishes first (EOF from daemon, or client
        # disconnect) cancels the other so we don't leak a task.
        _, pending = await asyncio.wait(tasks, return_when=asyncio.FIRST_COMPLETED)
        for t in pending:
            t.cancel()
            with suppress(asyncio.CancelledError, Exception):
                await t
    finally:
        with suppress(Exception):
            await stream.close()
        with suppress(Exception):
            await ws.close()
