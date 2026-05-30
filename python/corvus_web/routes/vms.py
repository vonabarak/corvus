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
from datetime import date, datetime
from typing import TYPE_CHECKING, Annotated, Any

from corvus_client.exceptions import CorvusError, VmNotFound
from fastapi import APIRouter, Depends, HTTPException, WebSocket, WebSocketDisconnect
from pydantic import BaseModel, Field

from ..deps import get_client

if TYPE_CHECKING:
    from corvus_client import AsyncClient

router = APIRouter(prefix="/vms", tags=["vms"])

ClientDep = Annotated["AsyncClient", Depends(get_client)]

logger = logging.getLogger(__name__)


def _as_dict(obj: Any) -> Any:
    """Recursively convert a frozen dataclass tree (incl. datetimes,
    nested lists) into JSON-friendly primitives.

    REST handlers used to rely on FastAPI's default JSON encoder to
    stringify datetimes — but the WebSocket path goes through
    Starlette's ``ws.send_json`` which calls plain ``json.dumps`` and
    raises ``TypeError`` on a raw ``datetime``. Convert ISO 8601 here
    so both surfaces emit the same string and the WS doesn't crash on
    fields like ``GuestAgentStatus.last_healthcheck``.
    """
    if is_dataclass(obj) and not isinstance(obj, type):
        return {k: _as_dict(v) for k, v in asdict(obj).items()}
    if isinstance(obj, list):
        return [_as_dict(v) for v in obj]
    if isinstance(obj, datetime):
        return obj.isoformat()
    if isinstance(obj, date):
        return obj.isoformat()
    return obj


@router.get("")
async def list_vms(client: ClientDep) -> list[dict[str, Any]]:
    """List every VM. Mirrors ``crv vm list``."""
    vms = await client.vms.list()
    return [_as_dict(v) for v in vms]


class VmCreateBody(BaseModel):
    """Mirrors corvus_client.AsyncVmManager.create kwargs.

    `node` accepts a node name or numeric id as a string — the daemon's
    EntityRef resolver handles both. Leave blank to let the scheduler
    pick (lowest free-RAM penalty, ties broken by node name).
    """

    name: str = Field(..., min_length=1, description="VM name (unique).")
    node: str | None = Field(None, description="Pin to a specific node by name or id.")
    cpu_count: int = Field(1, ge=1, le=256)
    ram_mb: int = Field(1024, ge=64)
    description: str | None = Field(None, max_length=1024)
    headless: bool = False
    guest_agent: bool = False
    cloud_init: bool = False
    autostart: bool = False
    reboot_quirk: bool = False
    cpu_model: str = Field(
        "host",
        description=(
            "QEMU -cpu value. Default `host` exposes every available "
            "host CPU feature; pick a stable model name "
            "(e.g. `Westmere`) for migration between dissimilar nodes."
        ),
    )


@router.post("")
async def create_vm(body: VmCreateBody, client: ClientDep) -> dict[str, Any]:
    """Create a bare VM record. Drives, NICs, SSH keys, cloud-init are
    attached after the fact (or in one shot via /api/apply). Returns
    the new VM's detail payload so the frontend can route straight to
    it."""
    try:
        vm = await client.vms.create(
            body.name,
            node=body.node,
            cpu_count=body.cpu_count,
            ram_mb=body.ram_mb,
            description=body.description,
            headless=body.headless,
            guest_agent=body.guest_agent,
            cloud_init=body.cloud_init,
            autostart=body.autostart,
            reboot_quirk=body.reboot_quirk,
            cpu_model=body.cpu_model,
        )
    except CorvusError as exc:
        # Most likely a name collision or unknown node — surface as 400
        # so the form can re-render with the daemon's message.
        raise HTTPException(status_code=400, detail=str(exc)) from exc
    details = await vm.show()
    return _as_dict(details)


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


# ---- attach/detach: drives, NICs, SSH keys -------------------------------
#
# Mid-life mutations on a VM. The daemon usually requires the VM to be
# stopped (drives) but tolerates hot-attach for some NIC types — we
# leave that policy to the daemon and surface its error verbatim.


def _coerce_ref(ref: str) -> int | str:
    """`EntityRef` resolver accepts either an integer id or a name
    string. Coerce numeric-looking strings so the dropdown's id value
    takes the id path."""
    try:
        return int(ref)
    except ValueError:
        return ref


class DriveAttachBody(BaseModel):
    """Mirrors corvus_client.AsyncVm.attach_disk kwargs.

    ``disk_ref`` is a disk id or name (the daemon resolves both via
    ``EntityRef``). All other knobs are optional — the daemon picks
    sensible defaults from the disk's existing format and the VM's
    other drives.
    """

    disk_ref: str = Field(..., min_length=1, description="Disk id or name.")
    interface: str | None = Field(
        None, description="virtio (default) / scsi / ide / sata / floppy."
    )
    media: str | None = Field(None, description="disk (default) / cdrom / floppy.")
    read_only: bool = False
    cache_type: str | None = Field(
        None, description="none / writethrough / writeback / directsync / unsafe."
    )
    discard: bool = False


@router.post("/{vm_id}/drives")
async def attach_drive(
    vm_id: int, body: DriveAttachBody, client: ClientDep
) -> dict[str, int]:
    try:
        vm = await client.vms.get(vm_id)
    except VmNotFound as exc:
        raise HTTPException(status_code=404, detail=str(exc)) from exc
    try:
        drive_id = await vm.attach_disk(
            _coerce_ref(body.disk_ref),
            interface=body.interface,
            media=body.media,
            read_only=body.read_only,
            cache_type=body.cache_type,
            discard=body.discard,
        )
    except CorvusError as exc:
        raise HTTPException(status_code=400, detail=str(exc)) from exc
    return {"drive_id": drive_id}


@router.delete("/{vm_id}/drives/{drive_id}")
async def detach_drive(vm_id: int, drive_id: int, client: ClientDep) -> dict[str, str]:
    try:
        vm = await client.vms.get(vm_id)
    except VmNotFound as exc:
        raise HTTPException(status_code=404, detail=str(exc)) from exc
    try:
        await vm.detach_disk(drive_id)
    except CorvusError as exc:
        raise HTTPException(status_code=400, detail=str(exc)) from exc
    return {"status": "detached"}


class NetIfAddBody(BaseModel):
    """Mirrors corvus_client.AsyncVm.add_net_if kwargs.

    All fields are optional — the schema's defaults model the
    common case (``type=user`` SLIRP-mode with a daemon-generated
    MAC). ``network_ref`` is required for ``type=managed``; the
    daemon refuses otherwise.
    """

    type: str | None = Field(
        None, description="user (default) / tap / bridge / macvtap / managed."
    )
    host_device: str | None = Field(
        None, description="Host-side device name for tap / bridge / macvtap."
    )
    mac_address: str | None = Field(
        None,
        description=(
            "Override the auto-generated MAC. Use the prefix from "
            "``crv network show`` to stay inside the managed range."
        ),
    )
    network_ref: str | None = Field(
        None, description="Managed-network id or name (required for type=managed)."
    )


@router.post("/{vm_id}/net-ifs")
async def add_net_if(
    vm_id: int, body: NetIfAddBody, client: ClientDep
) -> dict[str, int]:
    try:
        vm = await client.vms.get(vm_id)
    except VmNotFound as exc:
        raise HTTPException(status_code=404, detail=str(exc)) from exc
    try:
        net_if_id = await vm.add_net_if(
            type=body.type,
            host_device=body.host_device,
            mac_address=body.mac_address,
            network_ref=_coerce_ref(body.network_ref) if body.network_ref else None,
        )
    except CorvusError as exc:
        raise HTTPException(status_code=400, detail=str(exc)) from exc
    return {"net_if_id": net_if_id}


@router.delete("/{vm_id}/net-ifs/{net_if_id}")
async def remove_net_if(
    vm_id: int, net_if_id: int, client: ClientDep
) -> dict[str, str]:
    try:
        vm = await client.vms.get(vm_id)
    except VmNotFound as exc:
        raise HTTPException(status_code=404, detail=str(exc)) from exc
    try:
        await vm.remove_net_if(net_if_id)
    except CorvusError as exc:
        raise HTTPException(status_code=400, detail=str(exc)) from exc
    return {"status": "removed"}


class SshKeyAttachBody(BaseModel):
    """SSH-key attach body. ``key_ref`` accepts id or name."""

    key_ref: str = Field(..., min_length=1)


@router.get("/{vm_id}/ssh-keys")
async def list_vm_ssh_keys(vm_id: int, client: ClientDep) -> list[dict[str, Any]]:
    """SSH keys attached to this VM. The VmDetails payload doesn't
    carry the list, so the SSH-keys card on VmDetail fetches this
    separately."""
    try:
        vm = await client.vms.get(vm_id)
    except VmNotFound as exc:
        raise HTTPException(status_code=404, detail=str(exc)) from exc
    keys = await vm.list_ssh_keys()
    return [_as_dict(k) for k in keys]


@router.post("/{vm_id}/ssh-keys")
async def attach_ssh_key(
    vm_id: int, body: SshKeyAttachBody, client: ClientDep
) -> dict[str, str]:
    try:
        vm = await client.vms.get(vm_id)
    except VmNotFound as exc:
        raise HTTPException(status_code=404, detail=str(exc)) from exc
    try:
        await vm.attach_ssh_key(_coerce_ref(body.key_ref))
    except CorvusError as exc:
        raise HTTPException(status_code=400, detail=str(exc)) from exc
    return {"status": "attached"}


@router.delete("/{vm_id}/ssh-keys/{key_ref}")
async def detach_ssh_key(vm_id: int, key_ref: str, client: ClientDep) -> dict[str, str]:
    """``key_ref`` is a path parameter accepting either an integer id
    or a name. The corvus_client EntityRef resolver picks the right
    path."""
    try:
        vm = await client.vms.get(vm_id)
    except VmNotFound as exc:
        raise HTTPException(status_code=404, detail=str(exc)) from exc
    try:
        await vm.detach_ssh_key(_coerce_ref(key_ref))
    except CorvusError as exc:
        raise HTTPException(status_code=400, detail=str(exc)) from exc
    return {"status": "detached"}


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


# ---- guest-agent status WebSocket ----------------------------------------


@router.websocket("/{vm_id}/guest-agent/ws")
async def guest_agent_ws(ws: WebSocket, vm_id: int) -> None:
    """Subscribe to per-VM guest-agent reachability events.

    The daemon's poller emits a ``GuestAgentStatus`` on every poll
    cycle (default: 5 s) per subscriber. We forward each as one JSON
    frame:

        {"vm_id": 7, "enabled": true, "reachable": true,
         "last_healthcheck": "2026-05-29T...", "message": null}

    Closing the WS drops the subscription handle, which prunes us
    from the daemon's subscriber list."""
    client = ws.app.state.client
    await ws.accept()

    try:
        vm = await client.vms.get(vm_id)
    except VmNotFound:
        await ws.close(code=1008, reason="VM not found")
        return

    queue: asyncio.Queue[Any] = asyncio.Queue()

    async def on_event(status: Any) -> None:
        await queue.put(status)

    try:
        subscription = await vm.subscribe_guest_agent(on_event)
    except CorvusError as exc:
        logger.warning("guest-agent WS: subscribe failed for vm %d: %s", vm_id, exc)
        await ws.close(code=1011, reason=str(exc))
        return

    async def queue_to_ws() -> None:
        while True:
            status = await queue.get()
            try:
                await ws.send_json(_as_dict(status))
            except (WebSocketDisconnect, RuntimeError):
                return

    async def watch_disconnect() -> None:
        while True:
            try:
                msg = await ws.receive()
            except WebSocketDisconnect:
                return
            if msg["type"] == "websocket.disconnect":
                return

    tasks_ = [
        asyncio.create_task(queue_to_ws(), name=f"guest-agent-{vm_id}"),
        asyncio.create_task(watch_disconnect(), name=f"guest-agent-watch-{vm_id}"),
    ]
    try:
        _, pending = await asyncio.wait(tasks_, return_when=asyncio.FIRST_COMPLETED)
        for t in pending:
            t.cancel()
            with suppress(asyncio.CancelledError, Exception):
                await t
    finally:
        with suppress(Exception):
            await subscription.close()
        with suppress(Exception):
            await ws.close()


# ---- VM resource stats: history + live WebSocket -------------------------


@router.get("/{vm_id}/stats/history")
async def vm_stats_history(
    vm_id: int,
    client: Annotated[Any, Depends(get_client)],
) -> list[dict[str, Any]]:
    """Return the daemon's stats ring buffer for one VM (up to 60
    samples, oldest first). Empty when the VM is stopped or has
    not been polled yet."""
    try:
        vm = await client.vms.get(vm_id)
    except VmNotFound:
        raise HTTPException(404, "VM not found") from None
    history = await vm.get_stats_history()
    return [_as_dict(s) for s in history]


@router.websocket("/{vm_id}/stats/ws")
async def vm_stats_ws(ws: WebSocket, vm_id: int) -> None:
    """Live `VmStats` push (~10s cadence). Each frame is a JSON
    object mirroring the `VmStats` dataclass (snake_case fields,
    cumulative counters)."""
    client = ws.app.state.client
    await ws.accept()

    try:
        vm = await client.vms.get(vm_id)
    except VmNotFound:
        await ws.close(code=1008, reason="VM not found")
        return

    queue: asyncio.Queue[Any] = asyncio.Queue()

    async def on_event(stats: Any) -> None:
        await queue.put(stats)

    try:
        subscription = await vm.subscribe_stats(on_event)
    except CorvusError as exc:
        logger.warning("vm-stats WS: subscribe failed for vm %d: %s", vm_id, exc)
        await ws.close(code=1011, reason=str(exc))
        return

    async def queue_to_ws() -> None:
        while True:
            stats = await queue.get()
            try:
                await ws.send_json(_as_dict(stats))
            except (WebSocketDisconnect, RuntimeError):
                return

    async def watch_disconnect() -> None:
        while True:
            try:
                msg = await ws.receive()
            except WebSocketDisconnect:
                return
            if msg["type"] == "websocket.disconnect":
                return

    tasks_ = [
        asyncio.create_task(queue_to_ws(), name=f"vm-stats-{vm_id}"),
        asyncio.create_task(watch_disconnect(), name=f"vm-stats-watch-{vm_id}"),
    ]
    try:
        _, pending = await asyncio.wait(tasks_, return_when=asyncio.FIRST_COMPLETED)
        for t in pending:
            t.cancel()
            with suppress(asyncio.CancelledError, Exception):
                await t
    finally:
        with suppress(Exception):
            await subscription.close()
        with suppress(Exception):
            await ws.close()
