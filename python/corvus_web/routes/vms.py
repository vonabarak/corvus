"""VM endpoints: list, detail, lifecycle (start/stop/pause/reset/save), delete.

Routes are a thin shell over ``corvus_client.AsyncClient.vms``. Each
mutating endpoint returns ``{"task_id": ...}`` (when the daemon emits
one) or just the final status string, so the frontend can subscribe to
``/api/tasks/{id}/ws`` for live progress in a future slice.
"""

from __future__ import annotations

from dataclasses import asdict, is_dataclass
from typing import TYPE_CHECKING, Annotated, Any

from corvus_client.exceptions import VmNotFound
from fastapi import APIRouter, Depends, HTTPException

from ..deps import get_client

if TYPE_CHECKING:
    from corvus_client import AsyncClient

router = APIRouter(prefix="/vms", tags=["vms"])

ClientDep = Annotated["AsyncClient", Depends(get_client)]


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
