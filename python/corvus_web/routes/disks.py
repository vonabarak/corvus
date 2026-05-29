"""Disk endpoints: list, detail, resize, delete, and per-disk snapshots.

Thin shell over ``corvus_client.AsyncClient.disks``. This slice covers
the read path + the in-place mutations (resize, delete, snapshot CRUD
including rollback / merge). Creation flows — ``create``, ``register``,
``createOverlay``, ``clone``, ``import``, ``importUrl``, ``copy``,
``move`` — land in a follow-up once the New-Disk form UI is built.
"""

from __future__ import annotations

from dataclasses import asdict, is_dataclass
from typing import TYPE_CHECKING, Annotated, Any

from corvus_client.exceptions import DiskNotFound, SnapshotNotFound
from fastapi import APIRouter, Depends, HTTPException
from pydantic import BaseModel, Field

from ..deps import get_client

if TYPE_CHECKING:
    from corvus_client import AsyncClient

router = APIRouter(prefix="/disks", tags=["disks"])

ClientDep = Annotated["AsyncClient", Depends(get_client)]


def _as_dict(obj: Any) -> Any:
    if is_dataclass(obj) and not isinstance(obj, type):
        return {k: _as_dict(v) for k, v in asdict(obj).items()}
    if isinstance(obj, list):
        return [_as_dict(v) for v in obj]
    return obj


class ResizeBody(BaseModel):
    new_size_mb: int = Field(..., gt=0, description="New disk size in MB (must grow).")


class SnapshotCreateBody(BaseModel):
    name: str = Field(..., min_length=1, description="Snapshot tag (unique per disk).")


# ---- Disks --------------------------------------------------------------


@router.get("")
async def list_disks(client: ClientDep) -> list[dict[str, Any]]:
    """Mirrors ``crv disk list``. Returns every registered disk with
    its placements, attached-VMs summary, backing-image link, and
    ephemeral flag."""
    return [_as_dict(d) for d in await client.disks.list()]


@router.get("/{disk_id}")
async def get_disk(disk_id: int, client: ClientDep) -> dict[str, Any]:
    """Disk detail. Same payload as the list entry — the daemon's
    DiskImageInfo is already the full picture."""
    try:
        disk = await client.disks.get(disk_id)
    except DiskNotFound as exc:
        raise HTTPException(status_code=404, detail=str(exc)) from exc
    return _as_dict(await disk.show())


@router.post("/{disk_id}/resize")
async def resize_disk(
    disk_id: int, body: ResizeBody, client: ClientDep
) -> dict[str, str]:
    """Grow a qcow2/raw disk. Shrinking is rejected by qemu-img;
    the daemon surfaces that error verbatim."""
    try:
        disk = await client.disks.get(disk_id)
    except DiskNotFound as exc:
        raise HTTPException(status_code=404, detail=str(exc)) from exc
    await disk.resize(body.new_size_mb)
    return {"status": "resized"}


@router.delete("/{disk_id}")
async def delete_disk(disk_id: int, client: ClientDep) -> dict[str, str]:
    """Delete a disk. The daemon refuses if the disk has overlays or
    is attached to a VM — those errors surface as 4xx via the
    exception translator."""
    try:
        disk = await client.disks.get(disk_id)
    except DiskNotFound as exc:
        raise HTTPException(status_code=404, detail=str(exc)) from exc
    await disk.delete()
    return {"status": "deleted"}


# ---- Snapshots ----------------------------------------------------------


@router.get("/{disk_id}/snapshots")
async def list_snapshots(disk_id: int, client: ClientDep) -> list[dict[str, Any]]:
    """List qcow2 snapshots inside the disk."""
    try:
        disk = await client.disks.get(disk_id)
    except DiskNotFound as exc:
        raise HTTPException(status_code=404, detail=str(exc)) from exc
    return [_as_dict(s) for s in await disk.snapshot_list()]


@router.post("/{disk_id}/snapshots")
async def create_snapshot(
    disk_id: int, body: SnapshotCreateBody, client: ClientDep
) -> dict[str, Any]:
    """Take a snapshot. Returns the resulting SnapshotInfo."""
    try:
        disk = await client.disks.get(disk_id)
    except DiskNotFound as exc:
        raise HTTPException(status_code=404, detail=str(exc)) from exc
    snap = await disk.snapshot_create(body.name)
    return _as_dict(await snap.show())


async def _get_snapshot(client: AsyncClient, disk_id: int, snap_id: int):  # type: ignore[no-untyped-def]
    """Helper: resolve (disk_id, snap_id) -> AsyncSnapshot, raising
    404 for either step."""
    try:
        disk = await client.disks.get(disk_id)
    except DiskNotFound as exc:
        raise HTTPException(status_code=404, detail=str(exc)) from exc
    try:
        return await disk.snapshot_get(snap_id)
    except SnapshotNotFound as exc:
        raise HTTPException(status_code=404, detail=str(exc)) from exc


@router.post("/{disk_id}/snapshots/{snap_id}/rollback")
async def rollback_snapshot(
    disk_id: int, snap_id: int, client: ClientDep
) -> dict[str, str]:
    """Restore the disk to this snapshot. VMs using the disk must be
    stopped."""
    snap = await _get_snapshot(client, disk_id, snap_id)
    await snap.rollback()
    return {"status": "rolled-back"}


@router.post("/{disk_id}/snapshots/{snap_id}/merge")
async def merge_snapshot(
    disk_id: int, snap_id: int, client: ClientDep
) -> dict[str, str]:
    """Merge the snapshot into its parent (qemu-img snapshot -a + delete)."""
    snap = await _get_snapshot(client, disk_id, snap_id)
    await snap.merge()
    return {"status": "merged"}


@router.delete("/{disk_id}/snapshots/{snap_id}")
async def delete_snapshot(
    disk_id: int, snap_id: int, client: ClientDep
) -> dict[str, str]:
    snap = await _get_snapshot(client, disk_id, snap_id)
    await snap.delete()
    return {"status": "deleted"}
