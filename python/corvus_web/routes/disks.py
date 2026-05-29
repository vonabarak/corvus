"""Disk endpoints: list, detail, resize, delete, per-disk snapshots,
plus the four creation flows the New-Disk form drives (blank create,
qcow2 overlay, clone, URL import).

Thin shell over ``corvus_client.AsyncClient.disks``. Still deferred —
``register`` (existing on-disk file), ``import`` (local file copy),
``copy``/``move`` (between nodes) — those are operator-side workflows
the CLI already covers and aren't on the v1 critical path.
"""

from __future__ import annotations

from dataclasses import asdict, is_dataclass
from typing import TYPE_CHECKING, Annotated, Any

from corvus_client.exceptions import CorvusError, DiskNotFound, SnapshotNotFound
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


class DiskCreateBody(BaseModel):
    """Mirrors corvus_client.AsyncDiskManager.create. Allocates a fresh
    image on the picked node's base_path (or scheduler choice).
    """

    name: str = Field(..., min_length=1)
    size_mb: int = Field(..., gt=0)
    format: str | None = Field(
        None, description="qcow2 (default) / raw / vmdk / vdi — see DriveFormat enum."
    )
    ephemeral: bool = Field(
        False,
        description=(
            "Auto-delete with the VM the disk is attached to. Useful for "
            "scratch overlays the operator never wants to keep."
        ),
    )
    node: str | None = Field(
        None, description="Node name or id; null = scheduler picks."
    )


class DiskOverlayBody(BaseModel):
    """Create a qcow2 overlay over an existing disk image. The overlay
    lands on the same node(s) as its backing image."""

    name: str = Field(..., min_length=1)
    backing_disk_ref: str = Field(
        ...,
        description="Source disk id or name. Use the name for human-edited apply YAML.",
    )
    ephemeral: bool = False


class DiskCloneBody(BaseModel):
    """Deep-copy an existing image to a new disk record. The clone is
    placed on the source's node; ``path`` overrides the default
    ``<basePath>/<new_name>.<ext>`` destination."""

    source_ref: str
    new_name: str = Field(..., min_length=1)
    path: str | None = None
    ephemeral: bool = False


class DiskImportUrlBody(BaseModel):
    """Download a disk image from an HTTP URL into the daemon's
    storage. Runs asynchronously; the response includes a task id
    the frontend can watch on /tasks/{id}."""

    name: str = Field(..., min_length=1)
    url: str = Field(..., min_length=1)
    format: str | None = None
    size_mb: int | None = Field(
        None,
        gt=0,
        description=(
            "Resize the downloaded image to this size after fetching. "
            "Useful for cloud images that ship 2-GiB defaults."
        ),
    )
    ephemeral: bool = False
    node: str | None = None


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


def _ref_to_int_or_str(ref: str) -> int | str:
    """The corvus_client.entity_ref helper accepts either an integer id
    or a name string. Numeric-looking strings get coerced to int so
    the resolver takes the id path; everything else passes through
    as a name."""
    try:
        return int(ref)
    except ValueError:
        return ref


@router.post("")
async def create_disk(body: DiskCreateBody, client: ClientDep) -> dict[str, Any]:
    """Allocate a blank disk image."""
    try:
        disk = await client.disks.create(
            body.name,
            body.size_mb,
            format=body.format,
            ephemeral=body.ephemeral,
            node=_ref_to_int_or_str(body.node) if body.node else None,
        )
    except CorvusError as exc:
        raise HTTPException(status_code=400, detail=str(exc)) from exc
    return _as_dict(await disk.show())


@router.post("/overlay")
async def create_overlay(body: DiskOverlayBody, client: ClientDep) -> dict[str, Any]:
    """Create a qcow2 overlay over an existing backing image."""
    try:
        disk = await client.disks.create_overlay(
            body.name,
            _ref_to_int_or_str(body.backing_disk_ref),
            ephemeral=body.ephemeral,
        )
    except CorvusError as exc:
        raise HTTPException(status_code=400, detail=str(exc)) from exc
    return _as_dict(await disk.show())


@router.post("/clone")
async def clone_disk(body: DiskCloneBody, client: ClientDep) -> dict[str, Any]:
    """Deep-copy a disk image."""
    try:
        disk = await client.disks.clone(
            _ref_to_int_or_str(body.source_ref),
            body.new_name,
            path=body.path,
            ephemeral=body.ephemeral,
        )
    except CorvusError as exc:
        raise HTTPException(status_code=400, detail=str(exc)) from exc
    return _as_dict(await disk.show())


@router.post("/import-url")
async def import_url(body: DiskImportUrlBody, client: ClientDep) -> dict[str, int]:
    """Kick off an async URL import. Returns ``{task_id}``; the
    frontend should route to /tasks/{id} to watch progress and
    eventually find the new disk via the task's entity link."""
    try:
        task_id = await client.disks.import_url(
            body.name,
            body.url,
            format=body.format,
            size_mb=body.size_mb,
            ephemeral=body.ephemeral,
            node=_ref_to_int_or_str(body.node) if body.node else None,
        )
    except CorvusError as exc:
        raise HTTPException(status_code=400, detail=str(exc)) from exc
    return {"task_id": task_id}


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
