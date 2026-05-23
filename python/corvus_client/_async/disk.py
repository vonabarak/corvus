"""Async Disk manager + Disk + Snapshot wrappers."""

from __future__ import annotations

from .. import _schema
from .._entityref import entity_ref
from ..exceptions import translate_errors
from . import _convert as conv


@translate_errors
class AsyncDiskManager:
    def __init__(self, daemon):
        self._daemon = daemon
        self._mgr = None

    async def _ensure(self):
        if self._mgr is None:
            self._mgr = (await self._daemon.disks()).mgr
        return self._mgr

    async def list(self):
        mgr = await self._ensure()
        resp = await mgr.list()
        return [conv.disk_image_info(d) for d in resp.disks]

    async def get(self, ref: int | str, *, by_name: bool = False) -> AsyncDisk:
        mgr = await self._ensure()
        resp = await mgr.get(ref=entity_ref(ref, by_name=by_name))
        return AsyncDisk(resp.disk)

    async def create(
        self,
        name: str,
        size_mb: int,
        *,
        format: str | None = None,
        ephemeral: bool = False,
    ) -> AsyncDisk:
        mgr = await self._ensure()
        params = _schema.disk.DiskCreateParams.new_message()
        params.name = name
        params.sizeMb = size_mb
        if format is not None:
            params.format = format
        params.ephemeral = ephemeral
        resp = await mgr.create(params=params)
        return AsyncDisk(resp.disk)

    async def register(
        self,
        name: str,
        file_path: str,
        *,
        format: str | None = None,
        ephemeral: bool = False,
    ) -> AsyncDisk:
        mgr = await self._ensure()
        params = _schema.disk.DiskRegisterParams.new_message()
        params.name = name
        params.filePath = file_path
        if format is not None:
            params.format = format
        params.ephemeral = ephemeral
        resp = await mgr.register(params=params)
        return AsyncDisk(resp.disk)

    async def create_overlay(
        self,
        name: str,
        backing_disk_ref: int | str,
        *,
        ephemeral: bool = False,
    ) -> AsyncDisk:
        mgr = await self._ensure()
        params = _schema.disk.DiskCreateOverlayParams.new_message()
        params.name = name
        params.backingDiskRef = entity_ref(backing_disk_ref)
        params.ephemeral = ephemeral
        resp = await mgr.createOverlay(params=params)
        return AsyncDisk(resp.disk)

    async def clone(
        self,
        source_ref: int | str,
        new_name: str,
        *,
        path: str | None = None,
        ephemeral: bool = False,
    ) -> AsyncDisk:
        """Clone an existing disk to a new disk record.

        `path` is the destination on the daemon's filesystem; leave
        `None` (the default) and the daemon writes to
        `<basePath>/<new_name>.<ext>`. Relative paths are resolved
        against the daemon's basePath; absolute paths are honoured
        as-is.

        Pass ``ephemeral=True`` to mark the clone for auto-deletion
        with the VM it ends up attached to.
        """
        mgr = await self._ensure()
        params = _schema.disk.DiskCloneParams.new_message()
        params.sourceRef = entity_ref(source_ref)
        params.newName = new_name
        if path is not None:
            params.path = path
        params.ephemeral = ephemeral
        resp = await mgr.clone(params=params)
        return AsyncDisk(resp.disk)

    async def rebase(
        self,
        disk_ref: int | str,
        new_backing_disk_ref: int | str,
    ) -> None:
        mgr = await self._ensure()
        params = _schema.disk.DiskRebaseParams.new_message()
        params.diskRef = entity_ref(disk_ref)
        params.newBackingDiskRef = entity_ref(new_backing_disk_ref)
        await mgr.rebase(params=params)

    async def flatten(self, disk_ref: int | str) -> None:
        """Flatten an overlay disk: consolidate its delta with its
        backing image(s) into a standalone qcow2. The disk record
        keeps its id; only its `backing_image_*` fields go to
        None. VM must be stopped.
        """
        mgr = await self._ensure()
        await mgr.flatten(diskRef=entity_ref(disk_ref))

    async def import_url(
        self,
        name: str,
        url: str,
        *,
        format: str | None = None,
        size_mb: int | None = None,
        ephemeral: bool = False,
    ) -> int:
        """Returns the task id; the disk is created asynchronously."""
        mgr = await self._ensure()
        params = _schema.disk.DiskImportUrlParams.new_message()
        params.name = name
        params.url = url
        if format is not None:
            params.format = format
        if size_mb is not None:
            params.sizeMb = size_mb
        params.ephemeral = ephemeral
        resp = await mgr.importUrl(params=params)
        return resp.taskId

    async def import_(
        self,
        name: str,
        src_path: str,
        *,
        format: str | None = None,
        ephemeral: bool = False,
    ) -> AsyncDisk:
        mgr = await self._ensure()
        params = _schema.disk.DiskImportParams.new_message()
        params.name = name
        params.srcPath = src_path
        if format is not None:
            params.format = format
        params.ephemeral = ephemeral
        # `import` is a Python keyword; pycapnp uses the schema name verbatim
        # as a method on the cap, so we call it through getattr.
        resp = await getattr(mgr, "import")(params=params)
        return AsyncDisk(resp.disk)

    async def copy(
        self,
        disk_ref: int | str,
        to_node_ref: int | str,
    ) -> int:
        """Copy a disk image's bytes to another node.

        Adds a `DiskImageNode` placement on the destination while
        leaving the source intact. Bytes flow agent-to-agent; the
        daemon orchestrates but does not relay. Returns the task
        id so the caller can poll `tasks.get(tid).show()` for
        completion.
        """
        mgr = await self._ensure()
        params = _schema.disk.DiskCopyParams.new_message()
        params.diskRef = entity_ref(disk_ref)
        params.toNodeRef = entity_ref(to_node_ref)
        resp = await mgr.copy(params=params)
        return resp.taskId

    async def move(
        self,
        disk_ref: int | str,
        to_node_ref: int | str,
    ) -> int:
        """Move a disk image's bytes to another node.

        Adds a `DiskImageNode` placement on the destination and
        drops the source placement + file on success. Same byte
        path as :meth:`copy`. Returns the task id.
        """
        mgr = await self._ensure()
        params = _schema.disk.DiskMoveParams.new_message()
        params.diskRef = entity_ref(disk_ref)
        params.toNodeRef = entity_ref(to_node_ref)
        resp = await mgr.move(params=params)
        return resp.taskId


@translate_errors
class AsyncDisk:
    def __init__(self, cap):
        self._cap = cap

    async def show(self):
        resp = await self._cap.show()
        return conv.disk_image_info(resp.info)

    async def delete(self) -> None:
        await self._cap.delete()

    async def refresh(self):
        resp = await self._cap.refresh()
        return conv.disk_image_info(resp.info)

    async def resize(self, new_size_mb: int) -> None:
        await self._cap.resize(newSizeMb=new_size_mb)

    async def snapshot_create(self, name: str) -> AsyncSnapshot:
        # `name` collides with pycapnp's internal `_send(name, ...)`
        # method-name kwarg, so build the request explicitly.
        req = self._cap.snapshotCreate_request()
        req.name = name
        resp = await req.send()
        return AsyncSnapshot(resp.snapshot)

    async def snapshot_list(self):
        resp = await self._cap.snapshotList()
        return [conv.snapshot_info(s) for s in resp.snapshots]

    async def snapshot_get(
        self, ref: int | str, *, by_name: bool = False
    ) -> AsyncSnapshot:
        resp = await self._cap.snapshotGet(ref=entity_ref(ref, by_name=by_name))
        return AsyncSnapshot(resp.snapshot)


@translate_errors
class AsyncSnapshot:
    def __init__(self, cap):
        self._cap = cap

    async def show(self):
        resp = await self._cap.show()
        return conv.snapshot_info(resp.info)

    async def delete(self) -> None:
        await self._cap.delete()

    async def rollback(self) -> None:
        await self._cap.rollback()

    async def merge(self) -> None:
        await self._cap.merge()
