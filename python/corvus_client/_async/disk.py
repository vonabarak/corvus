"""Async Disk manager + Disk + Snapshot wrappers."""

from __future__ import annotations

from .. import _schema, types
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
        node: int | str | None = None,
    ) -> AsyncDisk:
        """Create a new disk image.

        Pass ``node=`` to pin the placement to a specific node
        (name or numeric id). When omitted, the daemon's scheduler
        picks one.
        """
        mgr = await self._ensure()
        params = _schema.disk.DiskCreateParams.new_message()
        params.name = name
        params.sizeMb = size_mb
        if format is not None:
            params.format = format
        params.ephemeral = ephemeral
        if node is not None:
            params.node = entity_ref(node)
        resp = await mgr.create(params=params)
        return AsyncDisk(resp.disk)

    async def register(
        self,
        name: str,
        file_path: str,
        *,
        format: str | None = None,
        ephemeral: bool = False,
        node: int | str | None = None,
    ) -> AsyncDisk:
        """Register an existing file as a disk image.

        Pass ``node=`` to declare which node hosts the file at
        ``file_path``. Omit it and the daemon's scheduler picks
        (useful for single-node deployments).
        """
        mgr = await self._ensure()
        params = _schema.disk.DiskRegisterParams.new_message()
        params.name = name
        params.filePath = file_path
        if format is not None:
            params.format = format
        params.ephemeral = ephemeral
        if node is not None:
            params.node = entity_ref(node)
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
        node: int | str | None = None,
    ) -> int:
        """Returns the task id; the disk is created asynchronously.

        Pass ``node=`` to direct the import to a specific node;
        defaults to the scheduler's pick.
        """
        mgr = await self._ensure()
        params = _schema.disk.DiskImportUrlParams.new_message()
        params.name = name
        params.url = url
        if format is not None:
            params.format = format
        if size_mb is not None:
            params.sizeMb = size_mb
        params.ephemeral = ephemeral
        if node is not None:
            params.node = entity_ref(node)
        resp = await mgr.importUrl(params=params)
        return resp.taskId

    async def import_(
        self,
        name: str,
        src_path: str,
        *,
        format: str | None = None,
        ephemeral: bool = False,
        node: int | str | None = None,
    ) -> AsyncDisk:
        """Import (copy) a local file as a new disk image.

        Pass ``node=`` to direct the import to a specific node;
        defaults to the scheduler's pick.
        """
        mgr = await self._ensure()
        params = _schema.disk.DiskImportParams.new_message()
        params.name = name
        params.srcPath = src_path
        if format is not None:
            params.format = format
        params.ephemeral = ephemeral
        if node is not None:
            params.node = entity_ref(node)
        # `import` is a Python keyword; pycapnp uses the schema name verbatim
        # as a method on the cap, so we call it through getattr.
        resp = await getattr(mgr, "import")(params=params)
        return AsyncDisk(resp.disk)

    async def copy(
        self,
        disk_ref: int | str,
        to_node_ref: int | str,
        *,
        to_path: str | None = None,
        with_backing_chain: bool = False,
    ) -> int:
        """Copy a disk image's bytes to another node.

        Adds a `DiskImageNode` placement on the destination while
        leaving the source intact. Bytes flow agent-to-agent; the
        daemon orchestrates but does not relay. Returns the task
        id so the caller can poll `tasks.get(tid).show()` for
        completion.

        ``to_path`` is the destination path on the new node.
        Leave ``None`` to preserve the source's relative path;
        an absolute source path requires an explicit ``to_path``,
        otherwise the daemon refuses the copy. Trailing ``/``
        means "this is a directory; pick the source basename".

        ``with_backing_chain``: when True, recursively copies
        every backing ancestor missing on the destination before
        transferring this disk. Default False keeps the historical
        refuse-if-chain-missing behaviour.
        """
        mgr = await self._ensure()
        params = _schema.disk.DiskCopyParams.new_message()
        params.diskRef = entity_ref(disk_ref)
        params.toNodeRef = entity_ref(to_node_ref)
        if to_path is not None:
            params.toPath = to_path
        params.withBackingChain = with_backing_chain
        resp = await mgr.copy(params=params)
        return resp.taskId

    async def move(
        self,
        disk_ref: int | str,
        to_node_ref: int | str,
        *,
        to_path: str | None = None,
        with_backing_chain: bool = False,
    ) -> int:
        """Move a disk image's bytes to another node.

        Adds a `DiskImageNode` placement on the destination and
        drops the source placement + file on success. Same byte
        path as :meth:`copy`. Returns the task id.

        ``to_path`` and ``with_backing_chain`` semantics match
        :meth:`copy`. Staged backing ancestors land as separate
        placements (not moved — they may still have other
        consumers on the source).
        """
        mgr = await self._ensure()
        params = _schema.disk.DiskMoveParams.new_message()
        params.diskRef = entity_ref(disk_ref)
        params.toNodeRef = entity_ref(to_node_ref)
        if to_path is not None:
            params.toPath = to_path
        params.withBackingChain = with_backing_chain
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

    async def snapshot_create(
        self,
        name: str,
        *,
        quiesce: types.QuiesceMode = types.QuiesceMode.AUTO,
        full_machine: bool = False,
    ) -> AsyncSnapshot:
        """Create a qcow2 snapshot. Dispatches transparently between
        offline (``qemu-img snapshot -c``) and live (QMP
        ``blockdev-snapshot-internal-sync``) depending on whether the
        disk is attached to a running/paused VM.

        :param quiesce: filesystem-quiesce policy for the live path.
            Ignored on the offline path and on ``full_machine``
            snapshots. See :class:`QuiesceMode`.
        :param full_machine: take a vmstate-aware snapshot (RAM +
            device model + CPU state). This disk becomes the
            "carrier" whose qcow2 holds the vmstate; every other
            writable qcow2 disk attached to the same running VM also
            gets a sibling block snapshot under the same name.
            Requires the VM to be running and QEMU >= 6.0.
        """
        # `name` collides with pycapnp's internal `_send(name, ...)`
        # method-name kwarg, so build the request explicitly.
        req = self._cap.snapshotCreate_request()
        req.name = name
        req.quiesce = quiesce.value
        req.fullMachine = full_machine
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

    async def rollback(self, *, auto_stop: bool = False) -> None:
        """Roll the parent disk back to this snapshot.

        QEMU has no online snapshot-rollback command. Without
        ``auto_stop`` the daemon refuses the call when an attached
        VM is running/paused. ``auto_stop=True`` asks the daemon
        to orchestrate a graceful VmStop + rollback + VmStart
        cycle — the operator still issues one call, but the VM
        does cycle.
        """
        await self._cap.rollback(autoStop=auto_stop)

    async def merge(self) -> None:
        await self._cap.merge()
