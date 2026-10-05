"""Sync mirrors for the async Disk + Snapshot wrappers."""

from __future__ import annotations

from pathlib import Path

from .. import types
from .._async.disk import AsyncDisk, AsyncDiskManager, AsyncSnapshot
from .._runloop import SyncRunloop
from ._resource import LoopBoundResource


class SyncDiskManager:
    def __init__(self, async_mgr: AsyncDiskManager, runloop: SyncRunloop) -> None:
        self._a = async_mgr
        self._rl = runloop

    def list(self) -> list[types.DiskImageInfo]:
        return self._rl.run(self._a.list())

    def get(self, ref: int | str, *, by_name: bool = False) -> SyncDisk:
        return SyncDisk(self._rl.run(self._a.get(ref, by_name=by_name)), self._rl)

    def create(
        self,
        name: str,
        size: int,
        *,
        format: str | None = None,
        path: str | None = None,
        ephemeral: bool = False,
        node: int | str | None = None,
    ) -> SyncDisk:
        return SyncDisk(
            self._rl.run(
                self._a.create(
                    name,
                    size,
                    format=format,
                    path=path,
                    ephemeral=ephemeral,
                    node=node,
                )
            ),
            self._rl,
        )

    def register(
        self,
        name: str,
        file_path: str,
        *,
        format: str | None = None,
        backing_disk_ref: int | str | None = None,
        ephemeral: bool = False,
        node: int | str | None = None,
    ) -> SyncDisk:
        return SyncDisk(
            self._rl.run(
                self._a.register(
                    name,
                    file_path,
                    format=format,
                    backing_disk_ref=backing_disk_ref,
                    ephemeral=ephemeral,
                    node=node,
                )
            ),
            self._rl,
        )

    def create_overlay(
        self,
        name: str,
        backing_disk_ref: int | str,
        *,
        path: str | None = None,
        ephemeral: bool = False,
    ) -> SyncDisk:
        return SyncDisk(
            self._rl.run(
                self._a.create_overlay(
                    name, backing_disk_ref, path=path, ephemeral=ephemeral
                )
            ),
            self._rl,
        )

    def clone(
        self,
        source_ref: int | str,
        new_name: str,
        *,
        path: str | None = None,
        ephemeral: bool = False,
    ) -> SyncDisk:
        return SyncDisk(
            self._rl.run(
                self._a.clone(source_ref, new_name, path=path, ephemeral=ephemeral)
            ),
            self._rl,
        )

    def rebase(
        self,
        disk_ref: int | str,
        new_backing_disk_ref: int | str,
        *,
        unsafe: bool = False,
    ) -> None:
        self._rl.run(self._a.rebase(disk_ref, new_backing_disk_ref, unsafe=unsafe))

    def flatten(self, disk_ref: int | str) -> None:
        return self._rl.run(self._a.flatten(disk_ref))

    def eject_media(self, drive_id: int) -> None:
        """Eject the media of a CD-ROM drive (drive row id). See
        :meth:`AsyncDiskManager.eject_media`."""
        return self._rl.run(self._a.eject_media(drive_id))

    def change_media(self, drive_id: int, new_disk: int | str) -> None:
        """Swap the media of a CD-ROM drive for another disk image.
        See :meth:`AsyncDiskManager.change_media`."""
        return self._rl.run(self._a.change_media(drive_id, new_disk))

    def import_url(
        self,
        name: str,
        url: str,
        *,
        path: str | None = None,
        format: str | None = None,
        ephemeral: bool = False,
        node: int | str | None = None,
    ) -> int:
        return self._rl.run(
            self._a.import_url(
                name,
                url,
                path=path,
                format=format,
                ephemeral=ephemeral,
                node=node,
            )
        )

    def import_(
        self,
        name: str,
        src_path: str,
        *,
        path: str | None = None,
        format: str | None = None,
        ephemeral: bool = False,
        node: int | str | None = None,
    ) -> int:
        return self._rl.run(
            self._a.import_(
                name,
                src_path,
                path=path,
                format=format,
                ephemeral=ephemeral,
                node=node,
            )
        )

    def upload_from_file(
        self,
        name: str,
        source: str | Path,
        *,
        format: str,
        path: str | None = None,
        ephemeral: bool = False,
        node: int | str | None = None,
        overwrite: bool = False,
    ) -> SyncDisk:
        return SyncDisk(
            self._rl.run(
                self._a.upload_from_file(
                    name,
                    source,
                    format=format,
                    path=path,
                    ephemeral=ephemeral,
                    node=node,
                    overwrite=overwrite,
                )
            ),
            self._rl,
        )

    def copy(
        self,
        disk_ref: int | str,
        to_node_ref: int | str,
        *,
        to_path: str | None = None,
        with_backing_chain: bool = False,
    ) -> int:
        return self._rl.run(
            self._a.copy(
                disk_ref,
                to_node_ref,
                to_path=to_path,
                with_backing_chain=with_backing_chain,
            )
        )

    def move(
        self,
        disk_ref: int | str,
        to_node_ref: int | str,
        *,
        to_path: str | None = None,
        with_backing_chain: bool = False,
    ) -> int:
        return self._rl.run(
            self._a.move(
                disk_ref,
                to_node_ref,
                to_path=to_path,
                with_backing_chain=with_backing_chain,
            )
        )


class SyncDisk(LoopBoundResource):
    def __init__(self, async_disk: AsyncDisk, runloop: SyncRunloop) -> None:
        self._a = async_disk
        self._rl = runloop

    def show(self) -> types.DiskImageInfo:
        return self._rl.run(self._a.show())

    def delete(self) -> None:
        return self._rl.run(self._a.delete())

    def refresh(self) -> types.DiskImageInfo:
        return self._rl.run(self._a.refresh())

    def resize(self, new_size: int) -> None:
        return self._rl.run(self._a.resize(new_size))

    def snapshot_create(
        self,
        name: str,
        *,
        quiesce: types.QuiesceMode = types.QuiesceMode.AUTO,
        full_machine: bool = False,
    ) -> SyncSnapshot:
        return SyncSnapshot(
            self._rl.run(
                self._a.snapshot_create(
                    name, quiesce=quiesce, full_machine=full_machine
                )
            ),
            self._rl,
        )

    def snapshot_list(self) -> list[types.SnapshotInfo]:
        return self._rl.run(self._a.snapshot_list())

    def snapshot_get(self, ref: int | str, *, by_name: bool = False) -> SyncSnapshot:
        return SyncSnapshot(
            self._rl.run(self._a.snapshot_get(ref, by_name=by_name)), self._rl
        )


class SyncSnapshot(LoopBoundResource):
    def __init__(self, async_snap: AsyncSnapshot, runloop: SyncRunloop) -> None:
        self._a = async_snap
        self._rl = runloop

    def show(self) -> types.SnapshotInfo:
        return self._rl.run(self._a.show())

    def delete(self) -> None:
        return self._rl.run(self._a.delete())

    def rollback(self, *, auto_stop: bool = False) -> None:
        return self._rl.run(self._a.rollback(auto_stop=auto_stop))

    def merge(self) -> None:
        return self._rl.run(self._a.merge())
