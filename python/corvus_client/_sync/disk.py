"""Sync mirrors for the async Disk + Snapshot wrappers."""

from __future__ import annotations

from ._resource import LoopBoundResource


class SyncDiskManager:
    def __init__(self, async_mgr, runloop):
        self._a = async_mgr
        self._rl = runloop

    def list(self):
        return self._rl.run(self._a.list())

    def get(self, ref: int | str, *, by_name: bool = False):
        return SyncDisk(self._rl.run(self._a.get(ref, by_name=by_name)), self._rl)

    def create(
        self,
        name: str,
        size_mb: int,
        *,
        format: str | None = None,
        ephemeral: bool = False,
    ):
        return SyncDisk(
            self._rl.run(
                self._a.create(name, size_mb, format=format, ephemeral=ephemeral)
            ),
            self._rl,
        )

    def register(
        self,
        name: str,
        file_path: str,
        *,
        format: str | None = None,
        ephemeral: bool = False,
    ):
        return SyncDisk(
            self._rl.run(
                self._a.register(name, file_path, format=format, ephemeral=ephemeral)
            ),
            self._rl,
        )

    def create_overlay(self, name: str, backing_disk_ref, *, ephemeral: bool = False):
        return SyncDisk(
            self._rl.run(
                self._a.create_overlay(name, backing_disk_ref, ephemeral=ephemeral)
            ),
            self._rl,
        )

    def clone(
        self,
        source_ref,
        new_name: str,
        *,
        path: str | None = None,
        ephemeral: bool = False,
    ):
        return SyncDisk(
            self._rl.run(
                self._a.clone(source_ref, new_name, path=path, ephemeral=ephemeral)
            ),
            self._rl,
        )

    def rebase(self, disk_ref, new_backing_disk_ref):
        return self._rl.run(self._a.rebase(disk_ref, new_backing_disk_ref))

    def flatten(self, disk_ref):
        return self._rl.run(self._a.flatten(disk_ref))

    def import_url(
        self,
        name: str,
        url: str,
        *,
        format: str | None = None,
        size_mb: int | None = None,
        ephemeral: bool = False,
    ) -> int:
        return self._rl.run(
            self._a.import_url(
                name,
                url,
                format=format,
                size_mb=size_mb,
                ephemeral=ephemeral,
            )
        )

    def import_(
        self,
        name: str,
        src_path: str,
        *,
        format: str | None = None,
        ephemeral: bool = False,
    ):
        return SyncDisk(
            self._rl.run(
                self._a.import_(name, src_path, format=format, ephemeral=ephemeral)
            ),
            self._rl,
        )

    def copy(
        self,
        disk_ref: int | str,
        to_node_ref: int | str,
    ) -> int:
        return self._rl.run(self._a.copy(disk_ref, to_node_ref))

    def move(
        self,
        disk_ref: int | str,
        to_node_ref: int | str,
    ) -> int:
        return self._rl.run(self._a.move(disk_ref, to_node_ref))


class SyncDisk(LoopBoundResource):
    def __init__(self, async_disk, runloop):
        self._a = async_disk
        self._rl = runloop

    def show(self):
        return self._rl.run(self._a.show())

    def delete(self):
        return self._rl.run(self._a.delete())

    def refresh(self):
        return self._rl.run(self._a.refresh())

    def resize(self, new_size_mb: int):
        return self._rl.run(self._a.resize(new_size_mb))

    def snapshot_create(self, name: str):
        return SyncSnapshot(self._rl.run(self._a.snapshot_create(name)), self._rl)

    def snapshot_list(self):
        return self._rl.run(self._a.snapshot_list())

    def snapshot_get(self, ref: int | str, *, by_name: bool = False):
        return SyncSnapshot(
            self._rl.run(self._a.snapshot_get(ref, by_name=by_name)), self._rl
        )


class SyncSnapshot(LoopBoundResource):
    def __init__(self, async_snap, runloop):
        self._a = async_snap
        self._rl = runloop

    def show(self):
        return self._rl.run(self._a.show())

    def delete(self):
        return self._rl.run(self._a.delete())

    def rollback(self):
        return self._rl.run(self._a.rollback())

    def merge(self):
        return self._rl.run(self._a.merge())
