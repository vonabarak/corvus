"""Sync mirrors for the async Disk + Snapshot wrappers."""
from __future__ import annotations

from typing import Optional, Union

from ._resource import LoopBoundResource


class SyncDiskManager:
    def __init__(self, async_mgr, runloop):
        self._a = async_mgr
        self._rl = runloop

    def list(self):
        return self._rl.run(self._a.list())

    def get(self, ref: Union[int, str], *, by_name: bool = False):
        return SyncDisk(self._rl.run(self._a.get(ref, by_name=by_name)), self._rl)

    def create(self, name: str, size_mb: int, *, format: Optional[str] = None):
        return SyncDisk(self._rl.run(self._a.create(name, size_mb, format=format)), self._rl)

    def register(self, name: str, file_path: str, *, format: Optional[str] = None):
        return SyncDisk(self._rl.run(self._a.register(name, file_path, format=format)), self._rl)

    def create_overlay(self, name: str, backing_disk_ref):
        return SyncDisk(
            self._rl.run(self._a.create_overlay(name, backing_disk_ref)),
            self._rl,
        )

    def clone(self, source_ref, new_name: str, *, path: Optional[str] = None):
        return SyncDisk(
            self._rl.run(self._a.clone(source_ref, new_name, path=path)),
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
        format: Optional[str] = None,
        size_mb: Optional[int] = None,
    ) -> int:
        return self._rl.run(self._a.import_url(name, url, format=format, size_mb=size_mb))

    def import_(self, name: str, src_path: str, *, format: Optional[str] = None):
        return SyncDisk(self._rl.run(self._a.import_(name, src_path, format=format)), self._rl)


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

    def snapshot_get(self, ref: Union[int, str], *, by_name: bool = False):
        return SyncSnapshot(self._rl.run(self._a.snapshot_get(ref, by_name=by_name)), self._rl)


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
