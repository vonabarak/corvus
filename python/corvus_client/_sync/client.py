"""Synchronous Corvus client.

Holds a background asyncio + kj_loop thread (`_SyncRunloop`) and an
`AsyncClient` instance whose lifecycle is driven from that thread.
Every public method calls into the async core via
`asyncio.run_coroutine_threadsafe`.
"""
from __future__ import annotations

from pathlib import Path
from typing import Optional

from .._async.client import AsyncClient
from .._runloop import SyncRunloop
from .cloudinit import SyncCloudInitManager
from .disk import SyncDiskManager
from .network import SyncNetworkManager
from .node import SyncNodeManager
from .sshkey import SyncSshKeyManager
from .task import SyncTaskManager
from .template import SyncTemplateManager
from .vm import SyncVmManager


class Client:
    def __init__(
        self,
        *,
        unix_socket: Optional[str] = None,
        host: Optional[str] = None,
        port: int = 9876,
        cert_dir: Optional[str | Path] = None,
        tls: Optional[bool] = None,
    ) -> None:
        """See :class:`corvus_client.AsyncClient` for the full
        argument list — the sync wrapper is intentionally a 1:1
        shape.

        ``cert_dir`` / ``tls`` are passed straight through and
        honoured for TCP connections (Unix sockets keep using
        filesystem permissions)."""

        self._rl = SyncRunloop()
        self._a = AsyncClient(
            unix_socket=unix_socket,
            host=host,
            port=port,
            cert_dir=cert_dir,
            tls=tls,
        )
        try:
            self._rl.run(self._a.__aenter__())
        except Exception:
            self._rl.close()
            raise
        self.vms = SyncVmManager(self._a.vms, self._rl)
        self.disks = SyncDiskManager(self._a.disks, self._rl)
        self.networks = SyncNetworkManager(self._a.networks, self._rl)
        self.ssh_keys = SyncSshKeyManager(self._a.ssh_keys, self._rl)
        self.templates = SyncTemplateManager(self._a.templates, self._rl)
        self.tasks = SyncTaskManager(self._a.tasks, self._rl)
        self.cloud_init = SyncCloudInitManager(self._a.cloud_init, self._rl)
        self.nodes = SyncNodeManager(self._a.nodes, self._rl)
        self._closed = False

    # ---- lifecycle --------------------------------------------------------

    def close(self) -> None:
        if self._closed:
            return
        self._closed = True
        try:
            self._rl.run(self._a.__aexit__(None, None, None))
        finally:
            self._rl.close()

    def __enter__(self) -> "Client":
        return self

    def __exit__(self, exc_type, exc, tb) -> None:
        self.close()

    def __del__(self):
        try:
            self.close()
        except Exception:
            pass

    # ---- top-level Daemon methods ----------------------------------------

    def ping(self) -> None:
        return self._rl.run(self._a.ping())

    def status(self):
        return self._rl.run(self._a.status())

    def shutdown(self) -> None:
        return self._rl.run(self._a.shutdown())

    def apply(self, yaml: str, *, skip_existing: bool = False, wait: bool = False):
        return self._rl.run(
            self._a.apply(yaml, skip_existing=skip_existing, wait=wait)
        )

    # ---- build streaming (yields events on the calling thread) ----------

    def build_stream(self, yaml_path: str):
        """Iterate build events from a YAML pipeline file.

        Each call to `next()` drives the background loop until the next
        event is available. Yields the same payloads as the async
        `AsyncClient.build_stream` (`BuildEvent` dataclasses, then a
        final `('task_id', N)` tuple).
        """
        agen = self._a.build_stream(yaml_path)
        return _SyncIterator(agen, self._rl)

    def build_stream_text(self, yaml_text: str):
        agen = self._a.build_stream_text(yaml_text)
        return _SyncIterator(agen, self._rl)


class _SyncIterator:
    """Wrap an async generator as a sync iterator via the runloop."""

    def __init__(self, agen, runloop: SyncRunloop):
        self._agen = agen
        self._rl = runloop

    def __iter__(self):
        return self

    def __next__(self):
        async def _step():
            try:
                return await self._agen.__anext__()
            except StopAsyncIteration:
                return _STOP

        result = self._rl.run(_step())
        if result is _STOP:
            raise StopIteration
        return result


_STOP = object()
