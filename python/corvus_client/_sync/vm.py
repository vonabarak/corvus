"""Sync mirrors for the async Vm wrappers."""

from __future__ import annotations

import asyncio

from ._resource import LoopBoundResource


class SyncByteStream:
    """Sync wrapper around `_async/streams.py::ByteStream`.

    Daemon→client bytes arrive via `read(timeout=…)` — returns `None`
    on EOF, `b""` when the timeout slice elapses with no data, and a
    non-empty chunk otherwise. Client→daemon bytes go through
    `write(chunk)`. `close()` signals end-of-input. Each call hops to
    the runloop thread.
    """

    def __init__(self, async_stream, runloop):
        self._a = async_stream
        self._rl = runloop

    def read(self, *, timeout: float | None = None) -> bytes | None:
        async def _go():
            if timeout is None:
                return await self._a.read()
            try:
                return await asyncio.wait_for(self._a.read(), timeout)
            except asyncio.TimeoutError:
                return b""

        return self._rl.run(_go())

    def write(self, chunk: bytes) -> None:
        self._rl.run(self._a.write(chunk))

    def close(self) -> None:
        self._rl.run(self._a.close())

    def __enter__(self):
        return self

    def __exit__(self, exc_type, exc, tb) -> None:
        self.close()


class SyncGuestAgentSubscription:
    """Sync mirror of `AsyncGuestAgentSubscription` from `_async/streams.py`.

    Held by callers to keep a subscription alive; `close()` (or
    garbage collection) tears down the daemon-side subscriber slot.
    """

    def __init__(self, async_sub, runloop):
        self._a = async_sub
        self._rl = runloop

    def close(self) -> None:
        self._rl.run(self._a.close())

    def __enter__(self):
        return self

    def __exit__(self, exc_type, exc, tb) -> None:
        self.close()


class SyncVmManager:
    def __init__(self, async_mgr, runloop):
        self._a = async_mgr
        self._rl = runloop

    def list(self):
        return self._rl.run(self._a.list())

    def get(self, ref, *, by_name: bool = False):
        return SyncVm(self._rl.run(self._a.get(ref, by_name=by_name)), self._rl)

    def create(self, name: str, **kwargs):
        return SyncVm(self._rl.run(self._a.create(name, **kwargs)), self._rl)


class SyncVm(LoopBoundResource):
    def __init__(self, async_vm, runloop):
        self._a = async_vm
        self._rl = runloop

    # queries
    def show(self):
        return self._rl.run(self._a.show())

    # lifecycle
    def start(self, *, wait: bool = False):
        return self._rl.run(self._a.start(wait=wait))

    def stop(self, *, wait: bool = False):
        return self._rl.run(self._a.stop(wait=wait))

    def pause(self):
        return self._rl.run(self._a.pause())

    def reset(self):
        return self._rl.run(self._a.reset())

    def edit(self, **kwargs):
        return self._rl.run(self._a.edit(**kwargs))

    def delete(self, *, delete_disks: bool = False):
        return self._rl.run(self._a.delete(delete_disks=delete_disks))

    def migrate(self, to_node_ref) -> int:
        return self._rl.run(self._a.migrate(to_node_ref))

    # cloud-init / view / guest exec / hotkeys
    def cloud_init(self):
        return self._rl.run(self._a.cloud_init())

    def view_grant(self):
        return self._rl.run(self._a.view_grant())

    def guest_exec(self, command: str):
        return self._rl.run(self._a.guest_exec(command))

    def send_ctrl_alt_del(self):
        return self._rl.run(self._a.send_ctrl_alt_del())

    def serial_console(self) -> SyncByteStream:
        """Open a bidirectional serial console stream.

        Returns a `SyncByteStream`; daemon→client bytes via `read()`,
        client→daemon via `write(chunk)`. Use as a context manager
        (closes the input side on exit) or call `close()` manually.
        Raises `ServerError` if the VM is stopped or non-headless.
        """
        async_stream = self._rl.run(self._a.serial_console())
        return SyncByteStream(async_stream, self._rl)

    def serial_console_flush(self):
        return self._rl.run(self._a.serial_console_flush())

    def hmp_monitor_flush(self):
        return self._rl.run(self._a.hmp_monitor_flush())

    def subscribe_guest_agent(self, on_event):
        """Subscribe to guest-agent push events from the daemon.

        `on_event` is a **sync** callable invoked once per
        `GuestAgentStatus` push. It runs on the runloop thread, not
        the caller's thread, so the body must use thread-safe
        primitives (`queue.Queue`, `threading.Lock`, …) if it shares
        state with the caller.

        Returns a `SyncGuestAgentSubscription`; call `close()` (or
        let it go out of scope) to unsubscribe.
        """

        async def _bridge(ev):
            on_event(ev)

        async_sub = self._rl.run(self._a.subscribe_guest_agent(_bridge))
        return SyncGuestAgentSubscription(async_sub, self._rl)

    # drives
    def attach_disk(self, disk_ref, **kwargs):
        return self._rl.run(self._a.attach_disk(disk_ref, **kwargs))

    def detach_disk(self, drive_id: int):
        return self._rl.run(self._a.detach_disk(drive_id))

    def detach_disk_by_name(self, disk_name: str):
        return self._rl.run(self._a.detach_disk_by_name(disk_name))

    # network ifs
    def add_net_if(self, **kwargs):
        return self._rl.run(self._a.add_net_if(**kwargs))

    def remove_net_if(self, net_if_id: int):
        return self._rl.run(self._a.remove_net_if(net_if_id))

    def list_net_ifs(self):
        return self._rl.run(self._a.list_net_ifs())

    # shared dirs
    def add_shared_dir(self, path: str, tag: str, **kwargs):
        return self._rl.run(self._a.add_shared_dir(path, tag, **kwargs))

    def remove_shared_dir(self, shared_dir_id: int):
        return self._rl.run(self._a.remove_shared_dir(shared_dir_id))

    def list_shared_dirs(self):
        return self._rl.run(self._a.list_shared_dirs())

    # snapshots
    def snapshot_create(self, name: str):
        from .disk import SyncSnapshot

        return SyncSnapshot(self._rl.run(self._a.snapshot_create(name)), self._rl)

    def snapshot_list(self):
        return self._rl.run(self._a.snapshot_list())

    def snapshot_get(self, ref: int | str, *, by_name: bool = False):
        from .disk import SyncSnapshot

        return SyncSnapshot(
            self._rl.run(self._a.snapshot_get(ref, by_name=by_name)), self._rl
        )

    # ssh keys
    def attach_ssh_key(self, key_ref):
        return self._rl.run(self._a.attach_ssh_key(key_ref))

    def detach_ssh_key(self, key_ref):
        return self._rl.run(self._a.detach_ssh_key(key_ref))

    def list_ssh_keys(self):
        return self._rl.run(self._a.list_ssh_keys())
