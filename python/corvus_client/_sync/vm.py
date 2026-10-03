"""Sync mirrors for the async Vm wrappers."""

from __future__ import annotations

import asyncio
from collections.abc import Callable, Sequence
from types import TracebackType

from .. import types as t
from .._async.streams import ByteStream, GuestAgentSubscription, VmStatsSubscription
from .._async.vm import AsyncVm, AsyncVmManager
from .._runloop import SyncRunloop
from ._resource import LoopBoundResource


class SyncByteStream:
    """Sync wrapper around `_async/streams.py::ByteStream`.

    Daemon→client bytes arrive via `read(timeout=…)` — returns `None`
    on EOF, `b""` when the timeout slice elapses with no data, and a
    non-empty chunk otherwise. Client→daemon bytes go through
    `write(chunk)`. `close()` signals end-of-input. Each call hops to
    the runloop thread.
    """

    def __init__(self, async_stream: ByteStream, runloop: SyncRunloop) -> None:
        self._a = async_stream
        self._rl = runloop

    def read(self, *, timeout: float | None = None) -> bytes | None:
        async def _go() -> bytes | None:
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

    def __enter__(self) -> SyncByteStream:
        return self

    def __exit__(
        self,
        exc_type: type[BaseException] | None,
        exc: BaseException | None,
        tb: TracebackType | None,
    ) -> None:
        self.close()


class SyncGuestAgentSubscription:
    """Sync mirror of `AsyncGuestAgentSubscription` from `_async/streams.py`.

    Held by callers to keep a subscription alive; `close()` (or
    garbage collection) tears down the daemon-side subscriber slot.
    """

    def __init__(
        self,
        async_sub: GuestAgentSubscription | VmStatsSubscription,
        runloop: SyncRunloop,
    ) -> None:
        self._a = async_sub
        self._rl = runloop

    def close(self) -> None:
        self._rl.run(self._a.close())

    def __enter__(self) -> SyncGuestAgentSubscription:
        return self

    def __exit__(
        self,
        exc_type: type[BaseException] | None,
        exc: BaseException | None,
        tb: TracebackType | None,
    ) -> None:
        self.close()


class SyncVmManager:
    def __init__(self, async_mgr: AsyncVmManager, runloop: SyncRunloop) -> None:
        self._a = async_mgr
        self._rl = runloop

    def list(self) -> list[t.VmInfo]:
        return self._rl.run(self._a.list())

    def get(self, ref: int | str, *, by_name: bool = False) -> SyncVm:
        return SyncVm(self._rl.run(self._a.get(ref, by_name=by_name)), self._rl)

    def create(
        self,
        name: str,
        *,
        node: str | None = None,
        cpu_count: int = 1,
        ram_mb: int = 1024,
        description: str | None = None,
        headless: bool = False,
        guest_agent: bool = False,
        tpm: bool = False,
        cloud_init: bool = False,
        autostart: bool = False,
        reboot_quirk: bool = False,
        cpu_model: str = "host",
        graphics_adapter: str = "virtio-vga",
        audio_devices: Sequence[tuple[str, str]] | None = None,
    ) -> SyncVm:
        return SyncVm(
            self._rl.run(
                self._a.create(
                    name,
                    node=node,
                    cpu_count=cpu_count,
                    ram_mb=ram_mb,
                    description=description,
                    headless=headless,
                    guest_agent=guest_agent,
                    tpm=tpm,
                    cloud_init=cloud_init,
                    autostart=autostart,
                    reboot_quirk=reboot_quirk,
                    cpu_model=cpu_model,
                    graphics_adapter=graphics_adapter,
                    audio_devices=audio_devices,
                )
            ),
            self._rl,
        )


class SyncVm(LoopBoundResource):
    def __init__(self, async_vm: AsyncVm, runloop: SyncRunloop) -> None:
        self._a = async_vm
        self._rl = runloop

    # queries
    def show(self) -> t.VmDetails:
        return self._rl.run(self._a.show())

    # lifecycle
    def start(self, *, wait: bool = False) -> str:
        return self._rl.run(self._a.start(wait=wait))

    def stop(self, *, wait: bool = False, timeout_sec: int = 300) -> str:
        return self._rl.run(self._a.stop(wait=wait, timeout_sec=timeout_sec))

    def pause(self) -> str:
        return self._rl.run(self._a.pause())

    def reset(self) -> str:
        return self._rl.run(self._a.reset())

    def save(self, *, wait: bool = False) -> str:
        return self._rl.run(self._a.save(wait=wait))

    def edit(
        self,
        *,
        name: str | None = None,
        cpu_count: int | None = None,
        ram_mb: int | None = None,
        description: str | None = None,
        headless: bool | None = None,
        guest_agent: bool | None = None,
        tpm: bool | None = None,
        cloud_init: bool | None = None,
        autostart: bool | None = None,
        reboot_quirk: bool | None = None,
        cpu_model: str | None = None,
        graphics_adapter: str | None = None,
    ) -> None:
        self._rl.run(
            self._a.edit(
                name=name,
                cpu_count=cpu_count,
                ram_mb=ram_mb,
                description=description,
                headless=headless,
                guest_agent=guest_agent,
                tpm=tpm,
                cloud_init=cloud_init,
                autostart=autostart,
                reboot_quirk=reboot_quirk,
                cpu_model=cpu_model,
                graphics_adapter=graphics_adapter,
            )
        )

    def delete(self, *, keep_disks: bool = False, force: bool = False) -> None:
        return self._rl.run(self._a.delete(keep_disks=keep_disks, force=force))

    def migrate(self, to_node_ref: int | str) -> int:
        return self._rl.run(self._a.migrate(to_node_ref))

    # cloud-init / view / guest exec / hotkeys
    def cloud_init(self) -> t.CloudInitInfo:
        return self._rl.run(self._a.cloud_init())

    def view_grant(self) -> t.ViewGrant:
        return self._rl.run(self._a.view_grant())

    def guest_exec(self, command: str) -> t.GuestExecResult:
        return self._rl.run(self._a.guest_exec(command))

    def send_ctrl_alt_del(self) -> None:
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

    def serial_console_flush(self) -> None:
        return self._rl.run(self._a.serial_console_flush())

    def hmp_monitor(self) -> SyncByteStream:
        """Open a bidirectional HMP monitor stream.

        Mirrors `serial_console()`: bytes from the daemon's HMP
        chardev arrive via `read()`, and bytes written via
        `write(chunk)` are forwarded to QEMU's monitor. Use as a
        context manager (closes the input side on exit) or call
        `close()` manually. Raises `VmRunning` if the VM is
        stopped.
        """
        async_stream = self._rl.run(self._a.hmp_monitor())
        return SyncByteStream(async_stream, self._rl)

    def hmp_monitor_flush(self) -> None:
        return self._rl.run(self._a.hmp_monitor_flush())

    def subscribe_guest_agent(
        self, on_event: Callable[[t.GuestAgentStatus], None]
    ) -> SyncGuestAgentSubscription:
        """Subscribe to guest-agent push events from the daemon.

        `on_event` is a **sync** callable invoked once per
        `GuestAgentStatus` push. It runs on the runloop thread, not
        the caller's thread, so the body must use thread-safe
        primitives (`queue.Queue`, `threading.Lock`, …) if it shares
        state with the caller.

        Returns a `SyncGuestAgentSubscription`; call `close()` (or
        let it go out of scope) to unsubscribe.
        """

        async def _bridge(ev: t.GuestAgentStatus) -> None:
            on_event(ev)

        async_sub = self._rl.run(self._a.subscribe_guest_agent(_bridge))
        return SyncGuestAgentSubscription(async_sub, self._rl)

    def subscribe_stats(
        self, on_event: Callable[[t.VmStats], None]
    ) -> SyncGuestAgentSubscription:
        """Subscribe to per-VM resource-stats push events (~10s cadence).

        See `subscribe_guest_agent` for the threading caveat: the
        `on_event` callback runs on the runloop thread, not on the
        caller's thread."""

        async def _bridge(ev: t.VmStats) -> None:
            on_event(ev)

        async_sub = self._rl.run(self._a.subscribe_stats(_bridge))
        return SyncGuestAgentSubscription(async_sub, self._rl)

    def get_stats_history(self) -> list[t.VmStats]:
        """One-shot fetch of the daemon's stats ring (up to 60
        samples, oldest first)."""
        return self._rl.run(self._a.get_stats_history())

    # drives
    def attach_disk(
        self,
        disk_ref: int | str,
        *,
        interface: str | None = None,
        media: str | None = None,
        read_only: bool = False,
        cache_type: str | None = None,
        discard: bool = False,
    ) -> int:
        return self._rl.run(
            self._a.attach_disk(
                disk_ref,
                interface=interface,
                media=media,
                read_only=read_only,
                cache_type=cache_type,
                discard=discard,
            )
        )

    def detach_disk(self, drive_id: int) -> None:
        return self._rl.run(self._a.detach_disk(drive_id))

    def detach_disk_by_name(self, disk_name: str) -> None:
        return self._rl.run(self._a.detach_disk_by_name(disk_name))

    # network ifs
    def add_net_if(
        self,
        *,
        type: str | None = None,
        host_device: str | None = None,
        mac_address: str | None = None,
        network_ref: int | str | None = None,
    ) -> int:
        return self._rl.run(
            self._a.add_net_if(
                type=type,
                host_device=host_device,
                mac_address=mac_address,
                network_ref=network_ref,
            )
        )

    def remove_net_if(self, net_if_id: int) -> None:
        return self._rl.run(self._a.remove_net_if(net_if_id))

    def list_net_ifs(self) -> list[t.NetIfInfo]:
        return self._rl.run(self._a.list_net_ifs())

    # shared dirs
    def add_shared_dir(
        self,
        path: str,
        tag: str,
        *,
        cache: str | None = None,
        read_only: bool = False,
    ) -> int:
        return self._rl.run(
            self._a.add_shared_dir(
                path,
                tag,
                cache=cache,
                read_only=read_only,
            )
        )

    def remove_shared_dir(self, shared_dir_id: int) -> None:
        return self._rl.run(self._a.remove_shared_dir(shared_dir_id))

    def list_shared_dirs(self) -> list[t.SharedDirInfo]:
        return self._rl.run(self._a.list_shared_dirs())

    # audio devices
    def add_audio_device(self, backend: str, options: str = "") -> int:
        return self._rl.run(self._a.add_audio_device(backend, options))

    def edit_audio_device(
        self, audio_device_id: int, backend: str, options: str = ""
    ) -> None:
        self._rl.run(self._a.edit_audio_device(audio_device_id, backend, options))

    def remove_audio_device(self, audio_device_id: int) -> None:
        self._rl.run(self._a.remove_audio_device(audio_device_id))

    def list_audio_devices(self) -> list[t.AudioDeviceInfo]:
        return self._rl.run(self._a.list_audio_devices())

    # VM-scoped full-machine snapshots
    def snapshot_create(self, name: str) -> t.VmSnapshotInfo:
        return self._rl.run(self._a.snapshot_create(name))

    def snapshot_list(self) -> list[t.VmSnapshotInfo]:
        return self._rl.run(self._a.snapshot_list())

    def snapshot_rollback(self, name: str) -> None:
        self._rl.run(self._a.snapshot_rollback(name))

    def snapshot_delete(self, name: str) -> None:
        self._rl.run(self._a.snapshot_delete(name))

    # ssh keys
    def attach_ssh_key(self, key_ref: int | str) -> None:
        return self._rl.run(self._a.attach_ssh_key(key_ref))

    def detach_ssh_key(self, key_ref: int | str) -> None:
        return self._rl.run(self._a.detach_ssh_key(key_ref))

    def list_ssh_keys(self) -> list[t.SshKeyInfo]:
        return self._rl.run(self._a.list_ssh_keys())
