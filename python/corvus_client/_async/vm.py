"""Async VM manager and resource cap wrappers.

Mirrors the schema in `schema/vm.capnp`. Streaming endpoints
(`serialConsole`, `hmpMonitor`, `subscribeGuestAgent`) live in
`corvus_client._async.streams` to keep this module focused on
request/response wrappers.
"""

from __future__ import annotations

from .. import _schema
from .._entityref import entity_ref
from ..exceptions import translate_errors
from . import _convert as conv


def _set_optional(builder, has_field: str, value_field: str, value):
    """Helper for `VmEditParams`-style `hasX` / `X` pairs."""
    if value is not None:
        setattr(builder, has_field, True)
        setattr(builder, value_field, value)


@translate_errors
class AsyncVmManager:
    """Wrapper for the `VmManager` cap returned by `Daemon.vms()`."""

    def __init__(self, daemon):
        self._daemon = daemon
        self._mgr = None

    async def _ensure(self):
        if self._mgr is None:
            self._mgr = (await self._daemon.vms()).mgr
        return self._mgr

    async def list(self):
        mgr = await self._ensure()
        resp = await mgr.list()
        return [conv.vm_info(v) for v in resp.vms]

    async def get(self, ref: int | str, *, by_name: bool = False) -> AsyncVm:
        mgr = await self._ensure()
        resp = await mgr.get(ref=entity_ref(ref, by_name=by_name))
        return AsyncVm(resp.vm)

    async def create(
        self,
        name: str,
        *,
        node: str | None = None,
        cpu_count: int = 1,
        ram_mb: int = 1024,
        description: str | None = None,
        headless: bool = False,
        guest_agent: bool = False,
        cloud_init: bool = False,
        autostart: bool = False,
        reboot_quirk: bool = False,
    ) -> AsyncVm:
        """Create a bare VM record.

        Pass `node=` to pin the VM to a specific node by name or
        numeric id. When omitted, the daemon's scheduler picks a
        node (lowest free-RAM penalty, ties broken by node name).

        Attach drives, network interfaces, SSH keys, and cloud-init
        configuration with the corresponding `AsyncVm` methods after
        create (`attach_disk`, `add_net_if`, `attach_ssh_key`,
        `set_cloud_init`). For one-shot bulk creation with all of
        those wired up, use the apply pipeline (`crv apply`).
        """
        mgr = await self._ensure()
        params = _schema.vm.VmCreateParams.new_message()
        params.name = name
        if node is not None:
            params.node = entity_ref(node)
        params.cpuCount = cpu_count
        params.ramMb = ram_mb
        if description is not None:
            params.description = description
        params.headless = headless
        params.guestAgent = guest_agent
        params.cloudInit = cloud_init
        params.autostart = autostart
        params.rebootQuirk = reboot_quirk
        resp = await mgr.create(params=params)
        return AsyncVm(resp.vm)


@translate_errors
class AsyncVm:
    """Wrapper for the `Vm` resource cap."""

    def __init__(self, cap):
        self._cap = cap

    # ---- queries ----------------------------------------------------------

    async def show(self):
        resp = await self._cap.show()
        return conv.vm_details(resp.details)

    # ---- lifecycle --------------------------------------------------------

    async def start(self, *, wait: bool = False) -> str:
        resp = await self._cap.start(wait=wait)
        return str(resp.status)

    async def stop(self, *, wait: bool = False) -> str:
        resp = await self._cap.stop(wait=wait)
        return str(resp.status)

    async def pause(self) -> str:
        resp = await self._cap.pause()
        return str(resp.status)

    async def reset(self) -> str:
        resp = await self._cap.reset()
        return str(resp.status)

    async def edit(
        self,
        *,
        name: str | None = None,
        cpu_count: int | None = None,
        ram_mb: int | None = None,
        description: str | None = None,
        headless: bool | None = None,
        guest_agent: bool | None = None,
        cloud_init: bool | None = None,
        autostart: bool | None = None,
        reboot_quirk: bool | None = None,
    ) -> None:
        params = _schema.vm.VmEditParams.new_message()
        _set_optional(params, "hasName", "name", name)
        _set_optional(params, "hasCpuCount", "cpuCount", cpu_count)
        _set_optional(params, "hasRamMb", "ramMb", ram_mb)
        _set_optional(params, "hasDescription", "description", description)
        _set_optional(params, "hasHeadless", "headless", headless)
        _set_optional(params, "hasGuestAgent", "guestAgent", guest_agent)
        _set_optional(params, "hasCloudInit", "cloudInit", cloud_init)
        _set_optional(params, "hasAutostart", "autostart", autostart)
        _set_optional(params, "hasRebootQuirk", "rebootQuirk", reboot_quirk)
        await self._cap.edit(params=params)

    async def delete(self, *, keep_disks: bool = False) -> None:
        """Delete this VM.

        By default reaps ephemeral disks attached to the VM
        (cloud-init ISOs, template-instantiated disks). Pass
        ``keep_disks=True`` to leave them in place — useful when
        debugging an instance's state after the VM is gone.
        """
        await self._cap.delete(keepDisks=keep_disks)

    # ---- migration ---------------------------------------------------------

    async def migrate(self, to_node_ref: int | str) -> int:
        """Migrate this stopped VM to another node.

        Returns the task id; the actual transfer runs in the
        background. Poll `tasks.get(tid).show()` until `result`
        transitions out of `running`. See `doc/vm-migration.md`
        for the constraint list.
        """
        params = _schema.vm.VmMigrateParams.new_message()
        params.toNodeRef = entity_ref(to_node_ref)
        resp = await self._cap.migrate(params=params)
        return resp.taskId

    # ---- cloud-init / view / guest exec / hotkeys -------------------------

    async def cloud_init(self):
        resp = await self._cap.cloudInit()
        return conv.cloud_init_info(resp.config)

    async def view_grant(self):
        resp = await self._cap.viewGrant()
        return conv.view_grant(resp.grant)

    async def guest_exec(self, command: str):
        resp = await self._cap.guestExec(command=command)
        return conv.guest_exec_result(resp.result)

    async def send_ctrl_alt_del(self) -> None:
        await self._cap.sendCtrlAltDel()

    # ---- console flush ----------------------------------------------------

    async def serial_console_flush(self) -> None:
        await self._cap.serialConsoleFlush()

    async def hmp_monitor_flush(self) -> None:
        await self._cap.hmpMonitorFlush()

    # ---- streaming endpoints ---------------------------------------------

    async def serial_console(self):
        """Open a bidirectional serial console.

        Returns a `ByteStream`; use `read()` for daemon-to-client bytes
        and `write(chunk)` for client-to-daemon. Call `close()` when done.
        """
        from .streams import open_byte_stream

        return await open_byte_stream(self._cap.serialConsole)

    async def hmp_monitor(self):
        """Open a bidirectional HMP monitor session (returns a `ByteStream`)."""
        from .streams import open_byte_stream

        return await open_byte_stream(self._cap.hmpMonitor)

    async def subscribe_guest_agent(self, on_event):
        """Subscribe to guest-agent state push events.

        `on_event` is an async callable invoked with each
        `GuestAgentStatus`. The returned `GuestAgentSubscription` keeps
        the subscription alive; drop or close it to unsubscribe.
        """
        from .streams import subscribe_guest_agent

        return await subscribe_guest_agent(self._cap, on_event)

    # ---- drives -----------------------------------------------------------

    async def attach_disk(
        self,
        disk_ref: int | str,
        *,
        interface: str | None = None,
        media: str | None = None,
        read_only: bool = False,
        cache_type: str | None = None,
        discard: bool = False,
    ) -> int:
        params = _schema.vm.DriveAttachParams.new_message()
        params.diskRef = entity_ref(disk_ref)
        if interface is not None:
            params.interface = interface
        if media is not None:
            params.media = media
        params.readOnly = read_only
        if cache_type is not None:
            params.cacheType = cache_type
        params.discard = discard
        resp = await self._cap.attachDisk(params=params)
        return resp.driveId

    async def detach_disk(self, drive_id: int) -> None:
        await self._cap.detachDisk(driveId=drive_id)

    async def detach_disk_by_name(self, disk_name: str) -> None:
        """Resolve a disk name to its drive id via show(), then detach.

        Mirrors `Corvus.Client.Capnp.Rpc.rpcDiskDetachByDisk`: the schema
        method takes a drive id, but `crv disk detach` accepts a disk
        name as a convenience. This helper does the lookup client-side.
        """
        details = await self.show()
        for d in details.drives:
            if d.disk_image_name == disk_name:
                await self.detach_disk(d.id)
                return
        raise ValueError(f"VM has no drive backed by disk {disk_name!r}")

    # ---- network interfaces ----------------------------------------------

    async def add_net_if(
        self,
        *,
        type: str | None = None,
        host_device: str | None = None,
        mac_address: str | None = None,
        network_ref: int | str | None = None,
    ) -> int:
        params = _schema.vm.NetIfAddParams.new_message()
        if type is not None:
            params.type = type
        if host_device is not None:
            params.hostDevice = host_device
        if mac_address is not None:
            params.macAddress = mac_address
        if network_ref is not None:
            params.networkRef = entity_ref(network_ref)
        resp = await self._cap.addNetIf(params=params)
        return resp.netIfId

    async def remove_net_if(self, net_if_id: int) -> None:
        await self._cap.removeNetIf(netIfId=net_if_id)

    async def list_net_ifs(self):
        resp = await self._cap.listNetIfs()
        return [conv.net_if_info(n) for n in resp.netIfs]

    # ---- shared dirs ------------------------------------------------------

    async def add_shared_dir(
        self,
        path: str,
        tag: str,
        *,
        cache: str | None = None,
        read_only: bool = False,
    ) -> int:
        params = _schema.vm.SharedDirAddParams.new_message()
        params.path = path
        params.tag = tag
        if cache is not None:
            params.cache = cache
        params.readOnly = read_only
        resp = await self._cap.addSharedDir(params=params)
        return resp.sharedDirId

    async def remove_shared_dir(self, shared_dir_id: int) -> None:
        await self._cap.removeSharedDir(sharedDirId=shared_dir_id)

    async def list_shared_dirs(self):
        resp = await self._cap.listSharedDirs()
        return [conv.shared_dir_info(s) for s in resp.sharedDirs]

    # ---- snapshots --------------------------------------------------------

    async def snapshot_create(self, name: str):
        from .disk import AsyncSnapshot

        req = self._cap.snapshotCreate_request()
        req.name = name
        resp = await req.send()
        return AsyncSnapshot(resp.snapshot)

    async def snapshot_list(self):
        resp = await self._cap.snapshotList()
        return [conv.snapshot_info(s) for s in resp.snapshots]

    async def snapshot_get(self, ref: int | str, *, by_name: bool = False):
        from .disk import AsyncSnapshot

        resp = await self._cap.snapshotGet(ref=entity_ref(ref, by_name=by_name))
        return AsyncSnapshot(resp.snapshot)

    # ---- ssh keys ---------------------------------------------------------

    async def attach_ssh_key(self, key_ref: int | str) -> None:
        await self._cap.attachSshKey(keyRef=entity_ref(key_ref))

    async def detach_ssh_key(self, key_ref: int | str) -> None:
        await self._cap.detachSshKey(keyRef=entity_ref(key_ref))

    async def list_ssh_keys(self):
        resp = await self._cap.listSshKeys()
        return [conv.ssh_key_info(k) for k in resp.keys]
