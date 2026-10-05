"""Cap'n Proto struct → Python dataclass converters.

pycapnp surfaces struct fields as attribute access on a reader object;
enums come back as their lowercased schema name (str). Sentinel
integer fields (`0` ⇒ "absent") are collapsed to `None`. Timestamps
(POSIX nanoseconds) become `datetime` with UTC tzinfo.
"""

from __future__ import annotations

from datetime import datetime, timezone

import capnp

from .. import types as t
from .._device_models import audio_from_wire, network_from_wire
from .._graphics import from_wire


def _ts(ns: int) -> datetime | None:
    """POSIX nanoseconds → timezone-aware datetime; 0 → None."""
    if ns == 0:
        return None
    return datetime.fromtimestamp(ns / 1_000_000_000, tz=timezone.utc)


def _nz_int(n: int) -> int | None:
    return None if n == 0 else n


def _nz_text(s: str) -> str | None:
    return s if s else None


def _nz_float(x: float) -> float | None:
    return None if x == 0.0 else x


def named_ref(r: capnp.lib.capnp._DynamicStructReader) -> t.NamedRef:
    """Convert a ``Common.NamedRef`` capnp struct to the Python dataclass.
    Use directly for required references."""
    return t.NamedRef(id=r.id, name=r.name)


def named_ref_or_none(r: capnp.lib.capnp._DynamicStructReader) -> t.NamedRef | None:
    """Optional variant: the wire sentinel ``id == 0`` becomes ``None``."""
    if r.id == 0:
        return None
    return t.NamedRef(id=r.id, name=r.name)


# ---------------------------------------------------------------------------
# Common
# ---------------------------------------------------------------------------


def status_info(r: capnp.lib.capnp._DynamicStructReader) -> t.StatusInfo:
    return t.StatusInfo(
        uptime_seconds=r.uptimeSeconds,
        connections=r.connections,
        version=r.version,
        protocol_version=r.protocolVersion,
        database_backend=r.databaseBackend,
        database_version=r.databaseVersion,
    )


def view_grant(r: capnp.lib.capnp._DynamicStructReader) -> t.ViewGrant:
    return t.ViewGrant(
        host=r.host,
        port=r.port,
        password=r.password,
        ttl_seconds=r.ttlSeconds,
    )


# ---------------------------------------------------------------------------
# Cloud-init
# ---------------------------------------------------------------------------


def cloud_init_info(r: capnp.lib.capnp._DynamicStructReader) -> t.CloudInitInfo:
    return t.CloudInitInfo(
        user_data=r.userData if r.hasUserData else None,
        network_config=r.networkConfig if r.hasNetworkConfig else None,
        inject_ssh_keys=r.injectSshKeys,
    )


# ---------------------------------------------------------------------------
# Vm
# ---------------------------------------------------------------------------


def vm_info(r: capnp.lib.capnp._DynamicStructReader) -> t.VmInfo:
    return t.VmInfo(
        id=r.id,
        name=r.name,
        node=named_ref(r.node),
        status=str(r.status),
        cpu_count=r.cpuCount,
        ram=r.ram,
        headless=r.headless,
        guest_agent=r.guestAgent,
        tpm=r.tpm,
        cloud_init=r.cloudInit,
        autostart=r.autostart,
        last_healthcheck=_ts(r.lastHealthcheck),
        reboot_quirk=r.rebootQuirk,
        cpu_model=r.cpuModel,
        graphics_adapter=from_wire(r.graphicsAdapter),
        vsock=r.vsock,
        balloon=r.balloon,
        rng=r.rng,
    )


def drive_info(r: capnp.lib.capnp._DynamicStructReader) -> t.DriveInfo:
    return t.DriveInfo(
        id=r.id,
        disk_image=named_ref_or_none(r.diskImage),
        interface=str(r.interface),
        file_path=r.filePath,
        format=str(r.format),
        media=str(r.media),
        read_only=r.readOnly,
        cache_type=str(r.cacheType),
        discard=r.discard,
    )


def net_if_info(r: capnp.lib.capnp._DynamicStructReader) -> t.NetIfInfo:
    return t.NetIfInfo(
        id=r.id,
        type=str(r.type),
        host_device=r.hostDevice,
        mac_address=r.macAddress,
        model=network_from_wire(r.model),
        network=named_ref_or_none(r.network),
        guest_ip_addresses=_nz_text(r.guestIpAddresses),
        ip_address=_nz_text(r.ipAddress),
    )


def shared_dir_info(r: capnp.lib.capnp._DynamicStructReader) -> t.SharedDirInfo:
    return t.SharedDirInfo(
        id=r.id,
        path=r.path,
        tag=r.tag,
        cache=str(r.cache),
        read_only=r.readOnly,
        pid=_nz_int(r.pid),
    )


def audio_device_info(r: capnp.lib.capnp._DynamicStructReader) -> t.AudioDeviceInfo:
    return t.AudioDeviceInfo(
        id=r.id,
        backend=str(r.backend),
        options=r.options,
        model=audio_from_wire(r.model),
    )


def vm_details(r: capnp.lib.capnp._DynamicStructReader) -> t.VmDetails:
    return t.VmDetails(
        id=r.id,
        name=r.name,
        node=named_ref(r.node),
        created_at=_ts(r.createdAt) or datetime.fromtimestamp(0, tz=timezone.utc),
        status=str(r.status),
        cpu_count=r.cpuCount,
        ram=r.ram,
        description=_nz_text(r.description),
        drives=[drive_info(d) for d in r.drives],
        net_ifs=[net_if_info(n) for n in r.netIfs],
        shared_dirs=[shared_dir_info(s) for s in r.sharedDirs],
        audio_devices=[audio_device_info(a) for a in r.audioDevices],
        headless=r.headless,
        monitor_socket=r.monitorSocket,
        spice_port=_nz_int(r.spicePort),
        vsock_cid=_nz_int(r.vsockCid),
        serial_socket=r.serialSocket,
        guest_agent_socket=r.guestAgentSocket,
        guest_agent=r.guestAgent,
        tpm=r.tpm,
        cloud_init=r.cloudInit,
        cloud_init_config=cloud_init_info(r.cloudInitConfig) if r.cloudInit else None,
        last_healthcheck=_ts(r.lastHealthcheck),
        autostart=r.autostart,
        error_message=_nz_text(r.errorMessage),
        last_error_at=_ts(r.lastErrorAt),
        reboot_quirk=r.rebootQuirk,
        cpu_model=r.cpuModel,
        graphics_adapter=from_wire(r.graphicsAdapter),
        stats=vm_stats(r.stats),
        vsock=r.vsock,
        balloon=r.balloon,
        rng=r.rng,
    )


def vm_stats(r: capnp.lib.capnp._DynamicStructReader) -> t.VmStats:
    return t.VmStats(
        sampled_at_nanos=r.sampledAtNanos,
        interval_millis=r.intervalMillis,
        cpu_jiffies_total=r.cpuJiffiesTotal,
        clk_tck=r.clkTck,
        host_rss_bytes=r.hostRssBytes,
        balloon_actual_bytes=r.balloonActualBytes,
        balloon_max_bytes=r.balloonMaxBytes,
        drives=[drive_io(d) for d in r.drives],
        nets=[net_io(n) for n in r.nets],
    )


def drive_io(r: capnp.lib.capnp._DynamicStructReader) -> t.DriveIo:
    return t.DriveIo(
        name=r.name,
        read_bytes_total=r.readBytesTotal,
        write_bytes_total=r.writeBytesTotal,
        read_ops_total=r.readOpsTotal,
        write_ops_total=r.writeOpsTotal,
    )


def net_io(r: capnp.lib.capnp._DynamicStructReader) -> t.NetIo:
    return t.NetIo(
        tap_name=r.tapName,
        rx_bytes_total=r.rxBytesTotal,
        tx_bytes_total=r.txBytesTotal,
    )


def guest_exec_result(r: capnp.lib.capnp._DynamicStructReader) -> t.GuestExecResult:
    return t.GuestExecResult(
        exit_code=r.exitCode,
        stdout=r.stdout,
        stderr=r.stderr,
    )


# ---------------------------------------------------------------------------
# Disk
# ---------------------------------------------------------------------------


def disk_attachment(r: capnp.lib.capnp._DynamicStructReader) -> t.DiskAttachment:
    return t.DiskAttachment(vm=named_ref(r.vm))


def disk_image_placement(
    r: capnp.lib.capnp._DynamicStructReader,
) -> t.DiskImagePlacement:
    return t.DiskImagePlacement(
        node=named_ref(r.node),
        file_path=r.filePath,
    )


def disk_image_info(r: capnp.lib.capnp._DynamicStructReader) -> t.DiskImageInfo:
    return t.DiskImageInfo(
        id=r.id,
        name=r.name,
        format=str(r.format),
        size=_nz_int(r.size),
        created_at=_ts(r.createdAt) or datetime.fromtimestamp(0, tz=timezone.utc),
        placements=[disk_image_placement(p) for p in r.placements],
        attached_to=[disk_attachment(a) for a in r.attachedTo],
        backing_image=named_ref_or_none(r.backingImage),
        ephemeral=r.ephemeral,
    )


def snapshot_info(r: capnp.lib.capnp._DynamicStructReader) -> t.SnapshotInfo:
    return t.SnapshotInfo(
        id=r.id,
        name=r.name,
        created_at=_ts(r.createdAt) or datetime.fromtimestamp(0, tz=timezone.utc),
        size=_nz_int(r.size),
        live=getattr(r, "live", False),
        quiesced=getattr(r, "quiesced", False),
        has_vmstate=getattr(r, "hasVmstate", False),
    )


def vm_snapshot_info(r: capnp.lib.capnp._DynamicStructReader) -> t.VmSnapshotInfo:
    return t.VmSnapshotInfo(
        name=r.name,
        created_at=_ts(r.createdAt) or datetime.fromtimestamp(0, tz=timezone.utc),
        vm=named_ref(r.vm),
        carrier_disk=named_ref(r.carrierDisk),
        disk_count=r.diskCount,
        total_size=r.totalSize,
    )


# ---------------------------------------------------------------------------
# Node
# ---------------------------------------------------------------------------


def node_info(r: capnp.lib.capnp._DynamicStructReader) -> t.NodeInfo:
    return t.NodeInfo(
        id=r.id,
        name=r.name,
        host=r.host,
        node_agent_port=r.nodeAgentPort,
        net_agent_port=r.netAgentPort,
        admin_state=str(r.adminState),
        created_at=_ts(r.createdAt) or datetime.fromtimestamp(0, tz=timezone.utc),
        cpu_count=_nz_int(r.cpuCount),
        ram_total=_nz_int(r.ramTotal),
        ram_free=_nz_int(r.ramFree),
        storage_bytes_total=_nz_int(r.storageBytesTotal),
        storage_bytes_free=_nz_int(r.storageBytesFree),
        load_avg1=_nz_float(r.loadAvg1),
        last_node_agent_push_at=_ts(r.lastNodeAgentPushAt),
        last_net_agent_push_at=_ts(r.lastNetAgentPushAt),
        netd_disabled=r.netdDisabled,
        netd_connected=r.netdConnected,
    )


def node_details(r: capnp.lib.capnp._DynamicStructReader) -> t.NodeDetails:
    return t.NodeDetails(
        id=r.id,
        name=r.name,
        host=r.host,
        node_agent_port=r.nodeAgentPort,
        net_agent_port=r.netAgentPort,
        base_path=r.basePath,
        description=_nz_text(r.description),
        admin_state=str(r.adminState),
        created_at=_ts(r.createdAt) or datetime.fromtimestamp(0, tz=timezone.utc),
        cpu_count=_nz_int(r.cpuCount),
        ram_total=_nz_int(r.ramTotal),
        ram_free=_nz_int(r.ramFree),
        storage_bytes_total=_nz_int(r.storageBytesTotal),
        storage_bytes_free=_nz_int(r.storageBytesFree),
        load_avg1=_nz_float(r.loadAvg1),
        load_avg5=_nz_float(r.loadAvg5),
        load_avg15=_nz_float(r.loadAvg15),
        kernel_release=_nz_text(r.kernelRelease),
        agent_version=_nz_text(r.agentVersion),
        last_node_agent_push_at=_ts(r.lastNodeAgentPushAt),
        last_net_agent_push_at=_ts(r.lastNetAgentPushAt),
        netd_disabled=r.netdDisabled,
        netd_connected=r.netdConnected,
    )


# ---------------------------------------------------------------------------
# Network
# ---------------------------------------------------------------------------


def network_info(r: capnp.lib.capnp._DynamicStructReader) -> t.NetworkInfo:
    return t.NetworkInfo(
        id=r.id,
        name=r.name,
        subnet=r.subnet,
        dhcp=r.dhcp,
        nat=r.nat,
        running=r.running,
        dnsmasq_pid=_nz_int(r.dnsmasqPid),
        created_at=_ts(r.createdAt) or datetime.fromtimestamp(0, tz=timezone.utc),
        autostart=r.autostart,
        vni=_nz_int(r.vni),
        peer_node_ids=tuple(r.peerNodeIds),
        dns_servers=tuple(r.dnsServers),
        domain=r.domain,
        host_dns=r.hostDns,
    )


# ---------------------------------------------------------------------------
# Ssh key
# ---------------------------------------------------------------------------


def vm_attachment(r: capnp.lib.capnp._DynamicStructReader) -> t.VmAttachment:
    return t.VmAttachment(vm=named_ref(r.vm))


def ssh_key_info(r: capnp.lib.capnp._DynamicStructReader) -> t.SshKeyInfo:
    return t.SshKeyInfo(
        id=r.id,
        name=r.name,
        public_key=r.publicKey,
        created_at=_ts(r.createdAt) or datetime.fromtimestamp(0, tz=timezone.utc),
        attached_vms=[vm_attachment(a) for a in r.attachedVms],
    )


# ---------------------------------------------------------------------------
# Template
# ---------------------------------------------------------------------------


def template_vm_info(r: capnp.lib.capnp._DynamicStructReader) -> t.TemplateVmInfo:
    return t.TemplateVmInfo(
        id=r.id,
        name=r.name,
        cpu_count=r.cpuCount,
        ram=r.ram,
        description=_nz_text(r.description),
        headless=r.headless,
        guest_agent=r.guestAgent,
        tpm=r.tpm,
        autostart=r.autostart,
        graphics_adapter=from_wire(r.graphicsAdapter),
        vsock=r.vsock,
        balloon=r.balloon,
        rng=r.rng,
    )


def template_drive_info(r: capnp.lib.capnp._DynamicStructReader) -> t.TemplateDriveInfo:
    return t.TemplateDriveInfo(
        disk_image=named_ref_or_none(r.diskImage),
        interface=str(r.interface),
        media=str(r.media) if r.hasMedia else None,
        read_only=r.readOnly,
        cache_type=str(r.cacheType),
        discard=r.discard,
        clone_strategy=str(r.cloneStrategy),
        size=_nz_int(r.size),
        format=str(r.format) if r.hasFormat else None,
    )


def template_net_if_info(
    r: capnp.lib.capnp._DynamicStructReader,
) -> t.TemplateNetIfInfo:
    return t.TemplateNetIfInfo(
        type=str(r.type),
        host_device=_nz_text(r.hostDevice),
        model=network_from_wire(r.model),
    )


def template_ssh_key_info(
    r: capnp.lib.capnp._DynamicStructReader,
) -> t.TemplateSshKeyInfo:
    return t.TemplateSshKeyInfo(id=r.id, name=r.name)


def template_shared_dir_info(
    r: capnp.lib.capnp._DynamicStructReader,
) -> t.TemplateSharedDirInfo:
    return t.TemplateSharedDirInfo(
        id=r.id,
        path=r.path,
        tag=r.tag,
        cache=str(r.cache),
        read_only=r.readOnly,
    )


def template_audio_device_info(
    r: capnp.lib.capnp._DynamicStructReader,
) -> t.TemplateAudioDeviceInfo:
    return t.TemplateAudioDeviceInfo(
        id=r.id,
        backend=str(r.backend),
        options=r.options,
        model=audio_from_wire(r.model),
    )


def template_details(r: capnp.lib.capnp._DynamicStructReader) -> t.TemplateDetails:
    return t.TemplateDetails(
        id=r.id,
        name=r.name,
        cpu_count=r.cpuCount,
        ram=r.ram,
        description=_nz_text(r.description),
        headless=r.headless,
        cloud_init=r.cloudInit,
        guest_agent=r.guestAgent,
        tpm=r.tpm,
        autostart=r.autostart,
        cloud_init_config=cloud_init_info(r.cloudInitConfig) if r.cloudInit else None,
        created_at=_ts(r.createdAt) or datetime.fromtimestamp(0, tz=timezone.utc),
        drives=[template_drive_info(d) for d in r.drives],
        net_ifs=[template_net_if_info(n) for n in r.netIfs],
        ssh_keys=[template_ssh_key_info(k) for k in r.sshKeys],
        shared_dirs=[template_shared_dir_info(s) for s in r.sharedDirs],
        audio_devices=[template_audio_device_info(a) for a in r.audioDevices],
        graphics_adapter=from_wire(r.graphicsAdapter),
        vsock=r.vsock,
        balloon=r.balloon,
        rng=r.rng,
    )


# ---------------------------------------------------------------------------
# Task
# ---------------------------------------------------------------------------


def task_info(r: capnp.lib.capnp._DynamicStructReader) -> t.TaskInfo:
    return t.TaskInfo(
        id=r.id,
        parent_id=_nz_int(r.parentId),
        started_at=_ts(r.startedAt) or datetime.fromtimestamp(0, tz=timezone.utc),
        finished_at=_ts(r.finishedAt),
        subsystem=str(r.subsystem),
        entity=named_ref_or_none(r.entity),
        command=r.command,
        result=str(r.result),
        message=_nz_text(r.message),
        client_name=r.clientName,
    )


# ---------------------------------------------------------------------------
# Apply
# ---------------------------------------------------------------------------


def apply_created(r: capnp.lib.capnp._DynamicStructReader) -> t.ApplyCreated:
    return t.ApplyCreated(name=r.name, id=r.id)


def apply_result(r: capnp.lib.capnp._DynamicStructReader) -> t.ApplyResult:
    return t.ApplyResult(
        ssh_keys=[apply_created(x) for x in r.sshKeys],
        disks=[apply_created(x) for x in r.disks],
        networks=[apply_created(x) for x in r.networks],
        vms=[apply_created(x) for x in r.vms],
        templates=[apply_created(x) for x in r.templates],
    )


# ---------------------------------------------------------------------------
# Streaming payloads
# ---------------------------------------------------------------------------


def build_one_result(r: capnp.lib.capnp._DynamicStructReader) -> t.BuildOneResult:
    return t.BuildOneResult(
        name=r.name,
        artifact_disk_id=_nz_int(r.artifactDiskId),
        error_message=_nz_text(r.errorMessage),
    )


def build_event(r: capnp.lib.capnp._DynamicStructReader) -> t.BuildEvent:
    """BuildEvent is a union; dispatch on the `which()` discriminator."""
    which = r.which()
    if which == "logLine":
        return t.BuildLogLine(line=r.logLine)
    if which == "stepStart":
        g = r.stepStart
        return t.BuildStepStart(step_index=g.stepIndex, name=g.name, command=g.command)
    if which == "stepOutput":
        g = r.stepOutput
        return t.BuildStepOutput(step_index=g.stepIndex, line=g.line)
    if which == "stepEnd":
        g = r.stepEnd
        return t.BuildStepEnd(
            step_index=g.stepIndex,
            result=str(g.result),
            message=_nz_text(g.message),
        )
    if which == "buildEnd":
        g = r.buildEnd
        return t.BuildBuildEnd(
            success=g.success,
            error_message=_nz_text(g.errorMessage),
            artifact_disk_id=_nz_int(g.artifactDiskId),
        )
    if which == "pipelineEnd":
        g = r.pipelineEnd
        return t.BuildPipelineEnd(builds=[build_one_result(b) for b in g.builds])
    if which == "stepCacheHit":
        g = r.stepCacheHit
        return t.BuildStepCacheHit(step_index=g.stepIndex, chain_hash=g.chainHash)
    if which == "stepCacheStore":
        g = r.stepCacheStore
        return t.BuildStepCacheStore(step_index=g.stepIndex, chain_hash=g.chainHash)
    if which == "stepCacheRestore":
        g = r.stepCacheRestore
        return t.BuildStepCacheRestore(prefix=g.prefix, chain_hash=g.chainHash)
    raise ValueError(f"unknown BuildEvent variant: {which!r}")


def apply_event(r: capnp.lib.capnp._DynamicStructReader) -> t.ApplyEvent:
    """ApplyEvent is a union; dispatch on the ``which()`` discriminator."""
    which = r.which()
    if which == "logLine":
        return t.ApplyLogLine(line=r.logLine)
    if which == "phaseStart":
        g = r.phaseStart
        return t.ApplyPhaseStart(phase=g.phase, total=g.total)
    if which == "entityStart":
        g = r.entityStart
        return t.ApplyEntityStart(phase=g.phase, name=g.name, kind=g.kind)
    if which == "entityEnd":
        g = r.entityEnd
        return t.ApplyEntityEnd(
            phase=g.phase,
            name=g.name,
            result=str(g.result),
            entity_id=g.entityId,
            message=_nz_text(g.message),
        )
    if which == "downloadStart":
        g = r.downloadStart
        return t.ApplyDownloadStart(name=g.name, url=g.url)
    if which == "downloadProgress":
        g = r.downloadProgress
        return t.ApplyDownloadProgress(
            name=g.name,
            downloaded=g.downloaded,
            total=g.total,
        )
    if which == "downloadEnd":
        g = r.downloadEnd
        return t.ApplyDownloadEnd(
            name=g.name,
            success=g.success,
            message=_nz_text(g.message),
        )
    if which == "applyEnd":
        g = r.applyEnd
        return t.ApplyEnd(
            result=str(g.result),
            task_id=g.taskId,
            message=_nz_text(g.message),
        )
    raise ValueError(f"unknown ApplyEvent variant: {which!r}")


def guest_agent_status(r: capnp.lib.capnp._DynamicStructReader) -> t.GuestAgentStatus:
    return t.GuestAgentStatus(
        vm_id=r.vmId,
        last_healthcheck=_ts(r.lastHealthcheck),
        enabled=r.enabled,
        reachable=r.reachable,
        message=_nz_text(r.message),
    )


def task_progress_event(r: capnp.lib.capnp._DynamicStructReader) -> t.TaskProgressEvent:
    """TaskProgressEvent is a union over started / progress / finished."""
    which = r.which()
    if which == "started":
        g = r.started
        return t.TaskProgressStarted(
            task_id=r.taskId,
            command=g.command,
            subsystem=str(g.subsystem),
        )
    if which == "progress":
        g = r.progress
        return t.TaskProgressProgress(
            task_id=r.taskId,
            completed=g.completed,
            total=_nz_int(g.total),
            label=_nz_text(g.label),
        )
    if which == "finished":
        g = r.finished
        return t.TaskProgressFinished(
            task_id=r.taskId,
            result=str(g.result),
            message=_nz_text(g.message),
        )
    raise ValueError(f"unknown TaskProgressEvent variant: {which!r}")
