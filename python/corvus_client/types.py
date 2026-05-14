"""Output dataclasses for the Corvus client.

Each capability method that returns a struct produces an instance of one
of these. Fields use Python conventions (PEP 8 snake_case) and apply the
"sentinel-to-None" mapping spelled out in the schema comments (e.g.
`namespacePid: Int64 = 0` ⇒ `namespace_pid: Optional[int] = None`).

Timestamps reach the wire as POSIX nanoseconds (Int64). We expose them
as `datetime.datetime` with UTC tzinfo; the conversion lives in
`_async/_convert.py`.
"""
from __future__ import annotations

from dataclasses import dataclass, field
from datetime import datetime
from typing import Optional


# ---------------------------------------------------------------------------
# Common
# ---------------------------------------------------------------------------


@dataclass(frozen=True)
class StatusInfo:
    uptime_seconds: int
    connections: int
    version: str
    protocol_version: int
    namespace_pid: Optional[int] = None


@dataclass(frozen=True)
class ViewGrant:
    host: str
    port: int
    password: str
    ttl_seconds: int


# ---------------------------------------------------------------------------
# Cloud-init
# ---------------------------------------------------------------------------


@dataclass(frozen=True)
class CloudInitInfo:
    user_data: Optional[str] = None
    network_config: Optional[str] = None
    inject_ssh_keys: bool = False


# ---------------------------------------------------------------------------
# Vm
# ---------------------------------------------------------------------------


@dataclass(frozen=True)
class VmInfo:
    id: int
    name: str
    status: str
    cpu_count: int
    ram_mb: int
    headless: bool
    guest_agent: bool
    cloud_init: bool
    autostart: bool
    last_healthcheck: Optional[datetime] = None


@dataclass(frozen=True)
class DriveInfo:
    id: int
    disk_image_id: int
    disk_image_name: str
    interface: str
    file_path: str
    format: str
    media: str
    read_only: bool
    cache_type: str
    discard: bool


@dataclass(frozen=True)
class NetIfInfo:
    id: int
    type: str
    host_device: str
    mac_address: str
    network_id: Optional[int] = None
    network_name: Optional[str] = None
    guest_ip_addresses: Optional[str] = None


@dataclass(frozen=True)
class SharedDirInfo:
    id: int
    path: str
    tag: str
    cache: str
    read_only: bool
    pid: Optional[int] = None


@dataclass(frozen=True)
class VmDetails:
    id: int
    name: str
    created_at: datetime
    status: str
    cpu_count: int
    ram_mb: int
    headless: bool
    monitor_socket: str
    serial_socket: str
    guest_agent_socket: str
    guest_agent: bool
    cloud_init: bool
    autostart: bool
    drives: list[DriveInfo] = field(default_factory=list)
    net_ifs: list[NetIfInfo] = field(default_factory=list)
    shared_dirs: list[SharedDirInfo] = field(default_factory=list)
    description: Optional[str] = None
    spice_port: Optional[int] = None
    vsock_cid: Optional[int] = None
    cloud_init_config: Optional[CloudInitInfo] = None
    last_healthcheck: Optional[datetime] = None


@dataclass(frozen=True)
class GuestExecResult:
    exit_code: int
    stdout: str
    stderr: str


# ---------------------------------------------------------------------------
# Disk
# ---------------------------------------------------------------------------


@dataclass(frozen=True)
class DiskAttachment:
    vm_id: int
    vm_name: str


@dataclass(frozen=True)
class DiskImageInfo:
    id: int
    name: str
    file_path: str
    format: str
    created_at: datetime
    attached_to: list[DiskAttachment] = field(default_factory=list)
    size_mb: Optional[int] = None
    backing_image_id: Optional[int] = None
    backing_image_name: Optional[str] = None


@dataclass(frozen=True)
class SnapshotInfo:
    id: int
    name: str
    created_at: datetime
    size_mb: Optional[int] = None


# ---------------------------------------------------------------------------
# Network
# ---------------------------------------------------------------------------


@dataclass(frozen=True)
class NetworkInfo:
    id: int
    name: str
    subnet: str
    dhcp: bool
    nat: bool
    running: bool
    autostart: bool
    created_at: datetime
    dnsmasq_pid: Optional[int] = None


# ---------------------------------------------------------------------------
# SSH key
# ---------------------------------------------------------------------------


@dataclass(frozen=True)
class VmAttachment:
    vm_id: int
    vm_name: str


@dataclass(frozen=True)
class SshKeyInfo:
    id: int
    name: str
    public_key: str
    created_at: datetime
    attached_vms: list[VmAttachment] = field(default_factory=list)


# ---------------------------------------------------------------------------
# Template
# ---------------------------------------------------------------------------


@dataclass(frozen=True)
class TemplateVmInfo:
    id: int
    name: str
    cpu_count: int
    ram_mb: int
    headless: bool
    guest_agent: bool
    autostart: bool
    description: Optional[str] = None


@dataclass(frozen=True)
class TemplateDriveInfo:
    interface: str
    read_only: bool
    cache_type: str
    discard: bool
    clone_strategy: str
    disk_image_id: Optional[int] = None
    disk_image_name: Optional[str] = None
    media: Optional[str] = None
    size_mb: Optional[int] = None
    format: Optional[str] = None


@dataclass(frozen=True)
class TemplateNetIfInfo:
    type: str
    host_device: Optional[str] = None


@dataclass(frozen=True)
class TemplateSshKeyInfo:
    id: int
    name: str


@dataclass(frozen=True)
class TemplateDetails:
    id: int
    name: str
    cpu_count: int
    ram_mb: int
    headless: bool
    cloud_init: bool
    guest_agent: bool
    autostart: bool
    created_at: datetime
    drives: list[TemplateDriveInfo] = field(default_factory=list)
    net_ifs: list[TemplateNetIfInfo] = field(default_factory=list)
    ssh_keys: list[TemplateSshKeyInfo] = field(default_factory=list)
    description: Optional[str] = None
    cloud_init_config: Optional[CloudInitInfo] = None


# ---------------------------------------------------------------------------
# Task
# ---------------------------------------------------------------------------


@dataclass(frozen=True)
class TaskInfo:
    id: int
    started_at: datetime
    subsystem: str
    command: str
    result: str
    parent_id: Optional[int] = None
    finished_at: Optional[datetime] = None
    entity_id: Optional[int] = None
    entity_name: Optional[str] = None
    message: Optional[str] = None


# ---------------------------------------------------------------------------
# Apply
# ---------------------------------------------------------------------------


@dataclass(frozen=True)
class ApplyCreated:
    name: str
    id: int


@dataclass(frozen=True)
class ApplyResult:
    ssh_keys: list[ApplyCreated] = field(default_factory=list)
    disks: list[ApplyCreated] = field(default_factory=list)
    networks: list[ApplyCreated] = field(default_factory=list)
    vms: list[ApplyCreated] = field(default_factory=list)
    templates: list[ApplyCreated] = field(default_factory=list)


# ---------------------------------------------------------------------------
# Streaming payloads
# ---------------------------------------------------------------------------


@dataclass(frozen=True)
class BuildLogLine:
    line: str


@dataclass(frozen=True)
class BuildStepStart:
    step_index: int
    name: str
    command: str


@dataclass(frozen=True)
class BuildStepOutput:
    step_index: int
    line: str


@dataclass(frozen=True)
class BuildStepEnd:
    step_index: int
    result: str
    message: Optional[str] = None


@dataclass(frozen=True)
class BuildBuildEnd:
    success: bool
    error_message: Optional[str] = None
    artifact_disk_id: Optional[int] = None


@dataclass(frozen=True)
class BuildOneResult:
    name: str
    artifact_disk_id: Optional[int] = None
    error_message: Optional[str] = None


@dataclass(frozen=True)
class BuildPipelineEnd:
    builds: list[BuildOneResult] = field(default_factory=list)


@dataclass(frozen=True)
class GuestAgentStatus:
    vm_id: int
    enabled: bool
    reachable: bool
    last_healthcheck: Optional[datetime] = None
    message: Optional[str] = None


@dataclass(frozen=True)
class TaskProgressStarted:
    task_id: int
    command: str
    subsystem: str


@dataclass(frozen=True)
class TaskProgressProgress:
    task_id: int
    completed: int
    total: Optional[int] = None
    label: Optional[str] = None


@dataclass(frozen=True)
class TaskProgressFinished:
    task_id: int
    result: str
    message: Optional[str] = None
