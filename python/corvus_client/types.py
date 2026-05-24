"""Output dataclasses for the Corvus client.

Each capability method that returns a struct produces an instance of one
of these. Fields use Python conventions (PEP 8 snake_case).

Timestamps reach the wire as POSIX nanoseconds (Int64). We expose them
as `datetime.datetime` with UTC tzinfo; the conversion lives in
`_async/_convert.py`.
"""

from __future__ import annotations

from dataclasses import dataclass, field
from datetime import datetime

# ---------------------------------------------------------------------------
# Common
# ---------------------------------------------------------------------------


@dataclass(frozen=True)
class StatusInfo:
    uptime_seconds: int
    connections: int
    version: str
    protocol_version: int


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
    user_data: str | None = None
    network_config: str | None = None
    inject_ssh_keys: bool = False


# ---------------------------------------------------------------------------
# Vm
# ---------------------------------------------------------------------------


@dataclass(frozen=True)
class VmInfo:
    id: int
    name: str
    node_id: int
    node_name: str
    status: str
    cpu_count: int
    ram_mb: int
    headless: bool
    guest_agent: bool
    cloud_init: bool
    autostart: bool
    last_healthcheck: datetime | None = None
    reboot_quirk: bool = False


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
    network_id: int | None = None
    network_name: str | None = None
    guest_ip_addresses: str | None = None
    ip_address: str | None = None


@dataclass(frozen=True)
class SharedDirInfo:
    id: int
    path: str
    tag: str
    cache: str
    read_only: bool
    pid: int | None = None


@dataclass(frozen=True)
class VmDetails:
    id: int
    name: str
    node_id: int
    node_name: str
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
    description: str | None = None
    spice_port: int | None = None
    vsock_cid: int | None = None
    cloud_init_config: CloudInitInfo | None = None
    last_healthcheck: datetime | None = None
    error_message: str | None = None
    last_error_at: datetime | None = None
    reboot_quirk: bool = False


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
class DiskImagePlacement:
    """Per-node placement of a logical disk image.

    A logical `DiskImage` may live on one or many nodes; each
    `DiskImagePlacement` records the on-disk path on a specific
    node. Multi-node deployments have one entry per node the
    image has been replicated to; single-node deployments have
    exactly one.
    """

    node_id: int
    node_name: str
    file_path: str


@dataclass(frozen=True)
class DiskImageInfo:
    id: int
    name: str
    format: str
    created_at: datetime
    placements: list[DiskImagePlacement] = field(default_factory=list)
    attached_to: list[DiskAttachment] = field(default_factory=list)
    size_mb: int | None = None
    backing_image_id: int | None = None
    backing_image_name: str | None = None
    ephemeral: bool = False


@dataclass(frozen=True)
class SnapshotInfo:
    id: int
    name: str
    created_at: datetime
    size_mb: int | None = None


# ---------------------------------------------------------------------------
# Node
# ---------------------------------------------------------------------------


@dataclass(frozen=True)
class NodeInfo:
    """Short node summary surfaced by ``crv node list``.

    Capacity fields (``cpu_count``, ``ram_*``, ``storage_*``,
    ``load_avg1``) are ``None`` until the node's agent has
    pushed its first stats snapshot.
    """

    id: int
    name: str
    host: str
    node_agent_port: int
    net_agent_port: int
    admin_state: str
    created_at: datetime
    cpu_count: int | None = None
    ram_mb_total: int | None = None
    ram_mb_free: int | None = None
    storage_bytes_total: int | None = None
    storage_bytes_free: int | None = None
    load_avg1: float | None = None
    last_node_agent_push_at: datetime | None = None
    last_net_agent_push_at: datetime | None = None


@dataclass(frozen=True)
class NodeDetails:
    """Full per-node detail surfaced by ``crv node show``."""

    id: int
    name: str
    host: str
    node_agent_port: int
    net_agent_port: int
    base_path: str
    admin_state: str
    created_at: datetime
    description: str | None = None
    cpu_count: int | None = None
    ram_mb_total: int | None = None
    ram_mb_free: int | None = None
    storage_bytes_total: int | None = None
    storage_bytes_free: int | None = None
    load_avg1: float | None = None
    load_avg5: float | None = None
    load_avg15: float | None = None
    kernel_release: str | None = None
    agent_version: str | None = None
    last_node_agent_push_at: datetime | None = None
    last_net_agent_push_at: datetime | None = None


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
    dnsmasq_pid: int | None = None
    vni: int | None = None
    peer_node_ids: tuple[int, ...] = ()


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
    description: str | None = None


@dataclass(frozen=True)
class TemplateDriveInfo:
    interface: str
    read_only: bool
    cache_type: str
    discard: bool
    clone_strategy: str
    disk_image_id: int | None = None
    disk_image_name: str | None = None
    media: str | None = None
    size_mb: int | None = None
    format: str | None = None


@dataclass(frozen=True)
class TemplateNetIfInfo:
    type: str
    host_device: str | None = None


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
    description: str | None = None
    cloud_init_config: CloudInitInfo | None = None


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
    client_name: str = "local"
    parent_id: int | None = None
    finished_at: datetime | None = None
    entity_id: int | None = None
    entity_name: str | None = None
    message: str | None = None


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
    message: str | None = None


@dataclass(frozen=True)
class BuildBuildEnd:
    success: bool
    error_message: str | None = None
    artifact_disk_id: int | None = None


@dataclass(frozen=True)
class BuildOneResult:
    name: str
    artifact_disk_id: int | None = None
    error_message: str | None = None


@dataclass(frozen=True)
class BuildPipelineEnd:
    builds: list[BuildOneResult] = field(default_factory=list)


@dataclass(frozen=True)
class GuestAgentStatus:
    vm_id: int
    enabled: bool
    reachable: bool
    last_healthcheck: datetime | None = None
    message: str | None = None


@dataclass(frozen=True)
class TaskProgressStarted:
    task_id: int
    command: str
    subsystem: str


@dataclass(frozen=True)
class TaskProgressProgress:
    task_id: int
    completed: int
    total: int | None = None
    label: str | None = None


@dataclass(frozen=True)
class TaskProgressFinished:
    task_id: int
    result: str
    message: str | None = None
