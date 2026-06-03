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
class NamedRef:
    """Reference to another entity by ``id`` + display name.

    Output-only counterpart to the (id-OR-name) RPC-input ``EntityRef``
    in :mod:`corvus_client._async.entityref`. Used wherever one
    response refers to a *different* entity (a drive's disk image, a
    VM's node, a NIC's network, …) so callers see a structured
    ``{id, name}`` object instead of paired flat ``<role>_id`` /
    ``<role>_name`` keys.

    Optional references are represented as ``NamedRef | None``; on the
    wire the absence is encoded as ``id == 0`` and translated at the
    converter boundary.

    See CLAUDE.md ``## Project Rules / Cross-entity references`` for
    the convention this type encodes.
    """

    id: int
    name: str


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
    node: NamedRef
    status: str
    cpu_count: int
    ram_mb: int
    headless: bool
    guest_agent: bool
    cloud_init: bool
    autostart: bool
    last_healthcheck: datetime | None = None
    reboot_quirk: bool = False
    cpu_model: str = "host"


@dataclass(frozen=True)
class DriveInfo:
    id: int
    disk_image: NamedRef
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
    network: NamedRef | None = None
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
    node: NamedRef
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
    cpu_model: str = "host"
    stats: VmStats | None = None
    # ^ Most-recent resource-consumption sample from the daemon.
    # `None` only on legacy responses that predate the field.
    # The daemon emits a zero-filled placeholder when the VM has no
    # sample yet — callers should treat `stats.sampled_at_nanos == 0`
    # as "no sample available".


@dataclass(frozen=True)
class DriveIo:
    """Per-drive cumulative I/O counters from `query-blockstats`."""

    name: str
    read_bytes_total: int
    write_bytes_total: int
    read_ops_total: int
    write_ops_total: int


@dataclass(frozen=True)
class NetIo:
    """Per-TAP cumulative throughput counters from sysfs."""

    tap_name: str
    rx_bytes_total: int
    tx_bytes_total: int


@dataclass(frozen=True)
class VmStats:
    """One resource-consumption sample. Cumulative counters + instant
    gauges; consumers compute rates as
    `delta(counter) / interval_millis`. The agent samples every 10s
    and the daemon caches the most recent 60 samples per VM."""

    sampled_at_nanos: int
    interval_millis: int
    cpu_jiffies_total: int
    clk_tck: int
    host_rss_bytes: int
    balloon_actual_bytes: int
    balloon_max_bytes: int
    drives: list[DriveIo] = field(default_factory=list)
    nets: list[NetIo] = field(default_factory=list)


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
    vm: NamedRef


@dataclass(frozen=True)
class DiskImagePlacement:
    """Per-node placement of a logical disk image.

    A logical `DiskImage` may live on one or many nodes; each
    `DiskImagePlacement` records the on-disk path on a specific
    node. Multi-node deployments have one entry per node the
    image has been replicated to; single-node deployments have
    exactly one.
    """

    node: NamedRef
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
    backing_image: NamedRef | None = None
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
    netd_disabled: bool = False
    netd_connected: bool = False


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
    netd_disabled: bool = False
    netd_connected: bool = False


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
    vm: NamedRef


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
    disk_image: NamedRef | None = None
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
    # ^ Flat parent reference (no NamedRef): tasks don't have a
    # human-readable name field, so there's nothing to nest. See
    # CLAUDE.md ``## Project Rules / Cross-entity references``.
    finished_at: datetime | None = None
    entity: NamedRef | None = None
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


# ---------------------------------------------------------------------------
# Apply (declarative environment) streaming events
# ---------------------------------------------------------------------------
#
# Mirror the ``ApplyEvent`` union in ``schema/streams.capnp``. Each
# event is a frozen dataclass; the union itself is just
# ``ApplyEvent = ApplyLogLine | ApplyPhaseStart | ...`` (a type
# alias declared at the bottom of this section). Phase strings are
# one of ``"sshKeys"``, ``"disks"``, ``"networks"``, ``"vms"``,
# ``"templates"``. The stream always terminates with a single
# :class:`ApplyEnd` followed by the sink's ``end()``.


@dataclass(frozen=True)
class ApplyLogLine:
    line: str


@dataclass(frozen=True)
class ApplyPhaseStart:
    phase: str
    total: int


@dataclass(frozen=True)
class ApplyEntityStart:
    phase: str
    name: str
    kind: str  # e.g. "disk-import", "vm-create", "skip"


@dataclass(frozen=True)
class ApplyEntityEnd:
    phase: str
    name: str
    result: str
    entity_id: int  # 0 == skipped / failed before insert
    message: str | None = None


@dataclass(frozen=True)
class ApplyDownloadStart:
    name: str
    url: str


@dataclass(frozen=True)
class ApplyDownloadProgress:
    name: str
    downloaded: int
    total: int  # 0 == Content-Length unknown


@dataclass(frozen=True)
class ApplyDownloadEnd:
    name: str
    success: bool
    message: str | None = None


@dataclass(frozen=True)
class ApplyEnd:
    result: str
    task_id: int
    message: str | None = None


ApplyEvent = (
    ApplyLogLine
    | ApplyPhaseStart
    | ApplyEntityStart
    | ApplyEntityEnd
    | ApplyDownloadStart
    | ApplyDownloadProgress
    | ApplyDownloadEnd
    | ApplyEnd
)


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
