@0xbc08f7ad74ba3f78;

using Streams = import "streams.capnp";

# Cap'n Proto wire schema for `corvus-nodeagent`, the per-host
# privileged agent that owns local-machine state: QEMU + virtiofsd
# process supervision, qemu-img and cloud-init ISO generation, the
# console ring buffer, and the guest-agent poller.
#
# Phase 1 ships the bootstrap surface only — `ping`, `version`,
# `session`. Disk / VM / console methods land in later phases.
#
# Symmetric with `netagent.capnp`: stateless agent, declarative
# `applyX` / `deleteX` / `listX` RPC, daemon-side reconnect-and-
# re-apply. The two agents are fully independent of each other;
# the daemon dials both at startup.
#
# Naming convention: every agent-managed resource on the host has a
# fixed prefix the agent uses for startup / shutdown cleanup:
#
#   * QEMU processes:     `corvus-qemu/*` (argv match)
#   * virtiofsd processes: `virtiofsd ... --shared-dir=*corvus*`
#   * runtime sockets:    `$XDG_RUNTIME_DIR/corvus/<vm>/`
#
# Authentication: out of scope for v1. The agent binds to
# 127.0.0.1:9878 and trusts every connection.

# ---------------------------------------------------------------------
# Bootstrap interface
# ---------------------------------------------------------------------

interface NodeAgent {
  # Open a new session. `owner` is a free-form tag (typically the
  # caller's numeric uid as a string); reserved for future per-owner
  # ledger partitioning. Not authenticated.
  session @0 (owner :Text) -> (session :Session);

  # Liveness + capability probe.
  ping    @1 () -> ();
  version @2 () -> (info :AgentInfo);
}

struct AgentInfo {
  semver       @0 :Text;       # "0.1.0"
  capabilities @1 :List(Text); # phase-by-phase: "disk", "vm", "console", ...
}

# ---------------------------------------------------------------------
# Session — scoped to one owner tag.
#
# Phase 1: liveness only. Later phases extend with:
#   * applyDisk / deleteDisk / snapshotDisk / rebaseDisk / importDisk
#   * applyCloudInit / deleteCloudInit
#   * applyVm / deleteVm / listVms / getVm / controlVm
#   * setSpiceTicket / guestExec
#   * openSerialConsole / openHmpMonitor
#   * subscribeStatus
# ---------------------------------------------------------------------

interface Session {
  # End-to-end check that the session is wired up. Phase 1 calls this
  # from the daemon after `session(owner)` succeeds, to prove the
  # session cap is live before declaring the connection healthy.
  ping @0 () -> ();

  # -------------------------------------------------------------------
  # Disk image operations (qemu-img wrappers + cp + curl/wget + xz +
  # md5sum). The daemon supplies absolute filesystem paths; the agent
  # operates on them.
  #
  # Operation-level outcomes (ok / not-found / format-unsupported /
  # generic-error) are carried back via DiskOpResult so the daemon
  # can pattern-match without parsing error strings — mirrors the
  # existing Haskell `ImageResult` ADT.
  # -------------------------------------------------------------------

  diskCreate         @1 (path :Text, format :Text, sizeMb :Int64)
                        -> (result :DiskOpResult);
  diskCreateOverlay  @2 (overlayPath :Text, backingPath :Text, backingFormat :Text)
                        -> (result :DiskOpResult);
  diskDelete         @3 (path :Text) -> (result :DiskOpResult);
  diskResize         @4 (path :Text, newSizeMb :Int64)
                        -> (result :DiskOpResult);
  # `hasNewBacking = false` flattens (removes backing); `true` rebases.
  diskRebase         @5 (overlayPath :Text,
                         newBacking :Text,
                         newBackingFormat :Text,
                         hasNewBacking :Bool,
                         unsafeUpdate :Bool)
                        -> (result :DiskOpResult);
  diskClone          @6 (sourcePath :Text, destPath :Text)
                        -> (result :DiskOpResult);
  diskInspect        @7 (path :Text) -> (info :DiskInspectInfo);

  # Snapshot operations (qcow2 only; format errors surface via DiskOpResult).
  snapshotCreate     @8  (path :Text, name :Text) -> (result :DiskOpResult);
  snapshotDelete     @9  (path :Text, name :Text) -> (result :DiskOpResult);
  snapshotRollback   @10 (path :Text, name :Text) -> (result :DiskOpResult);

  # Image download (curl, fall back to wget) + xz decompression +
  # md5 hashing. These shell out to host tools and write to the
  # supplied destination path.
  diskDownload       @11 (destPath :Text, url :Text)
                        -> (result :DiskOpResult);
  diskDecompressXz   @12 (xzPath :Text) -> (finalPath :Text);
  diskMd5            @13 (path :Text) -> (hex :Text);

  # Cloud-init NoCloud ISO assembly. Daemon supplies the composed
  # user-data / meta-data text (the SSH-key + user-data composition
  # stays daemon-side because it consults the DB). Agent creates
  # `targetDir`, writes the three input files, then invokes
  # genisoimage (falls back to mkisofs).
  cloudInitGenerateIso @14
    (targetDir :Text,
     userData :Text,
     metaData :Text,
     networkConfig :Text,
     hasNetworkConfig :Bool)
    -> (isoPath :Text);

  # -------------------------------------------------------------------
  # Process supervision primitives (Phase 3).
  #
  # The agent owns every subprocess Corvus spawns: QEMU, virtiofsd.
  # The daemon submits intent — "spawn this command", "kill that
  # PID" — and the agent executes. PIDs are agent-owned but
  # transparent to the daemon (it stores them in the DB and uses
  # them as opaque handles when calling back).
  #
  # The daemon still generates command lines from DB rows (via
  # Corvus.Node.Command, which is library code accessible to
  # both processes). Only the actual fork/exec happens here.
  # -------------------------------------------------------------------

  # -------------------------------------------------------------------
  # VM lifecycle. The agent owns every subprocess. Daemon submits
  # a VmSpec; agent generates QEMU argv, spawns virtiofsd iff
  # spec.sharedDirs is non-empty, then spawns QEMU; tracks PIDs
  # internally keyed by spec.vmId. Stop / pause / exec methods
  # take only the vmId. PIDs stay agent-private.
  # -------------------------------------------------------------------

  # Start a VM. Idempotent: if spec.vmId is already in the ledger,
  # returns its current VmRuntimeInfo without re-spawning. When
  # spec.waitForGuestAgentMs is non-zero, the agent polls QGA
  # after spawn and only returns once a ping succeeds (or throws
  # on timeout).
  vmStart @15 (spec :VmSpec) -> (info :VmRuntimeInfo);

  # Graceful shutdown: QMP system_powerdown (ACPI), wait up to
  # timeoutSec for QEMU to exit, then drop the ledger entry.
  # Returns 'timeout' if QEMU is still alive when the window ends
  # (caller falls through to vmStopHard).
  vmStopGraceful @16 (vmId :Int64, timeoutSec :UInt32)
    -> (result :VmStopResult);

  # Force-stop: SIGTERM-then-SIGKILL the QEMU process and every
  # virtiofsd helper for this vmId. Drops the ledger entry.
  vmStopHard @17 (vmId :Int64) -> (result :VmStopResult);

  # QMP `stop` — freeze CPU execution. VM stays in memory; ledger
  # entry stays in place.
  vmPause @18 (vmId :Int64) -> ();

  # QMP `cont` — resume from pause.
  vmResume @19 (vmId :Int64) -> ();

  # Execute a command via QGA on the running VM. Agent locates
  # the QGA socket from the ledger entry for req.vmId.
  vmGuestExec @20 (req :VmGuestExecReq) -> (info :VmGuestExecInfo);

  # Per-vmId status probe. Used by the daemon's monitor at
  # reconnect time before the status subscription's first tick
  # fires.
  vmStatus @21 (vmId :Int64) -> (status :VmAgentStatus);

  # Install a one-time SPICE password via QMP (set_password +
  # expire_password). Daemon picks the password.
  vmSetSpiceTicket @22
    (vmId :Int64, password :Text, ttlSeconds :UInt32) -> ();

  # Register a status sink. Once subscribed, the agent walks its
  # VM ledger every ~10 s, pings each VM's QGA, collects network
  # interfaces for the responsive ones, and pushes one
  # `VmStatusSnapshot` per tick covering every VM. Sinks that
  # throw on dispatch are pruned automatically — no explicit
  # unsubscribe. Same shape netd uses for `subscribeEvents`.
  subscribeVmStatus @23 (sink :VmStatusSink) -> ();

  # -------------------------------------------------------------------
  # Chardev streaming. The agent owns the QEMU chardev sockets and
  # ring-buffers their output for replay. These RPCs hand a relay
  # endpoint to the caller:
  #
  #   * `sink` (input): caller-supplied. Agent replays the ring
  #     buffer through it on connect, then streams live data.
  #   * `input` (return): server-side sink. Caller writes here to
  #     forward bytes into QEMU's chardev (keystrokes, HMP
  #     commands, …).
  #
  # The daemon proxies these calls today (relays bytes between the
  # client and the agent); a future direct-to-client variant can be
  # added without changing this shape.
  # -------------------------------------------------------------------

  openSerialConsole @24 (vmId :Int64, sink :Streams.ByteSink)
    -> (input :Streams.ByteSink);

  openHmpMonitor    @25 (vmId :Int64, sink :Streams.ByteSink)
    -> (input :Streams.ByteSink);

  # Discard the scrollback for the named chardev buffer. No-op if
  # the buffer doesn't exist (VM stopped).
  flushSerialConsole @26 (vmId :Int64) -> ();
  flushHmpMonitor    @27 (vmId :Int64) -> ();
}

# Daemon-implemented sink for periodic agent → daemon VM status
# push. One call per tick (~10 s) per subscriber.
interface VmStatusSink {
  onSnapshot @0 (snapshot :VmStatusSnapshot) -> ();
}

# DiskOpResult mirrors Corvus.Qemu.Image.ImageResult.
struct DiskOpResult {
  kind    @0 :DiskOpKind;
  message @1 :Text;  # empty on success; populated for error kinds
}

enum DiskOpKind {
  success                @0;
  errorGeneric           @1;
  errorNotFound          @2;
  errorFormatUnsupported @3;
}

# DiskInspectInfo mirrors Corvus.Qemu.Image.ImageInfo.
struct DiskInspectInfo {
  format         @0 :Text;     # "qcow2" / "raw" / …; matches DriveFormat enum
  virtualSizeMb  @1 :Int64;
  actualSizeMb   @2 :Int64;    # 0 if unknown
  hasActualSize  @3 :Bool;
  snapshots      @4 :List(DiskSnapshotInfo);
}

struct DiskSnapshotInfo {
  id      @0 :Text;
  name    @1 :Text;
  sizeMb  @2 :Int64;   # 0 if unknown
  hasSize @3 :Bool;
}

# ---------------------------------------------------------------------
# VM abstraction (Phase 3 refactor)
# ---------------------------------------------------------------------

# VmSpec carries everything the agent needs to build the QEMU argv,
# decide whether to spawn virtiofsd, and reach the VM's runtime
# sockets. The daemon assembles it from DB rows; the agent never
# queries the DB.
struct VmSpec {
  vmId             @0  :Int64;
  name             @1  :Text;
  cpuCount         @2  :Int32;
  ramMb            @3  :Int32;
  headless         @4  :Bool;
  guestAgent       @5  :Bool;
  vsockCid         @6  :UInt32;    # 0 when no vsock
  hasVsockCid      @7  :Bool;
  spicePort        @8  :Int32;     # 0 when no spice
  hasSpicePort     @9  :Bool;
  drives           @10 :List(VmDriveSpec);
  netIfs           @11 :List(VmNetIfSpec);
  sharedDirs       @12 :List(VmSharedDirSpec);
  # When non-zero, the agent polls QGA after spawn and only
  # returns from vmStart once a ping succeeds. Throws on timeout.
  # Daemon sets a positive value for guestAgent-enabled VMs so
  # `crv vm start --wait` blocks until QGA is reachable.
  waitForGuestAgentMs @13 :UInt32;
}

struct VmDriveSpec {
  diskFilePath @0 :Text;   # absolute path
  format       @1 :Text;   # "qcow2" / "raw" / "vmdk" / ...
  ifKind       @2 :Text;   # "virtio" / "ide" / "scsi" / "none"
  media        @3 :Text;   # "disk" / "cdrom" (empty = disk)
  readOnly     @4 :Bool;
  cache        @5 :Text;   # "none" / "writeback" / ...
  discard      @6 :Bool;
}

struct VmNetIfSpec {
  ifType     @0 :Text;     # "user" / "tap" / "bridge" / "macvtap" / "managed"
  hostDevice @1 :Text;     # resolved TAP / bridge name; daemon
                           # populates from netd for managed NICs
  macAddress @2 :Text;
}

struct VmSharedDirSpec {
  hostPath @0 :Text;
  tag      @1 :Text;
  cache    @2 :Text;       # "auto" / "always" / "never"
  readOnly @3 :Bool;
}

# Runtime info returned by vmStart and queryable thereafter.
struct VmRuntimeInfo {
  qemuPid       @0 :Int32;
  virtiofsdPids @1 :List(Int32);
  spicePort     @2 :Int32;   # echoed back from spec; 0 if none
}

# Result of stop operations.
struct VmStopResult {
  kind    @0 :VmStopKind;
  message @1 :Text;
}

enum VmStopKind {
  stopped        @0;   # QEMU exited; virtiofsd helpers reaped
  alreadyStopped @1;   # vmId wasn't in the ledger
  timeout        @2;   # graceful path exceeded timeoutSec
  failed         @3;   # signalling raised; message has details
}

# Per-vmId status probe.
struct VmAgentStatus {
  state        @0 :VmAgentState;
  qemuPid      @1 :Int32;   # 0 if not running
  lastExitCode @2 :Int32;   # populated for stopped/errored
}

enum VmAgentState {
  running @0;
  stopped @1;   # exited cleanly
  errored @2;   # exited non-zero
  unknown @3;   # not in ledger
}

# QGA exec request/response.
struct VmGuestExecReq {
  vmId          @0 :Int64;
  path          @1 :Text;          # binary
  args          @2 :List(Text);
  captureOutput @3 :Bool;
  inputData     @4 :Data;          # empty = no stdin
  timeoutSec    @5 :UInt32;        # 0 = use agent default
}

struct VmGuestExecInfo {
  exitCode @0 :Int32;
  hasExit  @1 :Bool;               # false = still running after timeoutSec
  signal   @2 :Int32;              # 0 = no signal
  stdout   @3 :Data;
  stderr   @4 :Data;
}

# Consolidated status snapshot pushed once per tick to every
# subscribed sink. Empty `entries` when no VMs are running.
struct VmStatusSnapshot {
  snapshotAtMillis @0 :Int64;            # agent wall-clock at tick start
  entries          @1 :List(VmStatusEntry);
}

struct VmStatusEntry {
  vmId           @0 :Int64;
  state          @1 :VmAgentState;       # same enum vmStatus uses
  qemuPid        @2 :Int32;              # 0 if not running
  lastExitCode   @3 :Int32;              # populated when state != running
  guestAgentOk   @4 :Bool;               # last QGA ping in this tick succeeded
  lastPingMillis @5 :Int64;              # epoch ms; 0 if guestAgentOk is false
  netIfs         @6 :List(GuestNetIf);   # populated when guestAgentOk
}

struct GuestNetIf {
  name        @0 :Text;
  hwAddress   @1 :Text;
  ipAddresses @2 :List(GuestIpAddress);
}

struct GuestIpAddress {
  ipAddress  @0 :Text;
  prefix     @1 :Int32;
  ipAddrType @2 :Text;   # "ipv4" / "ipv6"
}
