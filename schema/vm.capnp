@0xa7366eabdb0b1db4;

using Common = import "common.capnp";
using Enums = import "enums.capnp";
using Streams = import "streams.capnp";
using CloudInit = import "cloudinit.capnp";
using Disk = import "disk.capnp";
using Network = import "network.capnp";
using SshKey = import "sshkey.capnp";

# ---------------------------------------------------------------------
# Info / detail structs
# ---------------------------------------------------------------------

struct VmInfo {
  id              @0  :Int64;
  name            @1  :Text;
  status          @2  :Enums.VmStatus;
  cpuCount        @3  :Int32;
  ramMb           @4  :Int32;
  headless        @5  :Bool;
  guestAgent      @6  :Bool;
  cloudInit       @7  :Bool;
  lastHealthcheck @8  :Int64;  # POSIX nanoseconds; 0 == never
  autostart       @9  :Bool;
  # When true, QEMU is started with `-no-reboot` and the agent
  # restarts the VM after each guest-initiated exit (reboot or
  # poweroff). Daemon-initiated stop/reset continues to actually
  # stop the VM. Used to dodge OVMF firmware reboot hangs.
  rebootQuirk     @10 :Bool;
  # FK to the Node row hosting this VM, plus the node's display
  # name. The daemon resolves the name daemon-side so list-view
  # clients don't have to follow a second RPC; empty name signals
  # the (rare) race where the Node row was removed under the VM.
  nodeId          @11 :Int64;
  nodeName        @12 :Text;
  # QEMU `-cpu` model. Default `"host"` exposes the host CPU
  # (best perf, NOT migration-safe across non-identical hosts);
  # set to a stable model (`qemu64`, `Westmere-v3`, …) for safe
  # cross-host migration.
  cpuModel        @13 :Text;
}

struct VmDetails {
  id                  @0  :Int64;
  name                @1  :Text;
  createdAt           @2  :Int64;  # POSIX nanoseconds
  status              @3  :Enums.VmStatus;
  cpuCount            @4  :Int32;
  ramMb               @5  :Int32;
  description         @6  :Text;   # empty == none
  drives              @7  :List(DriveInfo);
  netIfs              @8  :List(NetIfInfo);
  sharedDirs          @9  :List(SharedDirInfo);
  headless            @10 :Bool;
  monitorSocket       @11 :Text;
  spicePort           @12 :Int32;   # 0 == none
  vsockCid            @13 :Int32;   # 0 == none
  serialSocket        @14 :Text;
  guestAgentSocket    @15 :Text;
  guestAgent          @16 :Bool;
  cloudInit           @17 :Bool;
  cloudInitConfig     @18 :CloudInit.CloudInitInfo;
  lastHealthcheck     @19 :Int64;
  autostart           @20 :Bool;
  # Last-known error reason and timestamp. Populated when the daemon
  # transitions the VM to `error` status (e.g. failed start). Cleared
  # on the next successful transition back to a healthy state. Empty
  # text / 0 nanoseconds == none.
  errorMessage        @21 :Text;
  lastErrorAt         @22 :Int64;
  # See `VmInfo.rebootQuirk`.
  rebootQuirk         @23 :Bool;
  # FK to the Node row hosting this VM + the node's display name.
  # See `VmInfo.nodeId` / `VmInfo.nodeName` for the sentinel.
  nodeId              @24 :Int64;
  nodeName            @25 :Text;
  # See `VmInfo.cpuModel`.
  cpuModel            @26 :Text;
  # Most-recent resource-consumption sample from the agent's
  # StatusPoller (10-second cadence). Zero-filled when the VM is
  # not running. See `VmStats`.
  stats               @27 :VmStats;
}

# Per-VM resource consumption sample. Cumulative counters + the
# instant gauges; consumers compute rates client-side via
# (counter_at_t - counter_at_t-1) / intervalMillis. Sampled by
# the agent's StatusPoller and ridden up the agent → daemon push
# channel; the daemon keeps the most recent 60 samples per VM (10
# minutes at the 10-second cadence) so the WebUI sparkline panel
# can seed itself on page load via `Vm.getStatsHistory`.
struct VmStats {
  sampledAtNanos     @0 :UInt64;   # epoch nanos at the agent
  # Wall time since this VM's previous sample. 0 on the first
  # sample after VM start.
  intervalMillis     @1 :UInt32;

  # CPU — cumulative jiffies since QEMU launch (utime + stime
  # from /proc/<qemu-pid>/stat). Convert to seconds via clkTck.
  cpuJiffiesTotal    @2 :UInt64;
  clkTck             @3 :UInt32;   # _SC_CLK_TCK at agent startup

  # RAM — instantaneous gauges.
  hostRssBytes       @4 :UInt64;   # /proc/<pid>/status:VmRSS
  balloonActualBytes @5 :UInt64;   # 0 == no balloon device
  balloonMaxBytes    @6 :UInt64;   # 0 == no balloon device

  drives             @7 :List(DriveIo);
  nets               @8 :List(NetIo);
}

struct DriveIo {
  name            @0 :Text;        # matches DriveInfo.diskImageName
  readBytesTotal  @1 :UInt64;
  writeBytesTotal @2 :UInt64;
  readOpsTotal    @3 :UInt64;
  writeOpsTotal   @4 :UInt64;
}

struct NetIo {
  tapName      @0 :Text;           # vmtap0, ...
  rxBytesTotal @1 :UInt64;
  txBytesTotal @2 :UInt64;
}

struct DriveInfo {
  id              @0  :Int64;
  diskImageId     @1  :Int64;
  diskImageName   @2  :Text;
  interface       @3  :Enums.DriveInterface;
  filePath        @4  :Text;
  format          @5  :Enums.DriveFormat;
  media           @6  :Enums.DriveMedia;
  readOnly        @7  :Bool;
  cacheType       @8  :Enums.CacheType;
  discard         @9  :Bool;
}

struct NetIfInfo {
  id                @0 :Int64;
  type              @1 :Enums.NetInterfaceType;
  hostDevice        @2 :Text;
  macAddress        @3 :Text;
  networkId         @4 :Int64;  # 0 == none
  networkName       @5 :Text;   # empty == none
  guestIpAddresses  @6 :Text;   # empty == none; observed by QGA
  ipAddress         @7 :Text;   # empty == none; daemon IPAM allocation
}

struct SharedDirInfo {
  id        @0 :Int64;
  path      @1 :Text;
  tag       @2 :Text;
  cache     @3 :Enums.SharedDirCache;
  readOnly  @4 :Bool;
  pid       @5 :Int32;   # 0 == not running
}

# ---------------------------------------------------------------------
# Parameter structs
# ---------------------------------------------------------------------

struct VmCreateParams {
  # `name` is the only mandatory field. The remaining defaults
  # mirror the `crv vm create` CLI defaults so callers can omit
  # any field they don't care about.
  #
  # `vmManager.create` creates a bare VM record; attach drives,
  # network interfaces, SSH keys, and cloud-init separately
  # (vm.attachDisk, vm.addNetIf, vm.attachSshKey, vm.setCloudInit).
  # For bulk creation with all of those wired up in one call, use
  # the apply pipeline.
  name            @0  :Text;
  cpuCount        @1  :Int32 = 1;
  ramMb           @2  :Int32 = 1024;
  description     @3  :Text;        # empty == none
  headless        @4  :Bool = false;
  guestAgent      @5  :Bool = false;
  cloudInit       @6  :Bool = false;
  autostart       @7  :Bool = false;
  # Node this VM is bound to. Required as of multi-node slice 1c
  # (no scheduler yet). Resolved as `node-by-name` or `id:N`.
  node            @8  :Common.EntityRef;
  rebootQuirk     @9  :Bool = false;
  # QEMU `-cpu` model. Empty string == use the daemon default
  # ("host"). See `VmInfo.cpuModel` for the migration-safety
  # trade-off.
  cpuModel        @10 :Text;
}

struct VmEditParams {
  # Each `Maybe`-like field is set when the user wants to change it.
  # Booleans/integers use a paired `has*` flag to disambiguate from 0/false.
  hasName            @0  :Bool;
  name               @1  :Text;
  hasCpuCount        @2  :Bool;
  cpuCount           @3  :Int32;
  hasRamMb           @4  :Bool;
  ramMb              @5  :Int32;
  hasDescription     @6  :Bool;
  description        @7  :Text;
  hasHeadless        @8  :Bool;
  headless           @9  :Bool;
  hasGuestAgent      @10 :Bool;
  guestAgent         @11 :Bool;
  hasCloudInit       @12 :Bool;
  cloudInit          @13 :Bool;
  hasAutostart       @14 :Bool;
  autostart          @15 :Bool;
  hasRebootQuirk     @16 :Bool;
  rebootQuirk        @17 :Bool;
  hasCpuModel        @18 :Bool;
  cpuModel           @19 :Text;
}

struct DriveAttachParams {
  # Defaults mirror `crv disk attach`: virtio interface, no media
  # override (treats as plain `disk`), writeback cache, no discard,
  # read-write.
  diskRef    @0 :Common.EntityRef;
  interface  @1 :Enums.DriveInterface = virtio;
  media      @2 :Enums.DriveMedia = disk;
  readOnly   @3 :Bool = false;
  cacheType  @4 :Enums.CacheType = writeback;
  discard    @5 :Bool = false;
}

struct NetIfAddParams {
  # Defaults mirror `crv net-if add`: a `user` (SLIRP) interface
  # with no host device, no MAC pin (the daemon picks one), no
  # managed-network binding.
  type         @0 :Enums.NetInterfaceType = user;
  hostDevice   @1 :Text;     # empty == auto
  macAddress   @2 :Text;     # empty == generate
  networkRef   @3 :Common.EntityRef;  # id=0 / name="" == no managed network
}

struct SharedDirAddParams {
  # `path` and `tag` are mandatory; cache defaults to `auto` to
  # match `crv shared-dir add` and read-only is opt-in.
  path      @0 :Text;
  tag       @1 :Text;
  cache     @2 :Enums.SharedDirCache = auto;
  readOnly  @3 :Bool = false;
}

struct GuestExecResult {
  exitCode @0 :Int32;
  stdout   @1 :Text;
  stderr   @2 :Text;
}

# ---------------------------------------------------------------------
# Manager + resource capabilities
# ---------------------------------------------------------------------

interface VmManager {
  list   @0 () -> (vms :List(VmInfo));
  get    @1 (ref :Common.EntityRef) -> (vm :Vm);
  create @2 (params :VmCreateParams) -> (vm :Vm);
}

interface Vm {
  show           @0  () -> (details :VmDetails);
  # Inline-param defaults mirror the CLI: start/stop return
  # immediately by default; `delete` reaps attached ephemeral disks
  # (cloud-init ISOs, template-instantiated disks) unless
  # `keepDisks` is set.
  start          @1  (wait :Bool = false) -> (status :Enums.VmStatus);
  stop           @2  (wait :Bool = false) -> (status :Enums.VmStatus);
  pause          @3  () -> (status :Enums.VmStatus);
  reset          @4  () -> (status :Enums.VmStatus);
  edit           @5  (params :VmEditParams) -> ();
  delete         @6  (keepDisks :Bool = false) -> ();
  cloudInit      @7  () -> (config :CloudInit.CloudInitInfo);
  viewGrant      @8  () -> (grant :Common.ViewGrant);
  guestExec      @9  (command :Text) -> (result :GuestExecResult);
  sendCtrlAltDel @10 () -> ();

  # Bidirectional console.
  # The client passes `sink`; the daemon will call `sink.write(...)`
  # with captured bytes and `sink.end()` on close. The returned
  # `input` cap accepts client → guest bytes.
  serialConsole      @11 (sink :Streams.ByteSink) -> (input :Streams.ByteSink);
  serialConsoleFlush @12 () -> ();
  hmpMonitor         @13 (sink :Streams.ByteSink) -> (input :Streams.ByteSink);
  hmpMonitorFlush    @14 () -> ();

  # Guest-agent status push subscription. Drop the returned `handle`
  # to unsubscribe.
  subscribeGuestAgent @15 (sink :Streams.GuestAgentStatusSink) -> (handle :Streams.Handle);

  # Drives
  attachDisk @16 (params :DriveAttachParams) -> (driveId :Int64);
  detachDisk @17 (driveId :Int64) -> ();

  # Network interfaces
  addNetIf    @18 (params :NetIfAddParams) -> (netIfId :Int64);
  removeNetIf @19 (netIfId :Int64) -> ();
  listNetIfs  @20 () -> (netIfs :List(NetIfInfo));

  # Shared directories (virtiofs)
  addSharedDir    @21 (params :SharedDirAddParams) -> (sharedDirId :Int64);
  removeSharedDir @22 (sharedDirId :Int64) -> ();
  listSharedDirs  @23 () -> (sharedDirs :List(SharedDirInfo));

  # Snapshots — return Snapshot cap for further ops.
  snapshotCreate   @24 (name :Text) -> (snapshot :Disk.Snapshot);
  snapshotList     @25 () -> (snapshots :List(Disk.SnapshotInfo));
  snapshotGet      @26 (ref :Common.EntityRef) -> (snapshot :Disk.Snapshot);

  # SSH key attachment
  attachSshKey @27 (keyRef :Common.EntityRef) -> ();
  detachSshKey @28 (keyRef :Common.EntityRef) -> ();
  listSshKeys  @29 () -> (keys :List(SshKey.SshKeyInfo));

  # Migrate this (stopped) VM to a different node. The bytes of
  # every attached drive are streamed agent-to-agent; the daemon
  # orchestrates but does not relay. The VM must be stopped, must
  # have no shared dirs, and must use only `user`-type netifs (or
  # none at all). Returns a task id for long-running progress
  # observation.
  migrate @30 (params :VmMigrateParams) -> (taskId :Int64);

  # Save the VM's running state to disk and terminate the QEMU
  # process. The agent issues QMP `migrate file:<conventional-path>`,
  # waits for completion, then `quit`s QEMU. Allowed from `running`
  # or `paused`. Resume later with `start` — same verb as a cold
  # boot. Drop the saved state with `reset`.
  save @31 () -> (status :Enums.VmStatus);

  # One-shot fetch of the daemon's resource-stats ring buffer for
  # this VM (up to 60 samples, oldest first). Empty when the VM
  # has never been polled or is stopped. The WebUI uses this to
  # seed its sparkline panel on page load; the CLI does not call
  # it (the latest sample is already on `VmDetails.stats`).
  getStatsHistory @32 () -> (samples :List(VmStats));

  # Live stream of resource-stats samples — one per agent poll
  # cycle (~10 s). Drop the returned `handle` to unsubscribe.
  subscribeStats  @33 (sink :VmStatsSink) -> (handle :Streams.Handle);
}

# Parameters for `Vm.migrate`. The VM is identified by the
# capability the caller already holds; only the destination node
# is conveyed in the params struct.
struct VmMigrateParams {
  toNodeRef @0 :Common.EntityRef;
}

# Live-stats subscription sink. The daemon calls `onStats` once per
# 10-second poll cycle for the subscribed VM.
interface VmStatsSink {
  onStats @0 (stats :VmStats) -> ();
}
