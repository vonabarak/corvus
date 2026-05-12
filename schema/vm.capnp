@0xa7366eabdb0b1db4;

using Corvus = import "corvus.capnp";
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
  guestIpAddresses  @6 :Text;   # empty == none
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
  name            @0  :Text;
  cpuCount        @1  :Int32;
  ramMb           @2  :Int32;
  description     @3  :Text;     # empty == none
  headless        @4  :Bool;
  guestAgent      @5  :Bool;
  cloudInit       @6  :Bool;
  autostart       @7  :Bool;
  drives          @8  :List(DriveAttachParams);
  netIfs          @9  :List(NetIfAddParams);
  sshKeys         @10 :List(Corvus.EntityRef);
  cloudInitConfig @11 :CloudInit.CloudInitInfo;
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
}

struct DriveAttachParams {
  diskRef    @0 :Corvus.EntityRef;
  interface  @1 :Enums.DriveInterface;
  media      @2 :Enums.DriveMedia;
  readOnly   @3 :Bool;
  cacheType  @4 :Enums.CacheType;
  discard    @5 :Bool;
}

struct NetIfAddParams {
  type         @0 :Enums.NetInterfaceType;
  hostDevice   @1 :Text;   # empty == auto
  macAddress   @2 :Text;   # empty == generate
  networkRef   @3 :Corvus.EntityRef;  # for `managed` type; else id=0 or name=""
}

struct SharedDirAddParams {
  path      @0 :Text;
  tag       @1 :Text;
  cache     @2 :Enums.SharedDirCache;
  readOnly  @3 :Bool;
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
  get    @1 (ref :Corvus.EntityRef) -> (vm :Vm);
  create @2 (params :VmCreateParams) -> (vm :Vm);
}

interface Vm {
  show           @0  () -> (details :VmDetails);
  start          @1  (wait :Bool) -> (status :Enums.VmStatus);
  stop           @2  (wait :Bool) -> (status :Enums.VmStatus);
  pause          @3  () -> (status :Enums.VmStatus);
  reset          @4  () -> (status :Enums.VmStatus);
  edit           @5  (params :VmEditParams) -> ();
  delete         @6  (deleteDisks :Bool) -> ();
  cloudInit      @7  () -> (config :CloudInit.CloudInitInfo);
  viewGrant      @8  () -> (grant :Corvus.ViewGrant);
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
  snapshotGet      @26 (ref :Corvus.EntityRef) -> (snapshot :Disk.Snapshot);

  # SSH key attachment
  attachSshKey @27 (keyRef :Corvus.EntityRef) -> ();
  detachSshKey @28 (keyRef :Corvus.EntityRef) -> ();
  listSshKeys  @29 () -> (keys :List(SshKey.SshKeyInfo));
}
