@0xbf9b09f64c0dd40d;

# All Corvus enumerations. Mirrors the EnumText typeclass instances
# in Corvus.Model, but on the wire each value is a UInt16.
#
# IMPORTANT: Cap'n Proto enums cannot have values reordered or
# removed without breaking the wire format. Append-only.

enum VmStatus {
  stopped   @0;
  starting  @1;
  running   @2;
  stopping  @3;
  paused    @4;
  error     @5;
  saved     @6;
  saving    @7;
  loading   @8;
  migrating @9;
}

enum DriveInterface {
  virtio  @0;
  ide     @1;
  scsi    @2;
  sata    @3;
  nvme    @4;
  pflash  @5;
  floppy  @6;
}

enum DriveFormat {
  qcow2 @0;
  raw   @1;
  vmdk  @2;
  vdi   @3;
  vpc   @4;
  vhdx  @5;
}

enum DriveMedia {
  disk  @0;
  cdrom @1;
}

enum CacheType {
  none          @0;
  writeback     @1;
  writethrough  @2;
  directsync    @3;
  unsafe        @4;
}

enum NetInterfaceType {
  user     @0;
  tap      @1;
  bridge   @2;
  macvtap  @3;
  vde      @4;
  managed  @5;
}

enum SharedDirCache {
  always @0;
  auto   @1;
  never  @2;
}

enum TemplateCloneStrategy {
  clone   @0;
  overlay @1;
  direct  @2;
  create  @3;
}

enum TaskSubsystem {
  vm        @0;
  disk      @1;
  network   @2;
  sshKey    @3;
  template  @4;
  sharedDir @5;
  snapshot  @6;
  system    @7;
  apply     @8;
  build     @9;
  node      @10;
  migration @11;
}

enum TaskResult {
  running    @0;
  success    @1;
  error      @2;
  notStarted @3;
  cancelled  @4;
}

enum NodeAdminState {
  online      @0;
  draining    @1;
  maintenance @2;
}

# How aggressive to be about in-guest filesystem quiescence when
# taking a live snapshot. See doc/snapshots.md for the resolution
# table; the daemon picks the effective mode per call from the
# request's mode + the target VM's runtime state.
enum QuiesceMode {
  # Best-effort: freeze if the VM has guest agent enabled AND a
  # quick guest-ping succeeds; silently skip otherwise.
  auto    @0;
  # Hard requirement: fail the snapshot if the guest agent is not
  # reachable or guest-fsfreeze-freeze itself errors. For
  # automated callers that need a consistency guarantee.
  require @1;
  # Never freeze, even when the guest agent is available. Use for
  # snapshots of stopped/paused VMs and to dodge a flaky in-guest
  # fsfreeze.
  skip    @2;
}

# Stable machine-readable error codes the daemon prepends to
# Cap'n Proto exception messages (`<code> :: <message>`). The
# member names here are the single source of truth; the wire
# tokens are their snake_case form (e.g. vmNotFound ->
# "vm_not_found"). The Haskell Corvus.Wire.Error mirror and the
# Python client's code map must stay in sync with this enum.
enum ErrorCode {
  vmNotFound            @0;
  diskNotFound          @1;
  snapshotNotFound      @2;
  driveNotFound         @3;
  networkNotFound       @4;
  netifNotFound         @5;
  sshKeyNotFound        @6;
  sharedDirNotFound     @7;
  templateNotFound      @8;
  taskNotFound          @9;
  nodeNotFound          @10;
  diskInUse             @11;
  diskHasOverlays       @12;
  vmMustBeStopped       @13;
  vmNotRunning          @14;
  vmHeadless            @15;
  networkInUse          @16;
  networkAlreadyRunning @17;
  networkNotRunning     @18;
  sshKeyInUse           @19;
  nodeInUse             @20;
  invalidTransition     @21;
  formatNotSupported    @22;
  guestAgentNotEnabled  @23;
  guestAgentError       @24;
  ambiguousRef          @25;
  internalError         @26;
  protocolError         @27;
}
