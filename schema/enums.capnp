@0xbf9b09f64c0dd40d;

# All Corvus enumerations. Mirrors the EnumText typeclass instances
# in Corvus.Model, but on the wire each value is a UInt16.
#
# IMPORTANT: Cap'n Proto enums cannot have values reordered or
# removed without breaking the wire format. Append-only.

enum VmStatus {
  stopped  @0;
  starting @1;
  running  @2;
  stopping @3;
  paused   @4;
  error    @5;
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
