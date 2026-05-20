@0xec449e11027b2949;

using Common = import "common.capnp";
using Enums = import "enums.capnp";

# ---------------------------------------------------------------------
# Info structs
# ---------------------------------------------------------------------

struct DiskImageInfo {
  id              @0 :Int64;
  name            @1 :Text;
  # Per-node placements. Multi-node deployments have one entry
  # per node the image has been replicated to; single-node
  # deployments have exactly one entry. Phase-3 replacement for
  # the legacy single 'filePath' field.
  placements      @2 :List(DiskImagePlacement);
  format          @3 :Enums.DriveFormat;
  sizeMb          @4 :Int64;            # 0 == unknown
  createdAt       @5 :Int64;            # POSIX nanoseconds
  attachedTo      @6 :List(DiskAttachment);
  backingImageId  @7 :Int64;            # 0 == none
  backingImageName @8 :Text;            # empty == none
}

struct DiskImagePlacement {
  nodeId   @0 :Int64;
  nodeName @1 :Text;
  filePath @2 :Text;
}

struct DiskAttachment {
  vmId   @0 :Int64;
  vmName @1 :Text;
}

struct SnapshotInfo {
  id         @0 :Int64;
  name       @1 :Text;
  createdAt  @2 :Int64;   # POSIX nanoseconds
  sizeMb     @3 :Int64;   # 0 == unknown
}

# ---------------------------------------------------------------------
# Parameter structs
# ---------------------------------------------------------------------

struct DiskCreateParams {
  # `name` and `sizeMb` are mandatory; `format` defaults to qcow2
  # to match `crv disk create`.
  name    @0 :Text;
  sizeMb  @1 :Int64;
  format  @2 :Enums.DriveFormat = qcow2;
}

struct DiskRegisterParams {
  # `format` is left as the schema default (qcow2). Pass an explicit
  # value when registering raw / vmdk / ... files; the daemon does
  # NOT auto-detect at the wire level (the CLI's auto-detection is
  # a client-side convenience).
  name     @0 :Text;
  filePath @1 :Text;
  format   @2 :Enums.DriveFormat = qcow2;
}

struct DiskCreateOverlayParams {
  name           @0 :Text;
  backingDiskRef @1 :Common.EntityRef;
}

struct DiskCloneParams {
  sourceRef @0 :Common.EntityRef;
  newName   @1 :Text;
  # Optional destination path. Empty string → daemon picks the
  # default location (`<basePath>/<newName>.<ext>`). Non-empty →
  # written verbatim through `qemu-img convert` to this path
  # (relative paths resolve against the daemon's basePath; absolute
  # paths are honoured as-is).
  path      @2 :Text;
}

struct DiskRebaseParams {
  diskRef           @0 :Common.EntityRef;
  newBackingDiskRef @1 :Common.EntityRef;
}

struct DiskImportUrlParams {
  name   @0 :Text;
  url    @1 :Text;
  format @2 :Enums.DriveFormat = qcow2;
  sizeMb @3 :Int64;                         # 0 == no resize after import
}

struct DiskImportParams {
  name    @0 :Text;
  srcPath @1 :Text;
  format  @2 :Enums.DriveFormat = qcow2;
}

# ---------------------------------------------------------------------
# Manager + resource capabilities
# ---------------------------------------------------------------------

interface DiskManager {
  list          @0 () -> (disks :List(DiskImageInfo));
  get           @1 (ref :Common.EntityRef) -> (disk :Disk);
  create        @2 (params :DiskCreateParams) -> (disk :Disk);
  register      @3 (params :DiskRegisterParams) -> (disk :Disk);
  createOverlay @4 (params :DiskCreateOverlayParams) -> (disk :Disk);
  clone         @5 (params :DiskCloneParams) -> (disk :Disk);
  rebase        @6 (params :DiskRebaseParams) -> ();
  importUrl     @7 (params :DiskImportUrlParams) -> (taskId :Int64);
  import        @8 (params :DiskImportParams) -> (disk :Disk);
  # Flatten an overlay: consolidates its delta with all backing
  # images into a single standalone qcow2 with no backing image.
  # The disk record stays at the same id; only its `backingImage*`
  # fields go to null. VM must be stopped.
  flatten       @9 (diskRef :Common.EntityRef) -> ();
}

interface Disk {
  show     @0 () -> (info :DiskImageInfo);
  delete   @1 () -> ();
  refresh  @2 () -> (info :DiskImageInfo);
  resize   @3 (newSizeMb :Int64) -> ();

  # Snapshots scoped to this disk image.
  snapshotCreate    @4 (name :Text) -> (snapshot :Snapshot);
  snapshotList      @5 () -> (snapshots :List(SnapshotInfo));
  snapshotGet       @6 (ref :Common.EntityRef) -> (snapshot :Snapshot);
}

interface Snapshot {
  show     @0 () -> (info :SnapshotInfo);
  delete   @1 () -> ();
  rollback @2 () -> ();
  merge    @3 () -> ();
}
