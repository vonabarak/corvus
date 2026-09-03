@0xec449e11027b2949;

using Common = import "common.capnp";
using Enums = import "enums.capnp";
using Streams = import "streams.capnp";

# ---------------------------------------------------------------------
# Info structs
# ---------------------------------------------------------------------

struct DiskImageInfo {
  id              @0 :Int64;
  name            @1 :Text;
  # Per-node placements. Multi-node deployments have one entry
  # per node the image has been replicated to; single-node
  # deployments have exactly one entry.
  placements      @2 :List(DiskImagePlacement);
  format          @3 :Enums.DriveFormat;
  sizeMb          @4 :Int64;            # 0 == unknown
  createdAt       @5 :Int64;            # POSIX nanoseconds
  attachedTo      @6 :List(DiskAttachment);
  # Backing image for qcow2 overlays. `id == 0` => no backing.
  backingImage    @7 :Common.NamedRef;
  # Ephemeral disks are auto-deleted together with the VM they are
  # attached to. Cloud-init ISOs and disks created during template
  # instantiation (clone/overlay/create strategies) default to
  # ephemeral; everything else defaults to non-ephemeral.
  ephemeral       @8 :Bool;
}

struct DiskImagePlacement {
  node     @0 :Common.NamedRef;
  filePath @1 :Text;
}

struct DiskAttachment {
  vm @0 :Common.NamedRef;
}

struct SnapshotInfo {
  id         @0 :Int64;
  name       @1 :Text;
  createdAt  @2 :Int64;   # POSIX nanoseconds
  sizeMb     @3 :Int64;   # 0 == unknown
  # Whether this snapshot was taken against a running VM via QMP
  # (`true`) versus offline via `qemu-img snapshot -c` (`false`).
  # Pure operator diagnostic; the qcow2 entry is bit-identical
  # either way.
  live       @4 :Bool;
  # Whether QGA `guest-fsfreeze-freeze` was active at the moment
  # the snapshot was stamped. `true` implies the operator can trust
  # filesystem-level consistency; `false` is hard-reset-equivalent
  # for in-guest writes.
  quiesced   @5 :Bool;
  # Whether this snapshot row carries QEMU vmstate (RAM + device
  # model + CPU state). Only the single "carrier" disk of a
  # full-machine snapshot is `true`; sibling rows that share the
  # same name carry block snapshots alone. Rollback of a carrier
  # routes through QMP `snapshot-load` (which resumes the VM in
  # the saved running state) rather than offline `qemu-img
  # snapshot -a`.
  hasVmstate @6 :Bool;
}

# ---------------------------------------------------------------------
# Parameter structs
# ---------------------------------------------------------------------

struct DiskCreateParams {
  # `name` and `sizeMb` are mandatory; `format` defaults to qcow2
  # to match `crv disk create`. `node` is the target placement
  # (unset/byId 0 → defer to the scheduler).
  name      @0 :Text;
  sizeMb    @1 :Int64;
  format    @2 :Enums.DriveFormat = qcow2;
  ephemeral @3 :Bool = false;
  node      @4 :Common.EntityRef;
  # Optional destination path. Empty means the default
  # `<basePath>/<name>.<format>` location. A non-empty path follows the
  # same relative/absolute and trailing-`/` rules as the CLI's `--path`.
  path      @5 :Text;
}

struct DiskRegisterParams {
  # `filePath` is a path on `node`, not on the API client. When
  # `formatProvided` is false, the daemon inspects the file on that
  # node and falls back to the filename extension.
  name      @0 :Text;
  filePath  @1 :Text;
  format    @2 :Enums.DriveFormat = qcow2;
  ephemeral @3 :Bool = false;
  node      @4 :Common.EntityRef;
  formatProvided  @5 :Bool = false;
  backingDiskRef  @6 :Common.EntityRef;
  backingProvided @7 :Bool = false;
}

struct DiskCreateOverlayParams {
  name           @0 :Text;
  backingDiskRef @1 :Common.EntityRef;
  ephemeral      @2 :Bool = false;
  path           @3 :Text;
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
  ephemeral @3 :Bool = false;
}

struct DiskRebaseParams {
  diskRef           @0 :Common.EntityRef;
  newBackingDiskRef @1 :Common.EntityRef;
  # False means flatten the disk. True selects the new backing ref.
  newBackingProvided @2 :Bool = false;
  unsafe             @3 :Bool = false;
}

struct DiskImportParams {
  # `srcPath` is either an HTTP(S) URL or a path visible on `node`.
  # API-client-local files must use DiskUpload instead.
  name      @0 :Text;
  srcPath   @1 :Text;
  format    @2 :Enums.DriveFormat = qcow2;
  ephemeral @3 :Bool = false;
  node      @4 :Common.EntityRef;
  destPath       @5 :Text;
  formatProvided @6 :Bool = false;
}

# Begins an upload of a file that exists on the API client's machine.
# The returned DiskUpload capability is intentionally streamed through the
# daemon to the selected node; nodeagent is never exposed to clients.
struct DiskUploadParams {
  name      @0 :Text;
  format    @1 :Enums.DriveFormat;
  path      @2 :Text; # empty -> <basePath>/<name>.<format>
  ephemeral @3 :Bool = false;
  node      @4 :Common.EntityRef;
  overwrite @5 :Bool = false;
}

# Copy a logical disk image from its current placement on one node
# to another. Source rows are left intact; the destination ends up
# with an additional `DiskImageNode` placement. Refused when the
# disk is attached read-write to a VM, when an overlay's backing
# chain isn't already present on the target, or when the target
# path collides with an existing placement.
#
# `toPath` is the optional destination path on the new node.
# Empty string means: preserve the source's relative path if it
# is relative; refuse the operation if the source path is
# absolute (operators must pick a destination explicitly because
# the same absolute path on a different node is rarely writable
# and almost never the intent). Non-empty value follows the same
# rules as `--path` on `disk create`: relative is anchored at the
# destination node's `basePath`, absolute is honoured verbatim,
# trailing `/` means "directory; pick the source's basename".
# `withBackingChain`, when true, asks the daemon to recursively
# stage every backing ancestor of the source disk that's missing
# on the destination before transferring the disk itself. Default
# (false) keeps the historical refuse-if-chain-missing behaviour.
# Mirrors what `vm.migrate` does automatically for the disks of a
# VM that has overlays in its backing chain.
struct DiskCopyParams {
  diskRef          @0 :Common.EntityRef;
  toNodeRef        @1 :Common.EntityRef;
  toPath           @2 :Text;
  withBackingChain @3 :Bool;
}

# Move a logical disk image from one node to another. The source's
# `DiskImageNode` row is deleted and the file unlinked on success.
# Refused for any disk that is currently attached to a VM (use
# `vm.migrate` for that path) — for non-attached disks both copy
# and move are allowed. `toPath` and `withBackingChain` semantics
# match `DiskCopyParams`.
struct DiskMoveParams {
  diskRef          @0 :Common.EntityRef;
  toNodeRef        @1 :Common.EntityRef;
  toPath           @2 :Text;
  withBackingChain @3 :Bool;
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
  import        @7 (params :DiskImportParams) -> (taskId :Int64);
  # Flatten an overlay: consolidates its delta with all backing
  # images into a single standalone qcow2 with no backing image.
  # The disk record stays at the same id; only its `backingImage*`
  # fields go to null. VM must be stopped.
  flatten       @8 (diskRef :Common.EntityRef) -> ();

  # Copy a disk image's bytes to a new node, adding a placement
  # row on the destination while leaving the source intact. Bytes
  # flow agent → agent; the daemon orchestrates but does not relay.
  # Returns a task id for long-running progress observation.
  copy          @9 (params :DiskCopyParams) -> (taskId :Int64);

  # Move a disk image's bytes to a new node, then delete the
  # source placement + file. Same data path as `copy`. Returns a
  # task id for long-running progress observation.
  move          @10 (params :DiskMoveParams) -> (taskId :Int64);
  beginUpload   @11 (params :DiskUploadParams) -> (upload :DiskUpload);
}

# A one-shot client-upload session. finish() publishes the completed image and
# returns its normal Disk resource. abort() discards the node-side partial file.
interface DiskUpload {
  write  @0 (chunk :Data) -> ();
  finish @1 () -> (disk :Disk);
  abort  @2 () -> ();
}

interface Disk {
  show     @0 () -> (info :DiskImageInfo);
  delete   @1 () -> ();
  refresh  @2 () -> (info :DiskImageInfo);
  resize   @3 (newSizeMb :Int64) -> ();

  # Snapshots scoped to this disk image.
  #
  # `snapshotCreate` dispatches transparently between the offline
  # (`qemu-img snapshot -c`) and live (QMP) paths based on whether
  # the disk is currently attached to a running/paused VM. The
  # `quiesce` mode only matters for the live path; it controls
  # whether QGA `guest-fsfreeze` brackets the snapshot for
  # in-guest filesystem consistency.
  #
  # `fullMachine = true` upgrades to a vmstate-aware snapshot:
  # this disk becomes the "carrier" whose qcow2 holds the QEMU
  # vmstate (RAM + device model + CPU state), and every other
  # writable qcow2 disk attached to the same running VM also gets
  # a sibling block snapshot under the same name. Requires the
  # VM to be running and QEMU >= 6.0; refuses with a clear error
  # otherwise. `quiesce` is ignored for full-machine snapshots —
  # vmstate captures the in-flight page cache and writeback queue,
  # so guest fsfreeze is unnecessary (and a multi-second freeze
  # under a vmstate save would be harmful).
  snapshotCreate    @4 (name        :Text,
                        quiesce     :Enums.QuiesceMode = auto,
                        fullMachine :Bool = false)
                       -> (snapshot :Snapshot, snapshotId :Int64);
  snapshotList      @5 () -> (snapshots :List(SnapshotInfo));
  snapshotGet       @6 (ref :Common.EntityRef) -> (snapshot :Snapshot);
}

interface Snapshot {
  show     @0 () -> (info :SnapshotInfo);
  delete   @1 () -> ();
  # QEMU has no online rollback. `autoStop = true` lets the daemon
  # wrap the offline rollback in a graceful VM stop + revert +
  # restart cycle, so the operator still issues a single call. With
  # `autoStop = false` (the default) the daemon refuses the call
  # when any attached VM is running.
  rollback @2 (autoStop :Bool = false) -> ();
  merge    @3 () -> ();
}
