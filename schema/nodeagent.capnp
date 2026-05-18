@0xbc08f7ad74ba3f78;

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
