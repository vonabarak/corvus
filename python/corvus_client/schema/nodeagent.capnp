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

  # @vmId@ is used solely to compute the runtime directory layout
  # ($XDG_RUNTIME_DIR/corvus/<vmId>/). The agent mkdir-p's it
  # before spawning. Returns the PID of the spawned process.
  processSpawnQemu @15
    (vmId :Int64, binary :Text, args :List(Text))
    -> (pid :Int32);

  # Spawn a virtiofsd subprocess. The agent waits up to
  # @waitForSocketTimeoutMs@ for the named UNIX socket file to
  # appear; if it doesn't, the spawn is treated as failed and
  # the agent kills the partial child.
  processSpawnVirtiofsd @16
    (binary :Text,
     args :List(Text),
     socketPath :Text,
     waitForSocketTimeoutMs :UInt32)
    -> (result :VirtiofsdSpawnResult);

  # SIGTERM-then-SIGKILL a previously-spawned child. The agent
  # remembers the @ProcessHandle@ keyed by PID and waitpid()s
  # to reap. Idempotent: a PID the agent doesn't know about
  # (or that's already exited) returns @notRunning@.
  processStop @17
    (pid :Int32, gracefulTimeoutSec :UInt32)
    -> (result :ProcessStopResult);

  # Cheap liveness probe (read /proc/<pid>). Used by the daemon's
  # VM monitor loop to learn when QEMU has exited.
  processIsAlive @18 (pid :Int32) -> (alive :Bool);
}

# ProcessStopResult mirrors Corvus.Process.StopResult.
struct ProcessStopResult {
  kind    @0 :ProcessStopKind;
  message @1 :Text;
}

enum ProcessStopKind {
  stoppedGracefully @0;  # caller's optional pre-stop hook handled it
  stoppedByTerm     @1;  # exited within graceful window after SIGTERM
  stoppedByKill     @2;  # required SIGKILL escalation
  notRunning        @3;  # PID wasn't alive when called
  stopFailed        @4;  # signalling raised an OS error (text in message)
}

# VirtiofsdSpawnResult covers the two non-success outcomes
# specific to virtiofsd spawning.
struct VirtiofsdSpawnResult {
  kind    @0 :VirtiofsdSpawnKind;
  pid     @1 :Int32;     # 0 when kind != success
  message @2 :Text;      # populated on failure
}

enum VirtiofsdSpawnKind {
  success            @0;
  spawnFailed        @1;   # createProcess raised
  socketNeverAppeared @2;  # process started but didn't open its socket in time
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
