@0xbc08f7ad74ba3f78;

using Enums = import "enums.capnp";
using Streams = import "streams.capnp";
using Vm = import "vm.capnp";

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
# Authentication: mTLS gates the listener. The agent binds to
# 0.0.0.0:9878 by default; override the bind address via the
# binary's --host flag.

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

  # Resolved `$HOME/VMs` of the user the agent is running as, used
  # as the default `basePath` for `crv node add` when the operator
  # did not pass `--base-path` explicitly. Each agent answers for
  # itself, so a heterogeneous cluster (alpha runs as alice, beta
  # runs as bob) naturally lands `/home/alice/VMs` vs
  # `/home/bob/VMs` without the operator having to know either
  # user's home.
  defaultBasePath @3 () -> (path :Text);
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
  diskClone          @6 (sourcePath :Text, destPath :Text, destFormat :Text)
                        -> (result :DiskOpResult);
  # `destFormat` selects the on-disk format of the clone (e.g. "qcow2",
  # "raw"). The agent uses `qemu-img convert -O <destFormat>` so a
  # qcow2-source-to-raw-destination clone behaves correctly and the
  # destination file's bytes match the format the daemon's DiskImage
  # row will record.
  diskInspect        @7 (path :Text) -> (info :DiskInspectInfo);

  # Snapshot operations (qcow2 only; format errors surface via DiskOpResult).
  #
  # The plain variants stop and lock the qcow2 file, so they
  # require the VM to be off. The `Live` companions reach the
  # running QEMU via QMP `blockdev-snapshot-internal-sync` /
  # `blockdev-snapshot-delete-internal-sync`. Rollback has no
  # live form (QEMU exposes none); the daemon's `autoStop` knob
  # wraps the offline rollback in a stop + revert + start cycle.
  snapshotCreate     @8  (path :Text, name :Text) -> (result :DiskOpResult);
  snapshotDelete     @9  (path :Text, name :Text) -> (result :DiskOpResult);
  snapshotRollback   @10 (path :Text, name :Text) -> (result :DiskOpResult);

  # Live snapshot create against a running VM. The agent uses
  # `path` to locate the matching block device via `query-block`
  # (the QEMU block-backend name isn't predictable from the daemon
  # side, so we resolve it agent-side from the file path).
  # `quiesced` reports whether QGA fsfreeze actually ran — `auto`
  # mode can silently downgrade to skip when the guest agent is
  # unavailable.
  snapshotCreateLive @37 (path :Text,
                          name :Text,
                          vmId :Int64,
                          quiesce :Enums.QuiesceMode = auto)
                        -> (result :DiskOpResult, quiesced :Bool);

  # Live snapshot delete. No QGA freeze needed — deletion only
  # touches the qcow2 snapshot table, not in-flight guest I/O.
  snapshotDeleteLive @38 (path :Text, name :Text, vmId :Int64)
                        -> (result :DiskOpResult);

  # Atomic multi-disk live snapshot. Wraps N
  # `blockdev-snapshot-internal-sync` actions in a single QMP
  # `transaction`, so either every disk gets the named snapshot or
  # none of them do. `quiesce` controls QGA fsfreeze the same way
  # `snapshotCreateLive` does; the freeze (if any) covers the whole
  # transaction. Used by the build-step cache to snapshot the bake
  # VM's artifact + system disks atomically per step.
  snapshotCreateLiveMany @39 (paths :List(Text),
                              name :Text,
                              vmId :Int64,
                              quiesce :Enums.QuiesceMode = auto)
                            -> (result :DiskOpResult, quiesced :Bool);

  # Full-machine snapshot create. The agent issues a single QMP
  # `snapshot-save` async job that writes QEMU vmstate (RAM +
  # device model + CPU state) into the carrier disk's qcow2 and
  # an internal block snapshot into every disk in `devicePaths`,
  # atomically under one `name`. The carrier must appear in
  # `devicePaths`. Requires QEMU >= 6.0 — the agent capability-
  # probes via `query-commands` and refuses with a clear error
  # otherwise. No QGA fsfreeze: vmstate captures the in-flight
  # page cache and writeback queue, so freezing would be both
  # unnecessary and harmful under a multi-second save.
  snapshotCreateWithVmstate @40 (vmstateDevicePath :Text,
                                 devicePaths       :List(Text),
                                 name              :Text,
                                 vmId              :Int64)
                               -> (result :DiskOpResult);

  # Full-machine snapshot load. The agent issues a single QMP
  # `snapshot-load` async job that restores the carrier's vmstate
  # AND rolls back every disk in `devicePaths` to the named
  # snapshot, atomically. The caller MUST ensure the VM's CPUs
  # are paused (QMP `stop`) before issuing; the agent does NOT
  # `cont` the VM afterwards — the caller does that after any
  # post-load setup (clock resync, QGA handshake).
  snapshotLoadWithVmstate   @41 (vmstateDevicePath :Text,
                                 devicePaths       :List(Text),
                                 name              :Text,
                                 vmId              :Int64)
                               -> (result :DiskOpResult);

  # Full-machine snapshot delete. The agent issues a single QMP
  # `snapshot-delete` async job that removes the vmstate AND the
  # block snapshots from every disk in `devicePaths`. Necessary
  # for vmstate-aware snapshots because the disk-only
  # `blockdev-snapshot-delete-internal-sync` leaves vmstate
  # orphaned. No `vmstateDevicePath` parameter: snapshot-delete's
  # QMP signature only takes `tag` + `devices`.
  snapshotDeleteWithVmstate @42 (devicePaths :List(Text),
                                 name        :Text,
                                 vmId        :Int64)
                               -> (result :DiskOpResult);

  # Tell QGA to resync the guest's wall clock from the host's
  # RTC (`guest-set-time` with no arguments). Used after a
  # vmstate restore — the restored guest thinks it's still
  # snapshot-time, which breaks anything time-sensitive
  # (cert validation, build mtime comparisons, NTP). Best-effort:
  # `DiskOpResult.error` is non-empty when QGA isn't reachable.
  guestSetTime              @43 (vmId :Int64)
                               -> (result :DiskOpResult);

  # Image download (curl, fall back to wget) + xz decompression +
  # md5 hashing. These shell out to host tools and write to the
  # supplied destination path.
  #
  # `sink`, when non-null, receives byte-counted progress updates
  # while the transfer runs (probe-once via HEAD for `total` +
  # 250 ms filesystem polling of `destPath` on the agent). The
  # daemon exports a thin translator and forwards each event to
  # its caller's `ApplyEventSink`.
  diskDownload       @11 (destPath :Text,
                          url      :Text,
                          sink     :Streams.DiskDownloadSink)
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

  # QMP `migrate "file:<basePath>/<vmName>/state.qemu"` + poll
  # `query-migrate` for completion + QMP `quit`. The agent then
  # cleans up the ledger entry the way `vmStopHard` does. Throws
  # on migration failure (leaving the partial file in place) so
  # the daemon keeps the VM in `saved` state for the operator to
  # retry or reset. No path on the wire — the agent derives it
  # from the vmName recorded in its ledger.
  vmSave @35 (vmId :Int64) -> ();

  # Unlink the saved-state file for `vmName`. Idempotent — a
  # missing file is success. Called by the daemon from
  # `handleVmReset` (drop the save explicitly) and from
  # `handleVmDelete` (clean up when removing a saved VM).
  deleteSavedState @36 (vmName :Text) -> ();

  # Execute a command via QGA on the running VM. Agent locates
  # the QGA socket from the ledger entry for req.vmId.
  vmGuestExec @20 (req :VmGuestExecReq) -> (info :VmGuestExecInfo);

  # Streaming variant of vmGuestExec. The agent pushes incremental
  # stdout/stderr bytes to the supplied sinks as QGA's
  # guest-exec-status drains them, then returns the final exit
  # code in VmGuestExecInfo (with stdout/stderr empty — bytes
  # already flowed through the sinks). Both sinks are end()-ed
  # on normal completion AND on error, so the caller can flush
  # any partial line.
  vmGuestExecStream @34
    (req :VmGuestExecReq,
     stdoutSink :Streams.ByteSink,
     stderrSink :Streams.ByteSink)
    -> (info :VmGuestExecInfo);

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

  # -------------------------------------------------------------------
  # QMP-mediated runtime VM changes. The agent runs as root and
  # owns the QMP socket; the daemon (unprivileged) can't connect
  # directly, so these operations get their own RPCs. Each throws
  # on QMP failure; the daemon translates that back into a
  # response on its side.
  # -------------------------------------------------------------------

  # Disk hot-plug. QEMU `blockdev-add` + `device_add` for an
  # already-attached drive row. driveId is the daemon's primary
  # key for the drive; the agent uses it to derive the QEMU
  # @node-name and @id (drive-<N> / device-<N>).
  vmAttachDrive @28 (req :VmAttachDriveReq) -> ();

  # Disk hot-unplug. QEMU `device_del` + `blockdev-del` with the
  # daemon's retry-on-busy loop folded in.
  vmDetachDrive @29 (vmId :Int64, driveId :Int64) -> ();

  # Probe whether a given AF_VSOCK CID is currently free on the
  # agent's host kernel (VHOST_VSOCK_SET_GUEST_CID ioctl against
  # /dev/vhost-vsock). The daemon owns the per-node DB filter
  # for already-recorded CIDs; this RPC just answers the kernel
  # uniqueness question, which is what stops two daemons sharing
  # a host from independently picking the same CID. Returns
  # `false` only when the kernel reports the CID is in use; a
  # missing /dev/vhost-vsock surfaces as `true` (the agent's
  # host has no vhost-vsock support, so there's no conflict to
  # report).
  probeVsockCid @30 (cid :Int64) -> (free :Bool);

  # -------------------------------------------------------------------
  # Inter-agent disk transfer (Phase: migration).
  #
  # The data path stays off the daemon: the destination agent dials
  # the source agent's nodeagent port itself, calls `attachReader`
  # over its own session, then drives the byte stream over that
  # destination↔source connection. The daemon only orchestrates the
  # handshake (open / start the import / drop on completion).
  #
  # Flow:
  #
  #   1. Daemon → source.diskOpenRead(path, ttlSec) →
  #        (reader, token, sizeBytes, md5).
  #   2. Daemon → destination.diskImportFromPeer(
  #        destPath, peerHost, peerPort, token,
  #        expectedBytes, expectedMd5).
  #      The destination agent opens a fresh NodeAgent session
  #      to `peerHost:peerPort`, calls `attachReader(token)` to
  #      pick up the (same) DiskReader cap on its own session,
  #      and then `reader.pipeInto(localSink)` where localSink
  #      writes to destPath.<rand>.part on disk.
  #   3. On clean EOF, destination fsync()s, renames to destPath,
  #      verifies its computed md5 against `expectedMd5`, throws
  #      if they disagree.
  #   4. Daemon drops the `reader` capability returned in (1);
  #      the source closes its open file handle.
  #
  # The token is a 128-bit random hex string. It does not have to
  # be secret on the wire (mTLS protects the connection), but it
  # must be unguessable so a misbehaving peer with a valid cert
  # cannot race the legitimate destination to claim the reader.
  # -------------------------------------------------------------------

  # Source side. Open `path` for reading and prepare a single-use
  # reader handle. Returns:
  #
  #   * `reader`     — the DiskReader cap on this session. The
  #                    daemon holds this for the lifetime of the
  #                    transfer; dropping it before
  #                    `diskImportFromPeer` returns is the abort
  #                    signal.
  #   * `token`      — opaque single-use ticket the destination
  #                    agent presents to `attachReader` on a
  #                    different session to obtain the same
  #                    reader.
  #   * `sizeBytes`  — file size at open time (the destination
  #                    cross-checks how many bytes arrived).
  #   * `md5`        — md5 hex hash of the file at open time
  #                    (the destination cross-checks the received
  #                    bytes against this).
  diskOpenRead @31 (path :Text, ttlSec :UInt32)
    -> (reader :DiskReader, token :Text, sizeBytes :Int64, md5 :Text);

  # Source side. Re-resolve a token issued by `diskOpenRead` to the
  # corresponding `DiskReader` cap on the current session — typically
  # called by the destination agent over a fresh connection to the
  # source. Single-use; the token is consumed on success.
  attachReader @32 (token :Text) -> (reader :DiskReader);

  # Destination side. Dial `peerHost:peerPort`, open a fresh
  # NodeAgent session, claim the reader via `attachReader(token)`,
  # then stream the bytes through a local ByteSink that writes to
  # `destPath`. Verifies the size + md5 against the daemon-supplied
  # expectations and throws on any mismatch. Cleans up any partial
  # file on error.
  diskImportFromPeer @33
    (destPath :Text,
     peerHost :Text, peerPort :Int32,
     token :Text,
     expectedBytes :Int64,
     expectedMd5 :Text)
    -> ();
}

# Handle the source agent hands out for one in-flight read. Held
# first by the daemon (the cap returned by `diskOpenRead`), then
# re-resolved by the destination agent over its own session via
# `attachReader(token)`. The destination side pumps bytes through
# `pipeInto`; the daemon's only direct interaction is dropping the
# cap to abort.
interface DiskReader {
  # Push bytes into `sink` until EOF, then call sink.end().
  # Throws on read error or cancellation.
  pipeInto @0 (sink :Streams.ByteSink) -> ();

  # Explicit cancel. Equivalent to dropping the cap; provided so
  # callers can fail fast without waiting for GC.
  cancel @1 () -> ();
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
  # When true, the agent runs QEMU with `-no-reboot` so it
  # exits on guest-initiated reboot, then re-spawns QEMU
  # transparently. Daemon-initiated stop / reset still wins —
  # the agent suppresses the auto-restart for them. Used to
  # dodge OVMF firmware reboot hangs (tianocore/edk2#12441).
  rebootQuirk @14 :Bool;
  # Address QEMU binds the SPICE TCP listener to. The daemon
  # is the source of truth: its `--spice-bind` flag (defaulting
  # to `--host` in --tcp mode, else 127.0.0.1) is forwarded
  # here so the agent's QEMU spawn matches what the daemon
  # reports back to clients in vmViewGrant. Ignored when
  # spicePort is 0 (headless VMs have no SPICE listener).
  spiceBindAddr @15 :Text;
  # When true, the agent spawns QEMU with
  # `-incoming "file:<basePath>/<name>/state.qemu"` to restore
  # a previously-saved RAM image. The agent waits for the
  # incoming migration to complete, issues QMP `cont`, and
  # unlinks the state file. False (the default) means a normal
  # cold boot. Path is conventional — no Text field on the wire.
  loadFromSavedState @16 :Bool;
  # QEMU `-cpu` model. Default `"host"` exposes the host CPU
  # (best perf, NOT migration-safe across non-identical hosts);
  # daemons that care about cross-host migration set this to a
  # stable model (`qemu64`, `Westmere-v3`, …). Empty string
  # falls back to `"host"` for forward-wire compatibility with
  # older daemons that don't yet send this field.
  cpuModel @17 :Text;
  # When true, the agent spawns QEMU with `-S` so the CPUs stay
  # paused after spawn. The caller is responsible for issuing
  # QMP `cont` later (typically after a QMP `snapshot-load` to
  # restore vmstate). The agent's vmStart returns immediately
  # after QMP comes up — it does NOT wait for the guest agent
  # in this mode (QGA can't respond while CPUs are paused).
  # Mutually exclusive with `loadFromSavedState` above, which
  # has its own pause/resume dance via `-incoming`.
  startPaused @18 :Bool;
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

# Disk hot-plug request. Mirrors the args the daemon-side
# 'Corvus.Node.Qmp.qmpBlockdevAdd' + 'qmpDeviceAddDrive' used
# to take.
struct VmAttachDriveReq {
  vmId      @0 :Int64;
  driveId   @1 :Int64;
  filePath  @2 :Text;
  format    @3 :Text;       # "qcow2" / "raw" / …
  ifKind    @4 :Text;       # "virtio" / "ide" / "scsi" / …
  readOnly  @5 :Bool;
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
  # Per-node observation, populated on every tick. Phase 5: the
  # daemon stamps these into the `Node` row's stat columns and
  # bumps `nodeAgentHealthcheck = snapshotAtMillis`. Optional
  # fields use 0 / empty as "unknown" sentinels.
  nodeStats        @2 :NodeStats;
}

# Per-tick observation of the node the agent runs on. Populated
# from /proc/meminfo, /proc/loadavg, statvfs(basePath), uname -r,
# and the agent's baked-in version.
struct NodeStats {
  cpuCount          @0 :Int32;           # 0 == unknown
  ramMbTotal        @1 :Int32;
  ramMbFree         @2 :Int32;
  storageBytesTotal @3 :Int64;
  storageBytesFree  @4 :Int64;
  loadAvg1          @5 :Float64;
  loadAvg5          @6 :Float64;
  loadAvg15         @7 :Float64;
  kernelRelease     @8 :Text;            # "" == unknown
  agentVersion      @9 :Text;
}

struct VmStatusEntry {
  vmId           @0 :Int64;
  state          @1 :VmAgentState;       # same enum vmStatus uses
  qemuPid        @2 :Int32;              # 0 if not running
  lastExitCode   @3 :Int32;              # populated when state != running
  guestAgentOk   @4 :Bool;               # last QGA ping in this tick succeeded
  lastPingMillis @5 :Int64;              # epoch ms; 0 if guestAgentOk is false
  netIfs         @6 :List(GuestNetIf);   # populated when guestAgentOk
  # Resource-consumption sample (cumulative counters + instant
  # gauges). Zero-filled when the VM is not running.
  stats          @7 :Vm.VmStats;
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
