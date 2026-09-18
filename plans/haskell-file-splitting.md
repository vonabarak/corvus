# Haskell Source File Splitting Plan

## Analysis

Total Haskell files: 207 in `src/`, 4 in `app/`.
Total source lines: ~56,353.

### Current largest refactoring candidates (post-split)

This ranking was refreshed after the completed Session, RPC, Build, VM, and
NodeAgentClient splits. It includes files of at least 580 lines; line count is
an indicator, while the final column records the likely separable concern.

| # | File | Lines | Likely refactoring boundary |
|---|---|---:|---|
| 1 | `src/Corvus/Handlers/Vm.hs` | 1627 | Remaining VM lifecycle actions and shared orchestration helpers |
| 2 | `src/Corvus/Node/Caps/Session/Vm.hs` | 1601 | Nodeagent VM-capability handlers, grouped by VM operation family |
| 3 | `src/Corvus/Node/Qmp.hs` | 1292 | QMP command families (P3: lifecycle, migration, hotplug, snapshots, stats) |
| 4 | `src/Corvus/Handlers/Disk.hs` | 1227 | Disk CRUD, placement/copy, and attach/detach workflows |
| 5 | `src/Corvus/Node/GuestAgent.hs` | 1222 | Connection/session management versus guest-agent commands (P3) |
| 6 | `src/Corvus/Handlers/Apply.hs` | 1044 | Apply/patch phases and resource-specific reconciliation |
| 7 | `src/Corvus/Node/Caps/Session.hs` | 904 | Session-cap infrastructure and dispatch; split only if cohesive boundaries emerge |
| 8 | `src/Corvus/Model.hs` | 874 | Persistent entities by table/domain (existing P2 proposal) |
| 9 | `src/Corvus/Handlers/Disk/Snapshot.hs` | 866 | Snapshot lifecycle versus restore/delete/metadata operations |
| 10 | `src/Corvus/Handlers/Network.hs` | 816 | Network CRUD, peer/topology, and host-side orchestration |
| 11 | `src/Corvus/Client/Commands/Vm.hs` | 764 | CLI commands by VM operation family |
| 12 | `src/Corvus/Node/Image.hs` | 734 | Image inspection, conversion, transfer, and metadata helpers |
| 13 | `src/Corvus/Handlers/Template.hs` | 686 | Template CRUD, versioning, and associated resource operations |
| 14 | `src/Corvus/Client/Capnp/Rpc/Vm.hs` | 682 | RPC wrappers by VM operation family, if further granularity pays off |
| 15 | `src/Corvus/Rpc/Vm.hs` | 658 | Daemon-side VM RPC handlers by operation family |
| 16 | `src/Corvus/Tls.hs` | 655 | Certificate authority, issuance, and transport configuration |
| 17 | `src/Corvus/Client/Commands/Disk.hs` | 614 | CLI commands by disk operation family |
| 18 | `src/Corvus/Action.hs` | 598 | Action runtime, task tracking, and execution helpers; preserve its shared core |
| 19 | `src/Corvus/Handlers/Vm/Migrate.hs` | 582 | Migration preparation, transfer, and cleanup phases |

---

## Split Plan

### Tier 1: Immediate splits (P0)

#### 1. `Node/Caps/Session.hs` (2742 → 4 files, **COMPLETE**)

**Status: Complete.** All handler implementations moved to Session/Vm.hs.

```
src/Corvus/Node/Caps/Session.hs      →   903 lines (SessionCap type, server instance, infrastructure)
src/Corvus/Node/Caps/Session/Utils.hs →   489 lines (decoders, encoders, infrastructure helpers)
src/Corvus/Node/Caps/Session/Disk.hs →   186 lines (disk operations)
src/Corvus/Node/Caps/Session/Vm.hs   →  1603 lines (handler implementations)
Total: 3181 lines (vs original 2742 — +439 from VM decoders)
```

**Build:** ✅ passes | **Format:** ✅ passes | **Lint:** ✅ passes | **Tests:** ✅ 794 examples, 0 failures

#### 2. `Client/Capnp/Rpc.hs` (2081 → 6 files)

```
src/Corvus/Client/Capnp/Rpc.hs        →  re-export + shared types (CapnpConnection)
src/Corvus/Client/Capnp/Rpc/Vm.hs     →  rpcVmCreate, rpcVmStart, rpcVmStop, rpcVmPause, rpcVmReset, rpcVmSave, rpcVmDelete, rpcVmMigrate, rpcVmEdit, rpcVmCloudInit, rpcVmViewGrant, rpcVmSendCtrlAltDel
src/Corvus/Client/Capnp/Rpc/Disk.hs   →  rpcDiskCreate, rpcDiskDelete, rpcDiskResize, rpcDiskClone, rpcDiskRebase, rpcDiskRefresh, rpcDiskCopy, rpcDiskMove, rpcDiskList, rpcDiskShow, rpcDiskInspect, rpcDiskDownload
src/Corvus/Client/Capnp/Rpc/Network.hs →  rpcNetworkList, rpcNetworkShow, rpcNetworkCreate, rpcNetworkDelete
src/Corvus/Client/Capnp/Rpc/Node.hs   →  rpcNodeList, rpcNodeShow, rpcNodeCreate, rpcNodeDelete
src/Corvus/Client/Capnp/Rpc/Task.hs   →  rpcTaskList, rpcTaskShow, rpcTaskCancel, rpcTaskListChildren, rpcTaskSubscribe, TaskProgressEvent
src/Corvus/Client/Capnp/Rpc/Stream.hs →  rpcVmSerialConsole, rpcVmHmpMonitor, rpcVmSubscribeGuestAgent, GuestAgentStatusEvent, callSink, runByteSinkRelay
```

Grouped by entity/resource domain. Each file ~300–400 lines.

### Tier 2: Moderate urgency (P1)

#### 3. `Handlers/Build.hs` (2197 → sub-modules, **COMPLETE**)

```
src/Corvus/Handlers/Build.hs          →  BuildAction, BuildOptions, pipeline orchestration glue
src/Corvus/Handlers/Build/Qga.hs      →  QGA helpers
src/Corvus/Handlers/Build/Provisioner.hs →  provisioner execution
src/Corvus/Handlers/Build/Artifact.hs →  artifact publication
src/Corvus/Handlers/Build/CacheResume.hs → cache resume
src/Corvus/Handlers/Build/CleanupBakeVm.hs → bake-VM cleanup
src/Corvus/Handlers/Build/Installer.hs → installer steps
src/Corvus/Handlers/Build/Run.hs      → run pipeline
src/Corvus/Handlers/Build/Template.hs → template steps
```

The bulk is `runBuildPipeline` — per-step helpers extracted into sub-modules.

#### 4. `Handlers/Vm.hs` (2129 → 5 files, **COMPLETE**)

```
src/Corvus/Handlers/Vm.hs             →  VmCreate, VmDelete, VmStart, VmStop, VmEdit, VmPause, VmReset, VmSave + core helpers (getVmDetails) + per-node autostart (autostartVmsOnNode, autostartClientName)
src/Corvus/Handlers/Vm/Db.hs          →  shared DB helpers (getVmWithStatus, getVmStatusOnly, setVmStatus, setVmStarted, setVmStopped, setVmError, hasNetdMediatedNetIf)
src/Corvus/Handlers/Vm/Console.hs     →  serial console, HMP monitor, Ctrl+Alt+Del, SPICE viewer grants (handleSerialConsole, handleHmpMonitor, handleVmSendCtrlAltDel, handleVmViewGrant, generateSpicePassword, viewableStatuses, isViewable)
src/Corvus/Handlers/Vm/CloudInit.hs   →  handleVmCloudInit, ensureCloudInitIso, hasCloudInitIso
src/Corvus/Handlers/Vm/Monitor.hs     →  background monitor threads + daemon (re)attach (attachVmMonitor, reattachVmMonitors, reapplyVm, releaseManagedTaps, pollVmUntilExit, ExitOutcome)
```

Console + interactive features, cloud-init ISO generation, and the
background monitor/supervisor concern are each distinct from VM lifecycle.
The shared DB helpers live in `Vm/Db.hs` so submodules can import them
without a cycle through the umbrella; `autostartVmsOnNode` stays in the
umbrella because it issues `VmStart` actions defined there. The parent
re-exports everything, so the public API is unchanged.

### Tier 3: Lower priority (P2)

#### 5. `Model.hs` (875 → 5–6 files)

```
src/Corvus/Model.hs                   →  migrateAll + re-exports
src/Corvus/Model/Node.hs              →  Node, NodeId, NodeAdminState
src/Corvus/Model/Vm.hs                →  Vm, VmId, VmStatus, Drive, NetworkInterface, Snapshot, DriveFormat, DriveMedia, CacheType
src/Corvus/Model/Disk.hs              →  DiskImage, DiskImageNode, DiskImageId
src/Corvus/Model/Network.hs           →  Network, NetworkPeer, NetworkId, NetworkPeerId, NetInterfaceType
src/Corvus/Model/Template.hs          →  Template, TemplateVersion, TemplateField, SshKey, VmSshKey
```

Persistent entity definitions naturally split by table.

#### 6. `NodeAgentClient.hs` (1544 → 6 files, **COMPLETE**)

```
src/Corvus/NodeAgentClient.hs           →  42-line API-preserving umbrella and re-exports
src/Corvus/NodeAgentClient/Core.hs      →  268 lines: connection handle, lifecycle, liveness, shared RPC helpers
src/Corvus/NodeAgentClient/Disk.hs      →  438 lines: image operations, download/hash/decompress, inter-agent transfer
src/Corvus/NodeAgentClient/Snapshot.hs  →  274 lines: offline, live, and vmstate snapshots; QuiesceMode
src/Corvus/NodeAgentClient/CloudInit.hs →   49 lines: cloudInitGenerateIso
src/Corvus/NodeAgentClient/Vm.hs        →  535 lines: lifecycle, guest exec, consoles, hotplug, and vsock RPCs
```

The umbrella retains the original public surface. `Core` owns the shared
connection internals so domain modules can import them without a cycle.
The additional `Vm` module keeps the parent small and isolates the largest
remaining domain; the resulting 1,606 lines include module headers/imports.

**Build:** ✅ passes

### Tier 4: Cleanup (P3)

#### 7. `Node/Qmp.hs` (1293)

Split by command family: `Node/Qmp/Lifecycle.hs`, `Node/Qmp/Migration.hs`, `Node/Qmp/Spice.hs`, `Node/Qmp/Hotplug.hs`, `Node/Qmp/Snapshot.hs`, `Node/Qmp/Stats.hs`.

#### 8. `Node/GuestAgent.hs` (1223)

Split: connection management (`Node/GuestAgent/Conn.hs`) vs commands (`Node/GuestAgent/Commands.hs`).

---

## Implementation approach

- **One split per PR** — each file split is an independent commit/PR.
- Use **module re-exports** from the parent to keep the public API unchanged.
- Run `make format && make lint && make test` after each split.
- No behavioral changes — pure structural refactoring.

## Priority summary

| Priority | File | Target size | Status |
|---|---|---|---|
| **P0** | `Node/Caps/Session.hs` | 4 × ~900 | ✅ Complete |
| **P0** | `Client/Capnp/Rpc.hs` | 6 × ~350 | ✅ Complete (6 domain modules) |
| **P1** | `Handlers/Build.hs` | 2–3 | ✅ Complete (10 sub-modules) |
| **P1** | `Handlers/Vm.hs` | 5 | ✅ Complete (Db, Console, CloudInit, Monitor) |
| **P2** | `NodeAgentClient.hs` | 6 | ✅ Complete (Core, Disk, Snapshot, CloudInit, Vm) |
| **P2** | `Model.hs` | 5–6 | Not started |
| **P3** | `Node/Qmp.hs` | 4–5 | Not started |
| **P3** | `Node/GuestAgent.hs` | 2 | Not started |

## P0-1: Session.hs split details

### Files created

| File | Lines | Content |
|---|---|---|
| `src/Corvus/Node/Caps/Session/Utils.hs` | 489 | Decoders (decodeVmSpec, etc.), encoders (encodeVmRuntimeInfo, etc.), infrastructure (agentQemuConfig, withVmOpLock, waitForFirstQgaPing, forwardPipeToLog, captureStderrTail, pollForExit, requireRemovableDrive, retryBlockdevDel, serialBufferCapacity, monitorBufferCapacity, parseFormat, decodeQuiesceMode, flushBufferForVm) |
| `src/Corvus/Node/Caps/Session/Disk.hs` | 186 | Disk operations (sessionDiskCreate, sessionDiskCreateOverlay, sessionDiskDelete, sessionDiskResize, sessionDiskRebase, sessionDiskClone, sessionDiskInspect) + disk encoders |
| `src/Corvus/Node/Caps/Session/Vm.hs` | 1603 | Handler implementations (handleVmStart, doVmStart, respawnAfterExit, spawnVirtiofsdHelper, spawnSwtpmHelper, handleVmStopGraceful, handleVmStopHard, handleVmPause, handleVmResume, handleVmSave, handleVmGuestExec, handleVmGuestExecStream, handleVmStatus, handleVmSetSpiceTicket, handleDeleteSavedState, handleDeleteTpmState, decodeVmSpec, decodeVmDriveSpec, decodeVmNetIfSpec, decodeVmSharedDirSpec, decodeVmGuestExecReq, encodeVmRuntimeInfo, encodeVmStopResult, encodeVmAgentStatus, encodeVmGuestExecInfo) |

### Session.hs current state (903 lines)

Kept:
- SessionCap data type + newSessionCap (in Utils.hs)
- withVmOpLock + vmOpLockFor (in Utils.hs)
- SomeServer instance
- Session'server_ instance (calls imported handlers from Vm.hs)
- importFromPeer, openChardev, flushBufferForVm (in Utils.hs)

Removed to Utils:
- parseFormat, encodeDiskOpResult, decodeQuiesceMode, encodeDiskInspectInfo, encodeDiskSnapshotInfo
- serialBufferCapacity, monitorBufferCapacity
- retryBlockdevDel, isBlockdevBusy, requireRemovableDrive
- agentQemuConfig, vfsBinary
- decodeVmSpec, decodeVmDriveSpec, decodeVmNetIfSpec, decodeVmSharedDirSpec, decodeVmGuestExecReq
- encodeVmRuntimeInfo, encodeVmStopResult, encodeVmAgentStatus, encodeVmGuestExecInfo
- waitForFirstQgaPing, forwardPipeToLog, captureStderrTail, stderrTailCapacity
- pollForExit, tshow
