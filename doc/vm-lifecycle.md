# VM Lifecycle and the Start/Reset Fence

This document describes the internal lifecycle of a VM: which component owns
each decision, how database states relate to the QEMU process, and how Corvus
prevents a delayed start from surviving a newer reset.

For operator-facing commands, see [VM Management](vm-management.md). For the
broader component boundaries, see [Architecture](architecture.md).

## Ownership

The daemon is the lifecycle authority. It:

- validates state transitions;
- serializes admission through conditional database updates;
- allocates lifecycle revisions and runtime generations;
- records the intended and observed VM state; and
- orchestrates nodeagent and netd calls.

The nodeagent owns host-local runtime resources. It starts and stops QEMU and
its helpers, keeps the live-process ledger, and reports runtime observations.
It does not choose the next database state or allocate ordering tokens.

The nodeagent nevertheless performs the final stale-command check. This check
has to be next to process creation: the daemon cannot atomically commit a
database transaction and spawn or kill a process on another host. A command
that was valid when the daemon sent it can arrive after a newer command.

There is consequently no distributed transaction spanning the database and
QEMU. The lifecycle protocol combines:

1. durable admission in the daemon database; and
2. process-local enforcement immediately before and after QEMU is spawned.

## Database states

`Vm.status` describes the daemon's lifecycle state. It is not by itself proof
that a process exists or does not exist at that instant.

| State | Meaning |
|---|---|
| `stopped` | No runtime is intended. The VM may be edited or freshly started. |
| `starting` | A guest-agent-enabled cold start is waiting for its first successful guest-agent check. |
| `running` | The runtime is considered available. A cold start without the guest agent enters this state directly. |
| `paused` | QEMU still exists, but its virtual CPUs are stopped with QMP `stop`. |
| `stopping` | A stop or reset has been admitted and termination is in progress or still needs confirmation. |
| `saving` | QEMU is writing a persistent state image. |
| `saved` | QEMU has exited and a state image can be loaded by a new QEMU process. |
| `loading` | A new QEMU process is loading a saved state image. |
| `migrating` | The migration orchestrator owns the operation. |
| `error` | A lifecycle operation or runtime failed and requires operator recovery, normally `reset`. |

The stable runtime path is:

```text
stopped --start--> starting --guest-agent ready--> running
   |                                                    |
   +--start without guest agent------------------------>+

running --pause--> paused --start/resume--> running
running/paused --save--> saving --success--> saved
saved --start--> loading --load complete--> running
running/starting --stop--> stopping --QEMU exited--> stopped
any state --reset--> stopping --fenced termination confirmed--> stopped
```

Save, load, and migration failures may enter `error` or follow the rollback
rules of their owning operation. `reset` is the universal recovery transition.

## Ordering fields

The VM row contains two lifecycle fields:

| Field | Purpose |
|---|---|
| `lifecycleRevision` | A daemon-owned, monotonically increasing order for admitted starts and resets. |
| `runtimeGeneration` | The identity of the QEMU incarnation admitted by a fresh start. It is included in runtime reports so observations from different incarnations cannot be confused. |

A fresh VM starts with revision `0` and no runtime generation. A fresh start
from `stopped` or `saved` conditionally updates the row as one admission step:

```text
lifecycleRevision = previous revision + 1
runtimeGeneration = lifecycleRevision
status = starting, loading, or running
```

Revision and generation currently receive the same numeric value on start,
but they represent different things. A reset advances the lifecycle revision
without creating a runtime, while the runtime generation continues to name the
runtime being terminated until the reset is confirmed.

Ordinary pause, resume, stop, and save do not allocate a new runtime and do not
advance these fields. An ordinary stop or successful save may leave the last
generation on the row; the next fresh start overwrites it. A completed reset
clears the generation.

The conditional database update is important. A start is admitted only while
the row still has the inactive status and revision that the handler read. A
reset is admitted only while its previously read revision is still current.
If another lifecycle operation wins first, the losing update affects no row
and its handler must not issue its stale command.

## Starting a fresh runtime

Starting from `stopped` and loading from `saved` create a new QEMU
incarnation. The flow is:

1. The daemon validates the transition and conditionally claims the VM row.
   The claim advances `lifecycleRevision`, assigns `runtimeGeneration`, and
   writes the initial status.
2. The daemon assembles a `VmSpec` containing both tokens and sends `vmStart`
   to the VM's nodeagent.
3. Before creating helpers or QEMU, the nodeagent calls `admitVmStart` in its
   process-wide VM ledger.
4. Admission records a start reservation only when the revision is newer than
   the nodeagent's high-water mark for that VM. An identical command for the
   currently live revision and generation is treated as idempotent.
5. The nodeagent starts virtiofsd and swtpm helpers as required, then QEMU.
6. Before publishing the new process in the live ledger, the nodeagent calls
   `publishVmStart`. Publication succeeds only if the same revision and
   generation are still reserved.
7. If a reset invalidated the reservation while the process was being
   created, publication fails and the nodeagent terminates the newly spawned
   QEMU and helpers instead of exposing an orphan runtime.
8. The daemon attaches a runtime monitor. Guest-agent and saved-state load
   completion reports move `starting` or `loading` to `running`.

Resuming from `paused` is different: it sends QMP `cont` to the existing QEMU
process, so it neither advances the lifecycle revision nor assigns a new
runtime generation.

## Ordinary stop versus reset

An ordinary `stop` is a cooperative lifecycle operation. The daemon writes
`stopping`, asks the nodeagent for an ACPI/guest shutdown, and escalates to an
unfenced hard stop if the timeout expires or the requested timeout is zero.
After the nodeagent confirms that QEMU has exited, the daemon writes
`stopped`.

`reset` is an emergency operation. It bypasses ordinary VM leases, cancels
all running and planned task trees that depend on the VM, and rejects new
mutating work while recovery is in progress. Read-only inspection remains
available. The reset task tree is exempt, so forced deletion can reset first
and acquire its deletion leases afterwards.

The reset flow is:

1. Before waiting for any ordinary resource, the daemon closes VM admission
   and requests cancellation of dependent root tasks, including their children
   and later build/apply phases. Cancellation does not release their leases or
   worker registrations: those remain until actual cleanup completes.
2. The daemon conditionally increments `lifecycleRevision` and writes
   `stopping`, retaining `runtimeGeneration` while the old runtime may exist.
3. It sends fenced `vmStopHard` to the source and every participating node,
   including migration destinations and hosts holding transfer capabilities.
   Each node advances its revision high-water mark, invalidates pending starts,
   and interrupts registered old RPC workers, including workers queued on locks.
4. The node terminates QEMU and helpers with bounded termination waits. It
   retires the old guest-agent connection without waiting for workers holding
   its lock. A counted retirement worker owns subsequent cleanup. The live
   ledger entry remains until cleanup finishes. Missing QEMU is also a valid
   termination result; the revision tombstone remains.
5. Reset returns after termination confirmation, reporting `stopped`. Worker
   drain continues separately; VM inspection can still show `stopping` and
   mutators remain blocked during this interval. A concurrent reset is rejected
   while the termination attempt is active; an explicit retry is allowed once
   that attempt ends, including after failure.
6. A watcher requires drain confirmation from the original node capabilities
   and all cancelled daemon task trees. It then removes saved-state files on
   participating hosts and releases managed TAPs, then conditionally commits
   `stopped`, clears runtime fields, and reopens mutation admission. Delayed results from
   another revision cannot complete this reset.

A failed termination, failed cleanup, disconnected agent, or missing drain
registry leaves recovery closed. The durable row stays `stopping`; after daemon
restart an explicit reset must establish drain again. Since participating-host
history is lost on restart, recovery retries conservatively contact all nodes.
Old scoped RPCs and transfer capabilities retain their original lifecycle
revision and runtime generation and cannot acquire fresh authorization.

`vm delete --force` performs the same emergency reset before acquiring VM or
disk leases. It waits up to 60 seconds for drain while retaining the recovery
gate through deletion. If drain cannot be proved, it reports that the VM was
stopped but not deleted and preserves the VM and disks. Resetting a bake VM
also cancels its build; build cleanup preserves the explicitly reset VM and
its artifacts, and cancelled builds cannot publish an image.

The `hasLifecycleFence` flag exists because Cap'n Proto scalar fields do not
express an optional integer directly in this RPC. `true` means that the
revision is a reset fence. `false` means ordinary hard stop and the numeric
revision field is ignored. Runtime generation is not part of `vmStopHard`:
reset orders lifecycle commands; it does not select a process by generation.

## The race that the fence closes

Without the nodeagent fence, this execution is possible:

1. The daemon admits start A, but its `vmStart` call is delayed before the
   process appears in the nodeagent ledger.
2. A reset sees no published process and returns `already stopped`.
3. The daemon records `stopped`.
4. Start A resumes and publishes QEMU, leaving a live VM behind a `stopped`
   database row.

With the fence, assume start A has revision and generation `41`:

```text
daemon database                 nodeagent ledger
-----------------------------   ------------------------------------
start A: revision=41, gen=41    vmStart(41, 41) delayed
reset:   revision=42            fence reset(42), retain tombstone 42
reset completes: stopped        no live process
                                delayed start(41, 41) is rejected
```

The fence also covers three narrower timings:

- If reset arrives after start reservation but before QEMU is spawned, it
  clears the reservation and the start cannot publish.
- If reset arrives after QEMU is spawned but before publication, publication
  fails and the start path tears the process and helpers down.
- If QEMU is already published, reset kills QEMU and retains its ledger entry
  until its workers and connections have drained.

The tombstone must live in the process-wide nodeagent ledger rather than an
RPC session. Reconnects create new sessions; putting the high-water mark in a
session would reopen the race across two concurrent or successive sessions.

## Rejecting stale runtime reports

The nodeagent includes both `lifecycleRevision` and `runtimeGeneration` in
status snapshots. Daemon-side updates driven by those snapshots require both
values to match the VM row before they can:

- record a guest-agent health check or guest IP addresses;
- promote `starting` or `loading` to `running`; or
- turn an early QEMU exit into `error`.

Thus a late report from an older QEMU incarnation cannot promote or damage a
newer runtime. The revision orders the lifecycle operation; the generation
identifies the runtime that produced the observation.

## Scope and limitations

The mechanism combines lifecycle fencing with task-tree recovery admission:

- It orders fresh starts and resets and rejects stale scoped VM work at the
  nodeagent boundary.
- It filters asynchronous runtime observations by revision and generation.
- Ordinary stop, pause, save, migration, disk, and build work participate in
  cancellation and recovery admission; this does not make whole workflows
  durable transactions.
- It does not persist a complete composite-operation journal in the VM row.

Durable recovery context for composite operations—operation kind, phase,
source/destination nodes, artifact ownership, and rollback data—belongs in a
separate operation record and design. Adding that context is not a reason to
weaken the start/reset fence: the final stale-start rejection still has to
remain beside process creation in the nodeagent.

## Implementation map

- Database fields: [`src/Corvus/Model.hs`](../src/Corvus/Model.hs)
- Start/reset admission and reset completion:
  [`src/Corvus/Handlers/Vm/Db.hs`](../src/Corvus/Handlers/Vm/Db.hs)
- Daemon lifecycle handlers:
  [`src/Corvus/Handlers/Vm.hs`](../src/Corvus/Handlers/Vm.hs)
- Nodeagent high-water marks and reservations:
  [`src/Corvus/Node/Ledger.hs`](../src/Corvus/Node/Ledger.hs)
- Nodeagent process start and fenced hard stop:
  [`src/Corvus/Node/Caps/Session/Vm.hs`](../src/Corvus/Node/Caps/Session/Vm.hs)
- Runtime-report filtering:
  [`src/Corvus/Handlers/VmStatusSink.hs`](../src/Corvus/Handlers/VmStatusSink.hs)
- RPC definitions: [`schema/nodeagent.capnp`](../schema/nodeagent.capnp)
