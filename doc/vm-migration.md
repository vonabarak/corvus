# VM Migration

Corvus supports **offline** VM migration: stop a VM on one node, move
its disk images to another node, and start it there. Bytes never
pass through the daemon — the two `corvus-nodeagent` processes
involved talk directly to each other over a separate Cap'n Proto
session.

A standalone `crv disk copy` / `crv disk move` pair exposes the
same byte-transfer machinery for disk-only operations.

## Quick reference

```bash
# Move a single disk image to another node.
crv disk move <DISK> --to-node <NODE>

# Copy a disk image to another node (adds a placement, leaves
# source intact).
crv disk copy <DISK> --to-node <NODE>

# Migrate a stopped VM (with all its drives) to another node.
crv vm migrate <VM> --to-node <NODE>
```

All three are asynchronous and return a task id; subscribe via
`crv task watch <ID>` (or just `crv task show <ID>`) to follow
progress.

## Constraints

`crv vm migrate` refuses unless **all** of the following hold:

- The VM is in the `stopped` state and is not already mid-migration.
- The destination node exists, is `online`, and is not the VM's
  current node.
- The VM has no shared directories (host paths are node-local).
- The VM has zero network interfaces, or every interface is of type
  `user` (SLIRP). Bridge / tap / macvtap / managed interfaces are
  not yet supported — cross-node networks are out of scope.
- For every disk attached to the VM, the destination has enough
  free storage to hold the bytes being copied (with a 1 GiB safety
  margin).
- The destination has enough free RAM for the VM (with the
  scheduler's `ramSafetyMb` margin and any in-flight reservations).
- Every overlay's backing chain is either already on the
  destination or will be transparently included in the migration's
  copy plan; you don't need to pre-stage a backing image when
  migrating its consumer.

For manual `crv disk move` / `crv disk copy` the rules are tighter
than for VM migration:

| Drive state in source DB | `disk copy --to-node` | `disk move --to-node` |
|---|---|---|
| Not attached to any VM | allowed | allowed |
| Attached **read-only** | allowed | refused |
| Attached **read-write** | refused (use `vm migrate`) | refused (use `vm migrate`) |

Additionally: a move/copy of an overlay whose backing image is
*not* on the destination is refused, with the suggested fix
(`crv disk copy <BACKING> --to-node <NODE>`). VM migration handles
this implicitly; the manual commands stay strict so the operator
explicitly stages the chain.

## What happens during a migration

1. **Lock acquisition.** The orchestrator flips `Vm.migrating`
   from `False` to `True` in a single transaction; concurrent
   `crv vm migrate` calls or `crv vm start` calls observe the flag
   and refuse.
2. **Pre-flight.** The pre-check (see
   [`Corvus.Handlers.Vm.Migrate.PreCheck`](../src/Corvus/Handlers/Vm/Migrate/PreCheck.hs))
   builds the per-drive plan: read-only drives are marked as
   "copy", read-write drives as "move". Backing-chain ancestors
   that aren't already on the destination are added as additional
   copies.
3. **Sequential transfers.** For each drive in the plan:
   - The daemon asks the **source** agent to open the file
     (`session.diskOpenRead`); it receives a single-use
     `DiskReader` capability, a token, the byte size, and the
     md5.
   - The daemon asks the **destination** agent to import the
     file (`session.diskImportFromPeer`) with the source's
     host:port and the token.
   - The destination agent opens its own Cap'n Proto session to
     the source agent, claims the reader by calling
     `session.attachReader(token)`, and streams the bytes into
     a `.part` file via `reader.pipeInto(localSink)`. Daemon is
     not on this connection.
   - The destination verifies the received size and md5, renames
     `.part` into its final location.
4. **Commit.** In a single transaction:
   - `Vm.nodeId` is updated.
   - `Vm.vsockCid` and `Vm.spicePort` are cleared — the
     destination's per-node allocators will hand out fresh values
     on the next `vm start`.
   - Source-side `DiskImageNode` rows for every moved drive are
     deleted.
   - `Vm.migrating` is cleared.
5. **Cleanup.** Source agents are asked, best-effort, to delete
   the moved files. A failure here is logged but doesn't undo
   the migration — the DB already reflects the new placement.

## Failure handling

If any transfer fails mid-flight, the orchestrator:

- Asks the destination agent to delete every file it had finished
  receiving during this attempt.
- Drops the matching `DiskImageNode` rows on the destination.
- Clears the `Vm.migrating` flag.

The source side is untouched until the very last step, so a
failed migration leaves the source's disks (and any pre-existing
destination placements for r/o copies that succeeded before the
failure — actually, no: the rollback clears those too) intact and
ready to run again.

## Authentication

The agent-to-agent connection rides the same mTLS infrastructure
the daemon uses ([doc/security.md](security.md)). The destination
agent presents its own `corvus-node:<name>` certificate; the
source agent's inbound allowlist accepts both `corvus-daemon:*`
and `corvus-node:*` peer prefixes, with the destination-supplied
token providing per-transfer authorization on top of that.

## Limitations (current scope)

- **Offline only.** No live state migration; the VM is stopped on
  the source and started on the destination.
- **No managed networks across nodes.** Per the existing networking
  constraints (see [doc/multi-node.md](multi-node.md)).
- **No VM-wide snapshots yet.** A pre-migration safety snapshot is
  a planned follow-up; for now the orchestrator does not snapshot
  before moving.
- **No bandwidth limiting, parallel transfers, or resumable
  retries.** Sequential, restart-on-failure.

## See also

- [doc/disk-management.md](disk-management.md) — disk CRUD,
  attach/detach, overlays.
- [doc/multi-node.md](multi-node.md) — cluster topology, the
  scheduler, per-node allocators.
- [doc/security.md](security.md) — mTLS, CN convention, cert
  layout.
