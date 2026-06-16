# Snapshots

Internal qcow2 snapshots capture the disk state at a point in time. They are stored inside the qcow2 file itself — no extra files are created.

## Commands

```bash
crv snapshot create <disk> <name>       # Create a snapshot
crv snapshot list <disk>                # List snapshots for a disk
crv snapshot rollback <disk> <snap>     # Rollback to a snapshot
crv snapshot merge <disk> <snap>        # Merge snapshot into base
crv snapshot delete <disk> <snap>       # Delete a snapshot
```

`<disk>` and `<snap>` accept names or numeric IDs.

## Requirements

- Only **qcow2** disk images support snapshots.
- Create, delete, merge, and list work whether the attached VM is
  running or stopped — the daemon routes transparently between the
  offline (`qemu-img snapshot`) and live (QMP) backends.
- **Rollback** still requires the VM to be stopped. QEMU has no
  online snapshot-rollback command; `qemu-img snapshot -a` would
  race with QEMU's in-flight I/O. Pass `--auto-stop` to have the
  daemon orchestrate a graceful stop + rollback + start cycle —
  the VM still cycles, but the operator issues a single command.

## Live (online) snapshots

When an attached VM is running or paused, Corvus uses QMP
`blockdev-snapshot-internal-sync` (create) and
`blockdev-snapshot-delete-internal-sync` (delete / merge) instead of
shelling out to `qemu-img`. The on-disk qcow2 record is bit-identical
to an offline snapshot, so `crv snapshot list` shows live and
offline entries in the same table; only the `LIVE` column
distinguishes them.

### Filesystem quiescence (`--quiesce`)

`blockdev-snapshot-internal-sync` flushes QEMU's writeback cache
before stamping the snapshot, but in-guest page cache that hasn't
hit `sync(2)` is opaque to QEMU. Without quiescing the guest, a
live snapshot is hard-reset-equivalent for any unflushed writes —
fine for crash-tolerant workloads, unacceptable for "snapshot
during an `apt upgrade`."

The `--quiesce` flag controls whether the snapshot is bracketed
with QGA `guest-fsfreeze-freeze` / `guest-fsfreeze-thaw`. Three
modes:

| Mode | Behaviour |
|---|---|
| `auto` (default) | Freeze if the VM has guest agent enabled AND a quick `guest-ping` succeeds. Silently skip otherwise. |
| `require` | Same probe; fail the snapshot if the agent is missing / not reachable / `guest-fsfreeze-freeze` itself errors. |
| `skip` | Never freeze, even when the guest agent is available. |

Examples:

```bash
crv snapshot create db pre-migration                    # auto: best-effort quiesce
crv snapshot create db pre-migration --quiesce require  # error out if no QGA
crv snapshot create db raw-snap --quiesce skip          # explicit unquiesced
```

The `Q` column in `crv snapshot list` shows whether the snapshot
was actually frozen (`+`) or not (`-`).

**Safety:** the daemon ALWAYS attempts the thaw, even when the
snapshot itself errors. If both the snapshot and the thaw fail,
the daemon's error reply includes a "guest may be wedged" hint —
the operator should investigate via `crv vm guest-exec` or the
serial console.

### Multi-disk caveat

`blockdev-snapshot-internal-sync` snapshots only the named block
device. A VM with multiple attached disks (e.g. a bake VM with
artifact + floppy + OVMF vars) snapshotting a single disk captures
ONLY that disk's state at the snapshot moment; the others are
unaffected. For atomic multi-disk snapshots, stop the VM first
and let the offline path handle each disk individually.

QGA fsfreeze, in contrast, is guest-wide: when it succeeds the
freeze flushes ALL writable filesystems regardless of which disk
they're on, so even a single-disk snapshot on a multi-disk VM
sees a consistent guest-side view at the snapshot moment.

## Full-machine snapshots (`--with-ram`)

Disk snapshots capture only block state. RAM, device model, CPU
state, mounted filesystems, loaded modules, running daemons, the
contents of `/tmp` and `/run` — all of that is lost the next time
the VM boots from a rolled-back disk. A `--with-ram` snapshot
captures the whole machine atomically: the qcow2 active state
**and** QEMU vmstate (RAM + device + CPU) via the QMP
`snapshot-save` async job (QEMU 6.0+).

```bash
# Take a full-machine snapshot of a running VM. The carrier disk
# (the one you name) holds the vmstate; every other writable qcow2
# attached to the same VM gets a sibling block snapshot under the
# same tag.
crv snapshot create my-vm-disk pre-experiment --with-ram

# crv snapshot list shows the new `V` column on the carrier row:
#   ID | NAME             | CREATED | SIZE_MB | LIVE | Q | V
#   -- | pre-experiment   | …       | …       | +    | - | +
crv snapshot list my-vm-disk

# Rollback resumes the VM in the saved running state — no separate
# VmStart needed. The daemon issues QMP `stop` to pause CPUs,
# `snapshot-load` to restore vmstate + every disk atomically,
# `cont` to unfreeze, then QGA `guest-set-time` to resync the
# wall clock from the host RTC.
crv snapshot rollback my-vm-disk pre-experiment
```

The on-disk record adds two columns to `crv snapshot list`:

| Column | Meaning |
|---|---|
| `L` (live) | `+` if the snapshot was stamped via QMP on a running VM (always `+` for vmstate snapshots), `-` for offline `qemu-img snapshot -c`. |
| `Q` (quiesced) | `+` if QGA `guest-fsfreeze` was active. Never set for vmstate snapshots — vmstate captures the in-flight page cache directly, so freezing would be unnecessary (and, during a multi-second `snapshot-save`, actively harmful). |
| `V` (vmstate) | `+` only on the **carrier** disk of a `--with-ram` snapshot. Sibling rows that share the same tag carry just the block snapshot; the carrier's qcow2 is where the RAM lives. |

**No QGA fsfreeze for vmstate snapshots.** They're inherently
guest-consistent because the saved RAM includes the in-flight
page cache and writeback queue; freezing the guest during a
multi-second `snapshot-save` would block real workloads with no
benefit.

**Storage cost.** Each vmstate snapshot adds roughly the VM's RAM
size to the carrier qcow2 (e.g. ≈8 GB for an 8 GB-RAM VM). Disk
snapshots are still copy-on-write deltas of the writable disks.
`crv snapshot list` reports `size_mb` per row so you can see
where the bytes went.

**Rollback semantics differ from disk-only.** A vmstate rollback
always leaves the VM running — `snapshot-load` IS itself a
resume. (Disk-only rollback preserves the prior VM state:
running → running, stopped → stopped.)

**Cleanup.** Deleting a vmstate-aware snapshot routes through the
QMP `snapshot-delete` async job, which removes the vmstate AND
every sibling block snapshot atomically. The block-only delete
path would orphan the vmstate; the daemon picks the right path
automatically based on the snapshot row's `has_vmstate` flag.

## VM-scoped snapshots (`crv vm snapshot`)

The disk-scoped `crv snapshot create --with-ram` flow above
requires the operator to pick a carrier disk by hand. For the
common "checkpoint this whole VM" workflow, `crv vm snapshot`
auto-picks the carrier (the writable qcow2 drive attached first to
the VM — typically the system disk) and exposes the snapshot under
the VM's namespace instead of any single disk's.

```bash
# Capture a running VM: every writable qcow2 disk + RAM/CPU/device
# state, atomically, under one name.
crv vm snapshot create my-vm pre-upgrade

# Inspect all VM-scoped snapshots for this VM. The carrier column
# shows which disk holds the vmstate.
crv vm snapshot list my-vm
#   NAME         CREATED               CARRIER     DISKS  SIZE_MB
#   pre-upgrade  2026-06-16 11:02:14   my-vm-boot  2      2148

# Roll back. Works regardless of current VM state:
#   - running/paused → QMP stop → snapshot-load → cont
#   - stopped       → launch paused (-S) → snapshot-load → cont
# Either way the VM ends up running at the captured state.
crv vm snapshot rollback my-vm pre-upgrade

# Delete a VM-scoped snapshot (VM must be running — same as the
# disk-scoped vmstate delete).
crv vm snapshot delete my-vm pre-upgrade
```

**When to use which?**

- `crv vm snapshot ...` — default for "checkpoint this VM before
  doing something risky". One identifier (the VM name + snapshot
  name); rollback works on stopped VMs too.
- `crv snapshot create DISK NAME --with-ram` — when you specifically
  want to pick the carrier disk (e.g. because you've reorganised
  drives and the lowest-id drive isn't the right vmstate home), or
  when you're using the per-disk `crv snapshot list/delete/rollback`
  surface for other reasons.

Both flows write the same `snapshot` rows under the hood. A VM
snapshot named `pre-upgrade` and a disk-scoped `--with-ram`
snapshot named `pre-upgrade` on the carrier disk are
indistinguishable in the database; the difference is purely UX.

**Carrier auto-selection.** The carrier is picked as the writable
qcow2 drive with the lowest `Drive.id` (insertion order). This is
stable across create / rollback / delete on a given VM — the
drive order doesn't change unless the operator explicitly
detaches and re-attaches disks. There is no override flag; if you
need to pin the carrier to a specific disk, use the disk-scoped
`--with-ram` form.

**Uniqueness.** A snapshot name must not already exist on any of
the VM's writable disks. The validation runs before the agent is
contacted, so a duplicate `vm snapshot create` is a clean no-op
(no orphaned sibling snapshots on disks that haven't tripped the
unique constraint yet).

## Creating Snapshots

```bash
crv snapshot create boot before-upgrade
crv snapshot create boot clean-install
```

Each snapshot records the full disk state. Multiple snapshots can exist on the same disk.

## Listing Snapshots

```bash
crv snapshot list boot
```

Shows snapshot ID, name, creation timestamp, and size (overhead relative to base).

## Rolling Back

```bash
crv snapshot rollback boot before-upgrade
crv snapshot rollback boot before-upgrade --auto-stop
```

Reverts the disk to the exact state it was in when the snapshot
was taken. All changes made since the snapshot are lost.

Without `--auto-stop` the command refuses the rollback if any
attached VM is running or paused — `qemu-img snapshot -a` would
collide with the live QEMU's exclusive file lock. With
`--auto-stop` the daemon performs the cycle (graceful VmStop →
rollback → VmStart) on the operator's behalf. The asymmetry
between create / delete (transparent) and rollback (always
cycles a running VM) reflects a QEMU limitation: there is no
online equivalent of `qemu-img snapshot -a`.

## Merging

```bash
crv snapshot merge boot before-upgrade
```

Merges the snapshot into the base image and removes the snapshot. This reclaims the space used by the snapshot's delta. After merging, the snapshot no longer exists but its data is preserved in the base.

## Deleting

```bash
crv snapshot delete boot before-upgrade
```

Removes the snapshot without merging. The snapshot's delta data is discarded.

## Snapshots in Templates

Template drives with `clone` or `overlay` strategy create new disks at instantiation time. Snapshots on the original disk are:
- **Copied** with the `clone` strategy (full copy includes snapshots).
- **Not copied** with the `overlay` strategy (only the overlay layer is new).
