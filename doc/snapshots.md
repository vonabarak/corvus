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
- The VM must be **stopped** for rollback and merge operations.
- Create, list, and delete work regardless of VM state.

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
```

Reverts the disk to the exact state it was in when the snapshot was taken. All changes made since the snapshot are lost.

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
