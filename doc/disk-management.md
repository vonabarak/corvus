# Disk Management

## Commands

```bash
crv disk create <name> --size <MB> [--format <fmt>] [--path <path>] [--ephemeral]
crv disk register <name> <path> [--format <fmt>] [--backing <disk>] [--ephemeral]
crv disk import <name> <source> [--path <dest>] [--format <fmt>] [--ephemeral] [--wait]
crv disk overlay <name> <base_disk> [--path <path>] [--ephemeral]
crv disk clone <name> <base_disk> [--path <path>] [--ephemeral]
crv disk rebase <disk> [--backing <new_backing>] [--unsafe]
crv disk resize <disk> --size <MB>
crv disk refresh <disk>
crv disk list
crv disk show <disk>
crv disk delete <disk>
crv disk attach <vm> <disk> [--interface <iface>] [--media <media>] [--read-only] [--discard] [--cache <cache>]
crv disk detach <vm> <drive>
crv disk copy <disk> --to-node <node>
crv disk move <disk> --to-node <node>
```

`<disk>`, `<vm>`, `<drive>`, and `<node>` accept names or numeric IDs.

## Per-node placement

Disk images are per-node: each row in the `disk_image` table is a
logical name, and each on-disk file lives in the
`disk_image_node` join keyed by `(disk_image_id, node_id,
file_path)`. The same logical image may have placements on
multiple nodes; an operator replicates an image by rsync-ing
the file and running `crv disk register --node <new-node>`
against the resulting path.

The daemon enforces a **same-node attach check**: `crv disk
attach <vm> <disk>` refuses unless a `disk_image_node` row
exists for `(disk, vm.node)`. Otherwise qemu on the VM's host
would have no file to open. The error names both sides:

```
Disk image 'debian-base' is not present on node 3
where VM 'web-1' lives
```

`crv disk show <disk>` renders each placement on its own line
as `<node>: <path>; …`.

On a single-node install all disk operations default to the
single registered node (the scheduler's first-online-node
fallback). Multi-node operators are responsible for explicit
`--node` placement during create / register / import.

## Creating Disk Images

```bash
crv disk create boot --size 20480 --format qcow2
crv disk create data --size 102400 -f raw
crv disk create scratch --size 4096 --path project/
crv disk create test-overlay --size 8192 --ephemeral
```

## Ephemeral disks

Mark a disk **ephemeral** to have it deleted automatically when the VM
it is attached to is deleted (`crv vm delete`). Useful for per-VM
artifacts that have no value outside the VM's lifetime.

* Cloud-init ISOs (`<vm>-cloud-init`) are always ephemeral — the
  daemon sets the flag when it generates them.
* Disks materialised by `crv template instantiate` via the
  `create`, `clone`, or `overlay` strategy are ephemeral by default;
  the template YAML can override per-drive with `ephemeral: false`.
* Disks created from the apply schema, the CLI, or the Python client
  default to **non-ephemeral**. Add `--ephemeral` / `ephemeral: true`
  to opt in.

`crv vm delete --keep-disks` overrides the auto-reap and keeps every
attached disk, ephemeral or not. The ephemeral flag itself is visible
in `crv disk show <name>` (the `Ephemeral` field) and in `crv disk
list` (the `EPH` column).

Non-ephemeral disks attached exclusively to a VM are **not**
auto-deleted on `vm delete` — remove them with `crv disk delete` after
the VM is gone, or attach them with `ephemeral=true` from the start.

## Registering Existing Files

Register points the database at an existing file without copying it.

```bash
crv disk register ovmf-code /usr/share/edk2/OvmfX64/OVMF_CODE.fd -f raw
crv disk register debian-base ~/VMs/debian.qcow2
crv disk register ws25-overlay ~/VMs/ws25/overlay.qcow2 --backing ws25-base
```

`--backing` records the overlay relationship in the database (for rebase/delete dependency tracking).

If the file is under the base images directory (`$HOME/VMs`), a relative path is stored for portability.

## Importing

Import copies a local file or downloads from a URL to the managed images directory.

```bash
crv disk import alpine https://example.com/alpine.qcow2 --wait
crv disk import iso /data/isos/windows.iso -f raw -p vms/
crv disk import local-copy /tmp/image.qcow2 --path project/ --wait
```

- `--wait`: block until the import completes (default: async, returns a task ID).
- `--path`: destination path (see [Path Resolution](#path-resolution)).
- Compressed `.xz` files are automatically decompressed.

## Overlays and Clones

```bash
crv disk overlay web-root alpine-base                # Thin overlay (COW)
crv disk clone vm1-vars ovmf-vars-template           # Full copy
crv disk overlay web-root alpine-base --path web/    # Custom destination
```

**Overlay**: creates a qcow2 copy-on-write layer. The base image is not modified. Best for root disks where many VMs share the same base.

**Clone**: full copy of the disk (data + snapshots). Best for files that need independent writability (e.g., OVMF UEFI variables).

## Rebasing and Flattening

```bash
crv disk rebase overlay --backing new-base            # Change backing image
crv disk rebase overlay                               # Flatten (merge backing into overlay)
crv disk rebase overlay --backing new-base --unsafe   # Pointer-only (no data copy)
```

**Flatten** (no `--backing`): merges the backing image's data into the overlay, making it standalone. The overlay is no longer dependent on any backing file.

**Rebase** (with `--backing`): changes which image backs the overlay. By default, data is transformed to match the new backing. `--unsafe` only updates the pointer (use when old and new backing have identical content).

## Resizing

```bash
crv disk resize boot --size 40960   # Resize to 40 GB
```

The VM must be stopped. Only grows — shrinking is not supported.

## Attaching and Detaching

```bash
crv disk attach my-vm boot --interface virtio
crv disk attach my-vm iso -i ide -m cdrom --read-only
crv disk attach my-vm data -i virtio --cache writeback --discard
crv disk detach my-vm 3   # By drive ID (from `crv vm show`)
```

## Moving / Copying Disks Between Nodes

```bash
crv disk copy <disk> --to-node <node> [--to-path <path>]   # add a placement, source intact
crv disk move <disk> --to-node <node> [--to-path <path>]   # add destination, drop source
```

Both commands run asynchronously and return a task id; bytes
flow agent-to-agent (the daemon orchestrates but never relays
data). They refuse for any disk attached read-write to a VM
(use `crv vm migrate` instead). `move` additionally refuses for
disks that are attached read-only anywhere — read-only
attachments can only be copied. Both also refuse for overlays
whose backing image is not already on the destination; copy
the backing image first.

For migrating a whole VM (which moves r/w drives and copies
r/o drives in one orchestrated step), see
[doc/vm-migration.md](vm-migration.md).

### Destination path

By default the daemon **preserves the source disk's stored
path** on the destination — if the disk lives at
`templates/ubuntu-24.qcow2` on the source node, the copy lands
at `templates/ubuntu-24.qcow2` (relative to the destination
node's `basePath`) on the target. The destination agent
creates any missing parent directories automatically.

`--to-path` overrides that default. It accepts the same shapes
as `--path` on `disk create` (see [Path Resolution](#path-resolution)):

| `--to-path` value | Result on the destination |
|-------------------|---------------------------|
| *(omitted)* | preserve the source's relative path; refuse if source path is absolute |
| `staging/x.qcow2` | `<destBase>/staging/x.qcow2` (stored relative) |
| `staging/` | `<destBase>/staging/<sourceBasename>` (trailing `/` = directory) |
| `/srv/data/x.qcow2` | absolute, stored verbatim |

**Absolute-source rule.** If the source disk was registered
with an absolute path *outside* the source node's `basePath`,
copy / move refuses unless `--to-path` is supplied. The same
absolute path on a different node is rarely writable, and
silently retargeting under the destination's `basePath` would
diverge the storage form between the two placements. The
operator must pick the destination explicitly.

**Collision guard.** If the resolved destination path already
holds a different `DiskImageNode` placement on the target, the
command refuses cleanly with `destination path '<P>' already
in use by disk id <N>` — no constraint-violation stack.

| Option | Values | Default |
|--------|--------|---------|
| `--interface` / `-i` | `virtio`, `ide`, `scsi`, `sata`, `nvme`, `pflash` | `virtio` |
| `--media` / `-m` | `disk`, `cdrom` | `disk` |
| `--cache` | `none`, `writeback`, `writethrough`, `directsync`, `unsafe` | `none` |
| `--read-only` | flag | `false` |
| `--discard` | flag | `false` |

## Path Resolution

The optional `--path` flag (on create, import, overlay, clone) controls where the disk image file is placed:

| Path | Interpretation |
|------|---------------|
| *(omitted)* | `$HOME/VMs/<name>.<ext>` |
| `subdir/` | `$HOME/VMs/subdir/<name>.<ext>` (trailing `/` = directory) |
| `custom.raw` | `$HOME/VMs/custom.raw` (no trailing `/` = file path) |
| `/data/vms/` | `/data/vms/<name>.<ext>` (absolute directory) |
| `/data/disk.raw` | `/data/disk.raw` (absolute file path) |

Directories are created automatically if they don't exist.

## Supported Formats

| Format | Extension | Notes |
|--------|-----------|-------|
| `qcow2` | `.qcow2` | Default. Supports overlays, snapshots, compression. |
| `raw` | `.raw` | Simple flat image. Best for firmware files. |
| `vmdk` | `.vmdk` | VMware format. |
| `vdi` | `.vdi` | VirtualBox format. |
| `vpc` | `.vpc` | VHD format. |
| `vhdx` | `.vhdx` | Hyper-V format. |

Format is auto-detected from file extension or via `qemu-img info` when possible.
