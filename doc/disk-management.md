# Disk Management

## Commands

```bash
crv disk create <name> --size <SIZE> [--format <fmt>] [--path <path>] [--node <node>] [--ephemeral]
crv disk register <name> <path> [--format <fmt>] [--backing <disk>] [--node <node>] [--ephemeral]
crv disk import <name> <source> [--path <dest>] [--format <fmt>] [--node <node>] [--ephemeral] [--wait]
crv disk upload <name> <local-file> --format <fmt> [--path <dest>] [--node <node>] [--ephemeral]
crv disk overlay <name> <base_disk> [--path <path>] [--ephemeral]
crv disk clone <name> <base_disk> [--path <path>] [--ephemeral]
crv disk rebase <disk> [--backing <new_backing>] [--unsafe]
crv disk resize <disk> --size <SIZE>
crv disk refresh <disk>
crv disk list
crv disk show <disk>
crv disk cleanup NAME | --all [--node NODE] [--include-tagged] [--dry-run]
crv disk delete <disk>
crv disk tag <disk> <tag>
crv disk untag <disk> <tag>
crv disk register-placement <disk> <path> --node <node>
crv disk attach <vm> <disk> [--interface <iface>] [--media <media>] [--read-only] [--discard] [--cache <cache>]
crv disk detach <vm> <disk>
crv disk media eject <drive>
crv disk media change <drive> <new_disk>
crv disk copy <disk> --to-node <node> [--to-path <path>] [--with-backing-chain]
crv disk move <disk> --to-node <node> [--to-path <path>] [--with-backing-chain]
```

`<vm>` and `<node>` accept names or numeric IDs. `<disk>` accepts an image ID,
`name:tag`, or a bare name (which means `name:latest`). An image name must be
nonempty, contain no colon, and must not start with a number. Digit-leading
selectors must be complete positive decimal Int64 IDs; `123abc` is an error.
Tags contain 1–128 ASCII letters, digits, underscores, periods or hyphens;
the first character must be a letter, digit or underscore. Tags are case sensitive.

## Versions and tags

Every create, register, import, upload, clone and overlay publishes a new image
ID. Publication accepts `name` or `name:tag`, assigns that tag, and moves
`latest` to the new version. Other versions, their files, placements, overlays,
and VM attachments are preserved. An image can have several tags; each
`name:tag` selects exactly one version. Untagged versions remain accessible by ID.

`crv disk tag 123 stable` moves `stable` to version 123 within its image name.
`crv disk untag 123 stable` removes it. `latest` may be moved but cannot be
removed. Deleting its image promotes the remaining version with the newest
creation date (highest ID breaks date ties). A nonempty image name always has
exactly one `latest`. Deletion still refuses versions used by VMs, backing
images, or templates pinned by ID. Floating template selectors do not block
deletion; they resolve their tag when a new VM is instantiated.

Generated filenames use `<image-id>-<name[:tag]>.<format>`, for example
`123-ubuntu:24.04.qcow2`. The prefix is the published image ID. IDs are reserved
before file creation and may have gaps after failed or cancelled operations;
reservations do not expose an image or change tags. Explicit destination paths
must be unused; publishing a new tag never overwrites the old file.
`<drive>` is the numeric drive row id of a VM's drive (see
`crv vm show <vm>`, the `ID` column of its drive list).

## Cleaning older versions

Preview or remove historical versions of one family or every registered family:

```bash
crv disk cleanup ubuntu --dry-run
crv disk cleanup ubuntu
crv disk cleanup --all --include-tagged
crv disk cleanup ubuntu --node worker-1
```

Choose exactly one bare image name or `--all`; IDs and `name:tag` selectors
are not cleanup targets. By default cleanup selects only untagged versions.
`--include-tagged` also selects ordinary tagged versions. The version currently
tagged `latest` is always retained, even if a newer version exists.

Templates protect both pinned image IDs and the current resolution of floating
name/tag references. VM attachments protect their node's placement. Retained
overlays protect their backing chain on each node; eligible overlays are removed
before their backing images. `--node` limits placement deletion to that node,
so an unused copy can be removed while another node still uses the version.
The final placement is retained while any VM or overlay references the version.

Snapshots do not protect historical versions. Snapshots,
source identities, and tags are removed with version metadata after its last
placement is deleted. While any copy remains, that metadata is retained.

Cleanup returns a completed report with nested image/node references, per-version
and per-placement outcomes (`retained`, `planned`, `removed`, `failed`), reasons,
and counts of removed versions, removed placements, and failures. An unreachable
node or failed deletion leaves its placement recorded for retry; cleanup continues
with other eligible versions. The CLI prints the full report and exits nonzero
when failures remain. `-o json` exposes the same report for scripts.

`--dry-run` simulates the same dependency ordering without deleting files or
recording tasks. Its removal counts are zero; `planned` outcomes show eligibility
and do not guarantee node availability. A missing named family is an error;
`--all` on an empty registry succeeds. Repeated cleanup is safe.

The daemon waits for active mutating operations (including builds) and holds an
exclusive mutation lease while cleaning up. This prevents publication, tag,
template, attachment, and backing-chain changes from racing with deletion.
Other mutations wait until cleanup finishes; read-only queries and task
cancellation remain available through separate connections. Cleanup snapshots
its candidate IDs once and supports cancellation between placement deletions.

Both Python clients expose `disks.cleanup(name=None, *, all_images=False,
node=None, include_tagged=False, dry_run=False)`, returning `DiskCleanupReport`.
RPC clients call `DiskManager.cleanup(DiskCleanupParams)` with exactly one
nonempty `name` or `allImages=true`; `node` is an optional `Common.EntityRef`.
The REST gateway exposes `POST /disks/cleanup` with the Python argument names
as JSON fields, returning the report (HTTP 200 also for partial failures;
inspect `failures`). There is no GUI workflow or automatic retention policy.

## Per-node placement

Disk images are per-node: each row in the `disk_image` table is a
version of a logical image name, and each on-disk file lives in the
`disk_image_node` join, unique by `(disk_image_id, node_id)` and by
`(node_id, file_path)`. The same logical image may have placements on
multiple nodes; an operator replicates an image by rsync-ing
the file and running `crv disk register-placement <image-id> <path> --node <new-node>`.
This adds a replica of that exact version without changing tags; the file must
have the same format and virtual size. Operators must copy the same bytes.

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
crv disk create boot --size 2G --format qcow2
crv disk create data --size 100G -f raw
crv disk create scratch --size 500M --path project/
crv disk create test-overlay --size 1T --ephemeral
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

Register points the database at an existing file without copying it. The path is
resolved on the selected node, not on the machine running `crv`.

```bash
crv disk register ovmf-code /usr/share/edk2/OvmfX64/OVMF_CODE.fd -f raw
crv disk register debian-base ~/VMs/debian.qcow2
crv disk register ws25-overlay ~/VMs/ws25/overlay.qcow2 --backing ws25-base
```

`--backing` records the overlay relationship in the database (for rebase/delete dependency tracking). When `--format` is omitted,
Corvus inspects the file with `qemu-img` and falls back to its extension.

If the file is under the base images directory (`$HOME/VMs`), a relative path is stored for portability.

## Importing

Import copies a file visible on the selected node or downloads an HTTP(S) URL to
the managed images directory. To import a file on the machine running `crv`, use
[`crv disk upload`](#uploading-client-local-media).

```bash
crv disk import alpine https://example.com/alpine.qcow2 --wait
crv disk import iso /data/isos/windows.iso -f raw -p vms/
crv disk import local-copy /tmp/image.qcow2 --path project/ --wait
```

- `--wait`: block until the asynchronous import task completes (without it, the command returns a task ID).
- `--path`: destination path (see [Path Resolution](#path-resolution)).
- Compressed `.xz` files are automatically decompressed.

## Uploading client-local media

`crv disk upload` streams a file from the machine running `crv` through the
daemon to the selected node; its source path is never interpreted on the
daemon or node. The node writes a temporary sibling file and atomically
publishes it when the stream finishes.

Use it for prepared installer media such as an answer-file ISO or USB image.
The uploaded object is an ordinary disk image and can be attached read-only as
an IDE/SCSI CD-ROM or disk in a template. Uploading the same name publishes a
new version and moves its tags after the complete stream succeeds. Aborting
an upload discards its temporary file and leaves the old tags unchanged.

## Overlays and Clones

```bash
crv disk overlay web-root alpine-base                # Thin overlay (COW)
crv disk clone vm1-vars ovmf-vars-template           # Full copy
crv disk overlay web-root alpine-base --path web/    # Custom destination
crv disk clone vm1-vars ovmf-vars-template --path firmware/
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
crv disk resize boot --size 40G   # Resize to 40 GB
```

The VM must be stopped. Only grows — shrinking is not supported.

## Attaching and Detaching

```bash
crv disk attach my-vm boot --interface virtio
crv disk attach my-vm iso -i ide -m cdrom --read-only
crv disk attach my-vm data -i virtio --cache writeback --discard
crv disk detach my-vm boot   # By disk name or disk ID
```

Virtio and SCSI drives can be detached while a VM is running. IDE, SATA,
NVMe, pflash, and floppy drives are not live-detachable; stop the VM before
detaching them. This includes IDE CD-ROM installer media.

CD-ROM drives are a special case: rather than detaching the drive while the
VM is running, keep the drive in place and swap the media in the tray with
[`crv disk media eject` / `crv disk media change`](#ejecting-and-changing-cd-rom-media).
This works on any interface (IDE, SATA, SCSI, …) because it never removes
the device — it only opens the tray.

VMs already running when this support is deployed must be restarted once so
QEMU is launched with the named-device layout required for live detach.

### QEMU device hierarchy for live detach

Corvus gives each live-detachable drive three stable QEMU names: the block
backend (`drive-<id>`), the guest-visible device (`device-<id>`), and the bus
to which that device is attached. The backend is storage plumbing rather than
a PCI child; the guest device refers to it through its `drive` property.

For a VirtIO disk present when the VM boots, Corvus creates a native PCIe root
port for that disk:

```text
Q35 PCIe root complex
└── pcie-root-port: virtio-rp-<id>
    └── virtio-blk-pci: device-<id>  ──uses──>  block node: drive-<id>
```

The root port supplies the hot-pluggable PCIe slot. On detach, Corvus removes
the `virtio-blk-pci` child first and then releases `drive-<id>`; the root port
remains in the VM's hardware layout.

SCSI drives use one stable controller and its SCSI child bus:

```text
Q35 PCIe root complex
└── pcie-root-port: scsi-rp
    └── virtio-scsi-pci: scsi0
        └── SCSI child device: device-<id>  ──uses──>  block node: drive-<id>
```

A VirtIO disk attached while a VM is already running is placed on the
pre-created hot-plug branch instead:

```text
Q35 PCIe root complex
└── pcie-root-port: hotplug-rp
    └── pcie-pci-bridge: hotplug
        └── virtio-blk-pci: device-<id>  ──uses──>  block node: drive-<id>
```

This hierarchy applies to drive attach and detach. CD-ROM media eject is
different: it keeps the guest device in place and changes only the medium in
its tray.

## Ejecting and Changing CD-ROM Media

A drive attached with `--media cdrom` behaves like a physical tray: the
drive row stays attached to the VM, and only the media in the tray changes.

```bash
crv disk media eject 3            # empty the tray
crv disk media change 3 new-iso   # swap the tray contents
```

`<drive>` is the drive's numeric id from `crv vm show <vm>`; `<new_disk>`
is a disk image name or id (typically a registered ISO).

The daemon checks that the drive is a removable CD-ROM before doing
anything and rejects non-`cdrom` drives with a clear error. Ejecting an
already-empty tray is a no-op error.

**Running or paused VM** — the daemon sends the QMP `eject` /
`blockdev-change-medium` command to QEMU first and only updates the
database after QEMU succeeds, so a failed QMP call never desynchronises
the database from the running VM. Changing the media works from an empty
tray too (that is how you load an ISO into a previously-ejected drive).

**Stopped VM** — only the database is touched; the tray state is applied
at the next boot, because the QEMU command line is generated from the
`drive` row (an empty tray simply omits `file=`/`format=`).

The tray state persists across VM reboots and daemon restarts: after an
eject, the drive row has no media, and `crv vm show` / the web UI report
the drive with no disk image.

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
the backing image first. Add `--with-backing-chain` to stage every missing
backing ancestor automatically before the primary transfer.

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
| `--interface` / `-i` | `virtio`, `ide`, `scsi`, `sata`, `nvme`, `pflash`, `floppy` | `virtio` |
| `--media` / `-m` | `disk`, `cdrom` | `disk` |
| `--cache` | `none`, `writeback`, `writethrough`, `directsync`, `unsafe` | `writeback` |
| `--read-only` | flag | `false` |
| `--discard` | flag | `false` |

## Path Resolution

The optional `--path` flag (on create, import, overlay, clone) controls where the disk image file is placed:

| Path | Interpretation |
|------|---------------|
| *(omitted)* | `$HOME/VMs/<image-id>-<name[:tag]>.<ext>` |
| `subdir/` | `$HOME/VMs/subdir/<image-id>-<name[:tag]>.<ext>` (trailing `/` = directory) |
| `custom.raw` | `$HOME/VMs/custom.raw` (no trailing `/` = file path) |
| `/data/vms/` | `/data/vms/<image-id>-<name[:tag]>.<ext>` (absolute directory) |
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
