# Disk Management

## Commands

```bash
crv disk create <name> --size <MB> [--format <fmt>] [--path <path>]
crv disk register <name> <path> [--format <fmt>] [--backing <disk>]
crv disk import <name> <source> [--path <dest>] [--format <fmt>] [--wait]
crv disk overlay <name> <base_disk> [--path <path>]
crv disk clone <name> <base_disk> [--path <path>]
crv disk rebase <disk> [--backing <new_backing>] [--unsafe]
crv disk resize <disk> --size <MB>
crv disk refresh <disk>
crv disk list
crv disk show <disk>
crv disk delete <disk>
crv disk attach <vm> <disk> [--interface <iface>] [--media <media>] [--read-only] [--discard] [--cache <cache>]
crv disk detach <vm> <drive>
```

`<disk>`, `<vm>`, and `<drive>` accept names or numeric IDs.

## Creating Disk Images

```bash
crv disk create boot --size 20480 --format qcow2
crv disk create data --size 102400 -f raw
crv disk create scratch --size 4096 --path project/
```

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
