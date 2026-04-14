# Shared Directories

Corvus uses [virtiofs](https://virtio-fs.gitlab.io/) to share host directories with guest VMs. The daemon manages `virtiofsd` processes automatically — one per shared directory per VM.

## Requirements

- `virtiofsd` must be installed on the host.
- **Linux guests**: kernel 5.4+ with virtiofs support.
- **Windows guests**: [WinFsp](https://winfsp.dev/) and the VirtIO-FS driver from the [virtio-win](https://github.com/virtio-win/virtio-win-pkg-scripts) package (see [Windows Guest Setup](#windows-guest-setup)).

## Commands

```bash
crv shared-dir add <vm> <path> <tag> [--cache <mode>] [--read-only]
crv shared-dir list <vm>
crv shared-dir remove <vm> <shared_dir>
```

`<shared_dir>` accepts a tag name or numeric ID.

## Adding a Shared Directory

```bash
crv shared-dir add my-vm /home/user/projects projects
crv shared-dir add my-vm /home/user/data data --read-only
crv shared-dir add my-vm /var/log logs --cache never
```

| Option | Values | Default | Description |
|--------|--------|---------|-------------|
| `--cache` | `auto`, `always`, `never` | `auto` | virtiofs cache policy |
| `--read-only` | flag | `false` | Mount as read-only |

### Cache Modes

| Mode | Description |
|------|-------------|
| `auto` | Automatic caching — virtiofsd decides based on access patterns |
| `always` | Always cache — best performance, may serve stale data |
| `never` | Never cache — every access goes to host, best consistency |

## Mounting in the Guest

After the VM starts, mount the shared directory inside the guest using the tag:

```bash
# Linux guest
mount -t virtiofs projects /mnt/projects

# Permanent mount (add to /etc/fstab)
echo 'projects /mnt/projects virtiofs defaults 0 0' >> /etc/fstab
```

The tag (`projects` in this example) must match what was specified in `crv shared-dir add`.

## Windows Guest Setup

Virtiofs on Windows requires two components installed inside the guest:

1. **WinFsp** — a user-mode file system framework. Download from https://winfsp.dev/ and install with default options.
2. **VirtIO-FS driver** — included in the virtio-win driver package. Install `viofs.sys` from the ISO/MSI available at https://github.com/virtio-win/virtio-win-pkg-scripts. After installation, the `VirtioFsSvc` service should appear.

After both are installed, start the VirtIO-FS service:

```powershell
sc start VirtioFsSvc
```

The shared directory appears as a new drive letter automatically. To assign a specific letter or mount point, use the VirtIO-FS service configuration or WinFsp's `fsptool`.

### Limitation: Single Shared Directory on Windows

The current VirtIO-FS Windows driver only supports **one shared directory per VM**. If multiple shared directories are configured, only the first one is accessible. This is a limitation of the `viofs` driver, not Corvus.

If you need multiple shared host paths on a Windows VM, share a single parent directory and use subdirectories within it.

## Lifecycle

- `virtiofsd` processes are started automatically when the VM starts.
- They are stopped when the VM stops.
- Shared directories can be added or removed while the VM is stopped. Changes take effect on the next start.
- Adding a shared directory to a running VM requires a restart for the change to take effect.

## In Apply Configuration

Shared directories can be defined in the `sharedDirs` section of a VM in an [apply configuration](apply-configuration.md) file:

```yaml
vms:
  - name: dev-vm
    cpuCount: 2
    ramMb: 2048
    drives:
      - disk: root
        interface: virtio
    sharedDirs:
      - path: /home/user/projects
        tag: projects
        cache: auto
      - path: /home/user/data
        tag: data
        cache: never
        readOnly: true
```
