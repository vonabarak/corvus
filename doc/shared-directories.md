# Shared Directories

Corvus supports [virtiofs](https://virtio-fs.gitlab.io/) for directories on the VM's host and [SPICE folder sharing](#spice-folder-sharing) for a directory on the viewer's machine. SPICE sharing works with the native viewer opened by the CLI or desktop application, including when the VM runs on another host.

The following virtiofs configuration uses `crv shared-dir` and `sharedDirs` in YAML. The nodeagent manages `virtiofsd` processes automatically — one per shared directory per VM.

## Requirements

- `virtiofsd` must be installed on the host.
- **Linux guests**: kernel 5.4+ with virtiofs support.
- **Windows guests**: [WinFsp](https://winfsp.dev/) and the VirtIO-FS driver from the [virtio-win](https://github.com/virtio-win/virtio-win-pkg-scripts) package (see [Windows Guest Setup](#windows-guest-setup)).

## Commands

```bash
crv shared-dir add <vm> <path> <tag> [--cache <mode>] [--read-only]
crv shared-dir list <vm>
crv shared-dir remove <vm> <shared_dir_id>
```

`<shared_dir_id>` is the numeric ID shown by `crv shared-dir list <vm>`.

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
    ram: 2G
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

## SPICE Folder Sharing

Every graphical VM exposes the `org.spice-space.webdav.0` channel. Sharing is controlled by the connected native SPICE viewer; Corvus stores no viewer-local paths. This feature requires Remmina or `remote-viewer` with SPICE WebDAV support on the client and `spice-webdavd` inside the guest. QEMU Guest Agent is independent of folder sharing. Guests without `spice-webdavd` can still start and use the graphical console normally.

### Selecting a Folder

Run `crv vm view <vm> --share-folder /path/to/folder` with an existing directory on your client machine. Corvus prefers virt-viewer when both viewers are installed; use `--viewer remmina` or `--viewer virt-viewer` to require one explicitly.

- **Remmina:** the folder is shared automatically with read/write access. Without `--share-folder`, Corvus configures no folder.
- **virt-viewer:** the folder is preselected, but sharing stays disabled until you open **File → Preferences** and enable **Share folder**. You can also choose a folder there without the CLI option. Use its read-only setting if the guest should not modify the folder.
- **Desktop application:** the **View** button continues to open virt-viewer; select and enable the folder in **File → Preferences**.

Access the share from inside the guest as described below.

The share is available while that viewer is connected and sharing is enabled. Disconnect to withdraw access, or disable sharing in the viewer where supported. Each CLI launch configures only the folder explicitly supplied for that launch; virt-viewer requires manual activation again. The browser console does not provide folder sharing. SPICE sharing does not create a `crv shared-dir` entry or use a virtiofs mount tag.

### Linux Guest Setup

Install your distribution's `spice-webdavd` package and start its service. For Debian/Ubuntu guests:

```sh
sudo apt install spice-webdavd
sudo systemctl start spice-webdavd.service
```

The guest also needs its VirtIO serial driver. On Linux, check that `/dev/virtio-ports/org.spice-space.webdav.0` exists. The shared folder appears in GNOME Files network locations; it can be browsed through GVfs and accessed by traditional applications with `gvfs-fuse`. Alternatively, connect a WebDAV client inside the guest to `http://localhost:9843/` (the standard `spice-webdavd` service port). Other distributions may use different service names or ports.

### Windows Guest Setup

Install the VirtIO serial driver (`vioser`) from the VirtIO-Win package and the separate **Spice WebDAV daemon** installer linked from the [SPICE downloads page](https://www.spice-space.org/download.html). This daemon is not included in Windows SPICE Guest Tools. Start its installed service and enable automatic startup.

Use the installer's `map-drive.bat` helper to map the shared folder to a drive, or use File Explorer's **Map network drive** with `http://localhost:9843/`. The Windows WebClient service must be running for Explorer's WebDAV access. This workflow does not require WinFsp or the VirtIO-FS service used for virtiofs above.

See the upstream [SPICE folder-sharing guide](https://spice.pages.freedesktop.org/spice-space/spice-user-manual.html#_folder_sharing) for additional guest and client setup details.
