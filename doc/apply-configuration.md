# Apply Configuration Reference

The `crv apply` command creates a complete VM environment from a single YAML file. The daemon processes the file and creates all declared resources in dependency order: SSH keys, disk images, virtual networks, then VMs.

## Usage

```bash
crv apply <file.yml>                # Create all resources
crv apply <file.yml> --skip-existing   # Skip resources that already exist
crv apply <file.yml> -o json        # Output result as JSON
```

**`--skip-existing` / `-s`**: When set, resources that already exist in the database (matched by name) are silently skipped and their IDs are reused for dependent references. Without this flag, attempting to create a resource with a duplicate name is an error. This is useful for re-applying a configuration after a partial failure or for incrementally adding resources to an existing environment.

## File Structure

A configuration file has four top-level sections, all optional:

```yaml
sshKeys:    [...]   # SSH public keys for cloud-init injection
disks:      [...]   # Disk images (import, create, or overlay)
networks:   [...]   # Virtual networks (bridge/TAP in daemon namespace)
vms:        [...]   # Virtual machines with drives, NICs, shared dirs
```

Resources are created in the order listed above. Within each section, items are processed sequentially — later items can reference earlier ones by name (e.g., an overlay disk can reference a base image defined earlier in the same file).

Resources can also reference items that already exist in the database. For example, a VM drive can reference a disk image that was previously imported via `crv disk import`, not just disks defined in the same YAML file.

## YAML Features

Configuration files support standard YAML features including anchors (`&name`), aliases (`*name`), and merge keys (`<<:`). These are useful for reducing duplication when multiple VMs share common settings.

Anchors can be defined on an ignored top-level key (any key other than `sshKeys`, `disks`, `networks`, `vms` is silently ignored):

```yaml
_vm_defaults: &vm_defaults
  cpuCount: 2
  ramMb: 2048
  headless: true
  guestAgent: true

vms:
  - name: web
    <<: *vm_defaults
    description: "Web server"
  - name: db
    <<: *vm_defaults
    cpuCount: 4        # override anchor default
    description: "Database server"
```

The merge key (`<<:`) only works for mappings, not lists — so it applies to VM-level fields but not to lists like `drives` or `networkInterfaces`.

See `example-apply.yml` for a complete example with anchors, UEFI boot, and multiple VMs.

## SSH Keys

```yaml
sshKeys:
  - name: <string>           # Required. Unique name for the key.
    publicKey: <string>       # Required. Full public key string.
```

SSH keys declared here can be referenced by name in the `sshKeys` list of any VM in the same file. Keys are used for cloud-init SSH injection — the VM must have cloud-init enabled (see [Cloud-Init Behavior](#cloud-init-behavior)).

### Example

```yaml
sshKeys:
  - name: deploy-key
    publicKey: "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIExample deploy@host"
  - name: admin-key
    publicKey: "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQ... admin@host"
```

## Disk Images

Each disk entry creates a disk image in one of four ways, determined by which fields are present.

```yaml
disks:
  - name: <string>           # Required. Unique name for the disk.
    import: <string>          # Option A: Path or URL to import.
    overlay: <string>         # Option B: Name of backing disk for qcow2 overlay.
    clone: <string>           # Option C: Name of disk to clone (full copy).
    format: <string>          # Disk format (for create; auto-detected on import). See below.
    sizeMb: <integer>         # Size in MB (for create; optional resize hint for overlay).
    path: <string>            # Optional destination path for overlay/clone/create output file.
```

Exactly one creation strategy must be specified:

| Strategy | Required Fields | Description |
|----------|----------------|-------------|
| **Import** | `import` | Register an existing local file or download from HTTP/HTTPS URL. |
| **Overlay** | `overlay` | Create a qcow2 overlay backed by the named disk. |
| **Clone** | `clone` | Full copy of a disk image (including snapshots). |
| **Create** | `format` + `sizeMb` | Create a new empty disk image. |

Specifying more than one of `import`, `overlay`, `clone` is an error.

### Import

The `import` field accepts:

- **Local absolute path**: `/data/images/alpine.qcow2` — the file is registered in the database at its current location. The file is not copied.
- **Local relative path**: `ws25/overlay.qcow2` — resolved relative to the daemon's base images directory (`$HOME/VMs` by default).
- **HTTP/HTTPS URL**: `https://example.com/image.qcow2` — the daemon downloads the file. Compressed `.xz` files are automatically decompressed.

The `format` field is optional for imports — it is auto-detected from the file extension or via `qemu-img info` when possible. Supported formats: `qcow2`, `raw`, `vmdk`, `vdi`, `vpc` (VHD), `vhdx`.

If a local file is already registered in the database (same path), the existing entry is reused without error.

### Overlay

Creates a qcow2 copy-on-write overlay backed by the named disk. The backing disk can be defined earlier in the same `disks` section or already exist in the database. The optional `sizeMb` field triggers a resize of the overlay after creation.

### Clone

Creates a full copy of the named disk, including all snapshots. The source disk can be defined earlier in the same `disks` section or already exist in the database. The clone preserves the source disk's format, size, and backing image relationship.

This is useful for UEFI OVMF variables: import the template once, then clone it for each VM that needs its own writable copy.

### Create

Creates a new empty disk image. Both `format` and `sizeMb` are required. The file is created in the daemon's base images directory.

### Custom Path

The optional `path` field controls where the disk image file is placed. It can be used with `overlay`, `clone`, and `create` strategies.

**Path interpretation rules:**

| Path | Interpretation |
|------|---------------|
| *(omitted)* | Default: `$BASE/<name>.<ext>` (base images directory) |
| `subdir/` | Directory (trailing `/`): `$BASE/subdir/<name>.<ext>` — filename auto-generated |
| `custom.raw` | File path (no trailing `/`): `$BASE/custom.raw` — used as-is |
| `/data/vms/` | Absolute directory: `/data/vms/<name>.<ext>` |
| `/data/disk.raw` | Absolute file path: `/data/disk.raw` |

- Paths starting with `/` are absolute; otherwise they are resolved relative to the base images directory (`$HOME/VMs` by default).
- Paths ending with `/` are treated as directories — the filename is auto-generated from the disk name and format extension (e.g., `my-disk.qcow2`). The directory is created if it does not exist.
- Paths **not** ending with `/` are treated as the full file path.

The same rules apply to the `--path` option on `crv disk create`, `crv disk overlay`, and `crv disk clone` CLI commands.

### Examples

```yaml
disks:
  # Download a cloud image from the internet
  - name: alpine-base
    import: "https://dl-cdn.alpinelinux.org/alpine/v3.20/releases/cloud/nocloud_alpine-3.20.6-x86_64-bios-cloudinit-r0.qcow2"

  # Register a local file (absolute path, not copied)
  - name: ovmf-code
    import: "/usr/share/edk2/OvmfX64/OVMF_CODE.fd"
    format: raw

  # Register a local file (relative to ~/VMs)
  - name: ws25-system
    import: "ws25/overlay.qcow2"
    format: qcow2

  # Create overlay backed by the alpine-base image
  - name: web-root
    overlay: alpine-base
    sizeMb: 10240

  # Clone OVMF UEFI vars template (each VM needs its own writable copy)
  - name: ovmf-vars-template
    import: "/usr/share/edk2/OvmfX64/OVMF_VARS.fd"
    format: raw
  - name: vm1-ovmf-vars
    clone: ovmf-vars-template
  - name: vm2-ovmf-vars
    clone: ovmf-vars-template

  # Create overlay in a subdirectory of the base images path
  - name: vm1-root
    overlay: alpine-base
    path: "vm1/"

  # Clone to an absolute directory
  - name: vm2-ovmf-vars
    clone: ovmf-vars-template
    path: "/data/vms/vm2/"

  # Clone with a specific absolute file path
  - name: special-vars
    clone: ovmf-vars-template
    path: "/data/vms/my-custom-vars.fd"

  # Create a new empty data disk
  - name: data-disk
    format: qcow2
    sizeMb: 20480

  # Create a data disk in a specific directory
  - name: vm1-data
    format: qcow2
    sizeMb: 51200
    path: "vm1/"
```

## Virtual Networks

```yaml
networks:
  - name: <string>           # Required. Unique name for the network.
    subnet: <string>          # Optional. CIDR subnet (e.g., "10.0.1.0/24").
    dhcp: <boolean>           # Optional. Default: false. Enable DHCP via dnsmasq.
    nat: <boolean>            # Optional. Default: false. Enable NAT to host network.
```

Creates a virtual network using bridge/TAP devices inside the daemon's network namespace. The network must be started separately with `crv network start <name>` after apply completes — apply only creates the database record.

When started, the daemon creates a bridge interface in its namespace. If `dhcp` is enabled, it starts dnsmasq on the bridge to provide DHCP and DNS for connected VMs. If `nat` is enabled, nftables MASQUERADE rules are added so VMs can reach the internet through the host via pasta.

Both `dhcp` and `nat` require a `subnet` to be specified. A network without a subnet creates a bridge-only L2 network (VMs can communicate with each other using static IPs).

Networks declared here can be referenced by name in `networkInterfaces` entries of VMs in the same file.

### Example

```yaml
networks:
  # Bridge-only network (no DHCP, no NAT — use static IPs)
  - name: internal
    subnet: "10.0.1.0/24"

  # Network with DHCP (VMs get addresses automatically)
  - name: lab-net
    subnet: "10.0.2.0/24"
    dhcp: true

  # Network with DHCP and NAT (VMs can reach the internet)
  - name: internet
    subnet: "10.0.3.0/24"
    dhcp: true
    nat: true
```

## Virtual Machines

```yaml
vms:
  - name: <string>            # Required. Unique VM name.
    cpuCount: <integer>        # Required. Number of virtual CPUs.
    ramMb: <integer>           # Required. RAM in megabytes.
    description: <string>      # Optional. Human-readable description.
    headless: <boolean>        # Optional. Default: false. Serial console instead of SPICE.
    guestAgent: <boolean>      # Optional. Default: false. Enable QEMU guest agent channel.
    cloudInit: <boolean>       # Optional. See Cloud-Init Behavior below.
    cloudInitConfig:           # Optional. Custom cloud-init configuration.
      userData: <object>       # Optional. Custom #cloud-config YAML (parsed as structured YAML).
      networkConfig: <object>  # Optional. Network-config YAML (version 2).
      injectSshKeys: <boolean> # Optional. Default: true. Merge DB SSH keys into first user.
    drives: [...]              # Optional. List of attached drives.
    networkInterfaces: [...]   # Optional. List of network interfaces.
    sharedDirs: [...]          # Optional. List of virtiofs shared directories.
    sshKeys: [...]             # Optional. List of SSH key names to attach.
```

VMs are created in `stopped` state. Start them with `crv vm start <id>` after apply completes.

### Drives

```yaml
    drives:
      - disk: <string>         # Required. Name of a disk image (from this file or existing in DB).
        interface: <string>     # Required. Drive interface type.
        media: <string>         # Optional. Drive media type.
        readOnly: <boolean>     # Optional. Default: false.
        cacheType: <string>     # Optional. Default: "writeback".
        discard: <boolean>      # Optional. Default: false. Enable TRIM/discard passthrough.
```

**`interface`** values:

| Value | Description |
|-------|-------------|
| `virtio` | VirtIO block device (best performance for data disks) |
| `ide` | IDE controller (legacy, good for CD-ROMs) |
| `scsi` | SCSI controller |
| `sata` | SATA controller |
| `nvme` | NVMe controller |
| `pflash` | Persistent flash (for UEFI firmware OVMF_CODE / OVMF_VARS) |

**`media`** values (optional):

| Value | Description |
|-------|-------------|
| `disk` | Hard disk (default when omitted) |
| `cdrom` | CD-ROM drive (automatically sets read-only) |

**`cacheType`** values:

| Value | Description |
|-------|-------------|
| `none` | No host caching (default, safest for data integrity) |
| `writeback` | Host page cache with writeback (better performance) |
| `writethrough` | Host page cache with writethrough |
| `directsync` | Direct I/O with fsync |
| `unsafe` | Ignore flush requests (fastest, risk of data loss) |

### Network Interfaces

```yaml
    networkInterfaces:
      - network: <string>       # Option A: Name of a managed virtual network (type defaults to "managed").
      - type: <string>          # Option B: Explicit network type.
        hostDevice: <string>    # Host device path or QEMU netdev options (for non-managed types).
        mac: <string>           # Optional. MAC address (auto-generated if omitted).
```

When `network` is specified, the interface type is automatically set to `managed` — you should not specify `type` explicitly (it is a validation error to specify a type other than `managed` when `network` is present).

**`type`** values:

| Value | Description |
|-------|-------------|
| `managed` | Managed virtual network (bridge/TAP in daemon namespace). Set automatically when `network` is specified. |
| `user` | QEMU user-mode networking (built-in NAT, no host device needed). |
| `vde` | External VDE virtual switch (specify `hostDevice` as VDE socket path). |
| `tap` | TAP device (requires pre-configured host device). |
| `bridge` | Bridge device (requires pre-configured host bridge). |
| `macvtap` | MACVTAP device. |

For `managed` interfaces, the daemon creates a TAP device inside its network namespace and passes the file descriptor to QEMU. The VM's NIC is automatically connected to the network's bridge.

A MAC address is generated automatically for each interface unless `mac` is specified. Use explicit MAC addresses when you need reproducible network configurations or specific addressing.

### Shared Directories

```yaml
    sharedDirs:
      - path: <string>         # Required. Absolute host path to share.
        tag: <string>          # Required. Mount tag (used by guest to mount: mount -t virtiofs <tag> /mnt).
        cache: <string>        # Optional. Default: "auto".
        readOnly: <boolean>    # Optional. Default: false.
```

Shared directories use virtiofs. The daemon starts a `virtiofsd` process for each shared directory when the VM starts.

**`cache`** values:

| Value | Description |
|-------|-------------|
| `auto` | Automatic caching policy (default) |
| `always` | Always cache |
| `never` | Never cache (direct access) |

### SSH Keys

```yaml
    sshKeys:
      - <key-name>            # Name of an SSH key (from this file or existing in DB).
      - <key-name>
```

A flat list of SSH key names to attach to the VM. See [Cloud-Init Behavior](#cloud-init-behavior) for how this interacts with cloud-init.

## Cloud-Init Behavior

Cloud-init controls whether a NoCloud ISO is generated and attached to the VM for first-boot provisioning (hostname, SSH keys, packages, etc.).

The `cloudInit` field on a VM has three states:

| Value | Behavior |
|-------|----------|
| `true` | Cloud-init is enabled. ISO is generated on VM start or SSH key attach. |
| `false` | Cloud-init is disabled. SSH key attachment is rejected. |
| *omitted* | **Auto-detect**: enabled if `sshKeys` is non-empty, disabled otherwise. |

In most cases, omit the field and let auto-detection handle it. Explicitly set `cloudInit: true` if you want cloud-init even without SSH keys (e.g., for hostname provisioning only).

It is a validation error to list `sshKeys` on a VM with `cloudInit: false`.

### Custom Cloud-Init Config

The optional `cloudInitConfig` section allows per-VM customization of the cloud-init ISO contents. Without it, a default config is generated (user `corvus`, password `corvus`, `qemu-guest-agent` package, SSH key injection).

```yaml
    cloudInitConfig:
      userData:              # Replaces the default #cloud-config user-data
        users:
          - name: deploy
            sudo: "ALL=(ALL) NOPASSWD:ALL"
            shell: /bin/bash
        packages:
          - nginx
          - postgresql
        runcmd:
          - systemctl enable nginx
      networkConfig:         # Optional network-config (not generated by default)
        version: 2
        ethernets:
          eth0:
            dhcp4: true
      injectSshKeys: true    # Default: true
```

**`userData`** and **`networkConfig`** are written as structured YAML objects (not strings). Corvus serializes them and prepends the `#cloud-config` header automatically — do not include it.

**`injectSshKeys`** controls whether SSH keys from the `sshKeys` list (and any keys attached via `crv ssh-key attach`) are merged into the custom user-data:

| Value | Behavior |
|-------|----------|
| `true` (default) | Corvus parses the user-data, finds the first user in the `users` list, and appends DB SSH keys to its `ssh_authorized_keys`. If no `users` section exists, a top-level `ssh_authorized_keys` key is added. |
| `false` | User-data is used exactly as provided. No SSH keys are injected. |

Custom configs can also be managed via the CLI: `crv cloud-init set`, `crv cloud-init show`, `crv cloud-init delete`.

## Validation Rules

The daemon validates the configuration before creating any resources:

- No duplicate names within each section (SSH keys, disks, networks, VMs).
- Each disk must use exactly one creation strategy (`import`, `overlay`, `clone`, or `format` + `sizeMb`).
- A disk cannot specify more than one of `import`, `overlay`, `clone`.
- The `path` field can only be used with `overlay`, `clone`, or `create` strategies (not `import`).
- VMs with `sshKeys` must not have `cloudInit: false`.

If validation fails, no resources are created.

## Error Handling

If resource creation fails partway through (e.g., a disk download fails or a unique constraint is violated), the daemon returns an error and stops processing. Resources created before the failure remain in the database.

Use `--skip-existing` to re-run an apply after a partial failure — already-created resources will be skipped, and processing continues from where it left off.

## Output

On success, the command prints a summary of created resources:

```
Applied 5 resources:
  SSH keys (1):
    - deploy-key (id: 1)
  Disks (2):
    - alpine-base (id: 3)
    - web-root (id: 4)
  Networks (1):
    - lab-net (id: 1)
  VMs (1):
    - web-server (id: 5)
```

Resources skipped via `--skip-existing` are not listed in the output.

With `-o json`, the output is a JSON object:

```json
{
  "sshKeys": [{"name": "deploy-key", "id": 1}],
  "disks": [{"name": "alpine-base", "id": 3}, {"name": "web-root", "id": 4}],
  "networks": [{"name": "lab-net", "id": 1}],
  "vms": [{"name": "web-server", "id": 5}]
}
```

## Complete Example

```yaml
sshKeys:
  - name: deploy-key
    publicKey: "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIExample deploy@host"

disks:
  # UEFI firmware (shared read-only by all UEFI VMs)
  - name: ovmf-code
    import: "/usr/share/edk2/OvmfX64/OVMF_CODE.fd"
    format: raw

  # Download a cloud image
  - name: alpine-base
    import: "https://dl-cdn.alpinelinux.org/alpine/v3.20/releases/cloud/nocloud_alpine-3.20.6-x86_64-bios-cloudinit-r0.qcow2"

  # Per-VM overlay (copy-on-write, backed by alpine-base)
  - name: web-root
    overlay: alpine-base
    sizeMb: 10240

  # OVMF UEFI variables template (import once, clone per-VM)
  - name: ovmf-vars-template
    import: "/usr/share/edk2/OvmfX64/OVMF_VARS.fd"
    format: raw

  # Per-VM OVMF vars (full copy, each VM gets its own writable file)
  - name: web-ovmf-vars
    clone: ovmf-vars-template

  # Empty data disk
  - name: data-disk
    format: qcow2
    sizeMb: 51200

networks:
  - name: lab-net
    subnet: "10.0.1.0/24"
    dhcp: true
    nat: true

vms:
  - name: web-server
    cpuCount: 4
    ramMb: 4096
    description: "Alpine Linux web server"
    headless: true
    guestAgent: true
    cloudInitConfig:
      userData:
        users:
          - name: deploy
            sudo: "ALL=(ALL) NOPASSWD:ALL"
        packages:
          - nginx
          - qemu-guest-agent
        runcmd:
          - systemctl enable nginx
          - systemctl enable qemu-guest-agent
    drives:
      - disk: ovmf-code
        interface: pflash
        readOnly: true
      - disk: web-ovmf-vars
        interface: pflash
      - disk: web-root
        interface: virtio
        cacheType: writeback
        discard: true
      - disk: data-disk
        interface: virtio
        cacheType: writeback
        discard: true
    networkInterfaces:
      - network: lab-net
      - type: user
    sharedDirs:
      - path: /home/user/www
        tag: www
        cache: auto
        readOnly: true
    sshKeys:
      - deploy-key
```

After applying:

```bash
crv apply environment.yml         # Create everything
crv network start lab-net         # Start the virtual network
crv vm start web-server           # Start the VM
```
