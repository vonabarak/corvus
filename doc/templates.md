# Template Reference

VM templates capture a complete machine configuration — CPU, RAM, console mode, drives, network interfaces, SSH keys, and cloud-init settings — in a reusable YAML definition. Instantiating a template creates a new VM with its own copies of the configured disks, ready to start.

## CLI Commands

```bash
crv template create [FILE]               # Create from YAML file, or open $EDITOR
crv template edit <TEMPLATE>             # Edit existing template in $EDITOR
crv template show <TEMPLATE>             # Show full details
crv template list                        # List all templates
crv template delete <TEMPLATE>           # Delete a template
crv template instantiate <TEMPLATE> <VM> # Create a VM from the template
```

`TEMPLATE` accepts a name or numeric ID. `VM` is the name for the new VM.

### Interactive Create

When `crv template create` is called without a file argument, the client writes a skeleton template to a temporary `.yml` file and opens `$EDITOR` (falling back to `vi` if unset). After saving and exiting the editor, the contents are sent to the server. If the editor exits with an error, nothing is created.

### Editing

`crv template edit <TEMPLATE>` fetches the current template definition from the server, converts it to the create-compatible YAML format, and opens it in `$EDITOR`. On save, the edited YAML is sent back as an atomic update: the old template rows are deleted and new ones inserted in a single database transaction.

- **Rename**: changing the `name` field renames the template. If the new name is already taken by another template, the update fails and the original template is left intact.
- **No changes**: if the file is saved without modifications, no RPC is sent.

### Instantiation

`crv template instantiate <TEMPLATE> <VM>` creates a new VM with the template's settings. Each drive is handled according to its strategy (see [Drive Strategies](#drive-strategies)). Network interfaces get fresh MAC addresses. SSH keys and cloud-init configuration are copied to the new VM.

---

## YAML Schema

A template is defined by a single YAML document. The same schema is accepted by `crv template create`, `crv template edit`, and the `templates:` section in [apply configuration](apply-configuration.md).

### Top-Level Fields

| Field | Type | Required | Default | Description |
|-------|------|----------|---------|-------------|
| `name` | string | yes | | Unique template name. Cannot be all digits. |
| `cpuCount` | integer | yes | | Number of virtual CPUs. |
| `ramMb` | integer | yes | | RAM in megabytes. |
| `description` | string | no | | Free-form description. |
| `headless` | bool | no | `false` | `true` = serial console only; `false` = SPICE graphics. |
| `cloudInit` | bool | no | `false` | Enable cloud-init ISO generation for instantiated VMs. |
| `guestAgent` | bool | no | `false` | Enable QEMU guest agent on instantiated VMs. |
| `autostart` | bool | no | `false` | Auto-start instantiated VMs when the daemon starts. |
| `cloudInitConfig` | object | no | | Custom cloud-init settings (see below). |
| `drives` | list | yes | | Drive definitions (may be empty). |
| `networkInterfaces` | list | no | `[]` | Network interface definitions. |
| `sshKeys` | list | no | `[]` | SSH key references. Requires `cloudInit: true`. |

### Drive Fields

Each entry in `drives` configures a disk that will be attached to VMs instantiated from this template.

| Field | Type | Required | Default | Description |
|-------|------|----------|---------|-------------|
| `diskImageName` | string | depends | | Name of an existing disk image. Required for `clone`, `overlay`, and `direct` strategies. Optional for `create` (used as a naming suffix). |
| `interface` | enum | yes | | Bus type: `virtio`, `ide`, `scsi`, `sata`, `nvme`, `pflash`. |
| `strategy` | enum | yes | | How the drive is provisioned on instantiation (see below). |
| `sizeMb` | integer | no | | For `create`: the new disk size. For `clone`/`overlay`: resize after provisioning. |
| `format` | enum | no | | Disk format. Required for `create`: `qcow2`, `raw`, `vmdk`, `vdi`, `vpc`, `vhdx`. |
| `media` | enum | no | | `disk` or `cdrom`. |
| `readOnly` | bool | no | `false` | Attach as read-only. |
| `cacheType` | enum | no | `none` | Cache mode: `none`, `writeback`, `writethrough`, `directsync`, `unsafe`. |
| `discard` | bool | no | `false` | Enable discard/TRIM support. |

### Drive Strategies

The `strategy` field controls what happens to the drive when a VM is instantiated from the template.

| Strategy | Requires `diskImageName` | Disk operation | Typical use case |
|----------|--------------------------|----------------|------------------|
| **`overlay`** | yes | Creates a qcow2 copy-on-write overlay backed by the named disk. The original is not modified. | Root disks — each VM gets a thin overlay on a shared base image. |
| **`clone`** | yes | Full copy of the named disk (all data + snapshots). | UEFI OVMF variables — each VM needs its own writable copy. |
| **`direct`** | yes | Attaches the named disk directly (no copy). | Shared read-only firmware images (e.g., OVMF_CODE.fd). |
| **`create`** | no | Creates a new empty disk with the given `format` and `sizeMb`. | Data disks, scratch space. |

**Naming convention for new disks**: clone and overlay produce disks named `<vm-name>-<diskImageName>` (overlay adds an `-overlay` suffix). The create strategy names the disk `<vm-name>-<diskImageName>` when `diskImageName` is provided, or `<vm-name>-disk` when omitted.

**Resize**: when `sizeMb` is set on a `clone` or `overlay` drive, the new disk is resized after creation. For `create`, `sizeMb` is the initial disk size.

### Network Interface Fields

| Field | Type | Required | Default | Description |
|-------|------|----------|---------|-------------|
| `type` | enum | yes | | Interface type: `user`, `managed`, `bridge`, `tap`, `vde`, `macvtap`. |
| `hostDevice` | string | no | | Host device name. Meaning depends on type (e.g., bridge name for `bridge`). |

Each instantiated VM gets a fresh random MAC address per interface.

### SSH Key Fields

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `name` | string | yes | Name of an SSH key that must already exist in the database (or be defined earlier in an apply file). |

SSH keys require `cloudInit: true` on the template. At instantiation, keys are attached to the VM and injected into the cloud-init ISO.

### Cloud-Init Configuration

The optional `cloudInitConfig` section customizes the NoCloud ISO generated for instantiated VMs. See [cloud-init.md](cloud-init.md) for the full cloud-init reference.

| Field | Type | Default | Description |
|-------|------|---------|-------------|
| `userData` | string or YAML | | Cloud-init user-data. See below for format. |
| `networkConfig` | string or YAML | | Cloud-init network-config (v2 format). |
| `injectSshKeys` | bool | `true` | Merge the VM's SSH keys into the first user in user-data. |

**User-data format**: the `userData` field accepts either structured YAML (parsed as a cloud-config document) or a raw string. Raw strings are useful for scripts — for example, PowerShell scripts for cloudbase-init on Windows:

```yaml
cloudInitConfig:
  userData: |
    #ps1_sysnative
    net user Administrator "Password123!" /y
    Install-WindowsFeature -Name RDS-RD-Server
  injectSshKeys: false
```

Structured YAML is serialized to text for storage and included verbatim in the NoCloud ISO:

```yaml
cloudInitConfig:
  userData:
    users:
      - name: deploy
        sudo: "ALL=(ALL) NOPASSWD:ALL"
    packages:
      - nginx
  injectSshKeys: true
```

---

## Apply Configuration Integration

Templates can be defined in the `templates:` top-level section of an [apply configuration](apply-configuration.md) file. They use the same YAML schema as standalone template files — definitions can be moved between the two formats without changes.

```yaml
disks:
  - name: base-image
    import: "https://example.com/image.qcow2"

templates:
  - name: webserver
    cpuCount: 2
    ramMb: 2048
    guestAgent: true
    drives:
      - diskImageName: base-image
        interface: virtio
        strategy: overlay
```

### Execution Order

Templates are processed in **Phase 5** of the apply pipeline, after all other resources:

1. SSH keys
2. Disk images
3. Virtual networks
4. VMs
5. **Templates**

This means templates can reference SSH keys and disks defined earlier in the same file by name.

### Skip Existing

With `crv apply --skip-existing`, templates whose names already exist in the database are silently skipped and their IDs are reused. This makes apply files idempotent.

### Validation

Before execution, the apply subsystem validates:

- No duplicate template names within the file.
- Template names are non-empty and not all-digit (to avoid ambiguity with numeric IDs).
- Drive references (`diskImageName`) for non-create strategies point to disks that exist in the database or are defined earlier in the same file.
- SSH keys referenced by templates exist.
- Templates with `sshKeys` have `cloudInit: true`.

---

## Examples

### Minimal Template

```yaml
name: scratch-vm
cpuCount: 1
ramMb: 512
drives:
  - interface: virtio
    strategy: create
    format: qcow2
    sizeMb: 4096
```

### UEFI Template with Overlay

```yaml
name: uefi-linux
description: "UEFI-booted Linux with overlay root disk"
cpuCount: 2
ramMb: 2048
headless: true
guestAgent: true
cloudInit: true
drives:
  # Shared OVMF firmware code (read-only, never copied)
  - diskImageName: ovmf-code
    interface: pflash
    readOnly: true
    strategy: direct

  # Per-VM OVMF variables (each VM gets its own copy)
  - diskImageName: ovmf-vars-template
    interface: pflash
    strategy: clone

  # Root disk overlay on a shared base image
  - diskImageName: alpine-base
    interface: virtio
    strategy: overlay
    sizeMb: 20480
    cacheType: writeback
    discard: true

networkInterfaces:
  - type: user

sshKeys:
  - name: admin-key
```

### Windows Template with Cloud-Init

```yaml
name: windows-server-2025
description: "Windows Server 2025 with RDP enabled"
cpuCount: 4
ramMb: 4096
headless: true
guestAgent: true
cloudInit: true
cloudInitConfig:
  userData: |
    #ps1_sysnative
    net user Administrator "corvus" /y
    Install-WindowsFeature -Name RDS-RD-Server
    Set-ItemProperty -Path 'HKLM:\System\CurrentControlSet\Control\Terminal Server' -Name fDenyTSConnections -Value 0
    Enable-NetFirewallRule -DisplayGroup "Remote Desktop"
    Set-Service -Name TermService -StartupType Automatic
    Start-Service TermService
  injectSshKeys: false
drives:
  - diskImageName: ovmf-code
    interface: pflash
    readOnly: true
    strategy: direct
  - diskImageName: ovmf-vars-template
    interface: pflash
    strategy: clone
  - diskImageName: ws25-base
    interface: virtio
    strategy: overlay
    sizeMb: 51200
networkInterfaces:
  - type: user
```

### Template with Mixed Drive Strategies

```yaml
name: dev-workstation
cpuCount: 4
ramMb: 8192
drives:
  # OS root — overlay on shared base
  - diskImageName: ubuntu-base
    interface: virtio
    strategy: overlay
    sizeMb: 40960

  # Empty data disk — created fresh per VM
  - diskImageName: data
    interface: virtio
    strategy: create
    format: qcow2
    sizeMb: 102400

  # Shared ISO (not copied)
  - diskImageName: tools-iso
    interface: ide
    media: cdrom
    readOnly: true
    strategy: direct

networkInterfaces:
  - type: user
```

### Template in Apply File

```yaml
sshKeys:
  - name: deploy-key
    publicKey: "ssh-ed25519 AAAA... deploy@host"

disks:
  - name: alpine-base
    import: "https://dl-cdn.alpinelinux.org/alpine/v3.20/releases/cloud/nocloud_alpine-3.20.6-x86_64-bios-cloudinit-r0.qcow2"

templates:
  - name: alpine-worker
    cpuCount: 1
    ramMb: 1024
    guestAgent: true
    cloudInit: true
    drives:
      - diskImageName: alpine-base
        interface: virtio
        strategy: overlay
        sizeMb: 10240
    networkInterfaces:
      - type: user
    sshKeys:
      - name: deploy-key
```

After applying, instantiate as many workers as needed:

```bash
crv template instantiate alpine-worker worker-01
crv template instantiate alpine-worker worker-02
crv vm start worker-01
crv vm start worker-02
```
