# Corvus

A lightweight QEMU/KVM virtual machine management daemon written in Haskell.

Corvus provides a daemon (`corvus`) that manages VM lifecycle and a CLI client (`crv`) for interacting with it. VMs are defined in a PostgreSQL database and executed via QEMU with KVM acceleration.

## Features

- **VM Lifecycle Management**: Start, stop, pause, and reset virtual machines
- **State Machine**: Enforces valid state transitions (stopped → running → paused, etc.)
- **Disk Image Management**: Create, resize, and attach qcow2/raw/vmdk/vdi disk images
- **Snapshot Support**: Create, rollback, merge, and delete qcow2 snapshots
- **Cloud-Init Integration**: SSH key injection via NoCloud datasource
- **SPICE Display**: Remote desktop access via SPICE protocol
- **QMP Integration**: Graceful shutdown and pause via QEMU Machine Protocol
- **Shared Directories**: virtiofs support for sharing host directories with guests
- **Multiple Storage Types**: IDE, SATA, VirtIO, NVMe, and pflash drives
- **Networking**: VirtIO-net with bridge, TAP, and VDE support

## Installation

### Prerequisites

- GHC 9.x and Stack
- PostgreSQL
- QEMU with KVM support
- `qemu-img` (for disk image operations)
- `virtiofsd` (for shared directories)
- `genisoimage` or `mkisofs` (for cloud-init ISO generation)
- `remote-viewer` (optional, for `vm view` command)

### Building

```bash
stack build
stack install  # Installs to ~/.local/bin/
```

### Database Setup

```bash
createdb corvus
# The daemon runs migrations automatically on startup
```

## Usage

### Starting the Daemon

```bash
# Run with Unix socket (default: $XDG_RUNTIME_DIR/corvus/corvus.sock)
corvus --database postgresql://localhost/corvus

# Custom socket path
corvus --socket /tmp/corvus.sock --database postgresql://localhost/corvus

# Run with TCP instead
corvus --tcp --host 127.0.0.1 --port 9876 --database postgresql://localhost/corvus

# Or install as a user service
mkdir -p ~/.config/systemd/user
cp corvus.service ~/.config/systemd/user/
systemctl --user daemon-reload
systemctl --user enable --now corvus
```

### Client Commands

#### Daemon Commands

```bash
crv ping             # Ping the daemon
crv status           # Get daemon status (uptime, running VMs)
crv shutdown         # Request daemon shutdown
```

#### VM Commands

```bash
crv vm list          # List all VMs
crv vm show <id>     # Show VM details (drives, network, sockets)
crv vm start <id>    # Start a stopped/paused VM
crv vm stop <id>     # Graceful shutdown (via QMP)
crv vm pause <id>    # Pause execution
crv vm reset <id>    # Force stop (SIGKILL)
crv vm view <id>     # Open SPICE viewer (remote-viewer)
crv vm monitor <id>  # Connect to HMP monitor (Ctrl+] to exit)
```

#### Disk Commands

```bash
# Create a new disk image
crv disk create <name> --size <size> [--format <format>]
crv disk create myvm-boot --size 20G --format qcow2
crv disk create data-disk --size 100G -f raw

# Import an existing disk image
crv disk import <name> <path> [--format <format>]
crv disk import debian-base ~/VMs/debian.qcow2
crv disk import windows-iso /data/isos/windows.iso -f raw

# Manage disk images
crv disk list                    # List all disk images
crv disk show <disk_id>          # Show disk details
crv disk delete <disk_id>        # Delete a disk image
crv disk resize <disk_id> -s <size>  # Resize disk (VM must be stopped)

# Attach/detach disks to VMs
crv disk attach <vm_id> <disk_id> [-i <interface>] [-m <media>]
crv disk attach 1 2 --interface virtio
crv disk attach 1 3 -i ide -m cdrom
crv disk detach <vm_id> <drive_id>
```

Supported formats: `qcow2`, `raw`, `vmdk`, `vdi` (auto-detected from extension)
Supported interfaces: `virtio`, `ide`, `scsi`, `sata`, `nvme`
Supported media: `disk`, `cdrom`

Note: When importing, if the disk is located in the base images directory (`$HOME/VMs`), a relative path is stored for portability.

#### Snapshot Commands

Snapshots are supported for qcow2 disk images only. VM must be stopped for rollback/merge operations.

```bash
crv snapshot create <disk_id> <name>      # Create a snapshot
crv snapshot list <disk_id>               # List snapshots for a disk
crv snapshot rollback <disk_id> <snap_id> # Rollback to a snapshot
crv snapshot merge <disk_id> <snap_id>    # Merge snapshot into base
crv snapshot delete <disk_id> <snap_id>   # Delete a snapshot
```

#### SSH Key Commands

SSH keys can be attached to VMs for cloud-init configuration. When SSH keys are attached, a cloud-init ISO is automatically generated and attached to the VM.

```bash
# Manage SSH keys
crv ssh-key create <name> "<public_key>"  # Create a new SSH key
crv ssh-key list                          # List all SSH keys
crv ssh-key delete <key_id>               # Delete an SSH key

# Attach/detach keys to VMs
crv ssh-key attach <vm_id> <key_id>       # Attach key to VM (regenerates cloud-init ISO)
crv ssh-key detach <vm_id> <key_id>       # Detach key from VM
crv ssh-key list-vm <vm_id>               # List keys attached to a VM
```

### Connection Options

```bash
# Unix socket (default: $XDG_RUNTIME_DIR/corvus/corvus.sock)
crv vm list

# Custom socket path
crv --socket /tmp/corvus.sock vm list

# TCP connection
crv --tcp --host 127.0.0.1 --port 9876 vm list
```

### Shell Completion

`crv` supports generating shell completion scripts via `optparse-applicative`. Completions are derived directly from the parser definitions, so they always reflect the current set of commands and options.

```bash
# Zsh — generate and install completion
crv --zsh-completion-script $(which crv) > ~/.local/share/zsh/site-functions/_crv

# Bash
crv --bash-completion-script $(which crv) > ~/.local/share/bash-completion/completions/crv

# Fish
crv --fish-completion-script $(which crv) > ~/.config/fish/completions/crv.fish
```

For a quick one-off test without installing:

```bash
source <(crv --zsh-completion-script $(which crv))
```

## Development

### Project Structure

```
corvus/
├── app/
│   ├── client/Main.hs      # CLI client entry point
│   └── daemon/Main.hs      # Daemon entry point
├── src/Corvus/
│   ├── Model.hs            # Database schema (Persistent)
│   ├── Protocol.hs         # RPC message types (Binary)
│   ├── Types.hs            # Shared daemon types
│   ├── Server.hs           # Network communication
│   ├── CloudInit.hs        # Cloud-init ISO generation
│   ├── Handlers/
│   │   ├── Core.hs         # Ping, status, shutdown handlers
│   │   ├── Vm.hs           # VM lifecycle handlers
│   │   ├── Disk.hs         # Disk image handlers
│   │   ├── SharedDir.hs    # Shared directory handlers
│   │   ├── NetIf.hs        # Network interface handlers
│   │   └── SshKey.hs       # SSH key handlers
│   ├── Client/
│   │   ├── Commands.hs     # Command execution
│   │   ├── Connection.hs   # Socket handling
│   │   ├── Parser.hs       # CLI argument parsing
│   │   ├── Rpc.hs          # RPC call wrappers
│   │   └── Types.hs        # CLI types
│   └── Qemu/
│       ├── Command.hs      # QEMU command-line builder
│       ├── Config.hs       # QEMU configuration
│       ├── Process.hs      # Process spawning
│       ├── Qmp.hs          # QMP protocol client
│       ├── QmpQQ.hs        # QMP quasi-quoter
│       ├── Runtime.hs      # Runtime directories/sockets
│       ├── Image.hs        # qemu-img operations
│       └── Virtiofsd.hs    # virtiofsd management
├── test/                   # Integration and unit tests
├── package.yaml
├── stack.yaml
└── corvus.service          # Systemd user service
```

### Building and Testing

```bash
# Build
stack build

# Build with file watching
stack build --file-watch

# Run tests
stack test

# Run daemon locally
stack run corvus -- --host 127.0.0.1 --port 9876 --database postgresql://localhost/corvus

# Run client
stack run crv -- vm list
```

### Code Quality

```bash
# Linting
~/.local/bin/hlint src app

# Formatting
~/.local/bin/ormolu --mode inplace $(find src app -name '*.hs')
```

## Architecture

### Communication Protocol

Client and daemon communicate over TCP or Unix socket using a simple binary protocol:

1. **Length prefix**: 8-byte big-endian integer
2. **Payload**: Binary-encoded request/response

Messages are serialized using GHC's `Data.Binary` with auto-derived instances.

### VM State Machine

```
 stop/reset
┌───────────────────────┐
│                       │
▼         start         │
stopped ────────► running
▲                  │    ▲
│             pause│    │unpause
│stop/reset        ▼    │
└───────────────── paused
```

State transitions are validated server-side. The `reset` command forces any VM to `stopped` state.

### Process Management

When a VM starts:
1. Status set to `running` in database
2. virtiofsd processes started for shared directories (if any)
3. QEMU spawned in a background thread (non-daemonized)
4. PID saved to database
5. Background thread waits for process exit
6. On exit: status updated to `stopped` (success) or `error` (failure)

Graceful shutdown uses QMP `system_powerdown`. Force stop uses `SIGKILL`.

### Runtime Files

Sockets and runtime files are stored in `$XDG_RUNTIME_DIR/corvus/<vm_id>/`:
- `monitor.sock` - HMP monitor socket
- `qmp.sock` - QMP control socket
- `spice.sock` - SPICE display socket
- `virtiofsd-<tag>.sock` - virtiofsd sockets

### Database Schema

```sql
vm (id, name, created_at, status, cpu_count, ram_mb, description, pid)
disk_image (id, name, file_path, format, size_mb, created_at)
snapshot (id, disk_image_id, name, created_at, size_mb)
drive (id, vm_id, disk_image_id, interface, media, read_only, cache_type, discard)
network_interface (id, vm_id, interface_type, host_device, mac_address)
shared_dir (id, vm_id, path, tag, cache, read_only, pid)
ssh_key (id, name, public_key, created_at)
vm_ssh_key (id, vm_id, ssh_key_id)  -- Junction table for VM-SSH key associations
```

## Limitations

- **Single Host Only**: No clustering or migration support
- **No Live Migration**: VMs cannot be migrated between hosts
- **No Live Snapshots**: Snapshots require VM to be stopped (qcow2 internal snapshots supported)
- **No Resource Limits**: No CPU/memory cgroup controls
- **No VNC**: Only SPICE display is supported
- **No Network Management**: Host bridges/TAP devices must be configured manually
- **No Authentication**: Client-daemon communication is unauthenticated
- **No TLS**: TCP communication is unencrypted (use Unix socket or SSH tunnel)
- **Linux Only**: Relies on KVM, Unix sockets, and POSIX signals

## License

BSD-3-Clause

