# Corvus

A lightweight QEMU/KVM virtual machine management daemon written in Haskell.

Corvus provides a daemon (`corvus`) that manages VM lifecycle and a CLI client (`crv`) for interacting with it. VMs are defined in a PostgreSQL database and executed via QEMU with KVM acceleration.

## Features

- **VM Lifecycle Management**: Start, stop, pause, and reset virtual machines
- **State Machine**: Enforces valid state transitions (stopped → running → paused, etc.)
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
- `virtiofsd` (for shared directories)
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

```bash
# Daemon status
crv status
crv ping

# List all VMs
crv vm list

# Show VM details (drives, network interfaces, sockets)
crv vm show <id>

# VM lifecycle
crv vm start <id>    # Start a stopped/paused VM
crv vm stop <id>     # Graceful shutdown (via QMP)
crv vm pause <id>    # Pause execution
crv vm reset <id>    # Force stop (SIGKILL)

# Interactive access
crv vm view <id>     # Open SPICE viewer (remote-viewer)
crv vm monitor <id>  # Connect to HMP monitor (Ctrl+] to exit)

# Shutdown daemon
crv shutdown
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

## Development

### Project Structure

```
corvus/
├── app/
│   ├── client/Main.hs    # CLI client entry point
│   └── daemon/Main.hs    # Daemon entry point
├── src/Corvus/
│   ├── Model.hs          # Database schema (Persistent)
│   ├── Protocol.hs       # RPC message types (Binary)
│   ├── Types.hs          # Shared daemon types
│   ├── Server.hs         # Network communication
│   ├── Handlers/
│   │   ├── Core.hs       # Ping, status, shutdown handlers
│   │   └── Vm.hs         # VM lifecycle handlers
│   ├── Client/
│   │   ├── Commands.hs   # Command execution
│   │   ├── Connection.hs # Socket handling
│   │   ├── Parser.hs     # CLI argument parsing
│   │   └── Rpc.hs        # RPC call wrappers
│   └── Qemu/
│       ├── Command.hs    # QEMU command-line builder
│       ├── Config.hs     # QEMU configuration
│       ├── Process.hs    # Process spawning
│       ├── Qmp.hs        # QMP protocol client
│       ├── Runtime.hs    # Runtime directories/sockets
│       └── Virtiofsd.hs  # virtiofsd management
├── package.yaml
├── stack.yaml
└── corvus.service        # Systemd user service
```

### Building and Testing

```bash
# Build
stack build

# Build with file watching
stack build --file-watch

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
drive (id, vm_id, interface, file_path, format, media, read_only, cache, discard)
network_interface (id, vm_id, type, host_device, mac_address)
shared_dir (id, vm_id, path, tag, cache, read_only, pid)
```

## Limitations

- **Single Host Only**: No clustering or migration support
- **No Live Migration**: VMs cannot be migrated between hosts
- **No Snapshots**: Snapshot management not implemented
- **No Resource Limits**: No CPU/memory cgroup controls
- **No VNC**: Only SPICE display is supported
- **No Cloud-Init**: No automatic guest configuration
- **No Storage Pools**: Disk images must be pre-created
- **No Network Management**: Host bridges/TAP devices must be configured manually
- **No Authentication**: Client-daemon communication is unauthenticated
- **No TLS**: TCP communication is unencrypted (use Unix socket or SSH tunnel)
- **Linux Only**: Relies on KVM, Unix sockets, and POSIX signals

## License

BSD-3-Clause

