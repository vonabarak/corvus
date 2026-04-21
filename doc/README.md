![Corvus logo](corvus.png)
# Corvus

A lightweight QEMU/KVM virtual machine management daemon written in Haskell.

Corvus provides a daemon (`corvus`) that manages VM lifecycle and a CLI client (`crv`) for interacting with it. VMs are defined in a PostgreSQL database and executed via QEMU with KVM acceleration.

## Features

- **VM Lifecycle Management**: Start, stop, pause, and reset virtual machines
- **State Machine**: Enforces valid state transitions (stopped -> running -> paused, etc.)
- **Disk Image Management**: Create, resize, clone, overlay, rebase, and attach qcow2/raw/vmdk/vdi disk images; import from local path or HTTP URL
- **Snapshot Support**: Create, rollback, merge, and delete qcow2 snapshots
- **Cloud-Init Integration**: Optional per-VM cloud-init with SSH key injection via NoCloud datasource; custom user-data and network-config per VM; lazy ISO generation on first SSH key attach or VM start
- **QEMU Guest Agent**: Execute commands in guests, periodic health checks and network address discovery via QGA protocol
- **SPICE Display**: Remote desktop access via SPICE protocol
- **Serial Console**: Buffered serial console for headless VMs with scrollback replay on reconnect
- **VM Templates**: Define VM blueprints in YAML and instantiate them easily
- **Declarative Apply**: Define entire environments (VMs, disks, networks, SSH keys) in a single YAML file with `crv apply`
- **Virtual Networks**: Bridge-based virtual networks with dnsmasq DHCP/DNS, running in an unprivileged user namespace
- **Shared Directories**: virtiofs support for sharing host directories with guests
- **Task History**: Every mutating operation is tracked with timestamps, results, and error messages

## Installation

### Prerequisites

- GHC 9.x and Stack
- PostgreSQL
- QEMU with KVM support
- `qemu-img` (for disk image operations)
- `virtiofsd` (for shared directories)
- `genisoimage` or `mkisofs` (for cloud-init ISO generation)
- `vde_switch` and `dnsmasq` (optional, for virtual networking)
- `curl` or `wget` (optional, for HTTP disk image import)
- `remote-viewer` (optional, for `vm view` command)

### Binary Releases

Pre-built Linux x86_64 binaries are available on GitHub:

https://github.com/vonabarak/corvus/releases

Download the tarball for your version and extract `corvus` and `crv` to a directory in your `$PATH` (e.g., `~/.local/bin/`).

### Gentoo

An ebuild is available in the [vonabarak/gentoo](https://github.com/vonabarak/gentoo) overlay:

```bash
# Add the overlay (using eselect-repository)
eselect repository add vonabarak git https://github.com/vonabarak/gentoo.git
emaint sync -r vonabarak

# Install
emerge app-emulation/corvus-bin
```

The `corvus-bin` package installs pre-built binaries from the GitHub releases above.

### Building from Source

```bash
make build
make install  # Installs to ~/.local/bin/ and sets up systemd user service
```

### Database Setup

```bash
createdb corvus
# The daemon runs migrations automatically on startup
```

## Quick Start

### Starting the Daemon

```bash
# Run directly
corvus --database postgresql://localhost/corvus
```

A systemd user service file (`corvus.service`) is included in the release tarball and installed automatically by the Gentoo ebuild and `make install`. To use it:

```bash
# Edit the database connection string if needed
systemctl --user edit corvus

# Enable and start
systemctl --user enable --now corvus

# Check status / logs
systemctl --user status corvus
journalctl --user -u corvus -f
```

The service listens on `$XDG_RUNTIME_DIR/corvus/corvus.sock` by default and restarts automatically on failure.

### Creating Your First VM

```bash
# Import a cloud image
crv disk import alpine https://dl-cdn.alpinelinux.org/alpine/v3.20/releases/cloud/nocloud_alpine-3.20.6-x86_64-bios-cloudinit-r0.qcow2 --wait

# Create an overlay (thin copy-on-write)
crv disk overlay my-root alpine

# Create the VM
crv vm create my-vm 2 2048 --cloud-init --guest-agent

# Attach the disk and a network interface
crv disk attach my-vm my-root --interface virtio
crv net-if add my-vm --type user

# Add an SSH key
crv ssh-key create admin "$(cat ~/.ssh/id_ed25519.pub)"
crv ssh-key attach my-vm admin

# Start the VM
crv vm start my-vm --wait
```

### Connection Options

```bash
# Unix socket (default)
crv vm list

# Custom socket path
crv --socket /tmp/corvus.sock vm list

# TCP connection
crv --tcp --host 127.0.0.1 --port 9876 vm list
```

### Network Transparency

Every `crv` command — including `vm view`, `vm monitor`, `vm exec`, `serial console`, and `apply` — runs over the daemon's RPC socket, so pointing `crv` at a daemon on another host with `--tcp --host <ip>` Just Works. The daemon relays serial and HMP I/O through the RPC connection using per-VM ring buffers; `vm view` for graphical VMs returns a short-lived SPICE host/port/password grant that `remote-viewer` on the client uses. Disk registration, import, and shared-directory paths are always interpreted on the daemon host (they refer to the daemon's filesystem).

**The one exception is `crv ns`**, which enters the daemon's network namespace via `nsenter` against `/proc/<pid>/ns/*` on the local machine. It only works on the daemon host, exists for debugging virtual networks, and **should not be used in production** — it's a troubleshooting tool, not part of the management surface.

The RPC socket has no built-in authentication, so binding the daemon to a non-loopback address exposes it to anyone who can reach that port. Restrict access with firewall rules, a VPN, or a reverse proxy with auth.

### Output Formats

All commands support `--output json` and `--output yaml` for machine-readable output:

```bash
crv vm list -o json
crv disk show my-root -o yaml
```

### Shell Completion

```bash
# Zsh
crv --zsh-completion-script $(which crv) > ~/.local/share/zsh/site-functions/_crv

# Bash
crv --bash-completion-script $(which crv) > ~/.local/share/bash-completion/completions/crv

# Fish
crv --fish-completion-script $(which crv) > ~/.config/fish/completions/crv.fish
```

## Documentation

See [doc/INDEX.md](INDEX.md) for the full documentation index.

| Topic | File |
|-------|------|
| VM lifecycle, state machine, console | [vm-management.md](vm-management.md) |
| Disk create, import, overlay, clone, attach | [disk-management.md](disk-management.md) |
| qcow2 snapshots | [snapshots.md](snapshots.md) |
| Virtual networks, interfaces, port forwarding | [networking.md](networking.md) |
| SSH key management | [ssh-keys.md](ssh-keys.md) |
| Cloud-init ISO, custom config, Windows | [cloud-init.md](cloud-init.md) |
| virtiofs shared directories | [shared-directories.md](shared-directories.md) |
| Guest agent command execution | [guest-exec.md](guest-exec.md) |
| Async operation tracking | [task-history.md](task-history.md) |
| Reusable VM templates | [templates.md](templates.md) |
| Declarative environments | [apply-configuration.md](apply-configuration.md) |

## Daemon Commands

```bash
crv ping             # Ping the daemon
crv status           # Get daemon status (uptime, connections, version)
crv shutdown         # Request daemon shutdown
```

## Development

### Building and Testing

```bash
make build                   # Build
make all-tests               # Run all tests
make unit-tests              # Unit tests only
make integration-tests       # Integration tests (requires QEMU/KVM)
make test MATCH="pattern"    # Run specific tests
make lint                    # HLint
make format                  # Fourmolu
```

### Test Images

Integration tests require VM images. Build them with:

```bash
make test-image              # Build both Alpine Linux and Windows Server images
make test-image-alpine       # Alpine Linux only
make test-image-windows      # Windows Server 2025 only (downloads evaluation ISO)
```

The Alpine image is a cloud-init-enabled qcow2 built via QEMU. The Windows image is built from a Microsoft evaluation ISO with cloudbase-init and VirtIO drivers pre-installed. These are the same images referenced by `doc/apply-examples/test-images.yml`.

### Running Locally

```bash
stack exec corvus -- -d postgresql://localhost/corvus_test --log-level debug
stack exec crv -- vm list
```

## Architecture

### Communication Protocol

Client and daemon communicate over TCP or Unix socket using a binary protocol:

1. **Version byte**: protocol version (for compatibility checks)
2. **Length prefix**: 8-byte big-endian integer
3. **Payload**: Binary-encoded request/response via `Data.Binary`

Most commands follow a simple request/response pattern. The serial console (`crv vm view` on headless VMs) is an exception: after the initial `ReqSerialConsole` / `RespSerialConsoleOk` handshake, the connection switches to **raw byte streaming** — the framed binary protocol is abandoned and both sides relay bytes directly between the client terminal and the VM's serial socket. The connection stays in streaming mode until the client disconnects (via the `Ctrl+]` escape sequence).

### Process Management

When a VM starts:
1. Status set to `running` (or `starting` with guest agent) in database
2. virtiofsd processes started for shared directories (if any)
3. Cloud-init ISO generated (if cloud-init enabled)
4. QEMU spawned in a background thread
5. PID saved to database
6. Background thread waits for process exit
7. On exit: status updated to `stopped` (success) or `error` (failure)

Graceful shutdown uses QMP `system_powerdown`. Force stop uses `SIGKILL`.

### Runtime Files

Sockets and runtime files are stored in `$XDG_RUNTIME_DIR/corvus/<vm_id>/`:
- `monitor.sock` -- HMP monitor socket
- `qmp.sock` -- QMP control socket
- `serial.sock` -- Serial console socket (headless VMs)
- `qga.sock` -- QEMU Guest Agent socket
- `virtiofsd-<tag>.sock` -- virtiofsd sockets

SPICE is served over TCP (not Unix socket) with ticketed password authentication — see [vm-management.md](vm-management.md#spice-graphical-vms).

## Limitations

- **Single Host Only**: No clustering or migration support
- **No Live Migration**: VMs cannot be migrated between hosts
- **No Live Snapshots**: Snapshots require VM to be stopped (qcow2 internal snapshots)
- **No Resource Limits**: No CPU/memory cgroup controls
- **No VNC**: Only SPICE display is supported
- **No Authentication**: Client-daemon communication is unauthenticated
- **No TLS**: TCP communication is unencrypted (use Unix socket or SSH tunnel)
- **Linux Only**: Relies on KVM, Unix sockets, and POSIX signals

## License

BSD-3-Clause
