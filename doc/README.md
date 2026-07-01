![Corvus logo](corvus.png)
# Corvus

A lightweight QEMU/KVM virtual machine management daemon written in Haskell.

Corvus provides a daemon (`corvus`) that manages VM lifecycle and a CLI client (`crv`) for interacting with it. VM state is stored in SQLite by default, with PostgreSQL available for deployments that prefer a server database, and VMs run via QEMU with KVM acceleration.

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
- **Virtual Networks**: Bridge-based virtual networks with dnsmasq DHCP/DNS, owned by the privileged `corvus-netd` agent (host root netns)
- **Shared Directories**: virtiofs support for sharing host directories with guests
- **Task History**: Every mutating operation is tracked with timestamps, results, and error messages
- **Web Interface**: Browser-based dashboard, VM/disk/network management, in-browser serial console, and live task feed via the `corvus-web` gateway ([web-interface.md](web-interface.md))

## Installation

### Prerequisites

- GHC 9.x and Stack
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
make install  # Installs binaries to ~/.local/bin/ + pipx-installs corvus-admin
```

### Database Setup

The default database is SQLite. When the daemon starts without `--database`, it
uses `$XDG_DATA_HOME/corvus/corvus.db` or
`~/.local/share/corvus/corvus.db`, creates the parent directory if needed, and
runs migrations automatically.

To use PostgreSQL instead, create the database and pass a PostgreSQL URL:

```bash
createdb corvus
corvus --database postgresql://localhost/corvus
```

### Single-host setup (one command)

For a turn-key single-node install, run `corvus-admin quickstart` after `make install`. It generates the CA, mints every component cert, writes systemd unit files (user-mode for daemon and nodeagent, system-mode for netd), brings up the services, and registers the node with the daemon. sudo or doas is auto-detected; if neither is available, corvus-netd is skipped with a warning and the daemon + nodeagent come up without network management.

```bash
corvus-admin quickstart
```

### Multi-host or system-service setup (granular)

For multi-host deployments or when you want daemon/nodeagent as system services, see [doc/security.md](security.md) for the per-component flow. For dev / single-host use only, you can skip TLS entirely and pass `--no-tls` to every Corvus binary; Unix-socket connections (the default for `crv` ↔ daemon on the same host) never wrap with TLS regardless.

## Quick Start

### Starting the Daemon

```bash
# Run directly
corvus

# Or use PostgreSQL explicitly
corvus --database postgresql://localhost/corvus
```

After `corvus-admin quickstart` has run, the daemon's systemd unit is already enabled and started — you can manage it with the usual `systemctl --user` commands:

```bash
# Edit the database path or connection string if needed
systemctl --user edit corvus

# Enable and start
systemctl --user enable --now corvus

# Check status / logs
systemctl --user status corvus
journalctl --user -u corvus -f
```

The service listens on **both** `$XDG_RUNTIME_DIR/corvus/corvus.sock` and TCP `0.0.0.0:9876` by default, and restarts automatically on failure. Pass `--no-unix` or `--no-tcp` to disable either listener; `--socket PATH` overrides the Unix socket path, `--host HOST` / `--port PORT` override the TCP bind.

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
# TCP to 127.0.0.1:9876 (default)
crv vm list

# Custom TCP host / port — equivalently set CORVUS_HOST / CORVUS_PORT
crv --host daemon.example.com --port 9876 vm list
CORVUS_HOST=daemon.example.com crv vm list

# Unix socket (default path)
crv --unix vm list

# Custom Unix socket path — implies --unix; equivalently set CORVUS_SOCKET
crv --socket /tmp/corvus.sock vm list
```

### Network Transparency

Every `crv` command — including `vm view`, `vm monitor`, `vm exec`, `serial console`, and `apply` — runs over the daemon's RPC socket, so pointing `crv` at a daemon on another host with `--host <ip>` Just Works. The daemon relays serial and HMP I/O through the RPC connection using per-VM ring buffers; `vm view` for graphical VMs returns a short-lived SPICE host/port/password grant that `remote-viewer` on the client uses. Disk registration, import, and shared-directory paths are always interpreted on the daemon host (they refer to the daemon's filesystem).

The TCP listener requires mutual TLS — `corvus-admin deploy` provisions a per-host CA and per-client certificates, and the daemon rejects any peer whose CN doesn't begin with `corvus-client:`. Pass `--no-tls` on both the daemon and the client to bypass it in dev. The Unix socket relies on filesystem permissions, and TLS is skipped there even when configured.

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
make build                                # Build
make unit-tests                           # Haskell unit tests
make unit-tests MATCH="pattern"           # Filtered unit tests
make integration-tests                    # Pytest integration suite (requires nested KVM)
make integration-tests MATCH="pattern"    # Filtered integration tests
make lint                                 # HLint
make format                               # Fourmolu
```

### Test Images

Integration tests require VM images. Build them with:

```bash
make test-image              # Build all test images (node + vm + multi-os + windows)
make test-image-vm           # Alpine Linux inner-VM image only
make test-image-node         # Gentoo outer-node image only
make test-image-windows      # Windows Server 2025 only (downloads evaluation ISO)
```

Both images are built declaratively via `crv build` — see
[yaml/alpine-test/](../yaml/alpine-test/) and
[yaml/windows-server-2025/](../yaml/windows-server-2025/). The
artifacts are the same images referenced by
[yaml/test-images/test-images.yml](../yaml/test-images/test-images.yml).

### Running Locally

```bash
stack exec corvus -- --log-level debug
stack exec crv -- vm list
```

## Architecture

### Communication Protocol

Client and daemon communicate over TCP or Unix socket using
[Cap'n Proto](https://capnproto.org/) RPC. The wire schema lives in
[`schema/`](../schema/) and is the source of truth; see
[`doc/rpc-protocol.md`](rpc-protocol.md) for a full tour of the
capability tree (Daemon → managers → resources), the streaming
sinks (`ByteSink`, `BuildEventSink`, `GuestAgentStatusSink`,
`TaskProgressSink`), and a pycapnp client example.

Streaming flows use Cap'n Proto sinks rather than a protocol-upgrade
escape hatch: `crv vm view` on a headless VM, `crv vm monitor`,
and `crv build --wait` each hand the daemon a sink cap, the daemon
pushes events through it, and the client closes the relay by
dropping the sink (typically via the `Ctrl+]` escape sequence on
interactive consoles).

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

- **No Resource Limits**: No CPU/memory cgroup controls
- **No VNC**: Only SPICE display is supported
- **Linux Only**: Relies on KVM, Unix sockets, and POSIX signals

Multi-node clustering, cold VM migration, live snapshots with
optional guest-agent quiesce, and mutual-TLS authentication on the
TCP listener are all implemented today -- see
[multi-node.md](multi-node.md), [vm-migration.md](vm-migration.md),
[snapshots.md](snapshots.md), and [security.md](security.md) for the
specifics.

## License

BSD-3-Clause
