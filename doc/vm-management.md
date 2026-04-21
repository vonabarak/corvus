# VM Management

## Commands

```bash
crv vm list                       # List all VMs
crv vm show <vm>                  # Show VM details (drives, network, sockets)
crv vm create <name> <cpus> <ram> # Create a VM
crv vm edit <vm>                  # Edit VM settings
crv vm delete <vm>                # Delete a VM
crv vm start <vm>                 # Start a stopped/paused VM
crv vm stop <vm>                  # Graceful shutdown
crv vm pause <vm>                 # Pause execution
crv vm reset <vm>                 # Force stop (SIGKILL)
crv vm view <vm>                  # Open SPICE viewer or serial console
crv vm monitor <vm>               # Connect to HMP monitor
```

`<vm>` accepts a name or numeric ID.

## Creating a VM

```bash
crv vm create my-vm 2 2048                    # 2 CPUs, 2 GB RAM
crv vm create my-vm 4 4096 --headless         # Serial console only
crv vm create my-vm 2 2048 --cloud-init       # Enable cloud-init
crv vm create my-vm 2 2048 --guest-agent      # Enable guest agent
crv vm create my-vm 2 2048 --autostart        # Auto-start on daemon startup
crv vm create my-vm 2 2048 -d "Web server"    # With description
```

A newly created VM has no disks or network interfaces. Attach them with `crv disk attach` and `crv net-if add` before starting.

## Editing a VM

The VM must be stopped to edit.

```bash
crv vm edit my-vm --cpus 4                    # Change CPU count
crv vm edit my-vm --ram 8192                  # Change RAM
crv vm edit my-vm --cloud-init true           # Enable cloud-init
crv vm edit my-vm --guest-agent true          # Enable guest agent
crv vm edit my-vm --headless true             # Switch to serial console
crv vm edit my-vm --autostart true            # Enable autostart
crv vm edit my-vm --description "New desc"    # Update description
```

## Starting and Stopping

```bash
crv vm start my-vm          # Start (async by default)
crv vm start my-vm --wait   # Wait until running
crv vm stop my-vm           # Graceful shutdown via QMP
crv vm stop my-vm --wait    # Wait until stopped
crv vm pause my-vm          # Suspend execution
crv vm start my-vm          # Resume paused VM
crv vm reset my-vm          # Force stop (any state -> stopped)
```

### State Machine

VMs follow a strict state machine:

| Current State | Start | Stop | Pause | Reset |
|---------------|-------|------|-------|-------|
| **stopped** | running/starting | error | error | stopped |
| **starting** | error | stopping | error | stopped |
| **running** | error | stopping | paused | stopped |
| **stopping** | error | error | error | stopped |
| **paused** | running | error | error | stopped |
| **error** | error | error | error | stopped |

VMs with `guestAgent: true` transition through a `starting` state and become `running` once the first guest agent health check succeeds. VMs without the guest agent go directly to `running`.

`reset` always returns the VM to `stopped` regardless of current state.

## Deleting a VM

```bash
crv vm delete my-vm                # Delete VM record only
crv vm delete my-vm --delete-disks # Also delete all attached disk images
```

The VM must be stopped before deletion. With `--delete-disks`, all disk images attached to the VM are also removed from the database and filesystem.

## Display and Console

### SPICE (graphical VMs)

```bash
crv vm view my-vm     # Requests a short-lived SPICE grant and opens remote-viewer
```

Non-headless VMs expose SPICE over **TCP with ticketed password authentication**, so `crv vm view` works from another host. Each invocation asks the daemon for a fresh password, which is installed via QMP `set_password` and expires after 120 seconds; unused grants disappear on their own. The client writes a chmod-600 `.vv` file and passes its path to `remote-viewer` — the password never appears on the command line.

The daemon picks a TCP port in the range `5900-5999` (configurable via `qcSpicePortMin` / `qcSpicePortMax`). The bind address defaults to the daemon's RPC listen host: Unix-socket daemons keep SPICE on `127.0.0.1`, TCP daemons expose it on the same interface as RPC. Use `--spice-bind ADDR` to decouple the two.

Requires `remote-viewer` (from `virt-viewer` package) installed on the client host.

For machine-readable output, `crv --output json vm view my-vm` emits the grant as JSON (`host`, `port`, `password`, `ttl_seconds`) and exits without launching a viewer.

### Serial Console (headless VMs)

```bash
crv vm view my-vm     # Connects to serial console for headless VMs
```

The daemon maintains a 1 MB ring buffer per headless VM. Reconnecting replays recent output (boot messages, login prompts) instead of showing a blank screen.

The serial console uses raw terminal mode with an escape prefix:

| Sequence | Action |
|----------|--------|
| `Ctrl+]` `q` | Quit the console session |
| `Ctrl+]` `d` | Send Ctrl+Alt+Del to the VM |
| `Ctrl+]` `f` | Flush (clear) the ring buffer |
| `Ctrl+]` `Ctrl+]` | Send a literal `Ctrl+]` to the VM |
| `Ctrl+]` `?` | Show help |

The buffer persists across guest reboots (the QEMU process stays alive) and is cleaned up when the VM stops.

**Guest setup**: the guest OS must output to the serial port. For Linux, add `console=ttyS0,115200n8` to the kernel command line and enable a serial getty (e.g., `systemctl enable serial-getty@ttyS0`). UEFI firmware menus also work over serial when no VGA device is present.

### HMP Monitor

```bash
crv vm monitor my-vm   # Connect to QEMU Human Monitor Protocol
```

Works for every running VM (headless or graphical) and over any RPC transport — the daemon holds the HMP chardev and relays bytes to the client through the RPC socket, the same way the headless serial console does.

Escape prefix:

| Sequence | Action |
|----------|--------|
| `Ctrl+]` `q` | Quit the monitor session |
| `Ctrl+]` `f` | Flush the HMP scrollback ring buffer |
| `Ctrl+]` `Ctrl+]` | Send a literal `Ctrl+]` |
| `Ctrl+]` `?` | Show help |

The daemon keeps a 64 KiB ring buffer per running VM so that reconnecting clients see the last prompt/response pair.

## Output Formats

```bash
crv vm list -o json     # JSON output
crv vm list -o yaml     # YAML output
crv vm show my-vm -o json
```

All commands support `--output json` and `--output yaml` for machine-readable output.
