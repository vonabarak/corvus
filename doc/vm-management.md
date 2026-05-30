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
crv vm pause <vm>                 # Pause execution (in-RAM, not persistent)
crv vm save <vm>                  # Save running/paused state to disk and stop QEMU
crv vm reset <vm>                 # Force stop (SIGKILL); drops saved-state file
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
crv vm create my-vm 2 2048 --node alpha       # Pin to a specific node
crv vm create my-vm 2 2048 --cpu-model qemu64 # Migratable CPU model (see below)
```

### `--cpu-model` and cross-host migration

`--cpu-model` (default `host`) selects the QEMU `-cpu` model. The
default `host` passes through every feature of the underlying CPU —
fastest, but the saved-state stream encodes feature bits that the
destination CPU may not have, and `crv vm migrate` to a different
host will fail at the `KVM_SET_SREGS` restore step with `Invalid
argument`.

For VMs that may need to migrate across non-identical hosts, pick a
stable model:

- `qemu64` — lowest common denominator, works on every x86-64 host
- `Nehalem`, `Westmere-v3`, `Skylake-Client-v1`, … — named models
  that expose progressively newer feature sets, picked to be the
  intersection of CPUs in your fleet

The field is editable on a stopped VM:

```bash
crv vm edit my-vm --cpu-model qemu64
```

Takes effect on next start (the agent reads the field only when
spawning QEMU).

`--node` (alias `-n`) is optional. When omitted the daemon's
scheduler picks a node — filtered to `admin_state = online`,
scored by free RAM + free disk minus a small load penalty, and
broken on ties by node name. On a single-node install the
scheduler always picks the one node, so single-host operators
never need to think about it. See [multi-node.md](multi-node.md)
for the full algorithm and the same-node invariants this
imposes downstream (disk attach + managed-NIC binding).

A newly created VM has no disks or network interfaces. Attach them with `crv disk attach` and `crv net-if add` before starting.

## Editing a VM

The VM must be stopped to edit.

```bash
crv vm edit my-vm --cpus 4                    # Change CPU count
crv vm edit my-vm --ram 8192                  # Change RAM
crv vm edit my-vm --cpu-model qemu64          # Change QEMU CPU model (see Creating a VM)
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

| Current State | Start | Stop | Pause | Save | Reset |
|---------------|-------|------|-------|------|-------|
| **stopped** | running/starting | error | error | error | stopped |
| **starting** | error | stopping | error | error | stopped |
| **running** | error | stopping | paused | saved | stopped |
| **stopping** | error | error | error | error | stopped |
| **paused** | running | error | error | saved | stopped |
| **saved** | running (resume) | error | error | error | stopped (drops state file) |
| **error** | error | error | error | error | stopped |

VMs with `guestAgent: true` transition through a `starting` state and become `running` once the first guest agent health check succeeds. VMs without the guest agent go directly to `running`.

`reset` always returns the VM to `stopped` regardless of current state.

### Save / Resume

`crv vm save` writes the VM's RAM image to disk via QEMU's external
migration piped through `zstd` (`migrate "exec:zstd -T0 > …"`), then
terminates QEMU. Disks are unchanged. `crv vm start` on a `saved` VM
spawns a fresh QEMU with `-incoming "exec:zstdcat …"`, waits for the
restore to finish, and resumes execution — same command as a cold
boot.

The state file lives at `<basePath>/<vmName>/state.qemu.zst` on the
node that was hosting the VM (zstd-compressed; multi-threaded compress
and decompress via `zstd -T0`). To discard a save without resuming,
use `crv vm reset` — `crv vm stop` refuses on a saved VM and points
operators at `start` (resume) or `reset` (discard).

**Node dependency**: the `zstd` binary (zstandard) must be on the
node's `PATH` — the agent shells out to it via QEMU's `exec:` URI.
Present by default on every major distro (`apk add zstd`,
`apt install zstd`, `emerge app-arch/zstd`). If missing, `vm save`
fails with a QMP error and the VM stays in its pre-save state.

Autostart picks up saved VMs the same way it picks up stopped ones:
on daemon restart, any VM with `autostart=true` and `status in
{stopped, saved}` is started — saved ones via the resume path.

Cross-host migration carries the saved state with it. `crv vm migrate`
accepts saved VMs directly: the state file streams to the destination
alongside the disks, and the row lands on the destination with
`status=saved` — operator runs `vm start` on the destination to
resume execution from where the source left off.

Running and paused VMs are auto-saved before migration: `vm migrate`
on a running VM records a child `vm/save` task in `crv task history`,
then proceeds with the now-saved VM. The end state is the same as
migrating an already-saved VM. If the auto-save fails (e.g. node
disconnected mid-flight), the migrate task surfaces the error and the
VM stays where it was.

The destination must run a compatible QEMU version with a compatible
CPU model: the saved RAM image is bound to the source's `-machine` /
`-cpu` settings. The defaults (`q35` + `-cpu host`) make heterogeneous
host-CPU clusters the practical risk. If your cluster mixes CPU
generations, stop the VM cleanly before `vm migrate` so QEMU cold-boots
on the destination instead of trying to resume incompatible state.

## Inspecting Resource Usage

```bash
crv vm show my-vm
```

When the VM is `running`, `vm show` adds a **Resource Usage** section
populated from the agent's `StatusPoller` (10-second cadence). The
sample is the latest one the daemon's in-memory ring received from
the agent — cached server-side, so the call is a quick cap-method
round-trip, not a per-call sample.

```
Resource Usage (sampled 4s ago, 10.0s interval):
  CPU                   142.7 s total
  RAM (host RSS)        3.8 GiB
  RAM (balloon)         3.0 GiB / 4.0 GiB
  Disk I/O
                  drive0: 48.2 GiB read / 12.1 GiB written (1240315 / 332108 ops)
  Net I/O
                  vmtap0: 31.7 GiB rx / 4.2 GiB tx
```

What each row reports:

- **CPU** — cumulative user + system seconds across all vCPU threads
  (host-thread accounting via `/proc/<qemu-pid>/stat`).
- **RAM (host RSS)** — what the host pays for the QEMU process via
  `/proc/<qemu-pid>/status:VmRSS`.
- **RAM (balloon)** — `query-balloon` actual + the VM's configured
  ceiling. Suppressed when the VM has no `virtio-balloon` device.
- **Disk I/O** — per-drive cumulative bytes and operations from
  QMP `query-blockstats`.
- **Net I/O** — per-TAP cumulative bytes from
  `/sys/class/net/<tap>/statistics/{rx,tx}_bytes`.

Counters are cumulative since QEMU launch. Rates and per-second
throughput aren't shown here (a single one-shot `show` doesn't have
a prior sample to subtract from); for live rates, use the WebUI's
Resource Usage panel (sparklines + 10-minute window) or Prometheus
via corvus-web's `/metrics` endpoint — see [observability.md](observability.md).

The Resource Usage block is omitted for `stopped`, `paused`, or
otherwise-not-running VMs, and for VMs the daemon hasn't yet
received a sample for (e.g. just-started VMs in the first 10s).

## Deleting a VM

```bash
crv vm delete my-vm              # Delete VM record + reap attached ephemeral disks
crv vm delete my-vm --keep-disks # Delete VM record only; leave every disk in place
```

The VM must be stopped before deletion. By default, `vm delete` also
removes every **ephemeral** disk attached to the VM — that's cloud-init
ISOs, disks created during template instantiation (clone / overlay /
create strategies), and anything else created with `ephemeral=true`.
Non-ephemeral disks are never auto-deleted, even if exclusively attached;
remove them with `crv disk delete`.

Pass `--keep-disks` when debugging an instance's state and you need the
ISO or overlay to outlive the VM record. See
[disk-management.md](disk-management.md) for the `ephemeral` flag.

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

### SSH over vsock

Every VM is launched with a `vhost-vsock-pci` device and a unique AF_VSOCK CID. `crv vm show` prints the CID and a ready-to-paste invocation:

```
$ crv vm show my-vm
...
Vsock CID:      1042
SSH (vsock):    ssh <user>@vsock%1042
```

The CID is allocated at VM-create time from the range `qcVsockCidMin..qcVsockCidMax` (default `1000..1_000_000`) and is stable across daemon restarts.

**Host requirements**

OpenSSH does not speak AF_VSOCK natively in the client; the connection always traverses a `ProxyCommand` helper. Two options:

1. **`systemd-ssh-proxy(1)`** — bundled with systemd v256+ at `/usr/lib/systemd/systemd-ssh-proxy`. Recommended.

   On distros that ship the matching systemd-ssh package, no setup is needed: a system-wide `ssh_config` drop-in (`/etc/ssh/ssh_config.d/20-systemd-ssh-proxy.conf`) registers the `vsock/*` and `vsock%*` host patterns globally, so

   ```
   ssh user@vsock%1042
   ```

   just works. The `vsock%CID` form is the modern syntax (recent OpenSSH + systemd-ssh-proxy v257+); the older `vsock/CID` form reads as a path component to some tooling, so the percent form is preferred.

   If your distro doesn't ship the drop-in, add the same lines to `~/.ssh/config`:

   ```
   Host vsock%*
       ProxyCommand /usr/lib/systemd/systemd-ssh-proxy %h %p
       ProxyUseFdpass yes
       CheckHostIP no
   ```

2. **`socat`** — works on any host. No `ssh_config` change needed; just pass the proxy inline:

   ```
   ssh -o ProxyCommand="socat - VSOCK-CONNECT:1042:22" user@vsock
   ```

**Guest requirements**

The guest must terminate AF_VSOCK on port 22. Either:

- **systemd v256+ guest with `openssh-server`** — sshd is socket-activated on AF_VSOCK automatically, no extra config.
- **Older / non-systemd guest** — run a small relay (e.g., `socat VSOCK-LISTEN:22,fork,reuseaddr TCP:127.0.0.1:22`) as a service. The Corvus Alpine test image (built by [yaml/alpine-test/alpine-test.yml](../yaml/alpine-test/alpine-test.yml)) sets this up via an OpenRC unit `vsock-sshd`.

The host kernel autoloads `vhost_vsock` when the device is added; no manual modprobe is required.

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
