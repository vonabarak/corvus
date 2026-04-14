# Networking

Corvus supports several networking modes for VMs. Virtual networks run inside an unprivileged user namespace created by the daemon — no root access is required.

## Virtual Networks

Virtual networks provide isolated bridge-based networking with optional DHCP and NAT, managed entirely by the daemon.

### Commands

```bash
crv network create <name> [options]     # Create a virtual network
crv network start <network>             # Start (creates bridge, dnsmasq)
crv network stop <network>              # Stop
crv network list                        # List all networks
crv network show <network>              # Show details
crv network edit <network> [options]    # Edit network properties
crv network delete <network>            # Delete
```

### Creating Networks

```bash
crv network create lab-net                                          # Minimal
crv network create lab-net --subnet 10.0.1.0/24                     # With subnet
crv network create lab-net --subnet 10.0.1.0/24 --dhcp              # With DHCP
crv network create lab-net --subnet 10.0.1.0/24 --dhcp --nat        # With DHCP + NAT
crv network create lab-net --subnet 10.0.1.0/24 --dhcp --nat --autostart
```

| Option | Description |
|--------|-------------|
| `--subnet` | IP subnet in CIDR notation (e.g., `10.0.1.0/24`) |
| `--dhcp` | Run dnsmasq for DHCP/DNS |
| `--nat` | Enable NAT for internet access |
| `--autostart` | Start network when daemon starts |

### Network Lifecycle

1. `crv network start` creates a bridge and TAP devices inside the daemon's network namespace, starts dnsmasq if DHCP is enabled.
2. VMs with `managed` interfaces connect via TAP devices on this bridge.
3. `crv network stop` tears down the bridge and stops dnsmasq. Running VMs using this network must be stopped first.

## Network Interfaces

Each VM can have multiple network interfaces, each with its own type and configuration.

### Commands

```bash
crv net-if list <vm>                    # List VM's network interfaces
crv net-if add <vm> [options]           # Add a network interface
crv net-if remove <vm> <netif_id>       # Remove a network interface
```

### Interface Types

| Type | Description | `--host-device` |
|------|-------------|-----------------|
| `user` | QEMU user-mode networking with built-in NAT | Optional: QEMU netdev options (e.g., `hostfwd=...`) |
| `managed` | Daemon-managed bridge/TAP in network namespace | Auto (set via `--network`) |
| `tap` | Pre-configured TAP device on host | Required: TAP device name |
| `bridge` | Pre-configured bridge on host | Required: bridge name |
| `vde` | External VDE virtual switch | Required: VDE socket path |
| `macvtap` | MACVTAP device | N/A (uses fd passing) |

### Adding Interfaces

```bash
# User-mode networking (default, built-in NAT, no setup required)
crv net-if add my-vm --type user
crv net-if add my-vm -t user -d "hostfwd=tcp::2222-:22"                     # SSH forwarding
crv net-if add my-vm -t user -d "hostfwd=tcp::2222-:22,hostfwd=tcp::8080-:80"  # Multi-port

# Managed virtual network (daemon creates bridge + TAP)
crv net-if add my-vm --network my-net

# TAP device (pre-configured on host)
crv net-if add my-vm --type tap --host-device tap0

# Bridge (pre-configured on host)
crv net-if add my-vm --type bridge --host-device br0

# VDE virtual switch (external vde_switch)
crv net-if add my-vm --type vde --host-device /var/run/vde.ctl

# Explicit MAC address
crv net-if add my-vm --type user --mac 52:54:00:12:34:56
```

Each interface gets a VirtIO NIC with an auto-generated MAC address unless `--mac` is specified.

### Port Forwarding (User Mode)

User-mode networking supports port forwarding via the `--host-device` option:

```
hostfwd=<proto>::<host_port>-:<guest_port>
```

Examples:
- `hostfwd=tcp::2222-:22` — forward host port 2222 to guest SSH
- `hostfwd=tcp::8080-:80` — forward host port 8080 to guest HTTP
- Multiple rules are comma-separated

### Namespace Architecture

The daemon creates a single shared user+network+UTS namespace at startup (no root required). All bridges, dnsmasq instances, and QEMU processes for managed networks run inside this namespace.

The namespace is created by a C helper (`cbits/netns.c`) that forks from the Haskell process, calls `unshare(2)` in the single-threaded child, brings up loopback, and waits. Haskell code manages bridges and dnsmasq via `nsenter` from outside.

### Namespace Exec

```bash
crv ns                  # Open a shell inside the daemon's namespace
crv ns ip addr          # Run a command inside the namespace
crv ns brctl show       # Inspect bridges
```

Useful for debugging network configuration inside the namespace.
