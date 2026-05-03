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

**Local-host only.** Unlike every other `crv` subcommand, `crv ns` does not go through the RPC socket — it asks the daemon for the namespace PID via `crv status` and then calls `nsenter` directly against `/proc/<pid>/ns/*` on the caller's machine. That's only meaningful when `crv` runs on the daemon host. This is a **debugging tool, not a production interface**: don't build workflows on top of it, and expect it to fail loudly when pointed at a remote daemon.

---

## Recommended Setup: VDE Switch with TAP

For full bidirectional TCP connectivity between the host and VMs — without requiring root to run VMs and without the network namespace isolation of Corvus managed networks — the recommended approach is a **VDE virtual switch** connected to a **TAP device** on the host.

This setup:
- Gives VMs routable IP addresses reachable from the host (unlike QEMU user-mode networking, which only supports port forwarding).
- Does not require root privileges to start VMs or attach network interfaces — only the initial VDE switch setup needs root (one-time systemd service).
- Is not isolated inside a namespace like Corvus managed networks, so host processes can reach VM IPs directly.

This is the network configuration assumed by the [multi-os.yml](../yaml/multi-os/multi-os.yml) and [test-images.yml](../yaml/test-images/test-images.yml) example apply configurations.

### 1. Install VDE

```bash
# Gentoo
emerge net-misc/vde

# Debian/Ubuntu
apt install vde2

# Arch
pacman -S vde2
```

### 2. Create a systemd service for the VDE switch

Save the following as `/etc/systemd/system/vde-switch.service`:

```ini
[Unit]
Description=VDE Virtual Switch with TAP
After=network.target

[Service]
Type=simple
ExecStartPre=/usr/bin/install -d -m 0755 /run/vde2
ExecStart=/usr/bin/vde_switch \
    --sock /run/vde2/switch.ctl \
    --tap tap0 \
    --mode 0770 \
    --group qemu
ExecStartPost=/bin/sh -c 'sleep 0.5 && ip addr add 192.168.89.1/24 dev tap0 && ip link set tap0 up'
ExecStop=/bin/kill $MAINPID
Restart=on-failure

[Install]
WantedBy=multi-user.target
```

This creates a VDE switch at `/run/vde2/switch.ctl` with a TAP device `tap0`, assigns the static IP `192.168.89.1/24` to it, and makes the socket accessible to the `qemu` group. Adjust the IP subnet, group, and socket path to your needs.

Enable and start:

```bash
sudo systemctl daemon-reload
sudo systemctl enable --now vde-switch
```

### 3. Set up DHCP on the TAP interface

Use dnsmasq (or any DHCP server) to hand out addresses to VMs on the TAP subnet:

```bash
# /etc/dnsmasq.d/vde.conf
interface=tap0
bind-interfaces
dhcp-range=192.168.89.100,192.168.89.200,24h
```

Restart dnsmasq:

```bash
sudo systemctl restart dnsmasq
```

VMs connected to the VDE switch will receive an IP in the `192.168.89.0/24` range via DHCP.

### 4. Optional: NAT / masquerade

If you want VMs to access the internet through the host, enable IP forwarding and add a masquerade rule:

```bash
# Enable forwarding
sudo sysctl -w net.ipv4.ip_forward=1

# nftables masquerade (adjust the output interface)
sudo nft add table nat
sudo nft add chain nat postrouting '{ type nat hook postrouting priority 100; }'
sudo nft add rule nat postrouting oifname "eth0" ip saddr 192.168.89.0/24 masquerade

# Or with iptables
sudo iptables -t nat -A POSTROUTING -s 192.168.89.0/24 -o eth0 -j MASQUERADE
```

To make forwarding persistent, add `net.ipv4.ip_forward=1` to `/etc/sysctl.conf`.

### 5. Connect VMs via VDE

Use the `vde` interface type pointing to the switch socket:

```bash
crv net-if add my-vm --type vde --host-device /run/vde2/switch.ctl
```

Or in apply YAML / templates:

```yaml
networkInterfaces:
  - type: vde
    hostDevice: /run/vde2/switch.ctl
```

After starting the VM, it will obtain an IP from dnsmasq and be reachable from the host:

```bash
crv vm start my-vm --wait
ssh user@192.168.89.100
```
