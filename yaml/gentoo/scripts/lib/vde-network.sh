#!/bin/bash
# Definitions only; callers initialize state and own traps.

# Install and enable the VDE switch used by nested test VMs.
vde_network_configure_service() {
    install -d "$root/etc/systemd/system"
    cat > "$root/etc/systemd/system/vde-switch.service" <<'EOF'
[Unit]
Description=VDE virtual switch for Corvus test VMs
After=network-pre.target
Before=network.target
Wants=network-pre.target

[Service]
Type=forking
ExecStartPre=/bin/mkdir -p /run/vde2
ExecStart=/usr/bin/vde_switch \
    --sock /run/vde2/switch.ctl \
    --mgmt /run/vde2/switch.mgmt \
    -tap vde0 \
    -m 666 \
    -group kvm \
    --daemon \
    --pidfile /run/vde2/vde_switch.pid
PIDFile=/run/vde2/vde_switch.pid
Restart=on-failure

[Install]
WantedBy=multi-user.target
EOF
    systemctl --root="$root" enable vde-switch.service
}

# Assign the private VDE subnet and enable IPv4 forwarding.
vde_network_configure_network() {
    install -d "$root/etc/systemd/network"
    cat > "$root/etc/systemd/network/30-vde0.network" <<'EOF'
[Match]
Name=vde0

[Network]
Address=192.168.92.1/24
ConfigureWithoutCarrier=yes
EOF

    install -d "$root/etc/sysctl.d"
    cat > "$root/etc/sysctl.d/30-corvus-vde-forward.conf" <<'EOF'
net.ipv4.ip_forward = 1
EOF
}

# Masquerade traffic leaving the private VDE subnet.
vde_network_configure_nat() {
    # Gentoo nftables.service loads this path, not /etc/nftables.conf.
    install -d -m 0755 "$root/etc/nftables/rules"
    cat > "$root/etc/nftables/rules/main.nft" <<'EOF'
flush ruleset

table ip nat {
    chain postrouting {
        type nat hook postrouting priority srcnat
        policy accept
        ip saddr 192.168.92.0/24 oifname != "vde0" masquerade
    }
}

EOF
}

# Serve DHCP and DNS only on VDE, after its interface is available.
vde_network_configure_dhcp() {
    cat > "$root/etc/dnsmasq.conf" <<'EOF'
interface=vde0
bind-dynamic
dhcp-range=192.168.92.10,192.168.92.200,12h
dhcp-option=option:router,192.168.92.1
dhcp-option=option:dns-server,192.168.92.1
domain-needed
bogus-priv
EOF

    install -d "$root/etc/systemd/system/dnsmasq.service.d"
    cat > "$root/etc/systemd/system/dnsmasq.service.d/wait-vde.conf" <<'EOF'
[Unit]
After=vde-switch.service systemd-networkd.service
Wants=systemd-networkd.service
EOF

    systemctl --root="$root" enable nftables.service dnsmasq.service
}
