#!/bin/bash
# Runs inside the mounted artifact, with private /run and temporary build mounts.
set -euxo pipefail
cat >> /etc/portage/make.conf <<'EOF'
# This development image uses writable host caches through virtiofs.
FEATURES="${FEATURES} -userfetch -userpriv"
DISTDIR="/var/cache/distfiles"
PKGDIR="/var/cache/binpkgs"
EOF
cat >> /etc/portage/binrepos.conf/gentoo.conf <<'EOF'
location = /var/cache/binhost/binhost
EOF
mkdir -p /mnt/corvus-cache /var/cache/{distfiles,binpkgs,binhost}
cat >> /etc/fstab <<'EOF'
# Optional development caches; access activates the share and dependent binds.
corvus-cache /mnt/corvus-cache virtiofs nofail,x-systemd.automount 0 0
EOF
for cache in distfiles binpkgs binhost; do
    echo "/mnt/corvus-cache/$cache /var/cache/$cache none bind,nofail,x-systemd.automount,x-systemd.requires-mounts-for=/mnt/corvus-cache 0 0" >> /etc/fstab
done
emerge-webrsync -k
emerge --noreplace --usepkg --buildpkg --quiet-build dev-vcs/git

cat > /etc/portage/package.use/corvus <<'EOF'
app-emulation/qemu spice usb usbredir virtfs passt vde
net-libs/gnutls pkcs11 tools
EOF

cat > /etc/portage/package.accept_keywords/corvus <<'EOF'
app-emulation/corvus **
dev-python/pycapnp **
dev-python/fastapi **
dev-haskell/* ~amd64
dev-lang/ghc ~amd64
dev-util/ghcup-bin ~amd64
app-admin/haskell-updater ~amd64
EOF

install -d /etc/portage/repos.conf
cat > /etc/portage/repos.conf/corvus.conf <<'EOF'
[vonabarak]
location = /var/db/repos/vonabarak
sync-type = git
sync-uri = https://github.com/vonabarak/gentoo.git

[haskell]
location = /var/db/repos/haskell
sync-type = git
sync-uri = https://github.com/gentoo-haskell/gentoo-haskell.git
EOF

emerge --sync
emerge --usepkg --buildpkg --quiet-build --onlydeps app-emulation/corvus
emerge --verbose app-emulation/corvus

eselect postgresql set 18
# Initialize the same cluster used by Gentoo's systemd service, without booting it.
emerge --config dev-db/postgresql:18
systemctl enable postgresql-18.service
pgdata=/var/lib/postgresql/18/data
pgbin=/usr/lib64/postgresql-18/bin
install -d -o postgres -g postgres /run/postgresql
stop_postgres() {
    runuser -u postgres -- "$pgbin/pg_ctl" -D "$pgdata" -m fast -w stop
}
trap stop_postgres EXIT
trap 'exit 130' INT
trap 'exit 143' TERM
trap 'exit 129' HUP
runuser -u postgres -- "$pgbin/pg_ctl" -D "$pgdata" -w start \
    -o "-c listen_addresses='' -c unix_socket_directories=/run/postgresql"
runuser -u postgres -- "$pgbin/psql" -h /run/postgresql -v ON_ERROR_STOP=1 <<'SQL'
CREATE USER corvus SUPERUSER;
CREATE DATABASE corvus OWNER corvus;
SQL
stop_postgres
trap - EXIT

install -d /etc/systemd/system
cat > /etc/systemd/system/vde-switch.service <<'EOF'
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
systemctl enable vde-switch.service

install -d /etc/systemd/network
cat > /etc/systemd/network/30-vde0.network <<'EOF'
[Match]
Name=vde0

[Network]
Address=192.168.92.1/24
ConfigureWithoutCarrier=yes
EOF

install -d /etc/sysctl.d
cat > /etc/sysctl.d/30-corvus-vde-forward.conf <<'EOF'
net.ipv4.ip_forward = 1
EOF

cat > /etc/nftables.conf <<'EOF'
#!/usr/sbin/nft -f
flush ruleset

table ip nat {
    chain postrouting {
        type nat hook postrouting priority srcnat
        policy accept
        ip saddr 192.168.92.0/24 oifname != "vde0" masquerade
    }
}
EOF

cat > /etc/dnsmasq.conf <<'EOF'
interface=vde0
bind-dynamic
dhcp-range=192.168.92.10,192.168.92.200,12h
dhcp-option=option:router,192.168.92.1
dhcp-option=option:dns-server,192.168.92.1
domain-needed
bogus-priv
EOF

install -d /etc/systemd/system/dnsmasq.service.d
cat > /etc/systemd/system/dnsmasq.service.d/wait-vde.conf <<'EOF'
[Unit]
After=vde-switch.service systemd-networkd.service
Wants=systemd-networkd.service
EOF

systemctl enable nftables.service dnsmasq.service

cat > /etc/issue <<'EOF'
Gentoo Linux with Corvus installed

Log in as corvus. Passwordless sudo is available.
Set up Corvus:
  corvus-admin quickstart      Configure and start a local node

Corvus commands after setup:
  crv node list                List registered nodes
  crv vm list                  List virtual machines
  crv vm start NAME            Start a virtual machine
  crv vm stop NAME             Stop a virtual machine
  crv --help                   Show commands and options
  corvus-admin --help          Show administration commands

EOF
# Gentoo PAM displays /etc/motd; suppress duplicate SSH banners.
install -d /etc/ssh/sshd_config.d
cat > /etc/ssh/sshd_config.d/90-corvus-banner.conf <<'EOF'
Banner none
PrintMotd no
EOF
# Share the console banner with the PAM login message.
ln -sf /etc/issue /etc/motd
cat /etc/issue

{
  echo "image: gentoo-corvus-test"
  echo "baked: $(date -Iseconds)"
  echo "kernel: $(uname -r)"
  ghc --version
  qemu-system-x86_64 --version | head -1
  psql --version
  vde_switch --version 2>&1 | head -1 || true
} > /etc/corvus-test-info
cat /etc/corvus-test-info
