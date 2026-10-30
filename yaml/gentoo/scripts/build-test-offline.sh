#!/bin/bash
# Build the development image offline; only individual target commands use chroot.
set -euxo pipefail
. "${CORVUS_BUILD_LIB:-/usr/local/lib/corvus-build}/build.sh"

# Wait for the grown helper and validate artifact partitions before mounting.
check_build_prerequisites() {
    cloud-init status --wait
    # Cloud-init grows only the helper; compilation must not fill the smaller artifact.
    [ "$(df --output=size -B1 / | tail -1)" -ge 30000000000 ]
    wait_for_network
    [ "$(findmnt -n -o SOURCE /)" = /dev/vda3 ]
    [ "$(blkid -s TYPE -o value /dev/vdb3)" = ext4 ]
    [ "$(blkid -s TYPE -o value /dev/vdb2)" = vfat ]
}

# Stop the target server before releasing its private runtime filesystem.
stop_postgres() {
    if "$postgres_active"; then
        chroot "$root" runuser -u postgres -- "$pgbin/pg_ctl" -D "$pgdata" -m fast -w stop || return $?
        postgres_active=false
    fi
}

# Preserve the build error while stopping PostgreSQL and releasing every mount.
cleanup_test_build() {
    local status=$?
    trap - EXIT
    set +e
    stop_postgres || { [ "$status" -ne 0 ] || status=1; }
    unmount_tracked || { [ "$status" -ne 0 ] || status=1; }
    # Failed builds retain their workspace for inspection; never remove mounted data.
    if [ "$status" -eq 0 ] && [ -n "$workspace" ]; then
        rm -rf -- "$workspace"
    fi
    exit "$status"
}

# Use tracked, disposable repository storage and private target interfaces.
prepare_build_mounts() {
    workspace=$(mktemp -d /var/tmp/corvus-test.XXXXXX)
    # Repository sync drops privileges even when package userpriv is disabled.
    chmod 0755 "$workspace"
    # Native Portage resolves repositories on the helper, not beneath ROOT.
    mount_at /var/db/repos -t tmpfs tmpfs
    prepare_workspace
    # Select devices explicitly: helper and artifact have identical filesystem labels.
    mount_target_disk "$root" /dev/vdb
    bind /dev/pts "$root/dev/pts"
    bind /dev/shm "$root/dev/shm"
    # PostgreSQL pkg_config needs the same ebuild tree inside its individual chroot.
    bind /var/db/repos "$root/var/db/repos"
    local cache source
    for cache in distfiles binpkgs binhost; do
        case "$cache" in
            binpkgs) source=packages/x86-64 ;;
            *) source=$cache ;;
        esac
        bind "/mnt/corvus-cache/$source" "$root/var/cache/$cache"
    done
    # All compilation uses the larger helper disk, including native build dependencies.
    export PORTAGE_TMPDIR="$workspace"
    configure_glibc_caches
}

# Keep the development VM’s existing native cache paths and lazy automounts.
configure_target_caches() {
    cat >> "$root/etc/portage/make.conf" <<'EOF'
# This development image uses writable host caches through virtiofs.
FEATURES="${FEATURES} -userfetch -userpriv"
DISTDIR="/var/cache/distfiles"
PKGDIR="/var/cache/binpkgs"
EOF
    mkdir -p "$root/mnt/corvus-cache" "$root"/var/cache/{distfiles,binpkgs,binhost}
    configure_binhost glibc "$root" /var/cache/binhost
    local cache cache_source
    cat >> "$root/etc/fstab" <<'EOF'
# Optional development caches; access activates the share and dependent binds.
corvus-cache /mnt/corvus-cache virtiofs nofail,x-systemd.automount 0 0
EOF
    for cache in distfiles binpkgs binhost; do
        case "$cache" in
            binpkgs) cache_source=packages/x86-64 ;;
            *) cache_source=$cache ;;
        esac
        echo "/mnt/corvus-cache/$cache_source /var/cache/$cache none bind,nofail,x-systemd.automount,x-systemd.requires-mounts-for=/mnt/corvus-cache 0 0" >> "$root/etc/fstab"
    done
}

# Write identical USE, keyword, and overlay policy for helper and artifact.
write_package_policy() {
    local config_root=${1%/}
    install -d "$config_root/etc/portage"/{package.use,package.accept_keywords,repos.conf}
    cat > "$config_root/etc/portage/package.use/corvus" <<'EOF'
app-emulation/qemu spice usb usbredir virtfs passt vde
net-libs/gnutls pkcs11 tools
EOF

    cat > "$config_root/etc/portage/package.accept_keywords/corvus" <<'EOF'
app-emulation/corvus **
dev-python/pycapnp **
dev-python/fastapi **
dev-haskell/* ~amd64
dev-lang/ghc ~amd64
dev-util/ghcup-bin ~amd64
app-admin/haskell-updater ~amd64
EOF

    cat > "$config_root/etc/portage/repos.conf/corvus.conf" <<'EOF'
[vonabarak]
location = /var/db/repos/vonabarak
sync-type = git
sync-uri = https://github.com/vonabarak/gentoo.git

[haskell]
location = /var/db/repos/haskell
sync-type = git
sync-uri = https://github.com/gentoo-haskell/gentoo-haskell.git
EOF
}

# Install natively into ROOT while retaining the development dependency closure.
install_target_packages() {
    env -u SYSROOT ROOT="$root" emerge --root-deps "$@"
}

# Sync overlays on the helper, then populate the artifact in dependency order.
install_development_packages() {
    write_package_policy /
    write_package_policy "$root"
    # Git must also be executable on the helper for native overlay synchronization.
    env -u SYSROOT ROOT=/ emerge --noreplace --usepkg --buildpkg --quiet-build dev-vcs/git
    install_target_packages --noreplace --usepkg --buildpkg --quiet-build dev-vcs/git
    emerge --sync
    install_target_packages --usepkg --buildpkg --quiet-build --onlydeps app-emulation/corvus
    # Rebuild the live Corvus ebuild rather than reusing a potentially stale binary.
    install_target_packages --verbose app-emulation/corvus
}

# Initialize the target cluster and database without starting its init system.
initialize_postgres() {
    chroot "$root" eselect postgresql set 18
    # Run the installed package's configuration with target accounts, locale, and paths.
    env -u ROOT -u SYSROOT -u PORTAGE_TMPDIR -u DISTDIR -u PKGDIR \
        chroot "$root" emerge --config dev-db/postgresql:18
    systemctl --root="$root" enable postgresql-18.service
    chroot "$root" install -d -o postgres -g postgres /run/postgresql
    # Mark ownership before startup so cleanup also handles a partially failed start.
    postgres_active=true
    chroot "$root" runuser -u postgres -- "$pgbin/pg_ctl" -D "$pgdata" -w start \
        -o "-c listen_addresses='' -c unix_socket_directories=/run/postgresql"
    chroot "$root" runuser -u postgres -- "$pgbin/psql" -h /run/postgresql -v ON_ERROR_STOP=1 <<'SQL'
CREATE USER corvus SUPERUSER;
CREATE DATABASE corvus OWNER corvus;
SQL
    stop_postgres
}

# Install and enable the VDE switch used by nested test VMs.
configure_vde_service() {
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
configure_vde_network() {
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
configure_nat() {
    cat > "$root/etc/nftables.conf" <<'EOF'
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
}

# Serve DHCP and DNS only on VDE, after its interface is available.
configure_dhcp() {
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

# Share the console banner with PAM without duplicate SSH messages.
configure_login_banner() {
    cat > "$root/etc/issue" <<'EOF'
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
    install -d "$root/etc/ssh/sshd_config.d"
    cat > "$root/etc/ssh/sshd_config.d/90-corvus-banner.conf" <<'EOF'
Banner none
PrintMotd no
EOF
    # Share the console banner with the PAM login message.
    ln -sf /etc/issue "$root/etc/motd"
    cat "$root/etc/issue"
}

# Record the installed target tool versions, not the helper’s versions.
write_test_metadata() {
    {
      echo "image: gentoo-corvus-test"
      echo "baked: $(date -Iseconds)"
      echo "kernel: $(uname -r)"
      chroot "$root" ghc --version
      chroot "$root" qemu-system-x86_64 --version | head -1
      chroot "$root" psql --version
      chroot "$root" vde_switch --version 2>&1 | head -1 || true
    } > "$root/etc/corvus-test-info"
    cat "$root/etc/corvus-test-info"
}

# Build and configure the mounted artifact without running a script inside it.
main() {
    : "${IMAGE_ROOT:=/mnt/sysroot}"
    root=$IMAGE_ROOT
    IMAGE_LIBC=glibc
    workspace=
    postgres_active=false
    pgdata=/var/lib/postgresql/18/data
    pgbin=/usr/lib64/postgresql-18/bin
    unset ROOT SYSROOT PORTAGE_CONFIGROOT
    initialize_mount_tracking
    trap cleanup_test_build EXIT
    trap 'exit 130' INT
    trap 'exit 143' TERM
    trap 'exit 129' HUP
    check_build_prerequisites
    prepare_build_mounts
    configure_target_caches
    install_development_packages
    initialize_postgres
    configure_vde_service
    configure_vde_network
    configure_nat
    configure_dhcp
    configure_login_banner
    write_test_metadata
    finalize_image "$root" gentoo-corvus-test
}

main "$@"
