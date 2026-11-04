#!/bin/bash
# Build the development image offline; only individual target commands use chroot.
set -euxo pipefail

. "${CORVUS_BUILD_SCRIPTS:-/usr/local/lib/corvus-build}/lib/corvus-build.sh"
. "${CORVUS_BUILD_SCRIPTS:-/usr/local/lib/corvus-build}/lib/image.sh"
. "${CORVUS_BUILD_SCRIPTS:-/usr/local/lib/corvus-build}/lib/mounts.sh"
. "${CORVUS_BUILD_SCRIPTS:-/usr/local/lib/corvus-build}/lib/portage.sh"
. "${CORVUS_BUILD_SCRIPTS:-/usr/local/lib/corvus-build}/lib/postgres.sh"
. "${CORVUS_BUILD_SCRIPTS:-/usr/local/lib/corvus-build}/lib/vde-network.sh"

# Point Portage directly at the development VM's lazily mounted cache share.
configure_target_caches() {
    local config="$root/etc/portage/make.conf"
    if [ -f "$config" ]; then
        sed -i -E '/^[[:space:]]*(export[[:space:]]+)?(DISTDIR|PKGDIR)[[:space:]]*=/d' "$config"
    fi
    cat >> "$config" <<'EOF'
# This development image uses writable host caches through virtiofs.
DISTDIR="/mnt/corvus-cache/distfiles"
PKGDIR="/mnt/corvus-cache/packages/x86-64"
EOF
    mkdir -p "$root/mnt/corvus-cache"
    rm -rf "$root/etc/portage/binrepos.conf"
    portage_configure_binhost glibc "$root"
    # The host cache cannot map the guest portage user's ownership changes.
    cat >> "$config" <<'EOF'
FEATURES="-userfetch -userpriv"
EOF
    cat >> "$root/etc/fstab" <<'EOF'
# Optional development caches; access activates the share.
corvus-cache /mnt/corvus-cache virtiofs nofail,x-systemd.automount 0 0
EOF
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

    corvus_build_write_repositories "$1"
}

# Sync overlays on the helper, then populate the artifact in dependency order.
install_development_packages() {
    write_package_policy /
    write_package_policy "$root"
    command -v git >/dev/null
    portage_sync_repository
    portage_install_target_packages --buildpkg --quiet-build --onlydeps app-emulation/corvus
    # Rebuild the live Corvus ebuild rather than reusing a potentially stale binary.
    portage_install_target_packages --verbose --usepkg-exclude=app-emulation/corvus app-emulation/corvus
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
    mounts_initialize
    trap corvus_build_cleanup EXIT
    trap 'exit 130' INT
    trap 'exit 143' TERM
    trap 'exit 129' HUP
    corvus_build_check_prerequisites
    corvus_build_prepare_mounts
    configure_target_caches
    install_development_packages
    postgres_initialize
    vde_network_configure_service
    vde_network_configure_network
    vde_network_configure_nat
    vde_network_configure_dhcp
    configure_login_banner
    write_test_metadata
    image_finalize "$root" gentoo-corvus-test
}

main "$@"
