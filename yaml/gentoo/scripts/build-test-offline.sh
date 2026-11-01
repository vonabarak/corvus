#!/bin/bash
# Build the development image offline; only individual target commands use chroot.
set -euxo pipefail
. "${CORVUS_BUILD_LIB:-/usr/local/lib/corvus-build}/build.sh"

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

    write_corvus_repositories "$1"
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
