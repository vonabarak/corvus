#!/bin/bash
# Bootstrap the glibc/systemd guest using the common native build host.
set -eu

. "${CORVUS_BUILD_SCRIPTS:-/usr/local/lib/corvus-build}/lib/portage.sh"

# Create Portage directories and the merged-/usr layout.
prepare_target_directories() {
    install -d "$IMAGE_ROOT/etc/portage" \
               "$IMAGE_ROOT/etc/portage/package.use" \
               "$IMAGE_ROOT/etc/portage/package.accept_keywords" \
               "$IMAGE_ROOT/etc/portage/package.mask" \
               "$IMAGE_ROOT/etc/portage/package.unmask" \
               "$IMAGE_ROOT/etc/portage/package.license" \
               "$IMAGE_ROOT/etc/portage/env" \
               "$IMAGE_ROOT/etc/portage/repos.conf" \
               "$IMAGE_ROOT/etc/portage/binrepos.conf" \
               "$IMAGE_ROOT/var/cache/distfiles" \
               "$IMAGE_ROOT/var/cache/binpkgs" \
               "$IMAGE_ROOT/var/cache/binhost" \
               "$IMAGE_ROOT/var/db/repos" \
               "$IMAGE_ROOT/usr/bin" \
               "$IMAGE_ROOT/usr/lib" \
               "$IMAGE_ROOT/usr/lib64"

    # Cache paths are supplied explicitly in the build environment. Keep these
    # runtime directories empty; no host cache mounts are needed inside SYSROOT.

    ln -sf bin       "$IMAGE_ROOT/usr/sbin"
    ln -sf usr/bin   "$IMAGE_ROOT/bin"
    ln -sf usr/sbin  "$IMAGE_ROOT/sbin"
    ln -sf usr/lib   "$IMAGE_ROOT/lib"
    ln -sf usr/lib64 "$IMAGE_ROOT/lib64"
}

# Configure the regular glibc target and its trusted package sources.
configure_target_portage() {
    # Quoted heredoc so the ${VAR} references stay literal
    # (portage's make.conf parser will expand them) and so
    # the backticks in the comments below don't get evaluated
    # as shell command substitution. MAKEOPTS needs nproc at
    # bake time, which a quoted heredoc can't expand — we
    # append it separately after the heredoc closes.
    cat > "$IMAGE_ROOT/etc/portage/make.conf" <<'EOF'
COMMON_FLAGS="-O2 -pipe"
CFLAGS="${COMMON_FLAGS}"
CXXFLAGS="${COMMON_FLAGS}"
FCFLAGS="${COMMON_FLAGS}"
FFLAGS="${COMMON_FLAGS}"

DISTDIR="/var/cache/distfiles"
PKGDIR="/var/cache/binpkgs"
ACCEPT_LICENSE="*"
USE="bash-completion"

# Portage's gpkg verifier (gpkg.py:checksum_helper) drops
# privileges to nobody:nogroup whenever a gpkg contains a
# .sig — and that happens BEFORE any FEATURES check, so
# binpkg-ignore-signature alone can't avoid the lookup.
# Gentoo's baselayout doesn't ship a 'nogroup' group, so
# the lookup crashes with KeyError: "getgrnam(): name not
# found: 'nogroup'" the first time a partially-signed
# binpkg (e.g. acct-group/postgres) lands. Empty value
# tells portage to skip the privilege drop entirely.
GPG_VERIFY_USER_DROP=""
GPG_VERIFY_GROUP_DROP=""
EOF

    # Append MAKEOPTS with the bake-time nproc baked in;
    # portage's make.conf parser doesn't run $() so we
    # resolve it at write time instead.
    echo "MAKEOPTS=\"-j$(nproc)\"" >> "$IMAGE_ROOT/etc/portage/make.conf"

    cat > "$IMAGE_ROOT/etc/portage/package.use/grub" <<'EOF'
sys-boot/grub grub_platforms_pc grub_platforms_efi-64 -mount -nls -fonts -themes
EOF

    cat > "$IMAGE_ROOT/etc/portage/repos.conf/gentoo.conf" <<'EOF'
[DEFAULT]
main-repo = gentoo

[gentoo]
location = /var/db/repos/gentoo
sync-type = rsync
sync-uri = rsync://rsync.gentoo.org/gentoo-portage
auto-sync = yes
EOF

    cat > "$IMAGE_ROOT/etc/portage/binrepos.conf/gentoo.conf" <<'EOF'
[binhost]
priority = 9999
sync-uri = https://distfiles.gentoo.org/releases/amd64/binpackages/23.0/x86-64
verify-signature = false
EOF

    # Relative make.profile so the symlink resolves on the
    # booted image the first time the user runs
    # `emerge-webrsync` (or `emerge --sync`) — same convention
    # the stage3 tarball used.
    ln -sf ../../var/db/repos/gentoo/profiles/default/linux/amd64/23.0/no-multilib/systemd \
      "$IMAGE_ROOT/etc/portage/make.profile"

    # Trust keys for binpkg signature verification.
    cp -a /etc/portage/gnupg "$IMAGE_ROOT/etc/portage/gnupg"
}

# Break systemd dependency cycles in the native build host.
install_bootstrap_overrides() {
    # Break the well-known systemd ↔ util-linux build
    # circular dep: util-linux[systemd] needs sys-apps/systemd
    # to build, but systemd needs util-linux. Stage3 used to
    # paper over this by shipping a pre-built util-linux. With
    # no stage3 we have to bootstrap manually — install
    # util-linux without the systemd USE first, then re-emerge
    # it at the end of this step so the final image has full
    # systemd integration.
    #
    # The override goes in the BAKE VM's /etc/portage (not
    # the sysroot's) because the @system emerge below uses
    # ROOT=$IMAGE_ROOT only — it reads /etc/portage from the
    # live bake VM, not from the sysroot. We use a name
    # outside the sysroot so the bootstrap doesn't leak into
    # the final image.
    install -d /etc/portage/package.use
    # `build` USE on util-linux is the stage3-style minimal
    # mode that strips systemd + udev + most extras in one
    # flag — breaks both the systemd↔util-linux cycle AND
    # the libudev→systemd→util-linux→libudev cycle that comes
    # in via `virtual/libudev`. Pam still needs an explicit
    # -systemd to break the pam↔systemd cycle.
    cat > /etc/portage/package.use/zz-corvus-bootstrap <<'EOF'
sys-apps/util-linux build
sys-libs/pam -systemd
EOF
}

# Point native build dependencies at the shared package cache.
configure_native_dependencies() {
    portage_configure_glibc_caches
}

# Install the system natively, directing runtime files into ROOT.
install_base_system() {
    # Install @system using the bake VM's repository and package caches.
    # The bake VM's /etc/portage drives dependency resolution for this native
    # amd64 systemd build, with sources downloaded into the shared cache.
    #
    # SYSROOT would enable cross-compilation; ROOT alone redirects native output.
    ROOT="$IMAGE_ROOT" portage_emerge --buildpkg --quiet-build @system
}

# Remove bootstrap overrides and restore full systemd integration.
restore_full_packages() {
    # Cycles broken: drop the overrides and re-emerge the
    # bootstrapped packages with full USE now that
    # sys-apps/systemd is on disk in the sysroot.
    rm /etc/portage/package.use/zz-corvus-bootstrap
    ROOT="$IMAGE_ROOT" portage_emerge --buildpkg --quiet-build -1 sys-apps/util-linux sys-libs/pam
}

# Create the mountpoints required during the first guest boot.
prepare_runtime_mountpoints() {
    # systemd-PID1's required pseudo-fs mountpoints. baselayout
    # normally lays /proc, /sys, /dev, /run, /tmp down on a
    # fresh stage3 unpack, but its ROOT=$IMAGE_ROOT @system
    # install above doesn't reliably create them — without
    # these the booted image hangs at PID1 with "Failed to
    # mount proc/sys/dev: No such file or directory" plus a
    # very early "devtmpfs: error mounting -2" from the
    # kernel's auto-mount.
    install -d -m 755 "$IMAGE_ROOT/proc" "$IMAGE_ROOT/sys" \
                      "$IMAGE_ROOT/dev"  "$IMAGE_ROOT/run"
    install -d -m 1777 "$IMAGE_ROOT/tmp" "$IMAGE_ROOT/var/tmp"
}

# Keep target installation native: SYSROOT would incorrectly enable cross-compilation.
install_runtime_packages() {
    # Build natively against the bake host, as in the @system bootstrap. Treating
    # this same-architecture root as a cross-compilation sysroot makes Rust bindgen
    # look for build-host libclang inside the unfinished image.
    # Keep thin-provisioning-tools explicit: it was shipped by the regular flavor,
    # but Portage can treat it as a bake-host dependency when installing LVM.
    ROOT="$IMAGE_ROOT" portage_emerge --buildpkg --quiet-build \
      app-emulation/qemu-guest-agent \
      mail-mta/msmtp \
      app-admin/sudo \
      sys-fs/growpart \
      sys-fs/dosfstools \
      sys-boot/efibootmgr \
      net-fs/nfs-utils \
      sys-block/thin-provisioning-tools \
      app-editors/vim \
      app-shells/bash-completion
}

# Populate the mounted target in dependency order.
main() {
    prepare_target_directories
    configure_target_portage
    install_bootstrap_overrides
    configure_native_dependencies
    install_base_system
    restore_full_packages
    prepare_runtime_mountpoints
    install_runtime_packages
}

main "$@"
