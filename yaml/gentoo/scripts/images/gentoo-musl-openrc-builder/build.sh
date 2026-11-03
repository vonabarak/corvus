#!/bin/bash
# Bootstrap the persistent musl build disk once from the official OpenRC stage3.
set -euxo pipefail

. "${CORVUS_BUILD_SCRIPTS:-/usr/local/lib/corvus-build}/lib/disk.sh"
. "${CORVUS_BUILD_SCRIPTS:-/usr/local/lib/corvus-build}/lib/environment.sh"
. "${CORVUS_BUILD_SCRIPTS:-/usr/local/lib/corvus-build}/lib/image.sh"
. "${CORVUS_BUILD_SCRIPTS:-/usr/local/lib/corvus-build}/lib/mounts.sh"
. "${CORVUS_BUILD_SCRIPTS:-/usr/local/lib/corvus-build}/lib/portage.sh"

export IMAGE_LIBC=musl IMAGE_ROOT=/mnt/sysroot
download=""

# Verify the cached official stage3 and replace missing or corrupt downloads atomically.
fetch_stage3() {
    archive="/mnt/corvus-cache/distfiles/corvus-musl-${MUSL_STAGE3_SHA256}.tar.xz"
    if ! printf '%s  %s\n' "$MUSL_STAGE3_SHA256" "$archive" | sha256sum -c -; then
        download=$(mktemp "${archive}.XXXXXX")
        curl --fail --location --retry 3 "$MUSL_STAGE3_URL" -o "$download"
        printf '%s  %s\n' "$MUSL_STAGE3_SHA256" "$download" | sha256sum -c -
        mv "$download" "$archive"
    fi
}

# Extract a fresh builder root and give it the helper's working DNS resolver.
unpack_stage3() {
    tar --xattrs --numeric-owner -xpf "$archive" -C "$work"
    cp -L /etc/resolv.conf "$work/etc/resolv.conf"
}

# Configure an isolated musl toolchain and local binary-package cache.
configure_builder() {
    # Never inherit the helper's glibc binhost or compiler configuration.
    rm -rf "$work/etc/portage/binrepos.conf"
    cat >> "$work/etc/portage/make.conf" <<'CONF'
CHOST="x86_64-pc-linux-musl"
COMMON_FLAGS="-Os -pipe"
CFLAGS="${COMMON_FLAGS}"
CXXFLAGS="${COMMON_FLAGS}"
MAKEOPTS="-j8"
USE="-systemd -X -nls -doc -test"
DISTDIR="/mnt/corvus-cache/distfiles"
PKGDIR="/mnt/corvus-cache/packages/x86-64_musl"
GPG_VERIFY_USER_DROP=""
GPG_VERIFY_GROUP_DROP=""
ACCEPT_LICENSE="*"
CONF
    portage_configure_binhost musl "$work"
    mkdir -p "$work/etc/portage/package.use"
    cat > "$work/etc/portage/package.use/corvus-musl" <<'CONF'
# The guest agent does not need desktop MIME data or introspection tooling.
dev-libs/glib -elf -introspection -mime
sys-apps/openrc -netifrc
# OpenRC's hostname service needs this optional coreutils command.
sys-apps/coreutils hostname
CONF
}

# Populate the builder disk and retain repository metadata for future native builds.
populate_builder() {
    : "${MUSL_STAGE3_URL:?}" "${MUSL_STAGE3_SHA256:?}"
    [[ "$MUSL_STAGE3_SHA256" =~ ^[[:xdigit:]]{64}$ ]]
    work=/mnt/sysroot
    fetch_stage3
    unpack_stage3
    configure_builder
    mkdir -p "$work/var/db/repos/gentoo"
    cp -a /var/db/repos/gentoo/. "$work/var/db/repos/gentoo/"
    # Stage3 builds use musl packages only, even though bootstrap runs on glibc.
    mounts_bind /mnt/corvus-cache "$work/mnt/corvus-cache"
    install -d "$work/usr/local/lib/corvus-build"
    cp -a "${CORVUS_BUILD_SCRIPTS:-/usr/local/lib/corvus-build}/." "$work/usr/local/lib/corvus-build/"
    chroot "$work" /usr/bin/env -u CORVUS_BUILD_SCRIPTS DISTDIR=/mnt/corvus-cache/distfiles PKGDIR=/mnt/corvus-cache/packages/x86-64_musl \
        /bin/bash /usr/local/lib/corvus-build/images/gentoo-musl-openrc-builder/prepare-root.sh
    # Detach shared caches before image cleanup removes target cache contents.
    umount "$work/mnt/corvus-cache"
    unset 'mounts[-1]'
}

# Create a bootable build host while preserving Portage, headers, and compiler files.
configure_build_host() {
    # This command still runs on the glibc helper, not inside the musl root.
    # Keep any native kernel-source dependencies out of the musl package cache.
    mkdir -p /mnt/corvus-cache/packages/x86-64
    environment_check_cache_writable /mnt/corvus-cache/packages/x86-64
    PKGDIR=/mnt/corvus-cache/packages/x86-64 SYSROOT=/mnt/sysroot /bin/bash -eux "${CORVUS_BUILD_SCRIPTS:-/usr/local/lib/corvus-build}/stages/kernel/build-kernel.sh"
    disk_install_grub /mnt/sysroot /dev/vdb
    /bin/bash "${CORVUS_BUILD_SCRIPTS:-/usr/local/lib/corvus-build}/stages/musl/configure-root.sh"
    image_clean_temporary_files "$IMAGE_ROOT"
    image_reset_identity "$IMAGE_ROOT"
}

# Only this bootstrap step enters the future builder root before it can boot itself.
main() {
    mounts_initialize
    trap mounts_cleanup EXIT
    trap 'exit 130' INT
    trap 'exit 143' TERM
    trap 'exit 129' HUP
    cloud-init status --wait
    environment_prepare_workspace
    disk_partition /dev/vdb
    mounts_target_disk /mnt/sysroot /dev/vdb
    populate_builder
    configure_build_host
    image_finalize /mnt/sysroot "gentoo-musl-openrc-builder"
}

main "$@"
