#!/bin/sh
# Build or restore the kernel in the offline target, using the shared virtiofs cache.
set -eu

CACHE_DIR=/mnt/corvus-cache/kernels
TEMP_ARCHIVE=""

# Remove an incomplete archive without exposing it to subsequent builds.
cleanup() {
    if [ -n "$TEMP_ARCHIVE" ]; then
        rm -f "$TEMP_ARCHIVE"
    fi
}

# Require the target root and the shared cache before resolving packages.
prepare_paths() {
    : "${SYSROOT:?SYSROOT must point to the target root}"
    mountpoint -q /mnt/corvus-cache
    mkdir -p "$CACHE_DIR" "$SYSROOT"
}

# Query stable amd64 ebuild metadata without downloading or unpacking sources.
select_kernel() {
    PACKAGE=$(ACCEPT_KEYWORDS=amd64 ROOT=/ SYSROOT=/ portageq best_visible / ebuild sys-kernel/gentoo-sources)
    case "$PACKAGE" in
        sys-kernel/gentoo-sources-[0-9]*) ;;
        *) echo "No stable gentoo-sources ebuild available" >&2; exit 1 ;;
    esac
    VERSION=${PACKAGE#sys-kernel/gentoo-sources-}
    # Keep ebuild revisions in the key: Gentoo patches can change between revisions.
    CONFIG_HASH=$(sha256sum /tmp/gentoo-kernel-config | cut -d " " -f 1)
    ARCHIVE="$CACHE_DIR/kernel-${VERSION}-${CONFIG_HASH}.tar.gz"
}

# Install exactly the selected sources only when their compiled archive is absent.
install_sources() {
    ROOT=/ SYSROOT=/ emerge --buildpkg --oneshot --noreplace --quiet-build "=$PACKAGE"
    case "$VERSION" in
        *-r*) KSRC="/usr/src/linux-${VERSION%-r*}-gentoo-r${VERSION##*-r}" ;;
        *) KSRC="/usr/src/linux-${VERSION}-gentoo" ;;
    esac
    test -d "$KSRC"
}

# Resolve config defaults after installing sources; cache hits need neither step.
configure_kernel() {
    install -m 0644 /tmp/gentoo-kernel-config "$KSRC/.config"
    make -C "$KSRC" -j"$(nproc)" ARCH=x86_64 olddefconfig
    KVER=$(make -C "$KSRC" -s ARCH=x86_64 kernelrelease)
    test -n "$KVER"
}

# Compile and install only this kernel's boot files and modules into the target.
build_kernel() {
    make -C "$KSRC" -j"$(nproc)" ARCH=x86_64 bzImage modules
    make -C "$KSRC" -j"$(nproc)" ARCH=x86_64 \
        INSTALL_MOD_PATH="$SYSROOT" modules_install
    install -d "$SYSROOT/boot"
    install -m 0644 "$KSRC/arch/x86/boot/bzImage" "$SYSROOT/boot/vmlinuz-$KVER"
    install -m 0644 "$KSRC/System.map" "$SYSROOT/boot/System.map-$KVER"
    install -m 0644 "$KSRC/.config" "$SYSROOT/boot/config-$KVER"
}

# Publish atomically so interrupted or concurrent builds never expose partial tarballs.
save_kernel() {
    TEMP_ARCHIVE=$(mktemp "$ARCHIVE.XXXXXX")
    # Relative members restore under SYSROOT without capturing unrelated target files.
    tar -czpf "$TEMP_ARCHIVE" -C "$SYSROOT" \
        "boot/vmlinuz-$KVER" "boot/System.map-$KVER" "boot/config-$KVER" \
        "lib/modules/$KVER"
    mv -f "$TEMP_ARCHIVE" "$ARCHIVE"
    TEMP_ARCHIVE=""
    echo "Saved kernel archive: $ARCHIVE"
}

# Restore boot artifacts and module metadata without compiling or running modules_install.
restore_kernel() {
    echo "Restoring kernel archive: $ARCHIVE"
    tar -xzpf "$ARCHIVE" -C "$SYSROOT"
}

# Resolve the stable ebuild first; only cache misses need sources and compilation.
main() {
    trap cleanup 0
    prepare_paths
    select_kernel
    if [ -f "$ARCHIVE" ]; then
        restore_kernel
    else
        install_sources
        configure_kernel
        build_kernel
        save_kernel
    fi
}

main "$@"
