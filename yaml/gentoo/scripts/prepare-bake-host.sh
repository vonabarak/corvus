#!/bin/bash
# Prepare caches, install tools, and finalize the reusable glibc builder.
set -eu
. "${CORVUS_BUILD_LIB:-/usr/local/lib/corvus-build}/build.sh"

# Mount the shared cache and expose the paths expected by native Portage.
mount_build_caches() {
    mkdir -p /mnt/corvus-cache
    mountpoint -q /mnt/corvus-cache || mount -t virtiofs corvus-cache /mnt/corvus-cache
    local cache cache_source
    for cache in distfiles binpkgs binhost; do
        # Only locally built packages use an architecture-specific cache directory.
        case "$cache" in
            binpkgs) cache_source=packages/x86-64 ;;
            *) cache_source=$cache ;;
        esac
        mkdir -p "/mnt/corvus-cache/$cache_source" "/var/cache/$cache"
        mountpoint -q "/var/cache/$cache" || mount --bind "/mnt/corvus-cache/$cache_source" "/var/cache/$cache"
        check_cache_writable "/var/cache/$cache"
    done
}

# Generate the native Portage locale and persist repository metadata.
prepare_repository() {
    # Prepare the reusable bake host using the shared package cache.
    unset SYSROOT
    # Portage and package build scripts request C.UTF-8 (also spelled C.utf8).
    # Generate it before any emerge runs, even when the cloud image lacks it.
    LC_ALL=C localedef -i C -f UTF-8 C.UTF-8

    # Keep the repository on disk in this reusable builder image.
    emerge-webrsync -k
}

# Choose the native systemd profile and both GRUB platforms.
configure_builder() {
    ln -sfn ../../var/db/repos/gentoo/profiles/default/linux/amd64/23.0/no-multilib/systemd \
      /etc/portage/make.profile
    install -d /etc/portage/package.use
    cat > /etc/portage/package.use/grub <<'EOF'
sys-boot/grub grub_platforms_pc grub_platforms_efi-64 -mount -nls -fonts -themes
EOF

    # Local and official caches can contain unsigned or partially signed gpkgs.
    # The empty GPG drops also avoid Gentoo's missing nobody:nogroup lookup.
    cat >> /etc/portage/make.conf <<'EOF'

ACCEPT_LICENSE="*"
USE="bash-completion"
GPG_VERIFY_USER_DROP=""
GPG_VERIFY_GROUP_DROP=""
EOF

    configure_glibc_caches
}

# Install native tools once; the musl builder is bootstrapped separately.
install_builder_tools() {
    command -v getuto >/dev/null || emerge_packages --buildpkg -1q app-portage/getuto
    getuto

    emerge_packages --buildpkg --noreplace --quiet-build \
      sys-apps/gptfdisk \
      sys-block/parted \
      sys-fs/e2fsprogs \
      sys-fs/dosfstools \
      sys-boot/grub:2
    emerge_packages --buildpkg --noreplace --quiet-build net-misc/curl app-arch/tar app-arch/xz-utils
}

# Retry with the new Portage after a Python-target transition.
update_builder() {
    # Portage can crash after installing its replacement during a Python-target
    # transition. The second pass runs the new Portage and completes the update.
    emerge_packages --buildpkg --update --deep --newuse \
      --binpkg-respect-use=y --quiet-build --keep-going=y -v @world || \
    emerge_packages --buildpkg --update --deep --newuse \
      --binpkg-respect-use=y --quiet-build --keep-going=y -v @world
}

# Validate shared caches before any repository or package download.
prepare_caches() {
    wait_for_network
    mount_build_caches
}

# Keep shared package caches intact while publishing the live builder overlay.
finalize_builder() {
    clean_image_scratch_files "$IMAGE_ROOT"
    finalize_image "$IMAGE_ROOT" "${CORVUS_BUILD_NAME:?}"
}

# Complete each phase before starting the next; failures prevent publication.
main() {
    : "${IMAGE_ROOT:?}"
    unset ROOT SYSROOT
    prepare_caches
    prepare_repository
    configure_builder
    install_builder_tools
    update_builder
    finalize_builder
}

main "$@"
