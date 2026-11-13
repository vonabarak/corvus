#!/bin/bash
# Build the removable rescue disk using the native glibc builder.
set -euxo pipefail

. "${CORVUS_BUILD_SCRIPTS:-/usr/local/lib/corvus-build}/lib/environment.sh"
. "${CORVUS_BUILD_SCRIPTS:-/usr/local/lib/corvus-build}/lib/disk.sh"
. "${CORVUS_BUILD_SCRIPTS:-/usr/local/lib/corvus-build}/lib/image.sh"
. "${CORVUS_BUILD_SCRIPTS:-/usr/local/lib/corvus-build}/images/gentoo-rescue/lib.sh"

main() {
    : "${IMAGE_ROOT:?}" "${TARGET_DISK:?}"
    mounts_initialize
    trap mounts_cleanup EXIT
    trap 'exit 130' INT
    trap 'exit 143' TERM
    trap 'exit 129' HUP
    environment_prepare_workspace
    portage_sync_repository
    rescue_configure_portage
    rescue_partition
    mounts_mount "$IMAGE_ROOT" -t tmpfs -o size=16G,mode=0755 tmpfs
    mounts_mount "${RESCUE_MEDIA:?}" "${TARGET_DISK}3"
    mounts_mount "${RESCUE_ESP:?}" "${TARGET_DISK}2"
    mounts_target_interfaces "$IMAGE_ROOT"
    # Reuse the native @system bootstrap, never the VM kernel/runtime stages.
    /bin/bash "${CORVUS_BUILD_SCRIPTS:-/usr/local/lib/corvus-build}/stages/glibc/install-root.sh"
    rescue_install_packages
    rescue_configure_runtime
    /bin/bash "${CORVUS_BUILD_SCRIPTS:-/usr/local/lib/corvus-build}/images/gentoo-rescue/kernel.sh"
    image_reset_identity "$IMAGE_ROOT"
    image_finalize "$IMAGE_ROOT" gentoo-rescue
    rescue_pack_system
    rescue_install_grub
    rescue_write_grub
    sync
}

main "$@"
