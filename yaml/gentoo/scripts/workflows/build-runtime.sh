#!/bin/bash
# Build either runtime offline using its native reusable builder.
set -euxo pipefail

. "${CORVUS_BUILD_SCRIPTS:-/usr/local/lib/corvus-build}/lib/disk.sh"
. "${CORVUS_BUILD_SCRIPTS:-/usr/local/lib/corvus-build}/lib/environment.sh"
. "${CORVUS_BUILD_SCRIPTS:-/usr/local/lib/corvus-build}/lib/mounts.sh"
. "${CORVUS_BUILD_SCRIPTS:-/usr/local/lib/corvus-build}/lib/image.sh"

# Populate a fresh target and configure it without booting the guest.
main() {
    mounts_initialize
    trap mounts_cleanup EXIT
    trap 'exit 130' INT
    trap 'exit 143' TERM
    trap 'exit 129' HUP
    : "${IMAGE_ROOT:?}" "${TARGET_DISK:?}" "${IMAGE_INIT:?}"
    environment_prepare_workspace
    portage_sync_repository
    disk_partition "$TARGET_DISK"
    mounts_target_disk "$IMAGE_ROOT" "$TARGET_DISK"
    /bin/bash "${CORVUS_BUILD_SCRIPTS:-/usr/local/lib/corvus-build}/stages/${IMAGE_LIBC}/install-root.sh"
    SYSROOT="$IMAGE_ROOT" /bin/bash "${CORVUS_BUILD_SCRIPTS:-/usr/local/lib/corvus-build}/stages/kernel/build-kernel.sh"
    disk_install_grub "$IMAGE_ROOT" "$TARGET_DISK"
    /bin/bash "${CORVUS_BUILD_SCRIPTS:-/usr/local/lib/corvus-build}/stages/${IMAGE_LIBC}/configure-root.sh"
    image_reset_identity "$IMAGE_ROOT"
    image_finalize "$IMAGE_ROOT" "${CORVUS_BUILD_NAME:?}"
}

main "$@"
