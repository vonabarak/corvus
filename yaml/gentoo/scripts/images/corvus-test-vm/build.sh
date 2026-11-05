#!/bin/bash
# Build the integration-test guest offline using the native musl builder.
set -euxo pipefail

. "${CORVUS_BUILD_SCRIPTS:-/usr/local/lib/corvus-build}/lib/disk.sh"
. "${CORVUS_BUILD_SCRIPTS:-/usr/local/lib/corvus-build}/lib/environment.sh"
. "${CORVUS_BUILD_SCRIPTS:-/usr/local/lib/corvus-build}/lib/mounts.sh"
. "${CORVUS_BUILD_SCRIPTS:-/usr/local/lib/corvus-build}/lib/runtime.sh"
. "${CORVUS_BUILD_SCRIPTS:-/usr/local/lib/corvus-build}/images/corvus-test-vm/lib.sh"

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
    /bin/bash "${CORVUS_BUILD_SCRIPTS:-/usr/local/lib/corvus-build}/stages/musl/install-root.sh"
    /bin/bash "${CORVUS_BUILD_SCRIPTS:-/usr/local/lib/corvus-build}/images/corvus-test-vm/install-root.sh"
    SYSROOT="$IMAGE_ROOT" /bin/bash "${CORVUS_BUILD_SCRIPTS:-/usr/local/lib/corvus-build}/stages/kernel/build-kernel.sh"
    disk_install_grub "$IMAGE_ROOT" "$TARGET_DISK"
    /bin/bash "${CORVUS_BUILD_SCRIPTS:-/usr/local/lib/corvus-build}/stages/musl/configure-root.sh"
    /bin/bash "${CORVUS_BUILD_SCRIPTS:-/usr/local/lib/corvus-build}/images/corvus-test-vm/configure-root.sh"
    runtime_verify
    test_vm_finish
    runtime_finish
}

main "$@"
