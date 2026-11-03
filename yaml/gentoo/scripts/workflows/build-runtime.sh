#!/bin/bash
# Build either runtime offline using its native reusable builder.
set -euxo pipefail

. "${CORVUS_BUILD_SCRIPTS:-/usr/local/lib/corvus-build}/lib/disk.sh"
. "${CORVUS_BUILD_SCRIPTS:-/usr/local/lib/corvus-build}/lib/environment.sh"
. "${CORVUS_BUILD_SCRIPTS:-/usr/local/lib/corvus-build}/lib/mounts.sh"
. "${CORVUS_BUILD_SCRIPTS:-/usr/local/lib/corvus-build}/lib/runtime.sh"

# Populate a fresh target and configure it without booting the guest.
main() {
    mounts_initialize
    trap mounts_cleanup EXIT
    trap 'exit 130' INT
    trap 'exit 143' TERM
    trap 'exit 129' HUP
    : "${IMAGE_ROOT:?}" "${TARGET_DISK:?}" "${IMAGE_INIT:?}"
    environment_prepare_workspace
    disk_partition "$TARGET_DISK"
    mounts_target_disk "$IMAGE_ROOT" "$TARGET_DISK"
    /bin/bash "${CORVUS_BUILD_SCRIPTS:-/usr/local/lib/corvus-build}/stages/${IMAGE_LIBC}/install-root.sh"
    if [[ "${IMAGE_PROFILE:-}" == test-vm ]]; then
        /bin/bash "${CORVUS_BUILD_SCRIPTS:-/usr/local/lib/corvus-build}/images/corvus-test-vm/install-root.sh"
    fi
    SYSROOT="$IMAGE_ROOT" /bin/bash "${CORVUS_BUILD_SCRIPTS:-/usr/local/lib/corvus-build}/stages/kernel/build-kernel.sh"
    disk_install_grub "$IMAGE_ROOT" "$TARGET_DISK"
    /bin/bash "${CORVUS_BUILD_SCRIPTS:-/usr/local/lib/corvus-build}/stages/${IMAGE_LIBC}/configure-root.sh"
    if [[ "$IMAGE_LIBC" == musl ]] && compgen -G "$IMAGE_ROOT/var/db/pkg/dev-lang/python-[0-9]*" >/dev/null; then
        echo 'Unexpected runtime package: dev-lang/python' >&2
        exit 1
    fi
    if [[ "${IMAGE_PROFILE:-}" == test-vm ]]; then
        /bin/bash "${CORVUS_BUILD_SCRIPTS:-/usr/local/lib/corvus-build}/images/corvus-test-vm/configure-root.sh"
    fi
    if [[ "${IMAGE_PROFILE:-}" == test-vm ]]; then
        . "${CORVUS_BUILD_SCRIPTS:-/usr/local/lib/corvus-build}/images/corvus-test-vm/lib.sh"
        runtime_verify
        test_vm_finish
    fi
    runtime_finish
}

main "$@"
