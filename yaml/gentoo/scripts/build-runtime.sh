#!/bin/bash
# Build either runtime offline using its native reusable builder.
set -euxo pipefail
. "${CORVUS_BUILD_LIB:-/usr/local/lib/corvus-build}/build.sh"

# Populate a fresh target and configure it without booting the guest.
main() {
    initialize_mount_tracking
    trap cleanup EXIT
    trap 'exit 130' INT
    trap 'exit 143' TERM
    trap 'exit 129' HUP
    : "${IMAGE_ROOT:?}" "${TARGET_DISK:?}" "${IMAGE_INIT:?}"
    prepare_workspace
    partition_target_disk "$TARGET_DISK"
    mount_target_disk "$IMAGE_ROOT" "$TARGET_DISK"
    /bin/bash "/tmp/install-${IMAGE_LIBC}-root.sh"
    if [[ "${IMAGE_PROFILE:-}" == test-vm ]]; then
        /bin/bash /tmp/install-test-vm-root.sh
    fi
    SYSROOT="$IMAGE_ROOT" /bin/sh /tmp/build-kernel.sh
    install_target_grub "$IMAGE_ROOT" "$TARGET_DISK"
    /bin/bash "/tmp/configure-${IMAGE_LIBC}-root.sh"
    if [[ "$IMAGE_LIBC" == musl ]] && compgen -G "$IMAGE_ROOT/var/db/pkg/dev-lang/python-[0-9]*" >/dev/null; then
        echo 'Unexpected runtime package: dev-lang/python' >&2
        exit 1
    fi
    if [[ "${IMAGE_PROFILE:-}" == test-vm ]]; then
        /bin/bash /tmp/configure-test-vm-root.sh
    fi
    finish_runtime
}

main "$@"
