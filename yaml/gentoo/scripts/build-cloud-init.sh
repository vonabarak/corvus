#!/bin/bash
# Add cloud-init offline using the same native builder as the base runtime.
set -euxo pipefail
. "${CORVUS_BUILD_LIB:-/usr/local/lib/corvus-build}/build.sh"

# Mount the existing image, install target services, and publish a clean overlay.
main() {
    initialize_mount_tracking
    trap cleanup EXIT
    : "${IMAGE_ROOT:?}" "${TARGET_DISK:?}" "${IMAGE_INIT:?}"
    prepare_workspace
    mount_target_disk "$IMAGE_ROOT" "$TARGET_DISK"
    /bin/bash /tmp/install-cloud-init.sh
    finish_runtime
}

main "$@"
