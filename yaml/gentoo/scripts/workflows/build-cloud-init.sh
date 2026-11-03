#!/bin/bash
# Add cloud-init offline using the same native builder as the base runtime.
set -euxo pipefail

. "${CORVUS_BUILD_SCRIPTS:-/usr/local/lib/corvus-build}/lib/environment.sh"
. "${CORVUS_BUILD_SCRIPTS:-/usr/local/lib/corvus-build}/lib/mounts.sh"
. "${CORVUS_BUILD_SCRIPTS:-/usr/local/lib/corvus-build}/lib/runtime.sh"

# Mount the existing image, install target services, and publish a clean overlay.
main() {
    mounts_initialize
    trap mounts_cleanup EXIT
    trap 'exit 130' INT
    trap 'exit 143' TERM
    trap 'exit 129' HUP
    : "${IMAGE_ROOT:?}" "${TARGET_DISK:?}" "${IMAGE_INIT:?}"
    environment_prepare_workspace
    mounts_target_disk "$IMAGE_ROOT" "$TARGET_DISK"
    /bin/bash "${CORVUS_BUILD_SCRIPTS:-/usr/local/lib/corvus-build}/stages/install-cloud-init.sh"
    runtime_finish
}

main "$@"
