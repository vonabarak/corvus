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
    # Refresh trusted release keys through Gentoo's configured HKPS keyserver.
    # WKD availability must not prevent using that signed-repository path.
    install -d /etc/portage/repos.conf
    printf '[gentoo]\nsync-openpgp-key-refresh = keyserver\nsync-openpgp-keyserver = %s\n' \
        "${PORTAGE_KEYSERVER:-hkps://keys.gentoo.org}" \
        > /etc/portage/repos.conf/rescue-sync.conf
    portage_sync_repository
    rescue_configure_portage
    rescue_partition
    mounts_target_disk "$IMAGE_ROOT" "$TARGET_DISK"
    # Reuse the native @system bootstrap, never the VM kernel/runtime stages.
    /bin/bash "${CORVUS_BUILD_SCRIPTS:-/usr/local/lib/corvus-build}/stages/glibc/install-root.sh"
    rescue_install_packages
    rescue_configure_runtime
    /bin/bash "${CORVUS_BUILD_SCRIPTS:-/usr/local/lib/corvus-build}/images/gentoo-rescue/kernel.sh"
    disk_install_grub "$IMAGE_ROOT" "$TARGET_DISK"
    rescue_write_grub
    image_reset_identity "$IMAGE_ROOT"
    image_finalize "$IMAGE_ROOT" gentoo-rescue
    # The home filesystem is intentionally empty, including hidden seed files.
    sync
}

main "$@"
