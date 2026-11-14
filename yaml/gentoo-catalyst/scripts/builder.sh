#!/bin/bash
# Reusable tools only: no runtime root or kernel installation lives here.
set -euo pipefail

. "${CORVUS_BUILD_SCRIPTS:-/usr/local/lib/corvus-build}/lib/environment.sh"
. "${CORVUS_BUILD_SCRIPTS:-/usr/local/lib/corvus-build}/lib/image.sh"

main() {
    mounts_initialize
    trap mounts_cleanup EXIT
    IMAGE_LIBC=glibc environment_prepare_workspace
    LC_ALL=C localedef -i C -f UTF-8 C.UTF-8
    portage_sync_repository
    install -d /etc/portage/package.use /etc/portage/package.accept_keywords
    printf '%s\n' 'dev-util/catalyst -iso -qcow2' > /etc/portage/package.use/catalyst
    printf '%s\n' 'sys-apps/util-linux python' > /etc/portage/package.use/catalyst-deps
    printf '%s\n' '=dev-util/catalyst-4.2.0 ~amd64' > /etc/portage/package.accept_keywords/catalyst
    printf '%s\n' 'sys-boot/grub grub_platforms_pc grub_platforms_efi-64 -mount -nls -fonts -themes' > /etc/portage/package.use/grub
    portage_emerge --noreplace --quiet-build =dev-util/catalyst-4.2.0 \
        dev-vcs/git net-misc/curl app-arch/tar app-arch/xz-utils \
        sys-apps/gptfdisk sys-block/parted sys-fs/e2fsprogs sys-fs/dosfstools sys-boot/grub
    # Catalyst must mount squashfs snapshots and create isolated chroots.
    unshare --mount --uts --ipc true
    modprobe loop
    modprobe squashfs
    grep -qw squashfs /proc/filesystems
    image_finalize / "${CORVUS_BUILD_NAME:?}" --keep-repositories
}

main "$@"
