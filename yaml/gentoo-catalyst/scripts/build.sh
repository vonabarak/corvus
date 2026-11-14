#!/bin/bash
# Catalyst owns the root and kernel; this adapter owns verified inputs and the disk.
set -euo pipefail

. "${CORVUS_BUILD_SCRIPTS:-/usr/local/lib/corvus-build}/lib/environment.sh"
. "${CORVUS_BUILD_SCRIPTS:-/usr/local/lib/corvus-build}/lib/disk.sh"
. "${CORVUS_BUILD_SCRIPTS:-/usr/local/lib/corvus-build}/lib/image.sh"
. "${CORVUS_BUILD_SCRIPTS:-/usr/local/lib/corvus-build}/catalyst/lib.sh"

main() {
    catalyst_validate_inputs
    mounts_initialize
    trap mounts_cleanup EXIT
    IMAGE_LIBC=glibc environment_prepare_workspace
    local assets=${CORVUS_BUILD_SCRIPTS:-/usr/local/lib/corvus-build}/catalyst
    local work seed config_hash repo archive
    work=$(mktemp -d /var/tmp/corvus-catalyst.XXXXXX)
    seed=${STAGE3_URL##*/}
    config_hash=$(sha256sum "$assets/kernel.config" | cut -d ' ' -f1)
    mkdir -p "$work/builds/seed" "$work/repos" /mnt/corvus-cache/catalyst/packages/amd64-systemd
    # Serialize users of the shared Catalyst caches, including warm-cache builds.
    exec 9>/mnt/corvus-cache/catalyst/build.lock
    flock 9
    export GNUPGHOME="$work/gnupg"
    install -d -m 0700 "$GNUPGHOME"
    gpg --batch --import /usr/share/openpgp-keys/gentoo-release.asc
    curl -fsSL "$STAGE3_URL" -o "$work/builds/seed/$seed"
    curl -fsSL "$STAGE3_URL.sha256" -o "$work/seed.sha256.asc"
    gpg --batch --output "$work/seed.sha256" --decrypt "$work/seed.sha256.asc"
    catalyst_verify_seed "$work/builds/seed/$seed" "$work/seed.sha256"
    repo="$work/repos/gentoo.git"
    git init --bare "$repo"
    git -C "$repo" fetch --depth=1 https://anongit.gentoo.org/git/repo/sync/gentoo.git "$REPO_COMMIT"
    git -C "$repo" verify-commit "$REPO_COMMIT"
    sed "s|@WORK@|$work|g" "$assets/catalyst.toml" > "$work/catalyst.toml"
    sed -e "s|@REPO_COMMIT@|$REPO_COMMIT|g" -e "s|@SEED@|${seed%.tar.xz}|g" "$assets/stage4.spec" > "$work/stage4.spec"
    catalyst -c "$work/catalyst.toml" --snapshot "$REPO_COMMIT"
    catalyst -c "$work/catalyst.toml" -f "$work/stage4.spec"
    # Release the cache lock before the EXIT trap unmounts its filesystem.
    exec 9>&-
    archive="$work/builds/corvus/stage4-amd64-headless.tar.xz"
    catalyst_check_archive "$archive"
    : "${IMAGE_ROOT:?}" "${TARGET_DISK:?}"
    disk_partition "$TARGET_DISK"
    mounts_mount "$IMAGE_ROOT" "${TARGET_DISK}3"
    mounts_mount "$IMAGE_ROOT/boot/efi" "${TARGET_DISK}2"
    tar --extract --file "$archive" --directory "$IMAGE_ROOT" --numeric-owner --same-owner \
        --same-permissions --acls --xattrs --xattrs-include='*'
    mounts_target_interfaces "$IMAGE_ROOT"
    disk_write_fstab "$IMAGE_ROOT"
    disk_install_grub "$IMAGE_ROOT" "$TARGET_DISK"
    disk_write_grub_config "$IMAGE_ROOT" 'Gentoo Linux (Catalyst custom kernel)'
    # Catalyst rewrites make.conf during cleanup. Restore only runtime policy.
    rm -f "$IMAGE_ROOT/etc/portage/binrepos.conf/gentoo.conf"
    portage_configure_binhost glibc "$IMAGE_ROOT" /var/cache/binhost
    image_reset_identity "$IMAGE_ROOT"
    image_finalize "$IMAGE_ROOT" "${CORVUS_BUILD_NAME:?}"
    printf 'stage3: %s\nsha256: %s\nrepository: %s\ncatalyst: 4.2.0\nkernel_config: %s\n' \
        "$STAGE3_URL" "$STAGE3_SHA256" "$REPO_COMMIT" "$config_hash" >> "$IMAGE_ROOT/etc/corvus-image-info"
}

main "$@"
