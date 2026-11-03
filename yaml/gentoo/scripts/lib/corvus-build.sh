#!/bin/bash
# Definitions only; callers initialize state and own traps.

. "${CORVUS_BUILD_SCRIPTS:-/usr/local/lib/corvus-build}/lib/environment.sh"
. "${CORVUS_BUILD_SCRIPTS:-/usr/local/lib/corvus-build}/lib/mounts.sh"
. "${CORVUS_BUILD_SCRIPTS:-/usr/local/lib/corvus-build}/lib/portage.sh"
. "${CORVUS_BUILD_SCRIPTS:-/usr/local/lib/corvus-build}/lib/postgres.sh"

# Wait for the grown helper and validate artifact partitions before mounting.
corvus_build_check_prerequisites() {
    cloud-init status --wait
    # Cloud-init grows only the helper; compilation must not fill the smaller artifact.
    [ "$(df --output=size -B1 / | tail -1)" -ge 30000000000 ]
    environment_wait_for_network
    [ "$(blkid -s TYPE -o value "${TARGET_DISK:-/dev/vdb}3")" = ext4 ]
    [ "$(blkid -s TYPE -o value "${TARGET_DISK:-/dev/vdb}2")" = vfat ]
}

# Preserve failed builds untouched for debugging; clean up only successful builds.
corvus_build_cleanup() {
    local status=$?
    trap - EXIT
    if [ "$status" -ne 0 ]; then
        exit "$status"
    fi
    set +e
    postgres_stop || { [ "$status" -ne 0 ] || status=1; }
    mounts_unmount || { [ "$status" -ne 0 ] || status=1; }
    # Failed builds retain their workspace for inspection; never remove mounted data.
    if [ "$status" -eq 0 ] && [ -n "$workspace" ]; then
        rm -rf -- "$workspace"
    fi
    exit "$status"
}

# Use tracked, disposable repository storage and private target interfaces.
corvus_build_prepare_mounts() {
    workspace=$(mktemp -d /var/tmp/corvus-test.XXXXXX)
    # Repository sync drops privileges even when package userpriv is disabled.
    chmod 0755 "$workspace"
    # Native Portage resolves repositories on the helper, not beneath ROOT.
    mounts_mount /var/db/repos -t tmpfs tmpfs
    environment_prepare_workspace
    # Select devices explicitly: helper and artifact have identical filesystem labels.
    mounts_target_disk "$root" "${TARGET_DISK:-/dev/vdb}"
    mounts_bind /dev/pts "$root/dev/pts"
    mounts_bind /dev/shm "$root/dev/shm"
    # PostgreSQL pkg_config needs the same ebuild tree inside its individual chroot.
    mounts_bind /var/db/repos "$root/var/db/repos"
    local cache source
    for cache in distfiles binpkgs binhost; do
        case "$cache" in
            binpkgs) source=packages/x86-64 ;;
            *) source=$cache ;;
        esac
        mounts_bind "/mnt/corvus-cache/$source" "$root/var/cache/$cache"
    done
    # All compilation uses the larger helper disk, including native build dependencies.
    export PORTAGE_TMPDIR="$workspace"
    portage_configure_glibc_caches
}

# Declare the same Git overlays for the native helper and offline artifact.
corvus_build_write_repositories() {
    local config_root=${1%/}
    install -d "$config_root/etc/portage/repos.conf"
    cat > "$config_root/etc/portage/repos.conf/corvus.conf" <<'EOF'
[vonabarak]
location = /var/db/repos/vonabarak
sync-type = git
sync-uri = https://github.com/vonabarak/gentoo.git

[haskell]
location = /var/db/repos/haskell
sync-type = git
sync-uri = https://github.com/gentoo-haskell/gentoo-haskell.git
EOF
}
