#!/bin/bash
# Definitions only; callers initialize state and own traps.

. "${CORVUS_BUILD_SCRIPTS:-/usr/local/lib/corvus-build}/lib/mounts.sh"
. "${CORVUS_BUILD_SCRIPTS:-/usr/local/lib/corvus-build}/lib/portage.sh"

# Wait for DNS before downloading repository metadata or packages.
environment_wait_for_network() {
    for attempt in $(seq 1 60); do
        getent hosts distfiles.gentoo.org >/dev/null && return 0
        sleep 2
    done
    echo 'Network DNS did not become ready after 120 seconds' >&2
    return 1
}

# Test both directory creation and file writes through virtiofs.
environment_check_cache_writable() (
    cache_path=$1
    probe=$(mktemp -d "$cache_path/.corvus-write-test.XXXXXX") || {
        echo "Cannot create directories in cache $cache_path; check host cache permissions for the user running virtiofsd." >&2
        exit 1
    }
    if ! (umask 077; : > "$probe/package"); then
        rmdir "$probe"
        echo "Cannot write files in cache $cache_path; check host cache permissions." >&2
        exit 1
    fi
    rm "$probe/package"
    rmdir "$probe"
)

# Keep distfiles shared but never mix glibc and musl binary packages.
environment_prepare_workspace() {
    unset ROOT SYSROOT
    environment_wait_for_network
    mkdir -p /mnt/corvus-cache
    if ! mountpoint -q /mnt/corvus-cache; then
        mounts_mount /mnt/corvus-cache -t virtiofs corvus-cache
    fi
    export DISTDIR=/mnt/corvus-cache/distfiles
    case "${IMAGE_LIBC:?}" in
        glibc) export PKGDIR=/mnt/corvus-cache/packages/x86-64 ;;
        musl) export PKGDIR=/mnt/corvus-cache/packages/x86-64_musl ;;
        *) echo "Unsupported libc: $IMAGE_LIBC" >&2; return 1 ;;
    esac
    # Check the download cache separately from locally compiled packages.
    binhost_cache="/mnt/corvus-cache/binhost/${PKGDIR##*/}"
    mkdir -p "$DISTDIR" "$PKGDIR" "$binhost_cache"
    environment_check_cache_writable "$DISTDIR"
    environment_check_cache_writable "$PKGDIR"
    environment_check_cache_writable "$binhost_cache"
    portage_configure_build_host /
}
