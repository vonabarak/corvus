#!/bin/bash
# Definitions only; callers initialize state and own traps.

. "${CORVUS_BUILD_SCRIPTS:-/usr/local/lib/corvus-build}/lib/portage.sh"

# Initialize mount state explicitly; sourcing this library must not reset it.
mounts_initialize() {
    declare -ga mounts=()
}

# Mount a filesystem and register it only after mounting succeeds.
mounts_mount() {
    local destination=$1
    shift
    mkdir -p "$destination"
    mount "$@" "$destination"
    mounts+=("$destination")
}

# Track binds and isolate child mount events from the source mount.
mounts_bind() {
    mounts_mount "$2" --bind "$1"
    mount --make-rprivate "$2"
}

# Release nested mounts first, attempting every unmount even after a failure.
mounts_unmount() {
    local status=0 i
    for ((i=${#mounts[@]}-1; i>=0; i--)); do
        umount "${mounts[i]}" || status=1
    done
    return "$status"
}

# Preserve failed builds untouched for debugging; clean up only successful builds.
mounts_cleanup() {
    local status=$?
    trap - EXIT
    if [ "$status" -ne 0 ]; then
        exit "$status"
    fi
    set +e
    mounts_unmount || { [ "$status" -ne 0 ] || status=1; }
    exit "$status"
}

# Provide package hooks with devices and private proc/run, never the helper init socket.
mounts_target_interfaces() {
    mounts_bind /dev "$1/dev"
    mounts_mount "$1/proc" -t proc proc
    mounts_bind /sys "$1/sys"
    mounts_mount "$1/run" -t tmpfs tmpfs
}

# Mount artifact partitions by device: the helper can have identical filesystem labels.
mounts_target_disk() {
    mounts_mount "$1" "${2}3"
    mounts_mount "$1/boot/efi" "${2}2"
    mounts_target_interfaces "$1"
    portage_remove_defaults "$1"
}
