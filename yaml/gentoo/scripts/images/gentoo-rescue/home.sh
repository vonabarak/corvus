#!/bin/bash
# A failed persistent home must never block rescue access or expose saved data.
set -eu

home_try_persistent() {
    local destination=$2 device="/dev/disk/by-uuid/$1" attempt probe
    for attempt in {1..8}; do
        test -b "$device" && break
        sleep 1
    done
    test -b "$device" || return 1
    test "$(blockdev --getro "$device")" = 0 || return 1
    timeout 10 mount -t ext4 -o rw,nosuid,nodev "$device" "$destination" || return 1
    # Include inode exhaustion; df's available blocks exclude the reserved pool.
    test "$(timeout 3 df --output=avail "$destination" | tail -1)" -gt 0 || return 1
    test "$(timeout 3 df --output=iavail "$destination" | tail -1)" -gt 0 || return 1
    probe=$(mktemp "$destination/.rescue-write-test.XXXXXX") || return 1
    if ! timeout 5 dd if=/dev/zero of="$probe" bs=4096 count=1 conv=fsync status=none; then
        rm -f "$probe"
        return 1
    fi
    rm -f "$probe"
    chmod 0700 "$destination"
}

home_initialize() {
    local uuid=$1 destination=$2
    if home_try_persistent "$uuid" "$destination"; then
        echo 'Rescue home: persistent storage'
        return 0
    fi
    # Cover an unmountable or failed mounted filesystem with a fresh tmpfs.
    # Even if unmount fails, the fallback hides all saved files.
    if mountpoint -q "$destination"; then
        timeout 5 umount "$destination" || true
    fi
    mount -t tmpfs -o mode=0700,nosuid,nodev tmpfs "$destination"
    echo 'Rescue home: empty RAM fallback (changes will be lost)'
}

main() {
    home_initialize "$(cat /etc/rescue-home-uuid)" /root
}

if [[ ${BASH_SOURCE[0]} == "$0" ]]; then
    main "$@"
fi
