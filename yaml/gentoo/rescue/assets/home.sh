#!/bin/bash
# Share the initramfs storage mount; never unmount the backing root filesystem.
set -eu

home_try_persistent() {
    local media=$1 destination=$2 probe options
    mountpoint -q "$media" || return 1
    options=$(findmnt -nr -o OPTIONS --target "$media") || return 1
    case ",$options," in *,rw,*) ;; *) return 1 ;; esac
    test "$(timeout 3 df --output=avail "$media" | tail -1)" -gt 0 || return 1
    test "$(timeout 3 df --output=iavail "$media" | tail -1)" -gt 0 || return 1
    test -d "$media/home/root" && test ! -L "$media/home" && test ! -L "$media/home/root" || return 1
    probe=$(mktemp "$media/home/root/.rescue-write-test.XXXXXX") || return 1
    if ! timeout 5 dd if=/dev/zero of="$probe" bs=4096 count=1 conv=fsync status=none; then
        rm -f "$probe"
        return 1
    fi
    rm -f "$probe" || return 1
    chmod 0700 "$media/home/root" || return 1
    timeout 10 mount --bind "$media/home/root" "$destination" || return 1
    mount -o remount,bind,nosuid,nodev "$destination" || return 1
}

home_initialize() {
    local media=$1 destination=$2
    # /boot remains available even when home falls back to RAM.
    mount --bind "$media/boot" /boot
    if home_try_persistent "$media" "$destination"; then
        echo 'Rescue home: persistent storage'
        return 0
    fi
    if mountpoint -q "$destination"; then
        timeout 5 umount "$destination" || true
    fi
    # Cover any failed home bind, even if its unmount failed.
    mount -t tmpfs -o mode=0700,nosuid,nodev tmpfs "$destination"
    echo 'Rescue home: empty RAM fallback (changes will be lost)'
}

if [[ ${BASH_SOURCE[0]} == "$0" ]]; then
    home_initialize /run/initramfs/rescue-media /root
fi
