#!/bin/sh
. /lib/dracut-lib.sh
set -eu
uuid=$1
device="/dev/disk/by-uuid/$uuid"
media=/run/initramfs/rescue-media
lower=/run/initramfs/rescue-lower
overlay=/run/initramfs/rescue-overlay
mkdir -p "$media" "$lower" "$overlay" /sysroot
if [ "${2:-disk}" = ram ]; then
    source=/run/initramfs/rescue-source
    state=/run/initramfs/rescue-loop
    step='initialize RAM storage'
    loop=
    cleanup_ram_boot() {
        status=$?
        [ "$status" -ne 0 ] || return 0
        echo "Rescue RAM boot failed: $step. Reboot and select Gentoo rescue." >&2
        if mountpoint -q "$overlay"; then umount "$overlay" || :; fi
        if mountpoint -q "$lower"; then umount "$lower" || :; fi
        if [ -n "$loop" ]; then losetup -d "$loop" || :; fi
        if mountpoint -q "$media"; then umount "$media" || :; fi
        if mountpoint -q "$source"; then umount "$source" || :; fi
        rm -f "$state"
    }
    trap cleanup_ram_boot EXIT
    trap 'exit 1' HUP INT TERM
    step='reserve memory for the RAM copy'
    limit=$(awk '
        $1 == "MemTotal:" { half = int($2 / 2) }
        $1 == "MemAvailable:" { budget = $2 - 1048576 }
        END {
            if (half <= 0 || budget <= 0) exit 1
            print (half < budget ? half : budget)
        }
    ' /proc/meminfo)
    step='mount the USB read-only'
    mkdir -p "$source"
    timeout 20 mount -t ext4 -o ro,noload,noatime "$device" "$source"
    step='mount the RAM storage'
    mount -t tmpfs -o "mode=0700,noswap,size=${limit}k" tmpfs "$media"
    step='copy rescue-data (check the available RAM and USB readability)'
    echo 'Copying rescue-data to RAM...'
    set --
    for entry in "$source"/* "$source"/.[!.]* "$source"/..?*; do
        [ -e "$entry" ] || [ -L "$entry" ] || continue
        [ "${entry##*/}" != lost+found ] || continue
        set -- "$@" "$entry"
    done
    cp -a --sparse=always -- "$@" "$media/"
    step='unmount the USB'
    timeout 20 umount "$source"
    step='mount the copied SquashFS and root overlay'
else
    if [ "$(blockdev --getro "$device")" != 0 ] || \
        ! timeout 20 mount -t ext4 -o rw,noatime "$device" "$media"; then
        timeout 20 mount -t ext4 -o ro,noload,noatime "$device" "$media"
    fi
fi
test -f "$media/root.squashfs"
loop=$(losetup --find --show --read-only "$media/root.squashfs")
printf '%s\n' "$loop" > /run/initramfs/rescue-loop
mount -t squashfs -o ro "$loop" "$lower"
mount -t tmpfs -o mode=0755 tmpfs "$overlay"
mkdir -p "$overlay/upper" "$overlay/work"
need_shutdown
if [ -n "${source:-}" ]; then
    echo 'Rescue data is in RAM. The USB can be removed.'
fi
