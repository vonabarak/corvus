#!/bin/sh
. /lib/dracut-lib.sh
set -eu
uuid=$1
device="/dev/disk/by-uuid/$uuid"
media=/run/initramfs/rescue-media
lower=/run/initramfs/rescue-lower
overlay=/run/initramfs/rescue-overlay
mkdir -p "$media" "$lower" "$overlay" /sysroot
if [ "$(blockdev --getro "$device")" != 0 ] || \
    ! timeout 20 mount -t ext4 -o rw,noatime "$device" "$media"; then
    timeout 20 mount -t ext4 -o ro,noload,noatime "$device" "$media"
fi
test -f "$media/rescue/root.squashfs"
loop=$(losetup --find --show --read-only "$media/rescue/root.squashfs")
printf '%s\n' "$loop" > /run/initramfs/rescue-loop
mount -t squashfs -o ro "$loop" "$lower"
mount -t tmpfs -o mode=0755 tmpfs "$overlay"
mkdir -p "$overlay/upper" "$overlay/work"
need_shutdown
