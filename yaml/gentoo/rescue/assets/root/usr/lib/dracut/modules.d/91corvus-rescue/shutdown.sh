#!/bin/sh
# dracut has already unmounted /oldroot and moved its /run to /oldsys/run.
base=/oldsys/run/initramfs
if [ -f "$base/rescue-loop" ]; then
    if mountpoint -q "$base/rescue-lower"; then
        umount "$base/rescue-lower" || return 1
    fi
    loop=$(cat "$base/rescue-loop")
    if losetup "$loop" >/dev/null 2>&1; then
        losetup -d "$loop" || return 1
    fi
    if mountpoint -q "$base/rescue-overlay"; then
        umount "$base/rescue-overlay" || return 1
    fi
    if mountpoint -q "$base/rescue-media"; then
        umount "$base/rescue-media" || return 1
    fi
    rm -f "$base/rescue-loop"
fi
