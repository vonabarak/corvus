#!/bin/bash
# Grow only the positively identified rescue partition, never a recovery disk.
set -euo pipefail

grow_rescue_storage() {
    local media=$1 partition disk number options uuid argument expected= layout status
    mountpoint -q "$media" || return 0
    options=$(findmnt -nr -o OPTIONS --target "$media")
    case ",$options," in *,rw,*) ;; *) return 0 ;; esac
    partition=$(findmnt -nr -o SOURCE --target "$media")
    partition=$(readlink -f "$partition")
    test -b "$partition" || return 1
    test "$(blockdev --getro "$partition")" = 0 || return 0
    for argument in $(cat /proc/cmdline); do
        case "$argument" in root=rescue:UUID=*) expected=${argument#root=rescue:UUID=} ;; esac
    done
    uuid=$(blkid -s UUID -o value "$partition")
    test -n "$expected" && test "$uuid" = "$expected" || return 1
    test "$(blkid -s TYPE -o value "$partition")" = ext4 || return 1
    test "$(blkid -s LABEL -o value "$partition")" = rescue-data || return 1
    number=$(blkid -p -s PART_ENTRY_NUMBER -o value "$partition")
    test "$number" = 3 || return 1
    disk="/dev/$(lsblk -ndo PKNAME "$partition")"
    test -b "$disk" && test "$(lsblk -ndo TYPE "$disk")" = disk || return 1
    test "$(blkid -p -s PTTYPE -o value "$disk")" = gpt || return 1
    # Early boot can precede udev's partition metadata. Read types from disk.
    layout=$(lsblk --properties-by blkid -nr -o PARTN,PARTTYPE "$disk" | awk 'NF { print tolower($1), tolower($2) }')
    test "$layout" = $'1 21686148-6449-6e6f-744e-656564454649\n2 c12a7328-f81f-11d2-ba4b-00a0c93ec93b\n3 0fc63daf-8483-4772-8e79-3d69d8477de4' || return 1
    status=0
    growpart "$disk" "$number" || status=$?
    # growpart reports NOCHANGE with status 1. Still retry filesystem growth:
    # an earlier boot may have expanded the partition but not the filesystem.
    test "$status" -le 1 || return "$status"
    resize2fs "$partition"
}

if [[ ${BASH_SOURCE[0]} == "$0" ]]; then
    if ! grow_rescue_storage /run/initramfs/rescue-media; then
        echo 'Rescue storage expansion failed or layout was unexpected; continuing without expansion.' >&2
        exit 1
    fi
fi
