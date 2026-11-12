#!/bin/bash
# Export a compact first-install disk and a payload-only offline update archive.
set -euo pipefail
export LC_ALL=C

main() (
    test "$#" = 2 || { echo "Usage: $0 SOURCE_IMAGE OUTPUT_DIRECTORY" >&2; exit 2; }
    test "$(id -u)" = 0 || { echo 'Export requires root for loop devices and filesystem resizing.' >&2; exit 1; }
    local source output work loop= mounted=false status blocks previous capacity blocksize start end last bytes guid
    source=$(realpath "$1")
    test -f "$source"
    output=$(realpath -m "$2")
    test "$source" != "$output/gentoo-rescue.img"
    mkdir -p "$output"
    work=$(mktemp -d "$output/.export.XXXXXX")
    cleanup() {
        status=$?
        trap - EXIT
        if "$mounted"; then
            umount "$work/media" || { echo "Unmount $work/media before removing export work files" >&2; exit 1; }
        fi
        if test -n "$loop"; then losetup -d "$loop" || status=1; fi
        rm -rf -- "$work"
        exit "$status"
    }
    trap cleanup EXIT
    loop=$(losetup --find --show --partscan --read-only "$source")
    udevadm settle --timeout=30
    test "$(blkid -s LABEL -o value "${loop}p3")" = rescue-data
    mkdir "$work/media"
    mount -t ext4 -o ro,noload "${loop}p3" "$work/media"
    mounted=true
    (cd "$work/media" && sha256sum -c rescue/SHA256SUMS)
    tar -C "$work/media" -cf "$work/gentoo-rescue-update.tar" \
        boot/vmlinuz boot/initramfs.img rescue/root.squashfs rescue/SHA256SUMS
    umount "$work/media"
    mounted=false
    losetup -d "$loop"
    loop=
    cp --reflink=auto --sparse=always "$source" "$work/gentoo-rescue.img"
    loop=$(losetup --find --show --partscan "$work/gentoo-rescue.img")
    udevadm settle --timeout=30
    status=0
    e2fsck -pf "${loop}p3" || status=$?
    test "$status" -le 1
    capacity=$(dumpe2fs -h "${loop}p3" 2>/dev/null | awk '/^Block count:/ {print $3}')
    resize2fs -M "${loop}p3"
    blocks=$(dumpe2fs -h "${loop}p3" 2>/dev/null | awk '/^Block count:/ {print $3}')
    # Relocating metadata can lower the next minimum-size estimate.
    while :; do
        previous=$blocks
        resize2fs -M "${loop}p3"
        blocks=$(dumpe2fs -h "${loop}p3" 2>/dev/null | awk '/^Block count:/ {print $3}')
        test "$blocks" -le "$previous"
        test "$blocks" -ne "$previous" || break
    done
    blocksize=$(dumpe2fs -h "${loop}p3" 2>/dev/null | awk '/^Block size:/ {print $3}')
    # Keep only a small writable margin; first boot grows to fill the stick.
    blocks=$((blocks + 67108864 / blocksize))
    # A tightly sized build disk may already have less than this margin.
    if test "$blocks" -gt "$capacity"; then blocks=$capacity; fi
    resize2fs "${loop}p3" "$blocks"
    start=$(lsblk -ndo START "${loop}p3")
    guid=$(blkid -s PART_ENTRY_UUID -o value -p "${loop}p3")
    end=$(( ((start * 512 + blocks * blocksize + 1048575) / 1048576) * 2048 - 1 ))
    last=$(( $(blockdev --getsz "$loop") - 34 ))
    if test "$end" -gt "$last"; then end=$last; fi
    sgdisk --delete=3 --new="3:$start:$end" --typecode=3:8300 \
        --change-name=3:rescue-data --partition-guid="3:$guid" "$loop"
    losetup -d "$loop"
    loop=
    # Include the backup GPT, rounding the whole image up to a MiB.
    bytes=$(( ((end + 34 + 2047) / 2048) * 1048576 ))
    truncate -s "$bytes" "$work/gentoo-rescue.img"
    sgdisk --move-second-header "$work/gentoo-rescue.img"
    sgdisk --verify "$work/gentoo-rescue.img"
    test "$source" != "$output/gentoo-rescue.img"
    mv -f "$work/gentoo-rescue.img" "$output/gentoo-rescue.img"
    mv -f "$work/gentoo-rescue-update.tar" "$output/gentoo-rescue-update.tar"
    echo "Exported compact disk ($bytes bytes) and offline update bundle to $output"
)

main "$@"
