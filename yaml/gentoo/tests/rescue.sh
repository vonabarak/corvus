#!/bin/bash
# Exercise fallbacks without mounting or changing host devices.
set -euo pipefail
here=$(cd "$(dirname "$0")/.." && pwd)
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
. "$here/scripts/images/gentoo-rescue/home.sh"
. "$here/scripts/images/gentoo-rescue/lib.sh"
export RESCUE_ESP="$work/esp" TARGET_DISK=/dev/rescue-test
blkid() { echo 11111111-2222-3333-4444-555555555555; }
rescue_write_grub
grep -q '^set timeout=300$' "$RESCUE_ESP/grub/grub.cfg"
grep -q 'root=rescue:UUID=11111111-2222-3333-4444-555555555555' "$RESCUE_ESP/grub/grub.cfg"
grep -q 'linux /boot/vmlinuz .*vt.color=0x02' "$RESCUE_ESP/grub/grub.cfg"
grep -q 'initrd /boot/initramfs.img' "$RESCUE_ESP/grub/grub.cfg"
# Catch accidental inclusion of build functions in the GRUB heredoc.
! grep -q 'rescue_pack_system' "$RESCUE_ESP/grub/grub.cfg"
declare -F rescue_pack_system rescue_install_grub >/dev/null

media="$work/media"
destination="$work/home"
mkdir -p "$media/home/root" "$media/boot" "$destination"
echo retained > "$media/home/root/saved"
mountpoint() { [[ $scenario != missing ]]; }
findmnt() { if [[ $scenario == readonly ]]; then echo ro; else echo rw; fi; }
df() {
    local available=100
    if [[ $scenario == full && $* == *avail* && $* != *iavail* ]] ||
        [[ $scenario == inodes && $* == *iavail* ]]; then available=0; fi
    printf 'Available\n%s\n' "$available"
}
mount() {
    printf 'mount %s\n' "$*" >> "$work/calls"
    if [[ $* == *"$media/home/root"* && $scenario == bindfail ]] ||
        [[ $* == *remount* && $scenario == remountfail ]]; then return 1; fi
}
umount() { printf 'umount %s\n' "$*" >> "$work/calls"; return 1; }
dd() { [[ $scenario != ioerror ]]; }
timeout() { shift; "$@"; }
for scenario in healthy missing readonly full inodes ioerror bindfail remountfail; do
    : > "$work/calls"
    home_initialize "$media" "$destination"
    if [[ $scenario == healthy ]]; then
        grep -q "mount --bind $media/home/root $destination" "$work/calls"
        ! grep -q 'mount -t tmpfs' "$work/calls"
    else
        grep -q 'mount -t tmpfs' "$work/calls"
    fi
    ! grep -q "^umount $media$" "$work/calls"
    test "$(cat "$media/home/root/saved")" = retained
    ! compgen -G "$media/home/root/.rescue-write-test.*" >/dev/null
done
# A generator must ignore other roots and reject command injection in UUIDs.
getarg() { echo "$root_argument"; }
export -f getarg
root_argument=/dev/vda3; export root_argument
bash "$here/scripts/images/gentoo-rescue/dracut/generator.sh" "$work/ignored"
test ! -e "$work/ignored"
root_argument='rescue:UUID=bad;command'
if bash "$here/scripts/images/gentoo-rescue/dracut/generator.sh" "$work/bad"; then exit 1; fi
root_argument=rescue:UUID=11111111-2222-3333-4444-555555555555
bash "$here/scripts/images/gentoo-rescue/dracut/generator.sh" "$work/normal" "$work/generator"
test -L "$work/generator/initrd-root-fs.target.requires/sysroot.mount"
grep -q 'lowerdir=/run/initramfs/rescue-lower' "$work/generator/sysroot.mount"
# Verify expansion never reaches a writer until the boot device and layout match.
(
    . "$here/scripts/images/gentoo-rescue/grow.sh"
    test() { if [[ ${1:-} == -b ]]; then return 0; else builtin test "$@"; fi; }
    mountpoint() { return 0; }
    findmnt() { case "$*" in *OPTIONS*) echo "${mock_options:-rw}" ;; *SOURCE*) echo /dev/vdz3 ;; esac; }
    readlink() { echo /dev/vdz3; }
    blockdev() { echo 0; }
    cat() { echo root=rescue:UUID=expected; }
    blkid() {
        case "$*" in
            *PTTYPE*) echo gpt ;;
            *PART_ENTRY_NUMBER*) echo 3 ;;
            *UUID*) echo "${mock_uuid:-expected}" ;;
            *TYPE*) echo ext4 ;;
            *LABEL*) echo rescue-data ;;
        esac
    }
    lsblk() {
        case "$*" in
            *PARTN,PARTTYPE*)
                printf '%s\n' '1 21686148-6449-6e6f-744e-656564454649' '2 c12a7328-f81f-11d2-ba4b-00a0c93ec93b' "${last_partition:-3 0fc63daf-8483-4772-8e79-3d69d8477de4}" ;;
            *PARTN*) echo 3 ;;
            *PKNAME*) echo vdz ;;
            *TYPE*) echo disk ;;
        esac
    }
    growpart() { echo grow >> "$work/growth"; return "${grow_status:-0}"; }
    resize2fs() { echo resize >> "$work/growth"; }
    : > "$work/growth"
    mock_options=ro grow_rescue_storage "$media"
    mock_uuid=wrong grow_rescue_storage "$media" && exit 1
    last_partition='4 other' grow_rescue_storage "$media" && exit 1
    builtin test ! -s "$work/growth"
    grow_rescue_storage "$media"
    grow_status=1 grow_rescue_storage "$media"
    builtin test "$(wc -l < "$work/growth")" = 4
)
echo 'Rescue runtime regression checks passed'
