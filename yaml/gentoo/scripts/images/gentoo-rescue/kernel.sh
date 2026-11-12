#!/bin/bash
# Build a hardware-generic initramfs for the stable Gentoo distribution kernel.
set -euxo pipefail

main() {
    local root=${IMAGE_ROOT:?} kernel config option module listing
    kernel=$(find "$root/lib/modules" -mindepth 1 -maxdepth 1 -type d -printf '%f\n' | sort -V | tail -1)
    test -n "$kernel"
    config="$root/boot/config-$kernel"
    if ! test -f "$config"; then
        config="$root/usr/src/linux-$kernel/.config"
    fi
    test -f "$config"
    for option in BLK_DEV_INITRD BLK_DEV_LOOP DEVTMPFS EXT4_FS OVERLAY_FS TMPFS SQUASHFS SQUASHFS_ZSTD \
        EFI EFI_PARTITION USB_XHCI_HCD USB_STORAGE USB_UAS HID_GENERIC USB_HID \
        SATA_AHCI BLK_DEV_NVME SCSI BLK_DEV_SD VIRTIO_BLK VIRTIO_NET E1000E IGB \
        R8169 IWLWIFI ATH9K; do
        grep -Eq "^CONFIG_${option}=[ym]$" "$config" || {
            echo "Distribution kernel lacks CONFIG_$option" >&2
            return 1
        }
    done
    # Kernel installation may keep the image only in the installed source tree.
    if ! test -f "$root/boot/vmlinuz-$kernel"; then
        install -m 0644 "$root/usr/src/linux-$kernel/arch/x86/boot/bzImage" "$root/boot/vmlinuz-$kernel"
    fi
    chroot "$root" depmod -a "$kernel"
    install -d "$root/usr/lib/dracut/modules.d/91corvus-rescue"
    cp "${CORVUS_BUILD_SCRIPTS:-/usr/local/lib/corvus-build}/images/gentoo-rescue/dracut/"* \
        "$root/usr/lib/dracut/modules.d/91corvus-rescue/"
    cat > "$root/etc/dracut.conf.d/90-rescue.conf" <<'CONF'
hostonly="no"
hostonly_cmdline="no"
use_fstab="no"
add_dracutmodules+=" systemd systemd-initrd kernel-modules corvus-rescue shutdown "
omit_dracutmodules+=" lvm mdraid dmraid crypt resume network network-manager systemd-networkd "
filesystems+=" ext4 squashfs overlay "
add_drivers+=" xhci_pci usb_storage uas ahci nvme sd_mod virtio_pci virtio_blk virtio_scsi usbhid hid_generic "
CONF
    chroot "$root" dracut --force --no-hostonly --no-hostonly-cmdline \
        --kver "$kernel" "/boot/initramfs-$kernel.img"
    set +x
    listing=$(chroot "$root" lsinitrd "/boot/initramfs-$kernel.img")
    set -x
    for module in rescue-root rescue-generator rescue-shutdown; do
        grep -q "$module" <<< "$listing"
    done
    # Validate loadable boot drivers; built-in drivers need no initramfs file.
    for module in xhci_pci usb_storage uas ahci nvme sd_mod virtio_pci virtio_blk virtio_scsi usbhid hid_generic ext4 squashfs loop overlay; do
        local filename
        filename=$(chroot "$root" modinfo -k "$kernel" -F filename "$module")
        if test "$filename" != '(builtin)'; then
            grep -Fq "${filename##*/}" <<< "$listing"
        fi
    done
    # dracut's shutdown restore finds this versioned path before /boot. Keep a
    # small userspace-only archive inside squashfs, rather than unpacking the
    # hardware-generic boot initramfs and its firmware into RAM at shutdown.
    chroot "$root" dracut --force --conf /dev/null --confdir /var/empty \
        --no-hostonly --no-hostonly-cmdline --no-kernel --no-early-microcode \
        --gzip --omit 'squash squash-erofs squash-lib' --modules 'shutdown corvus-rescue' \
        --kver "$kernel" "/lib/modules/$kernel/initrd"
    chroot "$root" lsinitrd -f shutdown "/lib/modules/$kernel/initrd" | grep -q .
    printf '%s\n' "$kernel" > "$root/etc/rescue-kernel-version"
}

main "$@"
