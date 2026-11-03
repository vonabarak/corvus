#!/bin/bash
# Definitions only; callers initialize state and own traps.

# Format a new target with BIOS boot, EFI, and root partitions.
disk_partition() (
    disk=$1
    wipefs -a "$disk"
    sgdisk --zap-all "$disk"
    sgdisk -n 1:0:+1M -t 1:ef02 -c 1:bios "$disk"
    sgdisk -n "2:0:+${IMAGE_ESP_SIZE:-512M}" -t 2:ef00 -c 2:efi "$disk"
    sgdisk -n 3:0:0 -t 3:8300 -c 3:root "$disk"
    partprobe "$disk"
    for attempt in $(seq 1 60); do
        [ -b "${disk}1" ] && [ -b "${disk}2" ] && [ -b "${disk}3" ] && break
        sleep 1
    done
    if ! { [ -b "${disk}1" ] && [ -b "${disk}2" ] && [ -b "${disk}3" ]; }; then
        echo "Partition devices did not appear for $disk" >&2
        exit 1
    fi
    mkfs.vfat -F32 -n CORVUS-EFI "${disk}2"
    mkfs.ext4 -F -L corvus-root "${disk}3"
)

# Mount labeled filesystems with the same runtime policy for both flavors.
disk_write_fstab() {
    cat > "$1/etc/fstab" <<CONF
LABEL=corvus-root / ext4 defaults,noatime 0 1
LABEL=CORVUS-EFI /boot/efi vfat defaults,umask=0077 0 2
CONF
}

# Install both firmware variants without changing the helper's NVRAM.
disk_install_grub() {
    grub-install --target=i386-pc --boot-directory="$1/boot" "$2"
    grub-install --target=x86_64-efi --boot-directory="$1/boot" \
        --efi-directory="$1/boot/efi" --removable --no-nvram
}

# Select the installed kernel and write a serial-capable BIOS/UEFI menu.
disk_write_grub_config() (
    root=$1
    title=$2
    # Use version order, skip backup kernels, and fail if no boot image exists.
    kernel=$(find "$root/boot" -maxdepth 1 -type f -name 'vmlinuz-*' ! -name '*.old' | sort -V | tail -1)
    test -n "$kernel"
    kernel=${kernel##*/}
    ln -sf "$kernel" "$root/boot/vmlinuz"
    install -d "$root/boot/grub"
    cat > "$root/boot/grub/grub.cfg" <<CONF
set timeout=0
set default=0
insmod part_gpt
insmod ext2
insmod search_label
serial --unit=0 --speed=115200
terminal_input serial console
terminal_output serial console
menuentry '$title' {
    search --set=root --label corvus-root
    linux /boot/$kernel root=/dev/vda3 rootfstype=ext4 console=tty0 console=ttyS0,115200n8 ${IMAGE_KERNEL_ARGS:-}
}
CONF
)
