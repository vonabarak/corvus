#!/bin/bash
# Configure the offline musl guest without starting its init system.
set -euxo pipefail
. "${CORVUS_BUILD_LIB:-/usr/local/lib/corvus-build}/build.sh"
export PATH=/usr/sbin:/usr/bin:/sbin:/bin
root=${IMAGE_ROOT:-/mnt/sysroot}

# Mount the labeled root and EFI filesystems with runtime-appropriate options.
configure_filesystems() {
    write_target_fstab "$root"
}

# Set the hostname, UTF-8 locale, and localhost mappings.
configure_identity() {
    printf '%s\n' 'hostname="gentoo-musl"' > "$root/etc/conf.d/hostname"
    printf '%s\n' 'LANG=C.UTF-8' > "$root/etc/env.d/02locale"
    printf '%s\n' 'LANG=C.UTF-8' > "$root/etc/locale.conf"
    configure_image_defaults "$root"
}

# Let dhcpcd write a regular resolv.conf instead of inheriting helper symlinks.
configure_dns() {
    rm -f "$root/etc/resolv.conf"
    touch "$root/etc/resolv.conf"
}

# Assign device setup, networking, SSH, logging, and shutdown services to OpenRC runlevels.
enable_runtime_services() {
    local service
    for service in devfs dmesg udev udev-trigger; do
        chroot "$root" rc-update add "$service" sysinit
    done
    for service in hwclock modules sysctl hostname bootmisc root fsck localmount; do
        chroot "$root" rc-update add "$service" boot
    done
    for service in dhcpcd sshd sysklogd qemu-guest-agent; do
        chroot "$root" rc-update add "$service" default
    done
    for service in savecache killprocs mount-ro; do
        chroot "$root" rc-update add "$service" shutdown
    done
}

# Enable a login prompt on the serial console used by headless VMs.
configure_serial_console() {
    if ! grep -q '^s0:' "$root/etc/inittab"; then
        echo 's0:12345:respawn:/sbin/agetty -L 115200 ttyS0 vt100' >> "$root/etc/inittab"
    fi
}

# Select the installed kernel and write a serial-capable menu shared by BIOS and UEFI.
configure_grub() {
    write_target_grub_config "$root" "Gentoo musl/OpenRC"
}

# Apply guest settings; the calling build entry owns cleanup and publication.
main() {
    configure_filesystems
    configure_identity
    configure_dns
    enable_runtime_services
    configure_serial_console
    configure_grub
}

main "$@"
