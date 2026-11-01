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

# Skip the redundant RTC read: the kernel already initializes the system clock.
configure_clock() {
    # Keep hwclock's timezone setup and shutdown clock saving enabled, but avoid
    # waiting for RTC ticks that may not be delivered by the virtual hardware.
    sed -i '/^[[:space:]#]*clock_hctosys=/d' "$root/etc/conf.d/hwclock"
    printf '%s\n' 'clock_hctosys="NO"' >> "$root/etc/conf.d/hwclock"
}

# Forward host VSOCK SSH connections to OpenSSH's local TCP listener.
configure_vsock_ssh() {
    cat > "$root/etc/init.d/vsock-sshd" <<'EOF'
#!/sbin/openrc-run
description="Forward AF_VSOCK:22 to local sshd on 127.0.0.1:22"
command="/usr/bin/socat"
command_args="VSOCK-LISTEN:22,fork,reuseaddr TCP:127.0.0.1:22"
command_background=true
pidfile="/run/${RC_SVCNAME}.pid"

# Start only after the local SSH listener is available; no IP network is needed.
depend() {
    need sshd
}

# The kernel builds virtio-VSOCK as modules; load the transport and dependencies.
start_pre() {
    modprobe vmw_vsock_virtio_transport
}
EOF
    chmod 0755 "$root/etc/init.d/vsock-sshd"
}

# Wait for the syslog socket, not just the background daemon's process creation.
configure_logging() {
    cat >> "$root/etc/conf.d/sysklogd" <<'EOF'

# OpenRC backgrounds syslogd before initialization has created /dev/log.
# Keep dependent services waiting until local logging is actually available.
start_post() {
    local attempt
    for attempt in $(seq 1 30); do
        [ -S /dev/log ] && return 0
        sleep 1
    done
    eerror "sysklogd did not create /dev/log within 30 seconds"
    return 1
}
EOF
}

# Assign device setup, networking, SSH, logging, and shutdown services to OpenRC runlevels.
enable_runtime_services() {
    local service
    for service in devfs dmesg udev udev-trigger; do
        chroot "$root" rc-update add "$service" sysinit
    done
    # Create /dev/log during boot, before default-runlevel services emit logs.
    for service in hwclock modules sysctl hostname bootmisc root fsck localmount sysklogd; do
        chroot "$root" rc-update add "$service" boot
    done
    for service in dhcpcd sshd vsock-sshd qemu-guest-agent; do
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
    configure_clock
    configure_vsock_ssh
    configure_logging
    enable_runtime_services
    configure_serial_console
    configure_grub
}

main "$@"
