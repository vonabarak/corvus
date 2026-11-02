#!/bin/bash
# Configure test-only credentials and services after the common OpenRC setup.
set -euxo pipefail
root=${IMAGE_ROOT:?}

# Keep the existing SSH and serial-login contract used by the test harness.
configure_test_user() {
    chroot "$root" useradd --create-home --shell /bin/sh corvus
    printf '%s\n' 'corvus:corvus' | chroot "$root" chpasswd
    install -d -m 0700 "$root/home/corvus/.ssh"
    install -m 0600 /tmp/corvus-test-key.pub "$root/home/corvus/.ssh/authorized_keys"
    chroot "$root" chown -R corvus:corvus /home/corvus
    printf '%s\n' 'permit nopass setenv { PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin } corvus' > "$root/etc/doas.conf"
    chmod 0400 "$root/etc/doas.conf"
    chroot "$root" doas -C /etc/doas.conf
}

# Allow password-based test login without changing other musl image defaults.
configure_test_ssh() {
    # Put explicit settings first: OpenSSH uses the first value it reads.
    sed -i '1iPasswordAuthentication yes\nPermitRootLogin no\nSetEnv PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin' "$root/etc/ssh/sshd_config"
    # OpenRC's sshd service generates missing host keys on first boot.
}

# Expose split-/usr tools to login shells and doas, without needing Portage.
configure_test_environment() {
    cat > "$root/etc/profile.d/corvus-test.sh" <<'CONF'
export PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin
CONF
    # SLIRP and managed networks need a DHCPv4 lease before boot completes.
    cat > "$root/etc/dhcpcd.conf" <<'CONF'
ipv4only
noipv4ll
waitip 4
timeout 20
clientid
hostname
option domain_name_servers, domain_name, domain_search
option routers, subnet_mask, classless_static_routes
CONF
}

# Give DHCP, serial prompts, and hostname assertions the same guest identity.
configure_test_identity() {
    printf '%s\n' 'hostname="corvus-test-vm"' > "$root/etc/conf.d/hostname"
    printf '%s\n' '127.0.0.1 corvus-test-vm' >> "$root/etc/hosts"
}

# Handle QEMU's ACPI power button even when the guest agent is disabled.
configure_acpi_shutdown() {
    install -d "$root/etc/acpi/events" "$root/etc/acpi/actions"
    cat > "$root/etc/acpi/events/corvus-power-button" <<'CONF'
event=button/power.*
action=/etc/acpi/actions/corvus-power-button.sh
CONF
    cat > "$root/etc/acpi/actions/corvus-power-button.sh" <<'SCRIPT'
#!/bin/sh
# Ask OpenRC/sysvinit to shut down cleanly on a virtual ACPI button press.
exec /sbin/poweroff
SCRIPT
    chmod 0755 "$root/etc/acpi/actions/corvus-power-button.sh"
    # Replace the catch-all rule so a button press cannot trigger two handlers.
    rm -f "$root/etc/acpi/events/default"
    chroot "$root" rc-update add acpid default
}

# Apply only the settings needed by the immutable integration-test guest.
main() {
    configure_test_user
    configure_test_ssh
    configure_test_environment
    configure_test_identity
    configure_acpi_shutdown
}

main "$@"
