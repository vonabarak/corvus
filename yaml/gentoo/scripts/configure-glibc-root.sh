#!/bin/bash
# Configure the regular guest without booting its systemd instance.
set -eu
. "${CORVUS_BUILD_LIB:-/usr/local/lib/corvus-build}/build.sh"

# Write the shared runtime filesystem layout.
configure_filesystems() {
    write_target_fstab "$IMAGE_ROOT"
}

# Set the hostname, localhost resolution, and UTC.
configure_identity() {
    echo gentoo-headless > "$IMAGE_ROOT/etc/hostname"
    configure_image_defaults "$IMAGE_ROOT"
}

# Generate UTF-8 locales using the target glibc and locale data.
generate_locales() {
    # Pin the system locale to en_US.UTF-8 and generate it
    # in the sysroot. Without this, baselayout leaves
    # /etc/locale.conf empty and processes inherit the C /
    # POSIX locale (ASCII-only). Anything that prints a
    # non-ASCII character — `crv`'s em-dash separators, for
    # one — then crashes with "commitBuffer: invalid
    # argument (cannot encode character …)" on stdout, and
    # postgres' initdb refuses to run ("invalid locale
    # settings"). Gentoo's glibc doesn't include any
    # built-in UTF-8 locale, so we have to localedef one
    # into the sysroot — which means a chroot because the
    # sysroot's localedef wants the sysroot's
    # /usr/share/i18n data, not the bake VM's. The
    # locale-gen wrapper (sys-apps/locale-gen) is NOT in
    # @system, so we call localedef from glibc directly.
    cat > "$IMAGE_ROOT/etc/locale.gen" <<'EOF'
C.UTF-8 UTF-8
en_US.UTF-8 UTF-8
EOF
    cat > "$IMAGE_ROOT/etc/locale.conf" <<'EOF'
LANG=en_US.UTF-8
EOF
    install -d "$IMAGE_ROOT/usr/lib/locale"
    chroot "$IMAGE_ROOT" localedef -i en_US -f UTF-8 en_US.UTF-8
    # Keep Portage's C.UTF-8 locale available in downstream bake VMs too.
    LC_ALL=C chroot "$IMAGE_ROOT" localedef -i C -f UTF-8 C.UTF-8
}

# Enable DHCP, DNS, SSH, QGA, and the serial console.
configure_network_and_services() {
    install -d "$IMAGE_ROOT/etc/systemd/network"
    cat > "$IMAGE_ROOT/etc/systemd/network/20-wired.network" <<'EOF'
[Match]
Name=en* eth*

[Network]
DHCP=yes
EOF

    systemctl --root="$IMAGE_ROOT" enable \
      systemd-networkd.service \
      systemd-resolved.service \
      sshd.service \
      qemu-guest-agent.service \
      serial-getty@ttyS0.service

    # Resolved-managed /etc/resolv.conf
    ln -sf /run/systemd/resolve/stub-resolv.conf "$IMAGE_ROOT/etc/resolv.conf" || true
}

# Apply guest configuration after runtime packages are installed.
main() {
    configure_filesystems
    configure_identity
    generate_locales
    configure_network_and_services
    write_target_grub_config "$IMAGE_ROOT" "Gentoo Linux (custom kernel)"
}

main "$@"
