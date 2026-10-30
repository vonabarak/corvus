#!/bin/bash
# Install native tools into the persistent musl/OpenRC builder root.
set -euxo pipefail

# Configure both GRUB targets and provisioning dependencies before package resolution.
configure_packages() {
    cat > /etc/portage/package.use/corvus-builder <<'EOF'
sys-boot/grub grub_platforms_pc grub_platforms_efi-64 -mount -nls -fonts -themes
app-admin/sudo -sendmail
EOF
}

# Keep the complete stage3 system and install tools needed by all derivative images.
install_tools() {
    unset ROOT SYSROOT
    emerge --buildpkg --noreplace --quiet-build @system \
        sys-devel/gcc \
        sys-devel/binutils \
        sys-apps/portage \
        dev-build/make \
        sys-apps/gptfdisk \
        sys-block/parted \
        sys-fs/e2fsprogs \
        sys-fs/dosfstools \
        sys-boot/grub \
        app-arch/tar \
        app-arch/xz-utils \
        net-misc/curl \
        dev-libs/glib \
        net-misc/dhcpcd \
        net-misc/openssh \
        app-admin/sysklogd \
        app-misc/ca-certificates \
        app-emulation/qemu-guest-agent \
        sys-apps/kbd
}

# Retain repository metadata and package records when publishing the builder.
main() {
    configure_packages
    install_tools
}

main "$@"
