#!/bin/bash
# Install from the native musl builder; build dependencies remain outside the target.
set -euxo pipefail

# Install GLib on the builder because its post-install hooks execute cache tools outside the target chroot.
prepare_native_install_hooks() {
    unset ROOT SYSROOT
    emerge --noreplace --buildpkg --quiet-build dev-libs/glib
}

# Install the explicit minimal runtime instead of @system, which would include Portage and a compiler.
install_base_packages() {
    # ROOT redirects runtime dependencies; native DEPEND/BDEPEND stay on the builder.
    emerge --buildpkg --quiet-build \
        sys-apps/baselayout \
        sys-libs/musl \
        sys-apps/openrc \
        sys-apps/sysvinit \
        virtual/udev \
        app-shells/bash \
        sys-apps/coreutils \
        sys-apps/findutils \
        sys-apps/grep \
        sys-apps/sed \
        sys-apps/gawk \
        app-alternatives/awk \
        sys-apps/util-linux \
        sys-apps/kbd \
        sys-apps/shadow \
        sys-process/procps \
        sys-apps/iproute2 \
        net-misc/dhcpcd \
        net-misc/openssh \
        net-misc/socat \
        app-admin/sysklogd \
        app-misc/ca-certificates \
        app-emulation/qemu-guest-agent \
        sys-fs/e2fsprogs \
        sys-fs/dosfstools
}

# Prepare native hooks before redirecting runtime packages into the offline target root.
main() {
    prepare_native_install_hooks
    export ROOT="${IMAGE_ROOT:?}"
    install_base_packages
}

main "$@"
