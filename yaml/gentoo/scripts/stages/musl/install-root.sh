#!/bin/bash
# Install a complete OpenRC system from the native musl builder.
set -euxo pipefail

. "${CORVUS_BUILD_SCRIPTS:-/usr/local/lib/corvus-build}/lib/portage.sh"

# Require native post-install hooks before directing package files into ROOT.
prepare_native_install_hooks() {
    unset ROOT SYSROOT
    command -v glib-compile-schemas >/dev/null
}

# Prepare merged-/usr and the directories needed during installation and boot.
prepare_target_directories() {
    install -d "$IMAGE_ROOT"/etc/portage/{package.use,repos.conf,binrepos.conf} \
        "$IMAGE_ROOT"/var/cache/{distfiles,binpkgs,binhost} \
        "$IMAGE_ROOT/var/db/repos" "$IMAGE_ROOT"/usr/{bin,lib,lib64} \
        "$IMAGE_ROOT"/{proc,sys,dev,run}
    install -d -m 1777 "$IMAGE_ROOT/tmp" "$IMAGE_ROOT/var/tmp"
    ln -sf bin "$IMAGE_ROOT/usr/sbin"
    ln -sf usr/bin "$IMAGE_ROOT/bin"
    ln -sf usr/sbin "$IMAGE_ROOT/sbin"
    ln -sf usr/lib "$IMAGE_ROOT/lib"
    ln -sf usr/lib64 "$IMAGE_ROOT/lib64"
}

# @system adds net-tools; coreutils already provides OpenRC's hostname command.
configure_package_features() {
    printf '%s\n' 'sys-apps/net-tools -hostname' > /etc/portage/package.use/corvus-musl-system
}

# Persist usable guest configuration without copying the helper's cache policy.
configure_target_portage() {
    cat > "$IMAGE_ROOT/etc/portage/make.conf" <<'CONF'
CHOST="x86_64-pc-linux-musl"
COMMON_FLAGS="-O2 -pipe"
CFLAGS="${COMMON_FLAGS}"
CXXFLAGS="${COMMON_FLAGS}"
FCFLAGS="${COMMON_FLAGS}"
FFLAGS="${COMMON_FLAGS}"
DISTDIR="/var/cache/distfiles"
PKGDIR="/var/cache/binpkgs"
ACCEPT_LICENSE="*"
USE="bash-completion"
GPG_VERIFY_USER_DROP=""
GPG_VERIFY_GROUP_DROP=""
CONF
    printf 'MAKEOPTS="-j%s"\n' "$(nproc)" >> "$IMAGE_ROOT/etc/portage/make.conf"
    cat > "$IMAGE_ROOT/etc/portage/repos.conf/gentoo.conf" <<'CONF'
[DEFAULT]
main-repo = gentoo

[gentoo]
location = /var/db/repos/gentoo
sync-type = rsync
sync-uri = rsync://rsync.gentoo.org/gentoo-portage
auto-sync = yes
CONF
    # Keep the stage3's musl/OpenRC profile, using a guest-relative symlink.
    local profile
    profile=$(readlink -e /etc/portage/make.profile)
    case "$profile" in
        /var/db/repos/gentoo/profiles/*) ;;
        *) echo "Unexpected musl builder profile: $profile" >&2; return 1 ;;
    esac
    ln -sf "../../${profile#/}" "$IMAGE_ROOT/etc/portage/make.profile"
    cp /etc/portage/package.use/corvus-musl "$IMAGE_ROOT/etc/portage/package.use/"
    cp /etc/portage/package.use/corvus-musl-system "$IMAGE_ROOT/etc/portage/package.use/"
    portage_configure_binhost musl "$IMAGE_ROOT" /var/cache/binhost
}

# ROOT redirects installed files; native build dependencies remain on the helper.
install_base_system() {
    ROOT="$IMAGE_ROOT" portage_emerge --buildpkg --quiet-build @system \
        sys-apps/openrc \
        sys-apps/sysvinit \
        virtual/udev \
        sys-apps/kbd \
        sys-apps/iproute2 \
        sys-process/procps \
        net-misc/dhcpcd \
        net-misc/openssh \
        net-misc/socat \
        app-admin/sysklogd \
        app-misc/ca-certificates \
        sys-fs/e2fsprogs
}

# Populate the ordinary musl image with the same extras as its glibc counterpart.
main() {
    : "${IMAGE_ROOT:?}"
    prepare_native_install_hooks
    prepare_target_directories
    configure_package_features
    configure_target_portage
    install_base_system
    portage_install_runtime_packages "$IMAGE_ROOT"
}

main "$@"
