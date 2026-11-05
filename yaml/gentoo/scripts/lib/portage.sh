#!/bin/bash
# Definitions only; callers initialize state and own traps.

# Refresh the Gentoo snapshot, then sync all configured repositories on the helper.
# Callers must declare overlays before invoking this function.
portage_sync_repository() {
    env -u ROOT -u SYSROOT emerge-webrsync -k || return $?
    # Each sync attempt selects a mirror; tolerate transient mirror failures.
    local attempt status
    for attempt in 1 2 3; do
        if env -u ROOT -u SYSROOT emerge --sync; then
            return 0
        else
            status=$?
        fi
        if [ "$attempt" -lt 3 ]; then
            echo "Repository sync failed (status $status); retrying ($((attempt + 1))/3)." >&2
        fi
    done
    return "$status"
}

# Install natively with invocation policy; isolate options and environment from callers.
portage_emerge() (
    set -eu
    case "${USEPKG-true}" in
        true) usepkg=y ;;
        false) usepkg=n ;;
        *) echo 'USEPKG must be true or false' >&2; exit 2 ;;
    esac
    case "${GETBINPKG-true}" in
        true) getbinpkg=y ;;
        false) getbinpkg=n ;;
        *) echo 'GETBINPKG must be true or false' >&2; exit 2 ;;
    esac

    # Keep profile defaults, but never let FEATURES implicitly enable downloads.
    export FEATURES="${FEATURES:-} -userfetch -userpriv binpkg-multi-instance parallel-fetch -binpkg-request-signature binpkg-ignore-signature -getbinpkg"
    # A function-scoped SYSROOT assignment can expose the caller's exported
    # value again when unset. Remove it from the child environment explicitly.
    exec env -u SYSROOT emerge --ignore-default-opts --buildpkg --binpkg-respect-use=y \
        --binpkg-changed-deps=n --usepkg="$usepkg" --getbinpkg="$getbinpkg" "$@"
)

# Remove inherited command policy before configuring imported or previously built roots.
portage_remove_defaults() {
    local config="${1%/}/etc/portage/make.conf"
    if [ -f "$config" ]; then
        sed -i -E '/^[[:space:]]*(export[[:space:]]+)?(FEATURES|EMERGE_DEFAULT_OPTS)[[:space:]]*=/d' "$config"
    fi
}

# ROOT builds ignore environment FEATURES for native build dependencies.
# Persist cache and signature policy in the build host, never in the guest root.
portage_configure_build_host() {
    local config_root=${1%/}
    portage_remove_defaults "$1"
    cat >> "$config_root/etc/portage/make.conf" <<'CONF'
FEATURES="-userfetch -userpriv -getbinpkg -binpkg-request-signature binpkg-ignore-signature"
CONF
}

# Configure native dependencies and interactive recovery to use shared caches.
portage_configure_glibc_caches() {
    cat >> /etc/portage/make.conf <<'CONF'
DISTDIR="/mnt/corvus-cache/distfiles"
PKGDIR="/mnt/corvus-cache/packages/x86-64"
CONF
    portage_configure_binhost glibc /
    portage_configure_build_host /
}

# Select the matching official binhost; optional cache base supports runtime binds.
# The configuration root may be an unbooted stage3.
portage_configure_binhost() (
    libc=$1
    config_root=${2%/}
    portage_remove_defaults "$2"
    cache_base=${3:-/mnt/corvus-cache/binhost}
    case "$libc" in
        glibc) variant=x86-64 ;;
        musl) variant=x86-64_musl ;;
        *) echo "Unsupported libc: $libc" >&2; exit 1 ;;
    esac
    install -d "$config_root/etc/portage/binrepos.conf"
    cat > "$config_root/etc/portage/binrepos.conf/zz-corvus-cache.conf" <<CONF
[binhost]
priority = 9999
location = ${cache_base%/}/$variant
sync-uri = https://distfiles.gentoo.org/releases/amd64/binpackages/23.0/$variant
verify-signature = false
CONF
)

# Install natively into ROOT while retaining the target dependency closure.
portage_install_target_packages() {
    ROOT="$root" portage_emerge --root-deps "$@"
}

# Install the same extra package set into either ordinary libc flavor.
portage_install_runtime_packages() {
    ROOT="$1" portage_emerge --buildpkg --quiet-build \
      app-emulation/qemu-guest-agent \
      mail-mta/msmtp \
      app-admin/sudo \
      sys-fs/growpart \
      sys-fs/dosfstools \
      sys-boot/efibootmgr \
      net-fs/nfs-utils \
      sys-block/thin-provisioning-tools \
      app-editors/vim \
      app-shells/bash-completion
}
