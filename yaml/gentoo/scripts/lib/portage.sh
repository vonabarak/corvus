#!/bin/bash
# Definitions only; callers initialize state and own traps.

# Persist verification policy in a live or unbooted root. No commands run there.
portage_configure_verification() {
    local config_root=${1%/}
    local config="$config_root/etc/portage/make.conf"
    install -d "$config_root/etc/portage/repos.conf" || return $?
    portage_remove_defaults "$1" || return $?
    if [ -f "$config" ]; then
        sed -i -E '/^[[:space:]]*(export[[:space:]]+)?GPG_VERIFY_(USER|GROUP)_DROP[[:space:]]*=/d' "$config" || return $?
    fi
    # Require signatures per remote binhost, while accepting unsigned local builds.
    # Empty drops avoid Gentoo's unavailable nobody:nogroup pair.
    cat >> "$config" <<'CONF' || return $?
FEATURES="-binpkg-request-signature -binpkg-ignore-signature"
GPG_VERIFY_USER_DROP=""
GPG_VERIFY_GROUP_DROP=""
CONF
    cat > "$config_root/etc/portage/repos.conf/zz-corvus-signatures.conf" <<CONF
[gentoo]
sync-webrsync-verify-signature = true
sync-rsync-verify-metamanifest = true
sync-openpgp-key-package = sec-keys/openpgp-keys-gentoo-release
sync-openpgp-key-path = /usr/share/openpgp-keys/gentoo-release.asc
sync-openpgp-key-refresh = keyserver
sync-openpgp-keyserver = ${PORTAGE_KEYSERVER:-hkps://keys.gentoo.org}
CONF
}

# Initialize native verification before syncing or installing packages. Official
# cloud/stage3 inputs must supply these Portage dependencies; never bootstrap them
# using an unverified download. This also runs inside the musl bootstrap chroot.
portage_check_verification_prerequisites() {
    local tool
    for tool in gemato gpg gpgconf getuto; do
        if ! command -v "$tool" >/dev/null; then
            echo "Missing Gentoo verification prerequisite: $tool; refresh the official cloud/stage3 input." >&2
            return 1
        fi
    done
    if [ ! -s /usr/share/openpgp-keys/gentoo-release.asc ]; then
        echo 'Missing sec-keys/openpgp-keys-gentoo-release; refresh the official cloud/stage3 input.' >&2
        return 1
    fi
}

portage_prepare_verification() (
    set -eu
    unset ROOT SYSROOT
    portage_check_verification_prerequisites || return $?
    local chost libc
    # Portage queries load repository configuration and fail before the first
    # sync when repository directories are absent. The native stage3 compiler
    # identifies the running build host without consulting those repositories.
    chost=$(gcc -dumpmachine) || return $?
    case "$chost" in
        x86_64*-linux-musl) libc=musl ;;
        x86_64*-linux-gnu) libc=glibc ;;
        *) echo "Unsupported build host CHOST: $chost" >&2; exit 1 ;;
    esac
    portage_configure_binhost "$libc" / || return $?
    portage_configure_build_host / || return $?
    getuto
)

# Refresh the signed Gentoo snapshot, then sync configured repositories. Local
# overlays retain their own policy. Callers must declare overlays before syncing.
portage_sync_repository() (
    set -eu
    portage_prepare_verification || return $?
    local tools
    tools=$(mktemp -d "${TMPDIR:-/run}/portage-sync.XXXXXX") || return $?
    trap 'rm -rf "$tools"' EXIT
    # Direct emerge-webrsync does not honor repos.conf's WKD refresh policy.
    # Scope this adapter to the sync subprocesses; preserve the caller's PATH/traps.
    export CORVUS_GEMATO
    CORVUS_GEMATO=$(command -v gemato) || return $?
    export PORTAGE_KEYSERVER=${PORTAGE_KEYSERVER:-hkps://keys.gentoo.org}
    cat > "$tools/gemato" <<'SCRIPT' || return $?
#!/bin/sh
operation=$1
shift
exec "$CORVUS_GEMATO" "$operation" --no-wkd --keyserver "$PORTAGE_KEYSERVER" "$@"
SCRIPT
    chmod 0755 "$tools/gemato" || return $?
    export PATH="$tools:$PATH"
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
)

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
    export FEATURES="${FEATURES:-} -userfetch -userpriv binpkg-multi-instance parallel-fetch -binpkg-request-signature -binpkg-ignore-signature -getbinpkg"
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
    portage_configure_verification "$1" || return $?
    cat >> "$config_root/etc/portage/make.conf" <<'CONF'
FEATURES="-userfetch -userpriv -getbinpkg -binpkg-request-signature -binpkg-ignore-signature"
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
    portage_configure_verification "$2" || return $?
    cache_base=${3:-/mnt/corvus-cache/binhost}
    case "$libc" in
        glibc) variant=x86-64 ;;
        musl) variant=x86-64_musl ;;
        *) echo "Unsupported libc: $libc" >&2; exit 1 ;;
    esac
    install -d "$config_root/etc/portage/binrepos.conf" || return $?
    cat > "$config_root/etc/portage/binrepos.conf/zz-corvus-cache.conf" <<CONF
[binhost]
priority = 9999
location = ${cache_base%/}/$variant
sync-uri = https://distfiles.gentoo.org/releases/amd64/binpackages/23.0/$variant
verify-signature = true
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
