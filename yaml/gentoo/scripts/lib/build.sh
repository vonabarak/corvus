#!/bin/bash
# Shared Gentoo build helpers. Sourcing declares functions only; callers own state and traps.

# --- Network and shared caches ---

# Wait for DNS before downloading repository metadata or packages.
wait_for_network() {
    for attempt in $(seq 1 60); do
        getent hosts distfiles.gentoo.org >/dev/null && return 0
        sleep 2
    done
    echo 'Network DNS did not become ready after 120 seconds' >&2
    return 1
}

# Test both directory creation and file writes through virtiofs.
check_cache_writable() (
    cache_path=$1
    probe=$(mktemp -d "$cache_path/.corvus-write-test.XXXXXX") || {
        echo "Cannot create directories in cache $cache_path; check host cache permissions for the user running virtiofsd." >&2
        exit 1
    }
    if ! (umask 077; : > "$probe/package"); then
        rmdir "$probe"
        echo "Cannot write files in cache $cache_path; check host cache permissions." >&2
        exit 1
    fi
    rm "$probe/package"
    rmdir "$probe"
)

# Refresh native repository metadata once before dependency or kernel selection.
refresh_build_repository() {
    env -u ROOT -u SYSROOT emerge-webrsync -k
}

# --- Portage configuration ---

# Configure native dependencies and interactive recovery to use shared caches.
configure_glibc_caches() {
    cat >> /etc/portage/make.conf <<'CONF'
DISTDIR="/mnt/corvus-cache/distfiles"
PKGDIR="/mnt/corvus-cache/packages/x86-64"
FEATURES="${FEATURES} -userfetch -userpriv"
CONF
    configure_binhost glibc /
}

# Select the matching official binhost; optional cache base supports runtime binds.
# The configuration root may be an unbooted stage3.
configure_binhost() (
    libc=$1
    config_root=${2%/}
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

# --- Tracked mounts and cleanup ---

# Initialize mount state explicitly; sourcing this library must not reset it.
initialize_mount_tracking() {
    declare -ga mounts=()
}

# Mount a filesystem and register it only after mounting succeeds.
mount_at() {
    local destination=$1
    shift
    mkdir -p "$destination"
    mount "$@" "$destination"
    mounts+=("$destination")
}

# Bind a host path into the target with the same cleanup tracking.
bind() {
    mount_at "$2" --bind "$1"
}

# Release nested mounts first, attempting every unmount even after a failure.
unmount_tracked() {
    local status=0 i
    for ((i=${#mounts[@]}-1; i>=0; i--)); do
        umount "${mounts[i]}" || status=1
    done
    return "$status"
}

# Preserve a failed build's exit status while still releasing its mounts.
cleanup() {
    local status=$?
    trap - EXIT
    unmount_tracked || { [ "$status" -ne 0 ] || status=1; }
    exit "$status"
}

# Provide package hooks with devices and private proc/run, never the helper init socket.
mount_target_interfaces() {
    bind /dev "$1/dev"
    mount_at "$1/proc" -t proc proc
    bind /sys "$1/sys"
    mount_at "$1/run" -t tmpfs tmpfs
}

# Mount artifact partitions by device: the helper can have identical filesystem labels.
mount_target_disk() {
    mount_at "$1" "${2}3"
    mount_at "$1/boot/efi" "${2}2"
    mount_target_interfaces "$1"
}

# --- Disk layout and bootloader ---

# Format a new target with BIOS boot, EFI, and root partitions.
partition_target_disk() (
    disk=$1
    wipefs -a "$disk"
    sgdisk --zap-all "$disk"
    sgdisk -n 1:0:+1M -t 1:ef02 -c 1:bios "$disk"
    sgdisk -n 2:0:+512M -t 2:ef00 -c 2:efi "$disk"
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
write_target_fstab() {
    cat > "$1/etc/fstab" <<CONF
LABEL=corvus-root / ext4 defaults,noatime 0 1
LABEL=CORVUS-EFI /boot/efi vfat defaults,umask=0077 0 2
CONF
}

# Install both firmware variants without changing the helper's NVRAM.
install_target_grub() {
    grub-install --target=i386-pc --boot-directory="$1/boot" "$2"
    grub-install --target=x86_64-efi --boot-directory="$1/boot" \
        --efi-directory="$1/boot/efi" --removable --no-nvram
}

# Select the installed kernel and write a serial-capable BIOS/UEFI menu.
write_target_grub_config() (
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
    linux /boot/$kernel root=/dev/vda3 rootfstype=ext4 console=tty0 console=ttyS0,115200n8
}
CONF
)

# --- Image defaults and publication ---

# Record the published image identity and bake timestamp inside the guest.
write_image_metadata() {
    printf 'image: %s\nbuilt: %s\n' "$2" "$(date -Iseconds)" > "$1/etc/corvus-image-info"
}

# Flush writes and trim only the image filesystems, never shared cache mounts.
trim_image_filesystems() (
    image_root=$1
    sync
    for filesystem in "$image_root" "${image_root%/}/boot/efi"; do
        if mountpoint -q "$filesystem"; then
            fstrim "$filesystem" || echo "Discard unavailable for $filesystem; continuing publication." >&2
        fi
    done
)

# Clear one image directory only when no filesystem is mounted at or below it.
clear_image_directory() (
    directory=$1
    # Canonical paths also catch mounts reached through a symlink in the image.
    directory=$(readlink -m "$directory")
    mounted_paths=$(findmnt -rn -o TARGET) || return 1
    if printf '%s\n' "$mounted_paths" | awk -v dir="$directory" '
        $0 == dir || index($0, dir "/") == 1 { found=1 }
        END { exit !found }
    '; then
        echo "Refusing to clean mounted image directory: $directory" >&2
        return 1
    fi
    if [ -d "$directory" ]; then
        find "$directory" -mindepth 1 -maxdepth 1 -exec rm -rf -- {} +
    fi
)

# Remove disk-backed scratch files; tmpfs contents are not part of the image.
clean_image_scratch_files() {
    local directory
    for directory in "${1%/}/var/tmp" "${1%/}/tmp"; do
        # Running builders can mount /tmp as tmpfs. Leave that live filesystem
        # alone; retain the mount guard for disk-backed directories and binds.
        if [ "$(findmnt -rn -M "$directory" -o FSTYPE)" = tmpfs ]; then
            continue
        fi
        clear_image_directory "$directory" || return $?
    done
}

# Clear offline package caches only after shared cache binds have been detached.
clean_image_temporary_files() {
    clear_image_directory "${1%/}/var/cache"
    clean_image_scratch_files "$1"
}

# Keep clone identity independent and disable direct root password authentication.
reset_image_identity() {
    rm -f "${1%/}"/etc/ssh/ssh_host_*
    : > "${1%/}/etc/machine-id"
    rm -rf "${1%/}/var/lib/cloud"
    chroot "$1" passwd -l root
}

# Use UTC and predictable local host resolution with either init system.
configure_image_defaults() {
    printf '%s\n' '127.0.0.1 localhost' '::1 localhost' > "${1%/}/etc/hosts"
    if [ -e "${1%/}/usr/share/zoneinfo/UTC" ]; then
        ln -sf ../usr/share/zoneinfo/UTC "${1%/}/etc/localtime"
    else
        # Minimal musl images need no timezone database to use UTC.
        printf '%s\n' 'TZ=UTC0' > "${1%/}/etc/env.d/00timezone"
    fi
}

# Write metadata last, then return unused blocks before mount teardown/publication.
finalize_image() {
    write_image_metadata "$1" "$2"
    trim_image_filesystems "$1"
}

# --- Native runtime build policy ---

# Keep distfiles shared but never mix glibc and musl binary packages.
prepare_workspace() {
    unset ROOT SYSROOT
    wait_for_network
    mkdir -p /mnt/corvus-cache
    if ! mountpoint -q /mnt/corvus-cache; then
        mount_at /mnt/corvus-cache -t virtiofs corvus-cache
    fi
    export DISTDIR=/mnt/corvus-cache/distfiles
    case "${IMAGE_LIBC:?}" in
        glibc) export PKGDIR=/mnt/corvus-cache/packages/x86-64 ;;
        musl) export PKGDIR=/mnt/corvus-cache/packages/x86-64_musl ;;
        *) echo "Unsupported libc: $IMAGE_LIBC" >&2; return 1 ;;
    esac
    # Check the download cache separately from locally compiled packages.
    binhost_cache="/mnt/corvus-cache/binhost/${PKGDIR##*/}"
    mkdir -p "$DISTDIR" "$PKGDIR" "$binhost_cache"
    check_cache_writable "$DISTDIR"
    check_cache_writable "$PKGDIR"
    check_cache_writable "$binhost_cache"
    refresh_build_repository
}

# Reject toolchains, Portage, glibc, and systemd in any published musl runtime.
verify_runtime() {
    local root=${IMAGE_ROOT:?}
    local package
    local -a forbidden
    # A dependency change must fail the bake rather than silently ship a build toolchain.
    forbidden=(sys-devel/gcc sys-devel/binutils llvm-core/clang dev-lang/rust dev-lang/rust-bin dev-lang/go
        sys-apps/portage sys-libs/glibc sys-apps/systemd)
    for package in "${forbidden[@]}"; do
        if compgen -G "$root/var/db/pkg/${package}-[0-9]*" >/dev/null; then
            echo "Unexpected runtime package: $package" >&2
            exit 1
        fi
    done
}

# Remove documentation that is not needed by the minimal runtime.
remove_documentation() {
    local root=${IMAGE_ROOT:?}
    rm -rf "$root/usr/share/doc" "$root/usr/share/man" "$root/usr/share/info"
}

# Keep package records and licenses while removing development files and static libraries.
remove_build_artifacts() {
    local root=${IMAGE_ROOT:?}
    rm -rf "$root/etc/portage" "$root/var/db/repos" "$root/usr/src" "$root/usr/include"
    local libdir
    for libdir in "$root/usr/lib" "$root/usr/lib64"; do
        if [[ -d "$libdir" ]]; then
            find "$libdir" -type f \( -name '*.a' -o -name '*.la' \) -delete
        fi
    done
}

# Apply explicit flavor policy, then finalize both runtimes identically.
finish_runtime() {
    if [[ "$IMAGE_LIBC" == musl ]]; then
        remove_documentation
        remove_build_artifacts
        verify_runtime
    fi
    clean_image_temporary_files "$IMAGE_ROOT"
    reset_image_identity "$IMAGE_ROOT"
    finalize_image "$IMAGE_ROOT" "${CORVUS_BUILD_NAME:?}"
}
