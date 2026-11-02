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
    sgdisk -n "2:0:+${IMAGE_ESP_SIZE:-512M}" -t 2:ef00 -c 2:efi "$disk"
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
    linux /boot/$kernel root=/dev/vda3 rootfstype=ext4 console=tty0 console=ttyS0,115200n8 ${IMAGE_KERNEL_ARGS:-}
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
    if [[ "${IMAGE_PROFILE:-}" == test-vm ]]; then
        forbidden+=(app-admin/sudo dev-lang/python)
    fi
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

# Drop package-manager state only from the immutable test guest, after verification.
remove_test_vm_package_metadata() {
    local root=${IMAGE_ROOT:?}
    rm -rf "$root/var/db/pkg" "$root/var/lib/portage"
}

# Keep English gettext catalogs, including regional and modifier variants.
remove_non_english_locales() {
    local root=${IMAGE_ROOT:?}
    local directory
    for directory in "$root/usr/share/locale" "$root/usr/lib/locale"; do
        if [[ -d "$directory" ]]; then
            find "$directory" -mindepth 1 -maxdepth 1 \
                ! -name en ! -name 'en_*' ! -name 'en@*' \
                ! -name C ! -name C.utf8 ! -name C.UTF-8 -exec rm -rf -- {} +
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
    if [[ "${IMAGE_PROFILE:-}" == test-vm ]]; then
        remove_test_vm_package_metadata
        remove_non_english_locales
    fi
    clean_image_temporary_files "$IMAGE_ROOT"
    reset_image_identity "$IMAGE_ROOT"
    finalize_image "$IMAGE_ROOT" "${CORVUS_BUILD_NAME:?}"
}

# --- Offline Corvus development and test-node builds ---

# Wait for the grown helper and validate artifact partitions before mounting.
check_build_prerequisites() {
    cloud-init status --wait
    # Cloud-init grows only the helper; compilation must not fill the smaller artifact.
    [ "$(df --output=size -B1 / | tail -1)" -ge 30000000000 ]
    wait_for_network
    [ "$(blkid -s TYPE -o value "${TARGET_DISK:-/dev/vdb}3")" = ext4 ]
    [ "$(blkid -s TYPE -o value "${TARGET_DISK:-/dev/vdb}2")" = vfat ]
}

# Stop the target server before releasing its private runtime filesystem.
stop_postgres() {
    if "$postgres_active"; then
        chroot "$root" runuser -u postgres -- "$pgbin/pg_ctl" -D "$pgdata" -m fast -w stop || return $?
        postgres_active=false
    fi
}

# Preserve the build error while stopping PostgreSQL and releasing every mount.
cleanup_test_build() {
    local status=$?
    trap - EXIT
    set +e
    stop_postgres || { [ "$status" -ne 0 ] || status=1; }
    unmount_tracked || { [ "$status" -ne 0 ] || status=1; }
    # Failed builds retain their workspace for inspection; never remove mounted data.
    if [ "$status" -eq 0 ] && [ -n "$workspace" ]; then
        rm -rf -- "$workspace"
    fi
    exit "$status"
}

# Use tracked, disposable repository storage and private target interfaces.
prepare_build_mounts() {
    workspace=$(mktemp -d /var/tmp/corvus-test.XXXXXX)
    # Repository sync drops privileges even when package userpriv is disabled.
    chmod 0755 "$workspace"
    # Native Portage resolves repositories on the helper, not beneath ROOT.
    mount_at /var/db/repos -t tmpfs tmpfs
    prepare_workspace
    # Select devices explicitly: helper and artifact have identical filesystem labels.
    mount_target_disk "$root" "${TARGET_DISK:-/dev/vdb}"
    bind /dev/pts "$root/dev/pts"
    bind /dev/shm "$root/dev/shm"
    # PostgreSQL pkg_config needs the same ebuild tree inside its individual chroot.
    bind /var/db/repos "$root/var/db/repos"
    local cache source
    for cache in distfiles binpkgs binhost; do
        case "$cache" in
            binpkgs) source=packages/x86-64 ;;
            *) source=$cache ;;
        esac
        bind "/mnt/corvus-cache/$source" "$root/var/cache/$cache"
    done
    # All compilation uses the larger helper disk, including native build dependencies.
    export PORTAGE_TMPDIR="$workspace"
    configure_glibc_caches
}

# Install natively into ROOT while retaining the target dependency closure.
install_target_packages() {
    env -u SYSROOT ROOT="$root" emerge --root-deps "$@"
}

# Initialize the target cluster and database without starting its init system.
initialize_postgres() {
    chroot "$root" eselect postgresql set 18
    # Run the installed package's configuration with target accounts, locale, and paths.
    env -u ROOT -u SYSROOT -u PORTAGE_TMPDIR -u DISTDIR -u PKGDIR \
        chroot "$root" emerge --config dev-db/postgresql:18
    systemctl --root="$root" enable postgresql-18.service
    chroot "$root" install -d -o postgres -g postgres /run/postgresql
    # Mark ownership before startup so cleanup also handles a partially failed start.
    postgres_active=true
    chroot "$root" runuser -u postgres -- "$pgbin/pg_ctl" -D "$pgdata" -w start \
        -o "-c listen_addresses='' -c unix_socket_directories=/run/postgresql"
    chroot "$root" runuser -u postgres -- "$pgbin/psql" -h /run/postgresql -v ON_ERROR_STOP=1 <<'SQL'
CREATE USER corvus SUPERUSER;
CREATE DATABASE corvus OWNER corvus;
SQL
    stop_postgres
}

# Install and enable the VDE switch used by nested test VMs.
configure_vde_service() {
    install -d "$root/etc/systemd/system"
    cat > "$root/etc/systemd/system/vde-switch.service" <<'EOF'
[Unit]
Description=VDE virtual switch for Corvus test VMs
After=network-pre.target
Before=network.target
Wants=network-pre.target

[Service]
Type=forking
ExecStartPre=/bin/mkdir -p /run/vde2
ExecStart=/usr/bin/vde_switch \
    --sock /run/vde2/switch.ctl \
    --mgmt /run/vde2/switch.mgmt \
    -tap vde0 \
    -m 666 \
    -group kvm \
    --daemon \
    --pidfile /run/vde2/vde_switch.pid
PIDFile=/run/vde2/vde_switch.pid
Restart=on-failure

[Install]
WantedBy=multi-user.target
EOF
    systemctl --root="$root" enable vde-switch.service
}

# Assign the private VDE subnet and enable IPv4 forwarding.
configure_vde_network() {
    install -d "$root/etc/systemd/network"
    cat > "$root/etc/systemd/network/30-vde0.network" <<'EOF'
[Match]
Name=vde0

[Network]
Address=192.168.92.1/24
ConfigureWithoutCarrier=yes
EOF

    install -d "$root/etc/sysctl.d"
    cat > "$root/etc/sysctl.d/30-corvus-vde-forward.conf" <<'EOF'
net.ipv4.ip_forward = 1
EOF
}

# Masquerade traffic leaving the private VDE subnet.
configure_nat() {
    # Gentoo nftables.service loads this path, not /etc/nftables.conf.
    install -d -m 0755 "$root/etc/nftables/rules"
    cat > "$root/etc/nftables/rules/main.nft" <<'EOF'
flush ruleset

table ip nat {
    chain postrouting {
        type nat hook postrouting priority srcnat
        policy accept
        ip saddr 192.168.92.0/24 oifname != "vde0" masquerade
    }
}

EOF
}

# Serve DHCP and DNS only on VDE, after its interface is available.
configure_dhcp() {
    cat > "$root/etc/dnsmasq.conf" <<'EOF'
interface=vde0
bind-dynamic
dhcp-range=192.168.92.10,192.168.92.200,12h
dhcp-option=option:router,192.168.92.1
dhcp-option=option:dns-server,192.168.92.1
domain-needed
bogus-priv
EOF

    install -d "$root/etc/systemd/system/dnsmasq.service.d"
    cat > "$root/etc/systemd/system/dnsmasq.service.d/wait-vde.conf" <<'EOF'
[Unit]
After=vde-switch.service systemd-networkd.service
Wants=systemd-networkd.service
EOF

    systemctl --root="$root" enable nftables.service dnsmasq.service
}

# Declare the same Git overlays for the native helper and offline artifact.
write_corvus_repositories() {
    local config_root=${1%/}
    install -d "$config_root/etc/portage/repos.conf"
    cat > "$config_root/etc/portage/repos.conf/corvus.conf" <<'EOF'
[vonabarak]
location = /var/db/repos/vonabarak
sync-type = git
sync-uri = https://github.com/vonabarak/gentoo.git

[haskell]
location = /var/db/repos/haskell
sync-type = git
sync-uri = https://github.com/gentoo-haskell/gentoo-haskell.git
EOF
}
