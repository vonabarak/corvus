#!/bin/bash
# Definitions only; callers initialize state and own traps.

# Record the published image identity and bake timestamp inside the guest.
image_write_metadata() {
    printf 'image: %s\nbuilt: %s\n' "$2" "$(date -Iseconds)" > "$1/etc/corvus-image-info"
}

# Flush writes and trim only the image filesystems, never shared cache mounts.
image_trim() (
    image_root=$1
    sync
    for filesystem in "$image_root" "${image_root%/}/boot/efi"; do
        if mountpoint -q "$filesystem"; then
            fstrim "$filesystem" || echo "Discard unavailable for $filesystem; continuing publication." >&2
        fi
    done
)

# Clear one image directory only when no filesystem is mounted at or below it.
image_clear_directory() (
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
image_clean_scratch_files() {
    local directory
    for directory in "${1%/}/var/tmp" "${1%/}/tmp"; do
        # Running builders can mount /tmp as tmpfs. Leave that live filesystem
        # alone; retain the mount guard for disk-backed directories and binds.
        if [ "$(findmnt -rn -M "$directory" -o FSTYPE)" = tmpfs ]; then
            continue
        fi
        image_clear_directory "$directory" || return $?
    done
}

# Clear offline package caches only after shared cache binds have been detached.
image_clean_temporary_files() {
    image_clear_directory "${1%/}/var/cache"
    image_clean_scratch_files "$1"
}

# Keep clone identity independent and disable direct root password authentication.
image_reset_identity() {
    rm -f "${1%/}"/etc/ssh/ssh_host_*
    : > "${1%/}/etc/machine-id"
    rm -rf "${1%/}/var/lib/cloud"
    chroot "$1" passwd -l root
}

# Use UTC and predictable local host resolution with either init system.
image_configure_defaults() {
    printf '%s\n' '127.0.0.1 localhost' '::1 localhost' > "${1%/}/etc/hosts"
    if [ -e "${1%/}/usr/share/zoneinfo/UTC" ]; then
        ln -sf ../usr/share/zoneinfo/UTC "${1%/}/etc/localtime"
    else
        # Minimal musl images need no timezone database to use UTC.
        printf '%s\n' 'TZ=UTC0' > "${1%/}/etc/env.d/00timezone"
    fi
}

# Write metadata last, then return unused blocks before mount teardown/publication.
image_finalize() {
    image_write_metadata "$1" "$2"
    image_trim "$1"
}
