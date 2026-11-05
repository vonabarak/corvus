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

# Refuse ancestor symlinks: even find -P follows symlinks inside its starting path.
image_check_residue_path() {
    local parent=${2%/*} component current=${1%/}
    [ "$parent" != "$2" ] || return 0
    local -a components
    IFS=/ read -r -a components <<< "$parent"
    for component in "${components[@]}"; do
        current="$current/$component"
        if [ -L "$current" ] || { [ -e "$current" ] && [ ! -d "$current" ]; }; then
            echo "Invalid build-residue parent: $current" >&2
            return 1
        fi
    done
}

# Clean or check one path without following links, including dangling links.
image_process_residue_path() {
    local operation=$1 root=$2 relative=$3 absent=${4:-false}
    local target="${root%/}/$relative" residue
    image_check_residue_path "$root" "$relative" || return 1
    if [ ! -e "$target" ] && [ ! -L "$target" ]; then
        return 0
    fi
    case "$operation" in
        clean)
            if [ -d "$target" ] && [ ! -L "$target" ]; then
                image_clear_directory "$target" || return 1
                if "$absent"; then
                    rmdir -- "$target" || return 1
                fi
            else
                rm -f -- "$target" || return 1
            fi
            ;;
        verify)
            if [ -d "$target" ] && [ ! -L "$target" ] && ! "$absent"; then
                residue=$(find -P "$target" -mindepth 1 ! -type d -printf "/$relative/%P\n") || return 1
                [ -n "$residue" ] || return 0
            else
                residue="/$relative"
            fi
            printf 'Build residue remains in image:\n%s\n' "$residue" >&2
            return 1
            ;;
        *) echo "Unknown residue operation: $operation" >&2; return 1 ;;
    esac
}

# Builders explicitly retain repositories; every other build uses the same policy.
image_process_build_files() {
    local operation=$1 root=$2 policy=${3:-} relative path status=0
    local -a directories=(var/cache/binpkgs var/cache/binhost var/cache/distfiles
        tmp var/tmp var/log/portage var/lib/portage/elog var/lib/portage/failed
        var/lib/portage/logs usr/local/lib/corvus-build)
    case "$policy" in
        '') directories+=(var/db/repos) ;;
        --keep-repositories) ;;
        *) echo "Unknown image cleanup policy: $policy" >&2; return 1 ;;
    esac
    for relative in "${directories[@]}"; do
        image_process_residue_path "$operation" "$root" "$relative" || status=1
    done
    # Validate parents before expanding the log glob, too.
    image_check_residue_path "$root" var/log/emerge.log || return 1
    for path in "${root%/}"/var/log/emerge*; do
        relative=${path#"${root%/}/"}
        image_process_residue_path "$operation" "$root" "$relative" true || status=1
    done
    return "$status"
}

image_clean_build_files() {
    image_process_build_files clean "$@"
}

image_verify_build_files() {
    image_process_build_files verify "$@"
}

# Expose disk contents beneath temporary mounts without entering shared caches.
# A non-recursive bind omits child mounts. The temporary view is always retired;
# callers still own their original mounts and failure-preservation traps.
image_clean_and_verify() (
    set -eu
    root=$1
    policy=${2:-}
    view=$(mktemp -d /run/corvus-image.XXXXXX) || exit 1
    trap 'status=$?; if mountpoint -q "$view"; then umount "$view" || status=1; fi; rmdir "$view" || status=1; exit "$status"' EXIT
    mount --bind "$root" "$view" || exit 1
    mount --make-private "$view" || exit 1
    image_clean_build_files "$view" "$policy" || exit 1
    image_verify_build_files "$view" "$policy" || exit 1
)

# Gate publication on clean disk contents, then write metadata and trim the image.
image_finalize() {
    image_clean_and_verify "$1" "${3:-}" || return 1
    image_write_metadata "$1" "$2"
    image_trim "$1"
}
