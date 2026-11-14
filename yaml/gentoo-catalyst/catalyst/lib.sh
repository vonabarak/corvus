#!/bin/bash
# Input and artifact gates; no package installation or kernel compilation.
catalyst_validate_inputs() {
    [[ ${STAGE3_URL:-} =~ ^https://[^[:space:]]+/stage3-amd64-nomultilib-systemd-[0-9]+T[0-9]+Z\.tar\.xz$ ]] || {
        echo 'Invalid stage3 URL' >&2; return 1;
    }
    [[ ${STAGE3_SHA256:-} =~ ^[0-9a-fA-F]{64}$ ]] || { echo 'Invalid stage3 SHA256' >&2; return 1; }
    [[ ${REPO_COMMIT:-} =~ ^[0-9a-f]{40}$ ]] || { echo 'Invalid repository commit' >&2; return 1; }
}

catalyst_verify_seed() (
    local archive=$1 digest=$2 filename signed_sha
    filename=${archive##*/}
    signed_sha=$(awk -v name="$filename" 'length($1) == 64 && $1 !~ /[^0-9a-fA-F]/ && $2 == name {print $1}' "$digest")
    [ "${signed_sha,,}" = "${STAGE3_SHA256,,}" ] || {
        echo 'Signed stage3 digest does not match the declared input' >&2; exit 1;
    }
    printf '%s  %s\n' "$STAGE3_SHA256" "$archive" | sha256sum --check --status
)

catalyst_check_archive() (
    local archive=$1 listing config option
    [ -s "$archive" ] || { echo "Missing Catalyst archive: $archive" >&2; exit 1; }
    listing=$(tar -tf "$archive")
    printf '%s\n' "$listing" | sed 's|^\./||' | awk '
        /^usr\/lib\/systemd\/systemd$/ { systemd=1 }
        /^boot\/vmlinuz-[^/]+$/ { kernel=1 }
        /^boot\/config-[^/]+$/ { config=1 }
        END { exit !(systemd && kernel && config) }
    ' || { echo 'Catalyst archive lacks systemd, kernel, or kernel configuration' >&2; exit 1; }
    config=$(printf '%s\n' "$listing" | awk '/^(\.\/)?boot\/config-[^/]+$/ && !found { print; found=1 }')
    config=$(tar -xOf "$archive" "$config")
    for option in VIRTIO_BLK VIRTIO_PCI EXT4_FS DEVTMPFS DEVTMPFS_MOUNT; do
        grep -Fxq "CONFIG_$option=y" <<< "$config" || {
            echo "Kernel needs built-in $option for direct-root boot" >&2; exit 1;
        }
    done

)
