#!/bin/bash
# Resolve floating upstream inputs before Corvus fingerprints the build.
set -euo pipefail

main() {
    local image=$1 policy=$2 base file digest
    local -a args=(--var "image_if_exists=$policy"
        --var "cache_dir=${GENTOO_CACHE_DIR:-/data/corvus-cache}"
        --var "portage_keyserver=${GENTOO_KEYSERVER:-hkps://keys.gentoo.org}")
    if [ "$image" = gentoo-glibc-systemd-headless-catalyst ]; then
        base=https://distfiles.gentoo.org/releases/amd64/autobuilds/current-stage3-amd64-nomultilib-systemd
        if [ -z "${STAGE3_URL:-}" ]; then
            [ -z "${STAGE3_SHA256:-}" ] || { echo 'STAGE3_SHA256 requires STAGE3_URL' >&2; exit 1; }
            file=$(curl -fsSL "$base/latest-stage3-amd64-nomultilib-systemd.txt" |
                awk '/^stage3-amd64-nomultilib-systemd-[0-9]+T[0-9]+Z\.tar\.xz [0-9]+$/ {print $1; exit}')
            [ -n "$file" ] || { echo 'Invalid stage3 manifest' >&2; exit 1; }
            STAGE3_URL="$base/$file"
            digest=$(curl -fsSL "$STAGE3_URL.sha256")
            STAGE3_SHA256=$(printf '%s\n' "$digest" |
                awk -v file="$file" 'length($1) == 64 && $1 !~ /[^0-9a-fA-F]/ && $2 == file {print $1; exit}')
        fi
        [[ ${STAGE3_URL:-} =~ ^https://[^[:space:]]+/stage3-amd64-nomultilib-systemd-[0-9]+T[0-9]+Z\.tar\.xz$ ]] || {
            echo 'Expected an HTTPS no-multilib systemd stage3 URL' >&2; exit 1;
        }
        [[ ${STAGE3_SHA256:-} =~ ^[0-9a-fA-F]{64}$ ]] || { echo 'Invalid stage3 SHA256' >&2; exit 1; }
        if [ -z "${REPO_COMMIT:-}" ]; then
            REPO_COMMIT=$(git ls-remote https://anongit.gentoo.org/git/repo/sync/gentoo.git refs/heads/stable |
                awk '$2 == "refs/heads/stable" {print $1}')
        fi
        [[ $REPO_COMMIT =~ ^[0-9a-f]{40}$ ]] || { echo 'Invalid Gentoo repository commit' >&2; exit 1; }
        args+=(--var "stage3_url=$STAGE3_URL" --var "stage3_sha256=$STAGE3_SHA256" --var "repo_commit=$REPO_COMMIT")
        printf 'Stage3: %s\nSHA256: %s\nRepository: %s\n' "$STAGE3_URL" "$STAGE3_SHA256" "$REPO_COMMIT"
    fi
    exec "${CRV:-crv}" build "$image.yml" "${args[@]}" --wait
}

main "$@"
