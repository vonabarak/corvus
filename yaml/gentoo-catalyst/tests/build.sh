#!/bin/bash
# Offline input and publication gates. No daemon, mounts, or network required.
set -euo pipefail
cd "$(dirname "$0")/.."
. catalyst/lib.sh
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
reject() {
    if "$@" >"$work/rejected.log" 2>&1; then
        echo "Unexpected success: $*" >&2
        exit 1
    fi
}
export STAGE3_URL=https://example.invalid/stage3-amd64-nomultilib-systemd-20261108T170107Z.tar.xz
export REPO_COMMIT=0123456789012345678901234567890123456789
seed="$work/${STAGE3_URL##*/}"
printf 'seed fixture\n' > "$seed"
export STAGE3_SHA256
STAGE3_SHA256=$(sha256sum "$seed" | cut -d ' ' -f1)
catalyst_validate_inputs
reject env STAGE3_URL=http://example.invalid/seed bash -c '. catalyst/lib.sh; catalyst_validate_inputs'
reject env REPO_COMMIT=stable bash -c '. catalyst/lib.sh; catalyst_validate_inputs'
reject env STAGE3_SHA256=bad bash -c '. catalyst/lib.sh; catalyst_validate_inputs'
printf '%s  %s\n' "$STAGE3_SHA256" "${seed##*/}" > "$work/digest"
catalyst_verify_seed "$seed" "$work/digest"
printf 'tampered\n' >> "$seed"
reject catalyst_verify_seed "$seed" "$work/digest"
printf '%064d  %s\n' 0 "${seed##*/}" > "$work/digest"
reject catalyst_verify_seed "$seed" "$work/digest"
reject catalyst_check_archive "$work/missing.tar"
mkdir -p "$work/root/usr/lib/systemd" "$work/root/boot"
touch "$work/root/usr/lib/systemd/systemd" "$work/root/boot/vmlinuz-test"
tar -cf "$work/stage4.tar" -C "$work/root" .
reject catalyst_check_archive "$work/stage4.tar"
touch "$work/root/boot/config-test"
tar -cf "$work/stage4.tar" -C "$work/root" .
reject catalyst_check_archive "$work/stage4.tar"
printf 'CONFIG_%s=y\n' VIRTIO_BLK VIRTIO_PCI EXT4_FS DEVTMPFS DEVTMPFS_MOUNT > "$work/root/boot/config-test"
tar -cf "$work/stage4.tar" -C "$work/root" .
catalyst_check_archive "$work/stage4.tar"
# A real archive listing exceeds the pipe buffer. Consume it fully under pipefail.
mkdir "$work/root/residue"
for ((i=0; i<2000; i++)); do
    touch "$work/root/residue/long-archive-entry-name-$i"
done
tar -cf "$work/large-stage4.tar" -C "$work/root" boot usr residue
catalyst_check_archive "$work/large-stage4.tar"
mkdir "$work/bin"
cat > "$work/bin/crv" <<'SH'
#!/bin/bash
printf '%s\n' "$@" > "$CAPTURE"
SH
cat > "$work/bin/curl" <<'SH'
#!/bin/bash
exit 99
SH
cp "$work/bin/curl" "$work/bin/git"
chmod +x "$work/bin/"*
export CAPTURE="$work/arguments" CRV="$work/bin/crv"
PATH="$work/bin:$PATH" bash scripts/publish.sh gentoo-glibc-systemd-headless-catalyst update >/dev/null
for value in "stage3_url=$STAGE3_URL" "stage3_sha256=$STAGE3_SHA256" "repo_commit=$REPO_COMMIT" image_if_exists=update; do
    grep -Fxq -- "$value" "$CAPTURE"
done
rm "$CAPTURE"
reject env STAGE3_SHA256=bad bash scripts/publish.sh gentoo-glibc-systemd-headless-catalyst update
[ ! -e "$CAPTURE" ]
echo 'Catalyst input, artifact, and publication gates passed'
