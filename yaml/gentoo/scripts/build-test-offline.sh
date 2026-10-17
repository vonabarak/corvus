#!/bin/bash
# Only the helper boots. All package installation happens in the offline artifact.
set -euxo pipefail
cloud-init status --wait
# Cloud-init grows the helper's root disk; the artifact retains its source size.
[ "$(df --output=size -B1 / | tail -1)" -ge 30000000000 ]
for attempt in $(seq 1 60); do
    getent hosts distfiles.gentoo.org >/dev/null && break
    sleep 2
done
getent hosts distfiles.gentoo.org >/dev/null
[ "$(findmnt -n -o SOURCE /)" = /dev/vda2 ]
[ "$(blkid -s TYPE -o value /dev/vdb2)" = ext4 ]

root=/mnt/target
workspace=$(mktemp -d /var/tmp/corvus-test.XXXXXX)
mounts=()
resolver_saved=false
cleanup() {
    status=$?
    trap - EXIT
    set +e
    if $resolver_saved; then
        rm -f "$root/etc/resolv.conf"
        mv "$workspace/resolv.conf" "$root/etc/resolv.conf" || status=1
    fi
    for ((i=${#mounts[@]}-1; i>=0; i--)); do
        umount "${mounts[i]}" || status=1
    done
    # Keep the build workspace if unmounting failed, for inspection.
    if [ "$status" -eq 0 ]; then rm -rf "$workspace"; fi
    exit "$status"
}
trap cleanup EXIT
trap 'exit 130' INT
trap 'exit 143' TERM
trap 'exit 129' HUP
mount_at() {
    local destination=$1
    shift
    mkdir -p "$destination"
    mount "$@" "$destination"
    mounts+=("$destination")
}
mount_at /mnt/corvus-cache -t virtiofs corvus-cache
for cache in distfiles binpkgs binhost; do
    mkdir -p "/mnt/corvus-cache/$cache"
    probe=$(mktemp -d "/mnt/corvus-cache/$cache/.corvus-write-test.XXXXXX") || {
        echo "Cannot create directories in cache $cache; check host cache permissions for the virtiofsd user." >&2
        exit 1
    }
    if ! (umask 077; : > "$probe/package"); then
        rmdir "$probe"
        echo "Cannot write files in cache $cache; check host cache permissions." >&2
        exit 1
    fi
    rm "$probe/package"
    rmdir "$probe"
done
mount_at "$root" /dev/vdb2
mount_at "$root/dev" --bind /dev
mount_at "$root/dev/pts" --bind /dev/pts
mount_at "$root/dev/shm" --bind /dev/shm
mount_at "$root/proc" -t proc proc
mount_at "$root/sys" --bind /sys
# Never expose the helper's systemd socket to chrooted package scripts.
mount_at "$root/run" -t tmpfs tmpfs
mount_at "$root/var/db/repos" -t tmpfs tmpfs
mkdir -p "$workspace/portage"
mount_at "$root/var/tmp/portage" --bind "$workspace/portage"
for cache in distfiles binpkgs binhost; do
    mount_at "$root/var/cache/$cache" --bind "/mnt/corvus-cache/$cache"
done
mv "$root/etc/resolv.conf" "$workspace/resolv.conf"
resolver_saved=true
cp -L /etc/resolv.conf "$root/etc/resolv.conf"
cp /tmp/provision-test-root.sh "$root/run/provision-test-root.sh"
chroot "$root" /bin/bash /run/provision-test-root.sh
sync
