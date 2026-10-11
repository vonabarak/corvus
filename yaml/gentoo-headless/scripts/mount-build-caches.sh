#!/bin/sh
mkdir -p /var/db/repos
mountpoint -q /var/db/repos || mount -t virtiofs repos /var/db/repos

mkdir -p /var/cache/distfiles /run/distfiles-host \
         /var/tmp/distfiles-upper /var/tmp/distfiles-work
mountpoint -q /run/distfiles-host || \
  mount -t virtiofs distfiles /run/distfiles-host
mountpoint -q /var/cache/distfiles || \
  mount -t overlay overlay \
    -o lowerdir=/run/distfiles-host,upperdir=/var/tmp/distfiles-upper,workdir=/var/tmp/distfiles-work \
    /var/cache/distfiles

mkdir -p /var/cache/binpkgs
mountpoint -q /var/cache/binpkgs || \
  mount -t nfs4 -o rw,vers=4.2 \
    "$NFS_BINPKGS" /var/cache/binpkgs
