#!/bin/sh
for i in $(seq 1 60); do
  getent hosts distfiles.gentoo.org >/dev/null && break
  sleep 2
done
getent hosts distfiles.gentoo.org >/dev/null \
  || { echo "network never came up after 120s" >&2; exit 1; }
