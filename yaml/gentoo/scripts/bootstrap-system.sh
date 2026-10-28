#!/bin/sh
mkdir -p "$SYSROOT"
mount "${DEV}3" "$SYSROOT"
install -d "$SYSROOT/boot/efi"
mount "${DEV}2" "$SYSROOT/boot/efi"

install -d "$SYSROOT/etc/portage" \
           "$SYSROOT/etc/portage/package.use" \
           "$SYSROOT/etc/portage/package.accept_keywords" \
           "$SYSROOT/etc/portage/package.mask" \
           "$SYSROOT/etc/portage/package.unmask" \
           "$SYSROOT/etc/portage/package.license" \
           "$SYSROOT/etc/portage/env" \
           "$SYSROOT/etc/portage/repos.conf" \
           "$SYSROOT/etc/portage/binrepos.conf" \
           "$SYSROOT/var/cache/distfiles" \
           "$SYSROOT/var/cache/binpkgs" \
           "$SYSROOT/var/cache/binhost" \
           "$SYSROOT/var/db/repos" \
           "$SYSROOT/usr/bin" \
           "$SYSROOT/usr/lib" \
           "$SYSROOT/usr/lib64"

# Cache paths are supplied explicitly in the build environment. Keep these
# runtime directories empty; no host cache mounts are needed inside SYSROOT.

ln -sf bin       "$SYSROOT/usr/sbin"
ln -sf usr/bin   "$SYSROOT/bin"
ln -sf usr/sbin  "$SYSROOT/sbin"
ln -sf usr/lib   "$SYSROOT/lib"
ln -sf usr/lib64 "$SYSROOT/lib64"

# Quoted heredoc so the ${VAR} references stay literal
# (portage's make.conf parser will expand them) and so
# the backticks in the comments below don't get evaluated
# as shell command substitution. MAKEOPTS needs nproc at
# bake time, which a quoted heredoc can't expand — we
# append it separately after the heredoc closes.
cat > "$SYSROOT/etc/portage/make.conf" <<'EOF'
COMMON_FLAGS="-O2 -pipe"
CFLAGS="${COMMON_FLAGS}"
CXXFLAGS="${COMMON_FLAGS}"
FCFLAGS="${COMMON_FLAGS}"
FFLAGS="${COMMON_FLAGS}"

DISTDIR="/var/cache/distfiles"
PKGDIR="/var/cache/binpkgs"
FEATURES="buildpkg binpkg-multi-instance getbinpkg parallel-fetch -binpkg-request-signature binpkg-ignore-signature"
EMERGE_DEFAULT_OPTS="--getbinpkg --usepkg --binpkg-respect-use=y --binpkg-changed-deps=n"
ACCEPT_LICENSE="*"
USE="bash-completion"

# Portage's gpkg verifier (gpkg.py:checksum_helper) drops
# privileges to nobody:nogroup whenever a gpkg contains a
# .sig — and that happens BEFORE any FEATURES check, so
# binpkg-ignore-signature alone can't avoid the lookup.
# Gentoo's baselayout doesn't ship a 'nogroup' group, so
# the lookup crashes with KeyError: "getgrnam(): name not
# found: 'nogroup'" the first time a partially-signed
# binpkg (e.g. acct-group/postgres) lands. Empty value
# tells portage to skip the privilege drop entirely.
GPG_VERIFY_USER_DROP=""
GPG_VERIFY_GROUP_DROP=""
EOF

# Append MAKEOPTS with the bake-time nproc baked in;
# portage's make.conf parser doesn't run $() so we
# resolve it at write time instead.
echo "MAKEOPTS=\"-j$(nproc)\"" >> "$SYSROOT/etc/portage/make.conf"

cat > "$SYSROOT/etc/portage/package.use/grub" <<'EOF'
sys-boot/grub grub_platforms_pc grub_platforms_efi-64 -mount -nls -fonts -themes
EOF

cat > "$SYSROOT/etc/portage/repos.conf/gentoo.conf" <<'EOF'
[DEFAULT]
main-repo = gentoo

[gentoo]
location = /var/db/repos/gentoo
sync-type = rsync
sync-uri = rsync://rsync.gentoo.org/gentoo-portage
auto-sync = yes
EOF

cat > "$SYSROOT/etc/portage/binrepos.conf/gentoo.conf" <<'EOF'
[binhost]
priority = 9999
sync-uri = https://distfiles.gentoo.org/releases/amd64/binpackages/23.0/x86-64
verify-signature = false
EOF

# Relative make.profile so the symlink resolves on the
# booted image the first time the user runs
# `emerge-webrsync` (or `emerge --sync`) — same convention
# the stage3 tarball used.
ln -sf ../../var/db/repos/gentoo/profiles/default/linux/amd64/23.0/no-multilib/systemd \
  "$SYSROOT/etc/portage/make.profile"

# Trust keys for binpkg signature verification.
cp -a /etc/portage/gnupg "$SYSROOT/etc/portage/gnupg"

# Break the well-known systemd ↔ util-linux build
# circular dep: util-linux[systemd] needs sys-apps/systemd
# to build, but systemd needs util-linux. Stage3 used to
# paper over this by shipping a pre-built util-linux. With
# no stage3 we have to bootstrap manually — install
# util-linux without the systemd USE first, then re-emerge
# it at the end of this step so the final image has full
# systemd integration.
#
# The override goes in the BAKE VM's /etc/portage (not
# the sysroot's) because the @system emerge below uses
# ROOT=$SYSROOT only — it reads /etc/portage from the
# live bake VM, not from the sysroot. We use a name
# outside the sysroot so the bootstrap doesn't leak into
# the final image.
install -d /etc/portage/package.use
# `build` USE on util-linux is the stage3-style minimal
# mode that strips systemd + udev + most extras in one
# flag — breaks both the systemd↔util-linux cycle AND
# the libudev→systemd→util-linux→libudev cycle that comes
# in via `virtual/libudev`. Pam still needs an explicit
# -systemd to break the pam↔systemd cycle.
cat > /etc/portage/package.use/zz-corvus-bootstrap <<'EOF'
sys-apps/util-linux build
sys-libs/pam -systemd
EOF

# Native build dependencies (BDEPEND, e.g. rust-bin) use the bake host's
# configuration, not the target's environment. Configure both roots: the
# shell defaults cover the target, and this block covers native dependencies
# and interactive recovery commands, including with an older builder image.
cat >> /etc/portage/make.conf <<'EOF'
DISTDIR="/mnt/corvus-cache/distfiles"
PKGDIR="/mnt/corvus-cache/binpkgs"
FEATURES="${FEATURES} -userfetch -userpriv"
EOF
install -d /etc/portage/binrepos.conf
cat > /etc/portage/binrepos.conf/zz-corvus-cache.conf <<'EOF'
[binhost]
location = /mnt/corvus-cache/binhost/binhost
EOF

# Install @system using the bake VM's repository and package caches.
# The bake VM's /etc/portage drives dependency resolution for this native
# amd64 systemd build, with sources downloaded into the shared cache.
#
# `env -u SYSROOT` is critical: this pipeline exports a
# shell-level SYSROOT=/mnt/sysroot for ergonomics, but
# portage interprets a non-empty SYSROOT as the
# cross-compile target sysroot and pipes
# `--sysroot=/mnt/sysroot` through to GCC / CMake. With a
# freshly-formatted /mnt/sysroot (no /usr/include yet),
# every source-build ebuild then dies with "fatal error:
# assert.h: No such file or directory". Unsetting SYSROOT
# here puts portage in native mode — uses the bake VM's
# own headers/libs, installs into ROOT=/mnt/sysroot.
env -u SYSROOT ROOT="$SYSROOT" emerge --buildpkg --quiet-build @system

# Cycles broken: drop the overrides and re-emerge the
# bootstrapped packages with full USE now that
# sys-apps/systemd is on disk in the sysroot.
rm /etc/portage/package.use/zz-corvus-bootstrap
env -u SYSROOT ROOT="$SYSROOT" emerge --buildpkg --quiet-build -1 sys-apps/util-linux sys-libs/pam

# systemd-PID1's required pseudo-fs mountpoints. baselayout
# normally lays /proc, /sys, /dev, /run, /tmp down on a
# fresh stage3 unpack, but its ROOT=$SYSROOT @system
# install above doesn't reliably create them — without
# these the booted image hangs at PID1 with "Failed to
# mount proc/sys/dev: No such file or directory" plus a
# very early "devtmpfs: error mounting -2" from the
# kernel's auto-mount.
install -d -m 755 "$SYSROOT/proc" "$SYSROOT/sys" \
                  "$SYSROOT/dev"  "$SYSROOT/run"
install -d -m 1777 "$SYSROOT/tmp" "$SYSROOT/var/tmp"
