#!/bin/sh
# Prepare the reusable bake host using the shared package cache.
unset SYSROOT
# Portage and package build scripts request C.UTF-8 (also spelled C.utf8).
# Generate it before any emerge runs, even when the cloud image lacks it.
LC_ALL=C localedef -i C -f UTF-8 C.UTF-8

# Keep the repository on disk in this reusable builder image.
emerge-webrsync -k

ln -sfn ../../var/db/repos/gentoo/profiles/default/linux/amd64/23.0/no-multilib/systemd \
  /etc/portage/make.profile
install -d /etc/portage/package.use
cat > /etc/portage/package.use/grub <<'EOF'
sys-boot/grub grub_platforms_pc grub_platforms_efi-64 -mount -nls -fonts -themes
EOF

# Apply this block even when the cloud image already enables getbinpkg.
# Local and official caches can contain unsigned or partially signed gpkgs.
# The empty GPG drops also avoid Gentoo's missing nobody:nogroup lookup.
cat >> /etc/portage/make.conf <<'EOF'

DISTDIR="/mnt/corvus-cache/distfiles"
PKGDIR="/mnt/corvus-cache/binpkgs"
FEATURES="${FEATURES} -userfetch -userpriv buildpkg binpkg-multi-instance getbinpkg parallel-fetch -binpkg-request-signature binpkg-ignore-signature"
EMERGE_DEFAULT_OPTS="${EMERGE_DEFAULT_OPTS} --getbinpkg --usepkg --binpkg-respect-use=y --binpkg-changed-deps=n"
ACCEPT_LICENSE="*"
USE="bash-completion"
GPG_VERIFY_USER_DROP=""
GPG_VERIFY_GROUP_DROP=""
EOF

install -d /etc/portage/binrepos.conf
cat > /etc/portage/binrepos.conf/zz-corvus-cache.conf <<'EOF'
[binhost]
location = /mnt/corvus-cache/binhost/binhost
EOF

command -v getuto >/dev/null || emerge --buildpkg -1q app-portage/getuto
getuto

emerge --buildpkg --noreplace --quiet-build \
  sys-apps/gptfdisk \
  sys-block/parted \
  sys-fs/e2fsprogs \
  sys-fs/dosfstools \
  sys-boot/grub:2
emerge --buildpkg --noreplace --quiet-build sys-kernel/gentoo-sources

# Portage can crash after installing its replacement during a Python-target
# transition. The second pass runs the new Portage and completes the update.
emerge --buildpkg --update --deep --newuse --getbinpkg --usepkg \
  --binpkg-respect-use=y --quiet-build --keep-going=y -v @world || \
emerge --buildpkg --update --deep --newuse --getbinpkg --usepkg \
  --binpkg-respect-use=y --quiet-build --keep-going=y -v @world
