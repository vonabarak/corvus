#!/bin/sh
# Prepare the bake host; its package sources differ, its build policy does not.
unset SYSROOT
case "$BUILD_SOURCE" in
  mounted)
    # The template mounted the host repository and package caches before QGA.
    test -d /var/db/repos/gentoo/metadata
    ;;
  standalone)
    # QGA bootstrap normally populated the tree with a verified snapshot.
    # Avoid an immediate rsync: it can disagree with the snapshot's Manifest.
    if [ ! -d /var/db/repos/gentoo/metadata ]; then
      emerge-webrsync
    fi
    install -d /etc/portage/binrepos.conf
    cat > /etc/portage/binrepos.conf/gentoo.conf <<'EOF'
[binhost]
priority = 9999
sync-uri = https://distfiles.gentoo.org/releases/amd64/binpackages/23.0/x86-64
verify-signature = false
EOF
    ;;
  *)
    echo "unknown BUILD_SOURCE: $BUILD_SOURCE" >&2
    exit 1
    ;;
esac

ln -sfn ../../var/db/repos/gentoo/profiles/default/linux/amd64/23.0/no-multilib/systemd \
  /etc/portage/make.profile
install -d /etc/portage/package.use
cat > /etc/portage/package.use/grub <<'EOF'
sys-boot/grub grub_platforms_pc -mount -nls -fonts -themes
EOF

# Apply this block even when the cloud image already enables getbinpkg.
# Local and official caches can contain unsigned or partially signed gpkgs.
# The empty GPG drops also avoid Gentoo's missing nobody:nogroup lookup.
cat >> /etc/portage/make.conf <<'EOF'

FEATURES="${FEATURES} buildpkg getbinpkg parallel-fetch -binpkg-request-signature binpkg-ignore-signature"
EMERGE_DEFAULT_OPTS="${EMERGE_DEFAULT_OPTS} --getbinpkg --usepkg --binpkg-respect-use=y --binpkg-changed-deps=n"
ACCEPT_LICENSE="*"
USE="bash-completion"
GPG_VERIFY_USER_DROP=""
GPG_VERIFY_GROUP_DROP=""
EOF

command -v getuto >/dev/null || emerge -1q app-portage/getuto
getuto

emerge --noreplace --quiet-build \
  sys-apps/gptfdisk \
  sys-block/parted \
  sys-fs/e2fsprogs \
  sys-boot/grub:2
emerge --noreplace --quiet-build sys-kernel/gentoo-sources

if [ "$BUILD_SOURCE" = standalone ]; then
  # Initial cloud-init has finished installing QGA. Remove its old Python
  # target before updating @world, so it cannot pin conflicting dependencies.
  emerge -C --quiet app-emulation/cloud-init
fi

# Portage can crash after installing its replacement during a Python-target
# transition. The second pass runs the new Portage and completes the update.
emerge --update --deep --newuse --getbinpkg --usepkg \
  --binpkg-respect-use=y --quiet-build --keep-going=y -v @world || \
emerge --update --deep --newuse --getbinpkg --usepkg \
  --binpkg-respect-use=y --quiet-build --keep-going=y -v @world
