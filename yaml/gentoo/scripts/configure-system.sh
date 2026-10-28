#!/bin/sh
cat > "$SYSROOT/etc/fstab" <<'EOF'
LABEL=corvus-root  /  ext4  defaults,noatime  0 1
LABEL=corvus-efi  /boot/efi  vfat  defaults,umask=0077  0 2
EOF

echo gentoo-headless > "$SYSROOT/etc/hostname"
: > "$SYSROOT/etc/machine-id"   # populated by systemd on first boot

# Pre-set the timezone to UTC. systemd-firstboot
# otherwise blocks on stdin asking the operator to pick a
# zone — fine in a docked desktop install, fatal on a
# headless serial-console boot. The /etc/localtime
# symlink (relative, into /usr/share/zoneinfo) signals
# "timezone already chosen" to firstboot.
ln -sf ../usr/share/zoneinfo/UTC "$SYSROOT/etc/localtime"

# Pin the system locale to en_US.UTF-8 and generate it
# in the sysroot. Without this, baselayout leaves
# /etc/locale.conf empty and processes inherit the C /
# POSIX locale (ASCII-only). Anything that prints a
# non-ASCII character — `crv`'s em-dash separators, for
# one — then crashes with "commitBuffer: invalid
# argument (cannot encode character …)" on stdout, and
# postgres' initdb refuses to run ("invalid locale
# settings"). Gentoo's glibc doesn't include any
# built-in UTF-8 locale, so we have to localedef one
# into the sysroot — which means a chroot because the
# sysroot's localedef wants the sysroot's
# /usr/share/i18n data, not the bake VM's. The
# locale-gen wrapper (sys-apps/locale-gen) is NOT in
# @system, so we call localedef from glibc directly.
cat > "$SYSROOT/etc/locale.gen" <<'EOF'
C.UTF-8 UTF-8
en_US.UTF-8 UTF-8
EOF
cat > "$SYSROOT/etc/locale.conf" <<'EOF'
LANG=en_US.UTF-8
EOF
install -d "$SYSROOT/usr/lib/locale"
mount --bind /dev  "$SYSROOT/dev"
mount --bind /proc "$SYSROOT/proc"
mount --bind /sys  "$SYSROOT/sys"
chroot "$SYSROOT" localedef -i en_US -f UTF-8 en_US.UTF-8
# Keep Portage's C.UTF-8 locale available in downstream bake VMs too.
LC_ALL=C chroot "$SYSROOT" localedef -i C -f UTF-8 C.UTF-8
umount "$SYSROOT/sys" "$SYSROOT/proc" "$SYSROOT/dev"

install -d "$SYSROOT/etc/systemd/network"
cat > "$SYSROOT/etc/systemd/network/20-wired.network" <<'EOF'
[Match]
Name=en* eth*

[Network]
DHCP=yes
EOF

systemctl --root="$SYSROOT" enable \
  systemd-networkd.service \
  systemd-resolved.service \
  sshd.service \
  qemu-guest-agent.service \
  serial-getty@ttyS0.service

# Resolved-managed /etc/resolv.conf
ln -sf /run/systemd/resolve/stub-resolv.conf "$SYSROOT/etc/resolv.conf" || true
