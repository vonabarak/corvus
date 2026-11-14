#!/bin/bash
# Runs inside Catalyst's stage4 chroot after the static overlay is installed.
set -eu
# genkernel saves the effective configuration outside /boot.
cp /etc/kernels/config-* /boot/
localedef -i en_US -f UTF-8 en_US.UTF-8
LC_ALL=C localedef -i C -f UTF-8 C.UTF-8
systemctl --root=/ enable systemd-networkd.service systemd-resolved.service \
    sshd.service qemu-guest-agent.service serial-getty@ttyS0.service
# Gentoo's QGA unit is static; do not rely on its udev activation timing.
systemctl --root=/ add-wants multi-user.target qemu-guest-agent.service
ln -sfn /run/systemd/resolve/stub-resolv.conf /etc/resolv.conf
ln -sfn ../usr/share/zoneinfo/UTC /etc/localtime
