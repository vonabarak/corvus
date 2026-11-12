#!/bin/bash
# Rescue storage has one mount shared by the squashfs loop and persistent home.
check() { return 255; }
depends() { echo 'systemd systemd-initrd kernel-modules shutdown'; }
installkernel() { hostonly='' instmods ext4 squashfs loop overlay; }
install() {
    inst_multiple mount umount mountpoint losetup blockdev timeout mkdir systemd-escape
    inst_hook cmdline 95 "$moddir/parse-rescue.sh"
    inst_script "$moddir/prepare-root.sh" /sbin/rescue-root
    inst_script "$moddir/generator.sh" "$systemdutildir/system-generators/rescue-generator"
    inst_hook shutdown 90 "$moddir/shutdown.sh"
    # A named script also makes initramfs content validation unambiguous.
    inst_script "$moddir/shutdown.sh" /sbin/rescue-shutdown
}
