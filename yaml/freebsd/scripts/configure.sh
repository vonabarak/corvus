#!/bin/sh
set -eu

pkg info -e qemu-guest-agent
sysrc -f /boot/loader.conf console=comconsole boot_serial=YES comconsole_speed=115200 virtio_console_load=YES
sysrc qemu_guest_agent_enable=YES
sysrc 'qemu_guest_agent_flags=-d -m virtio-serial -p /dev/vtcon/org.qemu.guest_agent.0'
sysrc nuageinit_enable=YES growfs_enable=YES sshd_enable=YES firstboot_pkg_upgrade_enable=NO

# Replace the upstream onifconsole entry: this console must accept logins.
awk '
    $1 == "ttyu0" { print "ttyu0 \"/usr/libexec/getty std.115200\" vt100 on secure"; found = 1; next }
    { print }
    END { if (!found) print "ttyu0 \"/usr/libexec/getty std.115200\" vt100 on secure" }
' /etc/ttys > /etc/ttys.corvus
chmod 644 /etc/ttys.corvus
mv /etc/ttys.corvus /etc/ttys

# Root access is through QGA or a deliberately configured runtime account.
pw lock root
