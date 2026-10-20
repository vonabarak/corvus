#!/bin/sh
set -eu

# Do not re-arm firstboot while /etc/rc could still remove its sentinel.
if pgrep -f '^/bin/sh /etc/rc([[:space:]]|$)' >/dev/null; then
    echo 'Cannot seal while /etc/rc is running' >&2
    exit 1
fi
test ! -e /firstboot
pkg clean -ay
rm -rf /var/cache/nuageinit
rm -f /var/log/nuageinit.log /etc/ssh/ssh_host_* /etc/hostid /var/db/hostid
rm -f /etc/rc.conf.d/hostname /etc/rc.conf.d/network /etc/rc.conf.d/routing
rm -f /etc/resolv.conf /etc/resolvconf.conf /var/db/dhclient.leases.*
rm -f /var/db/entropy/* /boot/entropy /var/db/random.seed
rm -f /root/.history /root/.sh_history
# The startup scripts remove this once the new VM's NoCloud setup completes.
touch /firstboot
sync
