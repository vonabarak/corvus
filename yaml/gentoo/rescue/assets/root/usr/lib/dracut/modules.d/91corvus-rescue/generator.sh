#!/bin/sh
command -v getarg >/dev/null 2>&1 || . /lib/dracut-lib.sh
set -eu
root=$(getarg root=) || exit 0
case "$root" in rescue:UUID=*) uuid=${root#rescue:UUID=} ;; *) exit 0 ;; esac
# Only an ext4 UUID is accepted; never interpolate arbitrary command-line text.
case "$uuid" in *[!0-9a-fA-F-]*|'') exit 1 ;; esac
[ "${#uuid}" = 36 ] || exit 1
# Override the generic fstab generator's interpretation of root=rescue:...
# using systemd's higher-priority generator directory.
out=$2
mode=disk
service_timeout=90
if getarg corvus.rescue.ram=1 >/dev/null; then
    mode=ram
    service_timeout=1800
fi
device=$(systemd-escape --path --suffix=device "/dev/disk/by-uuid/$uuid")
mkdir -p "$out/initrd-root-fs.target.requires" "$out/$device.d"
cat > "$out/$device.d/rescue.conf" <<'CONF'
[Unit]
JobTimeoutSec=60
CONF
cat > "$out/rescue-root.service" <<CONF
[Unit]
Description=Mount rescue storage and compressed root
DefaultDependencies=no
Requires=$device
After=$device systemd-udev-trigger.service
Before=sysroot.mount
OnFailure=emergency.target

[Service]
Type=oneshot
RemainAfterExit=yes
ExecStart=/sbin/rescue-root $uuid $mode
TimeoutStartSec=$service_timeout
CONF
cat > "$out/sysroot.mount" <<'CONF'
[Unit]
Description=Volatile rescue root
DefaultDependencies=no
Requires=rescue-root.service
After=rescue-root.service
Before=initrd-root-fs.target
OnFailure=emergency.target

[Mount]
What=overlay
Where=/sysroot
Type=overlay
Options=lowerdir=/run/initramfs/rescue-lower,upperdir=/run/initramfs/rescue-overlay/upper,workdir=/run/initramfs/rescue-overlay/work
CONF
ln -sf ../sysroot.mount "$out/initrd-root-fs.target.requires/sysroot.mount"
