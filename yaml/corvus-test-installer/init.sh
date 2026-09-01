#!/bin/sh
# Synthetic "installer" PID 1 for the corvus integration suite.
#
# Bakes the installer-strategy build path in seconds rather than the
# 45-55 min Windows reference: mount the answer-media CD supplied by the
# bake, copy the marker payload onto /dev/vda, sync, poweroff. The
# daemon's installer phase treats the bake VM's first drive as the
# artifact, so /dev/vda is exactly the disk that will be published.
#
# The marker layout (CORVUS-INSTALLER-OK\n + per-run UUID) lets the
# test prove BOTH that the bake VM booted AND that it actually
# read the uploaded answer media this run.

# Send all output to the serial console so a bake failure is
# debuggable from the daemon's serial buffer. The kernel cmdline
# in isolinux.cfg pins /dev/console to ttyS0.
exec >/dev/console 2>&1
echo "[corvus-installer] PID 1 entering"

# Mount points the build script may not have populated in the
# cpio (busybox-static is the only required entry). 'mkdir -p'
# is a no-op if they're already present.
mkdir -p /proc /sys /dev /mnt/media /tmp

mount -t proc proc /proc
mount -t sysfs sys /sys
mount -t devtmpfs dev /dev

# Load the answer-media CD-ROM drivers, then the target-disk driver.
echo "[corvus-installer] loading modules"
insmod /lib/modules/cdrom.ko
insmod /lib/modules/sr_mod.ko
insmod /lib/modules/isofs.ko
# virtio_blk gives us /dev/vda; without it the dd to /dev/vda
# would silently create a regular file at /dev/vda (devtmpfs is
# writable) instead of hitting the bake VM's target disk.
insmod /lib/modules/virtio_blk.ko

# The second IDE CD-ROM is the uploaded answer medium; the first is
# this installer ISO. Poll briefly rather than racing.
echo "[corvus-installer] waiting for /dev/sr1"
for i in 1 2 3 4 5 6 7 8 9 10; do
  [ -b /dev/sr1 ] && break
  sleep 0.2
done

echo "[corvus-installer] mounting answer media"
if ! mount -t iso9660 -o ro /dev/sr1 /mnt/media; then
  echo "[corvus-installer] FATAL: /dev/sr1 mount failed"
  ls -la /dev/sr1 2>/dev/null || echo "/dev/sr1 absent"
  sleep 2
  poweroff -f
fi

echo "[corvus-installer] waiting for /dev/vda"
for i in 1 2 3 4 5 6 7 8 9 10; do
  [ -b /dev/vda ] && break
  sleep 0.2
done
if [ ! -b /dev/vda ]; then
  echo "[corvus-installer] FATAL: /dev/vda not a block device (virtio_blk failed?)"
  sleep 2
  poweroff -f
fi

echo "[corvus-installer] writing marker to /dev/vda"
printf 'CORVUS-INSTALLER-OK\n' > /tmp/header
cat /tmp/header /mnt/media/marker.txt > /tmp/payload
# Pad the payload to a full 512-byte sector so the write to the
# block device hits exactly one aligned sector — un-padded writes
# of <512 bytes through busybox dd are silently rejected on
# unbuffered block devices.
dd if=/dev/zero of=/tmp/pad bs=512 count=1 2>/dev/null
cat /tmp/pad >> /tmp/payload
dd if=/tmp/payload of=/dev/vda bs=512 count=1 conv=notrunc,fsync
sync
echo "[corvus-installer] done; powering off"
poweroff -f
