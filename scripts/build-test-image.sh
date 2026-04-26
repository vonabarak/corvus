#!/bin/sh
#
# Build a minimal Alpine Linux test image for Corvus integration tests.
#
# Produces a qcow2 image that supports BIOS + UEFI boot, serial + graphical
# console, SSH, doas, and qemu-guest-agent -- all pre-configured.
#
# Requires root (losetup + mount). Run via: doas scripts/build-test-image.sh
#
set -eu

# -- Configuration ------------------------------------------------------------

ALPINE_MIRROR="https://dl-cdn.alpinelinux.org/alpine"
ALPINE_VERSION="v3.21"
ALPINE_ARCH="x86_64"
IMAGE_SIZE="512M"

OUTPUT_DIR=".test-images"
OUTPUT_IMAGE="$OUTPUT_DIR/corvus-test.qcow2"
OUTPUT_KEY="$OUTPUT_DIR/corvus-test-key"
FORCE=0

# -- Parse arguments -----------------------------------------------------------

usage() {
    echo "Usage: $0 [--force] [--output PATH]"
    echo "  --force    Rebuild even if image already exists"
    echo "  --output   Output image path (default: $OUTPUT_IMAGE)"
    exit 1
}

while [ $# -gt 0 ]; do
    case "$1" in
        --force) FORCE=1; shift ;;
        --output) OUTPUT_IMAGE="$2"; shift 2 ;;
        -h|--help) usage ;;
        *) echo "Unknown argument: $1"; usage ;;
    esac
done

# -- Pre-flight checks --------------------------------------------------------

if [ "$(id -u)" -ne 0 ]; then
    echo "Error: must run as root (losetup + mount require it)"
    exit 1
fi

for cmd in qemu-img sgdisk mkfs.ext4 mkfs.vfat losetup chroot wget ssh-keygen; do
    if ! command -v "$cmd" >/dev/null 2>&1; then
        echo "Error: required command not found: $cmd"
        exit 1
    fi
done

if [ "$FORCE" -eq 0 ] && [ -f "$OUTPUT_IMAGE" ]; then
    echo "Image already exists: $OUTPUT_IMAGE (use --force to rebuild)"
    exit 0
fi

# -- Derived paths -------------------------------------------------------------

BUILD_IMAGE=$(mktemp /tmp/corvus-test-XXXXXX.raw)
MOUNTPOINT=$(mktemp -d /tmp/corvus-rootfs-XXXXXX)
APK_STATIC_DIR=$(mktemp -d /tmp/corvus-apk-XXXXXX)
LOOP_DEV=""

cleanup() {
    set +e
    echo "-- Cleaning up..."

    # Unmount in reverse order
    umount "$MOUNTPOINT/boot/efi" 2>/dev/null
    umount "$MOUNTPOINT/dev/pts" 2>/dev/null
    umount "$MOUNTPOINT/dev" 2>/dev/null
    umount "$MOUNTPOINT/sys" 2>/dev/null
    umount "$MOUNTPOINT/proc" 2>/dev/null
    umount "$MOUNTPOINT" 2>/dev/null

    # Detach loop device
    if [ -n "$LOOP_DEV" ]; then
        losetup -d "$LOOP_DEV" 2>/dev/null
    fi

    # Remove temp files
    rm -f "$BUILD_IMAGE"
    rm -rf "$MOUNTPOINT"
    rm -rf "$APK_STATIC_DIR"
}

trap cleanup EXIT

# -- Step 1: Download apk-tools-static ----------------------------------------

echo "-- Downloading apk-tools-static..."
APK_STATIC_URL="$ALPINE_MIRROR/$ALPINE_VERSION/main/$ALPINE_ARCH"

# Fetch the APKINDEX to find the exact apk-tools-static filename
wget -q "$APK_STATIC_URL/APKINDEX.tar.gz" -O "$APK_STATIC_DIR/APKINDEX.tar.gz"
APK_TOOLS_PKG=$(tar -xzf "$APK_STATIC_DIR/APKINDEX.tar.gz" -O APKINDEX 2>/dev/null \
    | awk '/^P:apk-tools-static/{found=1} found && /^V:/{print "apk-tools-static-" substr($0,3) ".apk"; exit}')

if [ -z "$APK_TOOLS_PKG" ]; then
    echo "Error: could not find apk-tools-static in APKINDEX"
    exit 1
fi

wget -q "$APK_STATIC_URL/$APK_TOOLS_PKG" -O "$APK_STATIC_DIR/$APK_TOOLS_PKG"
tar -xzf "$APK_STATIC_DIR/$APK_TOOLS_PKG" -C "$APK_STATIC_DIR" 2>/dev/null
APK_STATIC="$APK_STATIC_DIR/sbin/apk.static"

if [ ! -x "$APK_STATIC" ]; then
    echo "Error: apk.static not found after extraction"
    exit 1
fi
echo "   Got: $APK_TOOLS_PKG"

# -- Step 2: Create raw image --------------------------------------------------

echo "-- Creating ${IMAGE_SIZE} raw image..."
truncate -s "$IMAGE_SIZE" "$BUILD_IMAGE"

# -- Step 3: Partition the image file ------------------------------------------

echo "-- Partitioning (GPT: BIOS boot + ESP + root)..."
sgdisk --zap-all "$BUILD_IMAGE"
sgdisk --new=1:2048:+1M   --typecode=1:ef02 --change-name=1:"BIOS boot" "$BUILD_IMAGE"
sgdisk --new=2:0:+64M     --typecode=2:ef00 --change-name=2:"EFI System" "$BUILD_IMAGE"
sgdisk --new=3:0:0         --typecode=3:8300 --change-name=3:"Linux root" "$BUILD_IMAGE"
sgdisk --print "$BUILD_IMAGE"

# -- Step 4: Attach as loop device with partition scanning ---------------------

echo "-- Attaching loop device..."
LOOP_DEV=$(losetup --find --show --partscan "$BUILD_IMAGE")
echo "   Loop device: $LOOP_DEV"

# Wait for partition devices to appear
echo "   Waiting for partition devices..."
retries=0
while [ ! -b "${LOOP_DEV}p3" ] && [ "$retries" -lt 10 ]; do
    partprobe "$LOOP_DEV" 2>/dev/null || true
    sleep 1
    retries=$((retries + 1))
done

# Verify partitions exist
for p in "${LOOP_DEV}p1" "${LOOP_DEV}p2" "${LOOP_DEV}p3"; do
    if [ ! -b "$p" ]; then
        echo "Error: partition device $p not found after ${retries}s"
        exit 1
    fi
done

# -- Step 5: Format partitions ------------------------------------------------

echo "-- Formatting partitions..."
mkfs.vfat -F32 -n EFI "${LOOP_DEV}p2"
mkfs.ext4 -L root -q "${LOOP_DEV}p3"

# -- Step 6: Mount filesystems ------------------------------------------------

echo "-- Mounting filesystems..."
mount "${LOOP_DEV}p3" "$MOUNTPOINT"
mkdir -p "$MOUNTPOINT/boot/efi"
mount "${LOOP_DEV}p2" "$MOUNTPOINT/boot/efi"

# -- Step 6: Bootstrap Alpine --------------------------------------------------

echo "-- Bootstrapping Alpine Linux $ALPINE_VERSION..."

# Set up repository configuration
mkdir -p "$MOUNTPOINT/etc/apk"
cat > "$MOUNTPOINT/etc/apk/repositories" <<EOF
$ALPINE_MIRROR/$ALPINE_VERSION/main
$ALPINE_MIRROR/$ALPINE_VERSION/community
EOF

# Bootstrap base system
"$APK_STATIC" \
    --root "$MOUNTPOINT" \
    --initdb \
    --allow-untrusted \
    --repositories-file "$MOUNTPOINT/etc/apk/repositories" \
    add alpine-base

# -- Step 7: Configure system in chroot ----------------------------------------

echo "-- Mounting pseudo-filesystems for chroot..."
mount --bind /dev  "$MOUNTPOINT/dev"
mount --bind /dev/pts "$MOUNTPOINT/dev/pts"
mount -t proc proc "$MOUNTPOINT/proc"
mount -t sysfs sys "$MOUNTPOINT/sys"

# Copy resolv.conf for network access in chroot
cp /etc/resolv.conf "$MOUNTPOINT/etc/resolv.conf"

echo "-- Installing packages in chroot..."
chroot "$MOUNTPOINT" /bin/sh -e <<'CHROOT_SCRIPT'

# Install all required packages
apk add --no-cache \
    linux-lts \
    linux-firmware-none \
    grub grub-bios grub-efi \
    efibootmgr \
    openssh \
    qemu-guest-agent \
    socat \
    acpid \
    doas \
    e2fsprogs \
    pciutils \
    lshw \
    blkid \
    mkinitfs

# -- fstab

cat > /etc/fstab <<'EOF'
LABEL=root  /          ext4  defaults,noatime  0 1
LABEL=EFI   /boot/efi  vfat  defaults          0 2
EOF

# -- GRUB configuration

cat > /etc/default/grub <<'EOF'
GRUB_TIMEOUT=0
GRUB_CMDLINE_LINUX_DEFAULT="quiet"
GRUB_CMDLINE_LINUX="console=tty0 console=ttyS0,115200n8"
GRUB_TERMINAL="console serial"
GRUB_SERIAL_COMMAND="serial --speed=115200 --unit=0 --word=8 --parity=no --stop=1"
GRUB_DISABLE_OS_PROBER=true
EOF

# -- Serial console getty

echo 'ttyS0::respawn:/sbin/getty -L ttyS0 115200 vt100' >> /etc/inittab

# -- Networking

cat > /etc/network/interfaces <<'EOF'
auto lo
iface lo inet loopback

auto eth0
iface eth0 inet dhcp
EOF

echo "corvus-test" > /etc/hostname
echo "127.0.0.1 corvus-test localhost" > /etc/hosts

# -- vsock kernel modules
# Mainline OpenSSH does not speak AF_VSOCK natively; sshd listens on
# TCP and a small socat relay forwards AF_VSOCK:22 → 127.0.0.1:22.
# Listing the modules in /etc/modules makes load order deterministic
# even though udev/mdev would also auto-bind them on the virtio-vsock
# PCI device.
cat >> /etc/modules <<'EOF'
vsock
vmw_vsock_virtio_transport
EOF

# -- User setup

adduser -D -s /bin/sh corvus
echo 'corvus:corvus' | chpasswd

mkdir -p /home/corvus/.ssh
chmod 700 /home/corvus/.ssh
chown corvus:corvus /home/corvus/.ssh

# doas configuration
mkdir -p /etc/doas.d
echo 'permit nopass corvus' > /etc/doas.d/corvus.conf
chmod 400 /etc/doas.d/corvus.conf

# -- SSH configuration

ssh-keygen -A

sed -i \
    -e 's/^#*PermitRootLogin.*/PermitRootLogin no/' \
    -e 's/^#*PasswordAuthentication.*/PasswordAuthentication yes/' \
    -e 's/^#*PubkeyAuthentication.*/PubkeyAuthentication yes/' \
    /etc/ssh/sshd_config

# -- QEMU guest agent

mkdir -p /etc/conf.d
cat > /etc/conf.d/qemu-guest-agent <<'EOF'
GA_PATH="/dev/virtio-ports/org.qemu.guest_agent.0"
GA_METHOD="virtio-serial"
EOF

# -- vsock → sshd relay (OpenRC service)

cat > /etc/init.d/vsock-sshd <<'EOF'
#!/sbin/openrc-run
description="Forward AF_VSOCK:22 to local sshd on 127.0.0.1:22"
command="/usr/bin/socat"
command_args="VSOCK-LISTEN:22,fork,reuseaddr TCP:127.0.0.1:22"
command_background=true
pidfile="/run/${RC_SVCNAME}.pid"

depend() {
    need sshd
    after net
}
EOF
chmod +x /etc/init.d/vsock-sshd

# -- Enable services

rc-update add devfs sysinit
rc-update add dmesg sysinit
rc-update add mdev sysinit
rc-update add hwdrivers sysinit

rc-update add hwclock boot
rc-update add modules boot
rc-update add sysctl boot
rc-update add hostname boot
rc-update add bootmisc boot
rc-update add syslog boot
rc-update add qemu-guest-agent boot

rc-update add acpid default
rc-update add networking default
rc-update add sshd default
rc-update add vsock-sshd default

rc-update add mount-ro shutdown
rc-update add killprocs shutdown
rc-update add savecache shutdown

# -- initramfs

# Need virtio for disk (virtio_blk) and network (virtio_net) in QEMU
echo 'features="ata base ext4 keymap kms scsi usb virtio"' > /etc/mkinitfs/mkinitfs.conf

KERNEL_VERSION=$(ls /lib/modules/ | head -1)
echo "Generating initramfs for kernel $KERNEL_VERSION..."
mkinitfs "$KERNEL_VERSION"

CHROOT_SCRIPT

# -- Step 8: Install GRUB bootloaders -----------------------------------------

# GRUB installs need the loop device (visible via bind-mounted /dev).
echo "-- Installing GRUB bootloaders..."

# BIOS: writes core.img into the BIOS boot partition
chroot "$MOUNTPOINT" grub-install --target=i386-pc --boot-directory=/boot "$LOOP_DEV"

# UEFI: installs EFI binary into the ESP as the removable media path
chroot "$MOUNTPOINT" grub-install --target=x86_64-efi --efi-directory=/boot/efi \
    --boot-directory=/boot --no-nvram --removable

# Write a static grub.cfg (grub-mkconfig would bake in the loop device path)
cat > "$MOUNTPOINT/boot/grub/grub.cfg" <<'GRUB_CFG'
set timeout=0

serial --speed=115200 --unit=0 --word=8 --parity=no --stop=1
terminal_input serial console
terminal_output serial console

menuentry "Alpine Linux" {
    linux /boot/vmlinuz-lts root=LABEL=root modules=ext4 quiet console=tty0 console=ttyS0,115200n8
    initrd /boot/initramfs-lts
}
GRUB_CFG

# -- Step 9: Generate SSH keypair for test access ------------------------------

echo "-- Generating test SSH keypair..."
mkdir -p "$OUTPUT_DIR"

rm -f "$OUTPUT_KEY" "$OUTPUT_KEY.pub"
ssh-keygen -t ed25519 -f "$OUTPUT_KEY" -N "" -C "corvus-test"

# Install public key into the image
cat "$OUTPUT_KEY.pub" >> "$MOUNTPOINT/home/corvus/.ssh/authorized_keys"
chmod 600 "$MOUNTPOINT/home/corvus/.ssh/authorized_keys"
chroot "$MOUNTPOINT" chown corvus:corvus /home/corvus/.ssh/authorized_keys

# -- Step 10: Unmount and convert to qcow2 ------------------------------------

echo "-- Unmounting filesystems..."
umount "$MOUNTPOINT/dev/pts"
umount "$MOUNTPOINT/dev"
umount "$MOUNTPOINT/sys"
umount "$MOUNTPOINT/proc"
umount "$MOUNTPOINT/boot/efi"
umount "$MOUNTPOINT"

echo "-- Detaching loop device..."
losetup -d "$LOOP_DEV"

echo "-- Converting raw -> qcow2..."
qemu-img convert -O qcow2 "$BUILD_IMAGE" "$OUTPUT_IMAGE"

# Fix ownership and permissions for the invoking user
REAL_USER="${DOAS_USER:-${SUDO_USER:-}}"
if [ -n "$REAL_USER" ]; then
    chown "$REAL_USER:" "$OUTPUT_IMAGE" "$OUTPUT_KEY" "$OUTPUT_KEY.pub"
fi
chmod 644 "$OUTPUT_IMAGE"
chmod 600 "$OUTPUT_KEY"
chmod 644 "$OUTPUT_KEY.pub"

# Show results
IMAGE_SIZE_MB=$(du -m "$OUTPUT_IMAGE" | cut -f1)
echo ""
echo "-- Build complete!"
echo "   Image: $OUTPUT_IMAGE (${IMAGE_SIZE_MB} MiB)"
echo "   SSH key: $OUTPUT_KEY"
echo ""
echo "   Test with:"
echo "     qemu-system-x86_64 -machine q35,accel=kvm -cpu host -m 1024 \\"
echo "       -drive file=$OUTPUT_IMAGE,if=virtio,format=qcow2 \\"
echo "       -device vhost-vsock-pci,guest-cid=42 \\"
echo "       -device virtio-net-pci,netdev=net0 \\"
echo "       -netdev user,id=net0,hostfwd=tcp::2222-:22 \\"
echo "       -nographic"
echo "     ssh -i $OUTPUT_KEY -p 2222 corvus@localhost"
echo "     # over vsock (systemd-ssh-proxy in ~/.ssh/config):"
echo "     ssh -i $OUTPUT_KEY corvus@vsock/42"
echo "     # over vsock (socat fallback):"
echo "     ssh -i $OUTPUT_KEY -o ProxyCommand=\"socat - VSOCK-CONNECT:42:22\" corvus@vsock"
