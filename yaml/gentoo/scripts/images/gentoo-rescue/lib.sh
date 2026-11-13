#!/bin/bash
# Rescue-only build functions; sourcing does not change state.

rescue_configure_portage() {
    # ROOT installations resolve against the build host's configuration.
    # This is a disposable bake VM, so remove inherited local keyword exceptions.
    rm -rf /etc/portage/package.accept_keywords /etc/portage/package.unmask
    install -d /etc/portage/package.accept_keywords /etc/portage/package.unmask
    ln -sfn ../../var/db/repos/gentoo/profiles/default/linux/amd64/23.0/no-multilib/systemd /etc/portage/make.profile
    cat >> /etc/portage/make.conf <<'CONF'
ACCEPT_KEYWORDS="amd64"
GENTOO_MIRRORS="https://distfiles.gentoo.org"
COMMON_FLAGS="-O2 -pipe -march=x86-64 -mtune=generic"
CFLAGS="${COMMON_FLAGS}"
CXXFLAGS="${COMMON_FLAGS}"
USE="dbus wifi"
CONF
    export ACCEPT_KEYWORDS=amd64
    # Generate the generic initramfs ourselves after all runtime configuration.
    cat > /etc/portage/package.use/zz-rescue <<'CONF'
sys-kernel/gentoo-kernel-bin -initramfs
sys-kernel/linux-firmware -savedconfig
sys-kernel/installkernel -dracut -grub -systemd
sys-kernel/dracut systemd
sys-fs/squashfs-tools zstd xattr
sys-fs/lvm2 lvm -systemd -udev
CONF
}

rescue_partition() {
    test -b "$TARGET_DISK"
    # Reject the helper's root disk before any destructive operation.
    test "$(lsblk -ndo TYPE "$TARGET_DISK")" = disk
    if lsblk -nr -o MOUNTPOINTS "$TARGET_DISK" | grep -q '[^[:space:]]'; then
        echo 'Target disk has mounted filesystems' >&2
        return 1
    fi
    wipefs -a "$TARGET_DISK"
    sgdisk --zap-all "$TARGET_DISK"
    sgdisk -n 1:0:+1M -t 1:ef02 -c 1:bios "$TARGET_DISK"
    sgdisk -n 2:0:+64M -t 2:ef00 -c 2:efi "$TARGET_DISK"
    sgdisk -n 3:0:0 -t 3:8300 -c 3:rescue-data "$TARGET_DISK"
    partprobe "$TARGET_DISK"
    udevadm settle --timeout=30
    test -b "${TARGET_DISK}3"
    mkfs.vfat -F32 -n RESCUE-EFI "${TARGET_DISK}2"
    mkfs.ext4 -F -m 0 -L rescue-data "${TARGET_DISK}3"
}

rescue_install_packages() {
    # Install fresh stable selections, including updates to the bootstrap set.
    # Reject changed-dependency binaries and forbid keyword/USE auto-unmasking.
    ROOT="$IMAGE_ROOT" portage_emerge --update --deep --newuse --autounmask=n \
        --binpkg-changed-deps=y @world @system \
        sys-kernel/gentoo-kernel-bin \
        sys-kernel/linux-firmware \
        sys-kernel/dracut \
        sys-fs/squashfs-tools \
        sys-fs/growpart \
        sys-fs/e2fsprogs \
        sys-fs/btrfs-progs \
        sys-fs/xfsprogs \
        sys-fs/f2fs-tools \
        sys-fs/dosfstools \
        sys-fs/ntfs3g \
        sys-fs/cryptsetup \
        sys-fs/mdadm \
        sys-fs/lvm2 \
        sys-block/thin-provisioning-tools \
        sys-apps/gptfdisk \
        sys-block/parted \
        sys-apps/util-linux \
        sys-fs/ddrescue \
        app-admin/testdisk \
        sys-apps/smartmontools \
        sys-apps/nvme-cli \
        sys-apps/pciutils \
        sys-apps/usbutils \
        sys-apps/iproute2 \
        sys-apps/ethtool \
        net-misc/iputils \
        net-analyzer/tcpdump \
        net-misc/curl \
        net-misc/wget \
        net-misc/rsync \
        net-misc/openssh \
        net-misc/networkmanager \
        net-wireless/iw \
        net-wireless/wpa_supplicant \
        app-editors/vim \
        app-arch/tar \
        app-arch/xz-utils \
        app-arch/zstd \
        app-arch/unzip
    # Preserve the stable policy for optional interactive package installation.
    cat >> "$IMAGE_ROOT/etc/portage/make.conf" <<'CONF'
ACCEPT_KEYWORDS="amd64"
COMMON_FLAGS="-O2 -pipe -march=x86-64 -mtune=generic"
CFLAGS="${COMMON_FLAGS}"
CXXFLAGS="${COMMON_FLAGS}"
USE="dbus wifi"
CONF
    cp /etc/portage/package.use/zz-rescue "$IMAGE_ROOT/etc/portage/package.use/zz-rescue"
}

rescue_configure_system_defaults() {
    install -d "$IMAGE_ROOT/root" "$IMAGE_ROOT/usr/local/sbin" "$IMAGE_ROOT/etc/systemd/system" \
        "$IMAGE_ROOT/etc/systemd/network" "$IMAGE_ROOT/etc/dracut.conf.d"
    chmod 0700 "$IMAGE_ROOT/root"
    # The initramfs establishes the squashfs lower layer and volatile overlay.
    # Storage and home are managed explicitly; never mount unrelated disks.
    : > "$IMAGE_ROOT/etc/fstab"
    : > "$IMAGE_ROOT/etc/crypttab"
    echo gentoo-rescue > "$IMAGE_ROOT/etc/hostname"
    image_configure_defaults "$IMAGE_ROOT"
    echo LANG=C.UTF-8 > "$IMAGE_ROOT/etc/locale.conf"
    install -d "$IMAGE_ROOT/usr/lib/locale"
    LC_ALL=C chroot "$IMAGE_ROOT" localedef -i C -f UTF-8 C.UTF-8
}

rescue_configure_networking() {
    cat > "$IMAGE_ROOT/etc/systemd/network/20-wired.network" <<'CONF'
[Match]
Name=en* eth*
Type=ether

[Network]
DHCP=yes
CONF
    ln -sf /run/systemd/resolve/stub-resolv.conf "$IMAGE_ROOT/etc/resolv.conf"
}

rescue_configure_home() {
    local units="$IMAGE_ROOT/etc/systemd/system"
    install -m 0755 "${CORVUS_BUILD_SCRIPTS:-/usr/local/lib/corvus-build}/images/gentoo-rescue/home.sh" \
        "$IMAGE_ROOT/usr/local/sbin/rescue-home"
    install -m 0755 "${CORVUS_BUILD_SCRIPTS:-/usr/local/lib/corvus-build}/images/gentoo-rescue/grow.sh" \
        "$IMAGE_ROOT/usr/local/sbin/rescue-grow"
    cat > "$units/rescue-grow.service" <<'CONF'
[Unit]
Description=Expand the rescue storage partition when space is available
DefaultDependencies=no
Before=rescue-home.service
After=systemd-udev-trigger.service
Conflicts=shutdown.target
Before=shutdown.target

[Service]
Type=oneshot
ExecStart=/usr/local/sbin/rescue-grow
TimeoutStartSec=120
CONF
    cat > "$units/rescue-home.service" <<'CONF'
[Unit]
Description=Persistent rescue home with empty RAM fallback
DefaultDependencies=no
Wants=rescue-grow.service
After=systemd-udev-trigger.service systemd-remount-fs.service rescue-grow.service
Before=local-fs.target systemd-user-sessions.service getty.target
Conflicts=shutdown.target
Before=shutdown.target

[Service]
Type=oneshot
RemainAfterExit=yes
ExecStart=/usr/local/sbin/rescue-home
TimeoutStartSec=45

[Install]
WantedBy=local-fs.target
CONF
}

rescue_configure_consoles() {
    local units="$IMAGE_ROOT/etc/systemd/system" name
    for name in getty@ serial-getty@; do
        install -d "$units/$name.service.d"
        cat > "$units/$name.service.d/autologin.conf" <<'CONF'
[Unit]
Requires=rescue-home.service
After=rescue-home.service

[Service]
ExecStart=
ExecStart=-/sbin/agetty --autologin root --noclear %I $TERM
CONF
    done
}

rescue_enable_runtime_services() {
    systemctl --root="$IMAGE_ROOT" enable \
        systemd-networkd.service \
        systemd-resolved.service \
        serial-getty@ttyS0.service \
        rescue-home.service
}

rescue_disable_automatic_services() {
    local name
    # Disable socket activation as well as ordinary daemon startup.
    for name in sshd.service sshd.socket sshd-vsock.socket sshd-unix-local.socket \
        lvm2-monitor.service lvm2-pvscan@.service lvm2-activation.service \
        lvm2-activation-early.service lvm2-activation-net.service \
        mdmonitor.service mdadm-last-resort@.service udisks2.service \
        systemd-gpt-auto-generator systemd-ssh-generator; do
        case "$name" in
            *-generator)
                install -d "$IMAGE_ROOT/etc/systemd/system-generators"
                ln -sf /dev/null "$IMAGE_ROOT/etc/systemd/system-generators/$name" ;;
            *) systemctl --root="$IMAGE_ROOT" mask "$name" ;;
        esac
    done
}

rescue_configure_storage_activation() {
    # Prevent udev from assembling arbitrary recovery arrays; manual mdadm works.
    install -d "$IMAGE_ROOT/etc/udev/rules.d" "$IMAGE_ROOT/etc/lvm" "$IMAGE_ROOT/etc/mdadm"
    local rule
    for rule in "$IMAGE_ROOT"/usr/lib/udev/rules.d/*md-raid* \
        "$IMAGE_ROOT"/usr/lib/udev/rules.d/*mdadm* \
        "$IMAGE_ROOT"/usr/lib/udev/rules.d/*lvm*; do
        test -e "$rule" || continue
        ln -sf /dev/null "$IMAGE_ROOT/etc/udev/rules.d/${rule##*/}"
    done
    printf 'global { event_activation = 0 }\nactivation { auto_activation_volume_list = [] }\n' \
        > "$IMAGE_ROOT/etc/lvm/lvm.conf"
    printf 'AUTO -all\n' > "$IMAGE_ROOT/etc/mdadm.conf"
}

rescue_write_motd() {
    cat > "$IMAGE_ROOT/etc/motd" <<'CONF'
Gentoo rescue: local root console, volatile system, persistent /root when usable.
Only rescue storage is mounted and expanded automatically. Other disks, swap, RAID and LVM require manual activation.
Update kernel, initramfs and squashfs offline to preserve /root. Back up /root before reflashing.
Wired DHCP is enabled. Wi-Fi: iw, wpa_supplicant. SSH: ssh-keygen -A; systemctl unmask sshd; systemctl start sshd
For SSH login, install an authorized key in /root/.ssh/authorized_keys first.
CONF
}

rescue_configure_runtime() {
    rescue_configure_system_defaults
    rescue_configure_networking
    rescue_configure_home
    rescue_configure_consoles
    rescue_enable_runtime_services
    rescue_disable_automatic_services
    rescue_configure_storage_activation
    systemctl --root="$IMAGE_ROOT" set-default multi-user.target
    rescue_write_motd
}

rescue_write_grub() {
    local uuid kernel_root_args kernel_autodetect_args kernel_console_args kernel_args
    uuid=$(blkid -s UUID -o value "${TARGET_DISK}3")
    kernel_root_args="root=rescue:UUID=$uuid ro fsck.mode=skip"
    kernel_autodetect_args="rd.fstab=0 rd.luks=0 rd.lvm=0 rd.md=0 rd.dm=0 rd.auto=0 rd.systemd.gpt_auto=0 systemd.gpt_auto=0"
    kernel_console_args="vt.color=0x02 console=tty0 console=ttyS0,115200n8"
    kernel_args="$kernel_root_args $kernel_autodetect_args $kernel_console_args"
    install -d "$RESCUE_ESP/grub"
    cat > "$RESCUE_ESP/grub/grub.cfg" <<CONF
set timeout=300
set default=0
insmod part_gpt
insmod ext2
insmod search_fs_uuid
serial --unit=0 --speed=115200
terminal_input console serial
terminal_output console serial
menuentry 'Gentoo rescue' {
    search --no-floppy --fs-uuid --set=root $uuid
    echo 'Loading kernel...'
    linux /boot/vmlinuz $kernel_args
    echo 'loading initramfs...'
    initrd /boot/initramfs.img
}
CONF
}

# Store only the finalized payload on disk; the installed system stays in RAM.
rescue_pack_system() (
    set -eu
    local kernel view
    kernel=$(cat "$IMAGE_ROOT/etc/rescue-kernel-version")
    install -d "$RESCUE_MEDIA/boot" "$RESCUE_MEDIA/rescue" "$RESCUE_MEDIA/home/root"
    chmod 0700 "$RESCUE_MEDIA/home/root"
    install -m 0644 "$IMAGE_ROOT/boot/vmlinuz-$kernel" "$RESCUE_MEDIA/boot/vmlinuz"
    install -m 0644 "$IMAGE_ROOT/boot/initramfs-$kernel.img" "$RESCUE_MEDIA/boot/initramfs.img"
    view=$(mktemp -d /run/rescue-pack.XXXXXX)
    mounts_initialize
    trap 'status=$?; set +e; mounts_unmount || status=1; rmdir "$view" || status=1; exit "$status"' EXIT
    mounts_bind "$IMAGE_ROOT" "$view"
    # Use the target's stable squashfs tools, with the source mounted into it.
    mkdir -p "$IMAGE_ROOT/run/rescue-source" "$IMAGE_ROOT/run/rescue-output"
    mounts_bind "$view" "$IMAGE_ROOT/run/rescue-source"
    mounts_bind "$RESCUE_MEDIA/rescue" "$IMAGE_ROOT/run/rescue-output"
    chroot "$IMAGE_ROOT" mksquashfs /run/rescue-source /run/rescue-output/root.squashfs \
        -noappend -comp zstd -Xcompression-level 15 -b 1M -mem 2G \
        -wildcards -e 'boot/*' 'dev/*' 'proc/*' 'sys/*' 'run/*' 'tmp/*' 'var/tmp/*'
    chroot "$IMAGE_ROOT" unsquashfs -s /run/rescue-output/root.squashfs
    (cd "$RESCUE_MEDIA" && sha256sum boot/vmlinuz boot/initramfs.img rescue/root.squashfs > rescue/SHA256SUMS)
)

rescue_install_grub() {
    grub-install --target=i386-pc --boot-directory="$RESCUE_ESP" "$TARGET_DISK"
    grub-install --target=x86_64-efi --boot-directory="$RESCUE_ESP" \
        --efi-directory="$RESCUE_ESP" --removable --no-nvram
    test -f "$RESCUE_ESP/EFI/BOOT/BOOTX64.EFI"
}
