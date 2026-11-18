"""Build removable rescue media from a RAM-backed Gentoo installation."""

import hashlib
import json
import os
import re
import shutil
import subprocess
from pathlib import Path

from ..core import BuildContext
from ..portage import InstallRequest, PortageBackend
from ._files import link, remove, write
from ._validation import validate_disk, validate_kernel
from .base import GentooStage
from .finalize import FinalizeStage
from .mounts import Mounts
from .packages import GlibcPackageInstallStage
from .repository import RepositoryStage

POLICY = (
    'ACCEPT_KEYWORDS="amd64"\n'
    'GENTOO_MIRRORS="https://distfiles.gentoo.org"\n'
    'COMMON_FLAGS="-O2 -pipe -march=x86-64 -mtune=generic"\n'
    'CFLAGS="${COMMON_FLAGS}"\n'
    'CXXFLAGS="${COMMON_FLAGS}"\n'
    'USE="dbus wifi"\n'
    'QEMU_SOFTMMU_TARGETS=""\n'
    'QEMU_USER_TARGETS=""\n'
)
USE = (
    "sys-kernel/gentoo-kernel-bin -initramfs\n"
    "sys-kernel/linux-firmware -savedconfig\n"
    "sys-kernel/installkernel -dracut -grub -systemd\n"
    "sys-kernel/dracut systemd\n"
    "sys-fs/squashfs-tools lzma zstd xattr\n"
    "app-misc/yq yq-symlink\n"
    "net-analyzer/mtr ncurses -gtk -gui\n"
    "sys-fs/lvm2 lvm -systemd -udev\n"
    "sys-boot/grub grub_platforms_pc grub_platforms_efi-64 -mount -nls -fonts -themes\n"
    "app-emulation/qemu -accessibility -gtk -sdl -sdl-image -doc -vnc -oss\n"
    "net-analyzer/nmap -zenmap\n"
    "net-fs/samba client ads acl ldap python winbind -addc -cups -regedit\n"
    "net-fs/cifs-utils ads\n"
    "net-libs/ngtcp2 gnutls\n"
    "net-dns/bind gssapi\n"
    "sys-libs/tdb python\n"
    "sys-libs/tevent python\n"
    "sys-apps/lshw -gtk\n"
    "sys-fs/multipath-tools systemd\n"
)
GCC_RUNTIME = (
    "libgcc_s.so.1",
    "libstdc++.so.6",
    "libatomic.so.1",
    "libgomp.so.1",
)
ATOMS = (
    "sys-kernel/gentoo-kernel-bin",
    "sys-kernel/linux-firmware",
    "sys-kernel/dracut",
    "sys-fs/growpart",
    "sys-fs/e2fsprogs",
    "sys-fs/btrfs-progs",
    "sys-fs/xfsprogs",
    "sys-fs/f2fs-tools",
    "sys-fs/dosfstools",
    "sys-fs/exfatprogs",
    "sys-fs/ntfs3g",
    "sys-fs/cryptsetup",
    "sys-fs/mdadm",
    "sys-fs/lvm2",
    "sys-block/thin-provisioning-tools",
    "sys-apps/gptfdisk",
    "sys-block/parted",
    "sys-fs/ddrescue",
    "app-admin/testdisk",
    "sys-apps/smartmontools",
    "sys-apps/nvme-cli",
    "sys-apps/pciutils",
    "sys-apps/usbutils",
    "sys-apps/iproute2",
    "sys-apps/ethtool",
    "net-misc/iputils",
    "net-analyzer/tcpdump",
    "net-analyzer/mtr",
    "net-misc/curl",
    "net-misc/wget",
    "net-misc/rsync",
    "net-misc/openssh",
    "net-misc/networkmanager",
    "net-wireless/iw",
    "net-wireless/wpa_supplicant",
    "app-editors/vim",
    "app-misc/mc",
    "sys-process/htop",
    "sys-process/lsof",
    "dev-debug/strace",
    "app-misc/jq",
    "app-misc/yq",
    "app-misc/tmux",
    "sys-fs/ncdu",
    "app-arch/tar",
    "app-arch/rar",
    "app-arch/xz-utils",
    "app-arch/zstd",
    "app-arch/unzip",
    "sys-boot/grub",
    "app-backup/fsarchiver",
    "app-backup/borgbackup",
    "net-misc/rclone",
    "sys-fs/xfsdump",
    "app-emulation/qemu",
    "sys-fs/multipath-tools",
    "net-misc/socat",
    "net-misc/iperf:3",
    "net-analyzer/nmap",
    "net-fs/samba",
    "net-fs/cifs-utils",
    "net-dns/bind",
    "sys-apps/dmidecode",
    "sys-fs/lsscsi",
    "sys-apps/sg3_utils",
    "sys-apps/hdparm",
    "sys-apps/lshw",
    "sys-apps/hwinfo",
    "sys-apps/ipmitool",
    "sys-apps/memtester",
    "net-dialup/picocom",
)
RUNTIME_FILES = {
    "etc/systemd/system/rescue-grow.service": (
        "[Unit]\n"
        "Description=Expand the rescue storage partition when space is available\n"
        "DefaultDependencies=no\n"
        "Before=rescue-home.service\n"
        "After=systemd-udev-trigger.service\n"
        "Conflicts=shutdown.target\n"
        "Before=shutdown.target\n"
        "\n"
        "[Service]\n"
        "Type=oneshot\n"
        "ExecStart=/usr/local/sbin/rescue-grow\n"
        "TimeoutStartSec=120\n"
    ),
    "etc/systemd/system/rescue-home.service": (
        "[Unit]\n"
        "Description=Persistent rescue home with empty RAM fallback\n"
        "DefaultDependencies=no\n"
        "Wants=rescue-grow.service\n"
        "After=systemd-udev-trigger.service systemd-remount-fs.service rescue-grow.service\n"
        "Before=local-fs.target systemd-user-sessions.service getty.target\n"
        "Conflicts=shutdown.target\n"
        "Before=shutdown.target\n"
        "\n"
        "[Service]\n"
        "Type=oneshot\n"
        "RemainAfterExit=yes\n"
        "ExecStart=/usr/local/sbin/rescue-home\n"
        "TimeoutStartSec=45\n"
        "\n"
        "[Install]\n"
        "WantedBy=local-fs.target\n"
    ),
    "etc/systemd/system/rescue-network-state.service": (
        "[Unit]\n"
        "Description=Prepare persistent network and SSH state\n"
        "Requires=rescue-home.service\n"
        "After=rescue-home.service\n"
        "Before=NetworkManager.service sshd.service\n"
        "[Service]\n"
        "Type=oneshot\n"
        "RemainAfterExit=yes\n"
        "ExecStart=/usr/local/sbin/rescue-network-state\n"
    ),
    "usr/local/sbin/rescue-network-state": (
        "#!/bin/sh\n"
        "set -eu\n"
        "install -d -m 700 /root/.config/NetworkManager/system-connections /root/.ssh/host-keys\n"
        "mountpoint -q /etc/NetworkManager/system-connections || "
        "mount --bind /root/.config/NetworkManager/system-connections /etc/NetworkManager/system-connections\n"
        "for type in ed25519 rsa ecdsa; do\n"
        "  key=/root/.ssh/host-keys/ssh_host_${type}_key\n"
        '  test -f "$key" || ssh-keygen -q -t "$type" -N \'\' -f "$key"\n'
        "done\n"
    ),
    "etc/ssh/sshd_config.d/10-rescue-hostkeys.conf": (
        "HostKey /root/.ssh/host-keys/ssh_host_ed25519_key\n"
        "HostKey /root/.ssh/host-keys/ssh_host_rsa_key\n"
        "HostKey /root/.ssh/host-keys/ssh_host_ecdsa_key\n"
        "PermitRootLogin prohibit-password\n"
    ),
    "etc/motd": (
        "Gentoo rescue: local root console, volatile system, persistent /root when usable.\n"
        "Only rescue storage is mounted and expanded automatically. Other disks, swap, RAID and LVM require manual activation.\n"
        "Update kernel, initramfs and squashfs offline to preserve /root. Back up /root before reflashing.\n"
        "Wired DHCP and SSH are enabled. Configure Wi-Fi using nmcli.\n"
        "For SSH login, install an authorized key in /root/.ssh/authorized_keys first.\n"
    ),
}

ASSETS = Path("/run/rescue-assets")


class RescueRepositoryStage(RepositoryStage):
    """Sync verified repositories and select stable generic rescue packages."""

    def execute(self, context: BuildContext) -> None:
        for directory in ("package.accept_keywords", "package.unmask"):
            remove(Path("/"), "etc/portage/" + directory)
            Path("/etc/portage", directory).mkdir(parents=True)
        link(
            Path("/etc/portage/make.profile"),
            "../../var/db/repos/gentoo/profiles/default/linux/amd64/23.0/no-multilib/systemd",
        )
        super().execute(context)
        with Path("/etc/portage/make.conf").open("a") as stream:
            stream.write(POLICY)
        write(Path("/etc/portage/package.use/zz-rescue"), USE)
        os.environ["ACCEPT_KEYWORDS"] = "amd64"


class RescueDiskStage(GentooStage):
    """Partition media separately from the tmpfs installation root."""

    requires = frozenset({"repository"})
    produces = frozenset({"disk", "media", "esp"})

    def validate(self, context: BuildContext) -> None:
        validate_disk(self.settings.disk)
        for tool in (
            "wipefs",
            "sgdisk",
            "partprobe",
            "udevadm",
            "mkfs.vfat",
            "mkfs.ext4",
            "blkid",
            "sort",
        ):
            if not shutil.which(tool):
                raise ValueError(f"Missing rescue builder tool: {tool}")
        kind = subprocess.check_output(
            ["lsblk", "-ndo", "TYPE", str(self.settings.disk)], text=True
        ).strip()
        if kind != "disk":
            raise ValueError("Rescue target must be a whole disk")

    def execute(self, context: BuildContext) -> None:
        disk = str(self.settings.disk)
        self.command_string(context, f"wipefs -a {disk}")
        self.command_string(context, f"sgdisk --zap-all {disk}")
        for layout, kind, label in (
            ("1:0:+1M", "1:ef02", "1:bios"),
            ("2:0:+64M", "2:ef00", "2:efi"),
            ("3:0:0", "3:8300", "3:rescue-data"),
        ):
            self.command_string(
                context, f"sgdisk -n {layout} -t {kind} -c {label} {disk}"
            )
        self.command_string(context, f"partprobe {disk}")
        self.command_string(context, "udevadm settle --timeout=30")
        prefix = disk + ("p" if disk[-1].isdigit() else "")
        esp_partition = prefix + "2"
        media_partition = prefix + "3"
        self.command_string(context, f"mkfs.vfat -F32 -n RESCUE-EFI {esp_partition}")
        self.command_string(
            context, f"mkfs.ext4 -F -m 0 -L rescue-data {media_partition}"
        )
        mounts = Mounts(self, context)
        mounts.mount(context.root, "-t", "tmpfs", "-o", "size=16G,mode=0755", "tmpfs")
        # Media lives beneath the staging root, but outside its packed filesystem view.
        media, esp = context.root / ".rescue-media", context.root / ".rescue-esp"
        mounts.mount(media, prefix + "3")
        mounts.mount(esp, prefix + "2")
        context.artifacts.update(disk=self.settings.disk, media=media, esp=esp)
        context.artifacts["carrier-partition"] = prefix + "3"


class RescuePackagesStage(GlibcPackageInstallStage):
    """Bootstrap glibc and install stable rescue packages and distribution kernel."""

    bootstrap_atoms = (
        "sys-apps/baselayout",
        "app-alternatives/awk",
        "app-alternatives/bzip2",
        "app-alternatives/gzip",
        "app-alternatives/sh",
        "app-alternatives/tar",
        "app-shells/bash:0",
        "sys-apps/coreutils",
        "sys-apps/findutils",
        "sys-apps/gawk",
        "sys-apps/grep",
        "sys-apps/kmod",
        "sys-apps/sed",
        "sys-apps/shadow",
        "sys-apps/systemd",
        "sys-apps/util-linux",
        "sys-libs/glibc",
        "sys-libs/pam",
    )

    atoms = (
        *bootstrap_atoms,
        *ATOMS,
        "app-arch/bzip2",
        "app-arch/gzip",
        "mail-mta/msmtp",
        "net-mail/mailbase",
        "app-admin/sudo",
        "sys-apps/diffutils",
        "sys-apps/file",
        "sys-apps/kbd",
        "sys-apps/less",
        "sys-apps/man-pages",
        "sys-apps/net-tools",
        "sys-apps/which",
        "sys-boot/efibootmgr",
        "sys-devel/binutils",
        "sys-devel/patch",
        "sys-process/procps",
        "sys-process/psmisc",
        "net-fs/nfs-utils",
        "app-shells/bash-completion",
        "virtual/man",
    )

    def execute(self, context: BuildContext) -> None:
        self.packages(context, ("sys-fs/squashfs-tools",), native=True)
        super().execute(context)
        PortageBackend(context.runner, context.root).install(
            InstallRequest(
                self.atoms,
                context.root,
                self.settings.use_binary,
                self.settings.fetch_binary,
                tmpdir=Path(str(context.artifacts["scratch"])),
                update=True,
                deep=True,
                newuse=True,
                autounmask=False,
                binpkg_changed_deps=True,
            )
        )
        with (context.root / "etc/portage/make.conf").open("a") as stream:
            stream.write(POLICY)
        write(context.root / "etc/portage/package.use/zz-rescue", USE)

    def install_system(self, context: BuildContext) -> None:
        # Gentoo packages can assume GCC's shared libraries are available
        # through @system without declaring a dependency on the compiler.
        for soname in GCC_RUNTIME:
            source = Path(
                subprocess.check_output(
                    ["gcc", f"-print-file-name={soname}"], text=True
                ).strip()
            )
            if not source.is_absolute() or not source.is_file():
                raise RuntimeError(f"Bake VM GCC runtime is missing: {soname}")
            shutil.copy2(source, context.root / "usr/lib64" / soname)
        license_dir = context.root / "usr/share/licenses/gcc-runtime"
        license_dir.mkdir(parents=True, exist_ok=True)
        repository = Path(str(context.artifacts["repository"]))
        for name in ("GPL-3", "gcc-runtime-library-exception-3.1"):
            shutil.copyfile(repository / "licenses" / name, license_dir / name)
        super().install_system(context)


class RescueRuntimeStage(GentooStage):
    """Install rescue boot hooks and disable automatic activation of other disks."""

    requires = frozenset({"system", "interfaces"})
    produces = frozenset({"configured"})

    def validate(self, context: BuildContext) -> None:
        for name in (
            "home.sh",
            "grow.sh",
            "dracut/module-setup.sh",
            "dracut/parse-rescue.sh",
            "dracut/generator.sh",
            "dracut/prepare-root.sh",
            "dracut/shutdown.sh",
        ):
            if not (ASSETS / name).is_file():
                raise ValueError(f"Missing rescue asset: {name}")

    def execute(self, context: BuildContext) -> None:
        root = context.root
        for path, content in RUNTIME_FILES.items():
            write(root / path, content)
        (root / "usr/local/sbin/rescue-network-state").chmod(0o755)
        (root / "etc/NetworkManager/system-connections").mkdir(
            parents=True, exist_ok=True
        )
        for service in ("NetworkManager", "sshd"):
            write(
                root / f"etc/systemd/system/{service}.service.d/rescue.conf",
                (
                    "[Unit]\n"
                    "Requires=rescue-network-state.service\n"
                    "After=rescue-network-state.service\n"
                ),
            )
        for path, content in (
            ("etc/fstab", ""),
            ("etc/crypttab", ""),
            ("etc/hostname", "gentoo-rescue\n"),
            ("etc/locale.conf", "LANG=C.UTF-8\n"),
            ("etc/hosts", (
                "127.0.0.1 localhost\n"
                "::1 localhost\n"
            )),
            (
                "etc/lvm/lvm.conf",
                (
                    "global { event_activation = 0 }\n"
                    "activation { auto_activation_volume_list = [] }\n"
                ),
            ),
            ("etc/mdadm.conf", "AUTO -all\n"),
        ):
            write(root / path, content)
        (root / "root").mkdir(exist_ok=True)
        (root / "root").chmod(0o700)
        link(root / "etc/localtime", "../usr/share/zoneinfo/UTC")
        link(root / "etc/resolv.conf", "/run/systemd/resolve/stub-resolv.conf")
        (root / "usr/lib/locale").mkdir(parents=True, exist_ok=True)
        self.chroot_string(context, "localedef -i C -f UTF-8 C.UTF-8")
        self.chroot_string(context, "localedef -i en_US -f UTF-8 en_US.UTF-8")
        for name in ("home", "grow"):
            destination = root / f"usr/local/sbin/rescue-{name}"
            destination.parent.mkdir(parents=True, exist_ok=True)
            shutil.copyfile(ASSETS / f"{name}.sh", destination)
            destination.chmod(0o755)
        for name in ("getty@", "serial-getty@"):
            write(
                root / f"etc/systemd/system/{name}.service.d/autologin.conf",
                (
                    "[Unit]\n"
                    "Requires=rescue-home.service\n"
                    "After=rescue-home.service\n"
                    "[Service]\n"
                    "ExecStart=\n"
                    "ExecStart=-/sbin/agetty --autologin root --noclear %I $TERM\n"
                ),
            )
        for name in (
            "sshd.socket",
            "sshd-vsock.socket",
            "sshd-unix-local.socket",
            "lvm2-monitor.service",
            "lvm2-pvscan@.service",
            "lvm2-activation.service",
            "lvm2-activation-early.service",
            "lvm2-activation-net.service",
            "mdmonitor.service",
            "mdadm-last-resort@.service",
            "udisks2.service",
            "multipathd.service",
            "multipathd.socket",
            "named.service",
            "samba.service",
            "smb.service",
            "nmb.service",
            "winbind.service",
        ):
            self.command(context, "systemctl", f"--root={root!s}", "mask", str(name))
        for name in ("systemd-gpt-auto-generator", "systemd-ssh-generator"):
            link(root / "etc/systemd/system-generators" / name, "/dev/null")
        for pattern in ("*md-raid*", "*mdadm*", "*lvm*", "56-multipath.rules"):
            for rule in (root / "usr/lib/udev/rules.d").glob(pattern):
                link(root / "etc/udev/rules.d" / rule.name, "/dev/null")
        self.command(
            context,
            "systemctl",
            f"--root={root!s}",
            "enable",
            "NetworkManager.service",
            "sshd.service",
            "systemd-resolved.service",
            "serial-getty@ttyS0.service",
            "rescue-home.service",
        )
        self.command(
            context, "systemctl", f"--root={root!s}", "set-default", "multi-user.target"
        )
        context.artifacts["configured"] = root


class RescueKernelStage(GentooStage):
    """Generate and verify hardware-generic boot and userspace shutdown archives."""

    requires = frozenset({"configured"})
    produces = frozenset({"kernel"})

    def execute(self, context: BuildContext) -> None:
        root = context.root
        versions = [p.name for p in (root / "lib/modules").iterdir() if p.is_dir()]
        if not versions:
            raise ValueError("Distribution kernel did not install modules")
        # sort -V handles Gentoo kernel release suffixes and numerical versions.
        kernel = subprocess.check_output(
            ["sort", "-V"], input="\n".join(versions), text=True
        ).splitlines()[-1]
        validate_kernel(kernel)
        config = root / f"boot/config-{kernel}"
        if not config.exists():
            config = root / f"usr/src/linux-{kernel}/.config"
        options = config.read_text().splitlines()
        for option in [
            "BLK_DEV_INITRD",
            "BLK_DEV_LOOP",
            "BLK_DEV_NBD",
            "CIFS",
            "DM_MULTIPATH",
            "CHR_DEV_SG",
            "IPMI_HANDLER",
            "IPMI_DEVICE_INTERFACE",
            "DEVTMPFS",
            "EXT4_FS",
            "OVERLAY_FS",
            "TMPFS",
            "SQUASHFS",
            "SQUASHFS_XZ",
            "EFI",
            "EFI_PARTITION",
            "USB_XHCI_HCD",
            "USB_STORAGE",
            "USB_UAS",
            "HID_GENERIC",
            "USB_HID",
            "SATA_AHCI",
            "BLK_DEV_NVME",
            "SCSI",
            "BLK_DEV_SD",
            "VIRTIO_BLK",
            "VIRTIO_NET",
            "E1000E",
            "IGB",
            "R8169",
            "IWLWIFI",
            "ATH9K",
        ]:
            if not any(f"CONFIG_{option}={mode}" in options for mode in ("y", "m")):
                raise ValueError(f"Distribution kernel lacks CONFIG_{option}")
        image = root / f"boot/vmlinuz-{kernel}"
        if not image.exists():
            shutil.copyfile(
                root / f"usr/src/linux-{kernel}/arch/x86/boot/bzImage", image
            )
        self.chroot_string(context, f"depmod -a {kernel}")
        shutil.copytree(
            ASSETS / "dracut",
            root / "usr/lib/dracut/modules.d/91corvus-rescue",
            dirs_exist_ok=True,
        )
        for asset in (root / "usr/lib/dracut/modules.d/91corvus-rescue").iterdir():
            asset.chmod(0o755)
        write(
            root / "etc/dracut.conf.d/90-rescue.conf",
            (
                'hostonly="no"\n'
                'hostonly_cmdline="no"\n'
                'use_fstab="no"\n'
                'add_dracutmodules+=" systemd systemd-initrd kernel-modules corvus-rescue shutdown "\n'
                'omit_dracutmodules+=" lvm mdraid dmraid multipath crypt resume network network-manager systemd-networkd "\n'
                'filesystems+=" ext4 squashfs overlay "\n'
                'add_drivers+=" xhci_pci usb_storage uas ahci nvme sd_mod virtio_pci virtio_blk virtio_scsi usbhid hid_generic "\n'
            ),
        )
        archive = f"/boot/initramfs-{kernel}.img"
        self.chroot(
            context,
            "dracut",
            "--force",
            "--no-hostonly",
            "--no-hostonly-cmdline",
            "--kver",
            str(kernel),
            str(archive),
        )
        listing = subprocess.check_output(
            ["chroot", str(root), "lsinitrd", archive], text=True
        )
        for name in ("rescue-root", "rescue-generator", "rescue-shutdown"):
            if name not in listing:
                raise ValueError(f"Initramfs lacks {name}")
        for module in [
            "xhci_pci",
            "usb_storage",
            "uas",
            "ahci",
            "nvme",
            "sd_mod",
            "virtio_pci",
            "virtio_blk",
            "virtio_scsi",
            "usbhid",
            "hid_generic",
            "ext4",
            "squashfs",
            "loop",
            "overlay",
        ]:
            filename = subprocess.check_output(
                [
                    "chroot",
                    str(root),
                    "modinfo",
                    "-k",
                    kernel,
                    "-F",
                    "filename",
                    module,
                ],
                text=True,
            ).strip()
            if filename != "(builtin)" and Path(filename).name not in listing:
                raise ValueError(f"Initramfs lacks driver {module}")
        shutdown = f"/lib/modules/{kernel}/initrd"
        self.chroot(
            context,
            "dracut",
            "--force",
            "--conf",
            "/dev/null",
            "--confdir",
            "/var/empty",
            "--no-hostonly",
            "--no-hostonly-cmdline",
            "--no-kernel",
            "--no-early-microcode",
            "--gzip",
            "--omit",
            "squash squash-erofs squash-lib",
            "--modules",
            "shutdown corvus-rescue",
            "--kver",
            str(kernel),
            str(shutdown),
        )
        if not subprocess.check_output(
            ["chroot", str(root), "lsinitrd", "-f", "shutdown", shutdown]
        ):
            raise ValueError("Missing shutdown archive entry point")
        write(root / "etc/rescue-kernel-version", kernel + "\n")
        context.artifacts["kernel"] = kernel


class RescueCleanupStage(FinalizeStage):
    """Clean the staging system without prematurely publishing a media artifact."""

    requires = frozenset({"kernel"})
    produces = frozenset({"clean-root"})

    def clean(self, context: BuildContext) -> None:
        super().clean(context)
        root = context.root
        kernel = str(context.artifacts["kernel"])
        validate_kernel(kernel)
        for relative in (
            "usr/src",
            "usr/include",
            "usr/libexec/gcc",
            "var/db/pkg",
            f"usr/lib/modules/{kernel}/build",
            f"usr/lib/modules/{kernel}/source",
        ):
            remove(root, relative)
        # Gentoo stores versioned GCC drivers outside /usr/bin and exposes
        # them through gcc-config links. The GCC library tree also contains
        # shared runtimes, so only remove its private header directories.
        for path in root.glob("usr/*-linux-gnu/gcc-bin"):
            remove(root, str(path.relative_to(root)))
        for version in root.glob("usr/lib/gcc/*/*"):
            for suffix in (
                "include",
                "include-fixed",
                "finclude",
                "install-tools",
                "plugin/include",
            ):
                path = version / suffix
                remove(root, str(path.relative_to(root)))
        compiler = re.compile(
            r"(?:[\w.]+(?:-[\w.]+)*-linux-gnu-)?"
            r"(?:cc|c89|c99|c\+\+|cpp|g\+\+|gfortran|gcov|gcc(?:-(?:ar|nm|ranlib))?)"
            r"(?:-\d[\d.]*)?"
        )
        for path in (root / "usr/bin").iterdir():
            if compiler.fullmatch(path.name):
                remove(root, str(path.relative_to(root)))

    def execute(self, context: BuildContext) -> None:
        self.reset_identity(context)
        mounts = context.artifacts.get("chroot-mounts")
        if isinstance(mounts, Mounts):
            mounts.close()
        self.clean(context)
        write(
            context.root / "etc/corvus-image-info",
            json.dumps(self.metadata(context)) + "\n",
        )
        context.artifacts["clean-root"] = context.root


class RescuePackStage(GentooStage):
    """Compress the cleaned RAM root and checksum the three boot payloads."""

    requires = frozenset({"clean-root", "media"})
    produces = frozenset({"payload"})

    def execute(self, context: BuildContext) -> None:
        root = context.root
        media = Path(str(context.artifacts["media"]))
        kernel = str(context.artifacts["kernel"])
        media.chmod(0o700)
        shutil.copyfile(root / f"boot/vmlinuz-{kernel}", media / "vmlinuz")
        shutil.copyfile(root / f"boot/initramfs-{kernel}.img", media / "initramfs.img")
        mounts = Mounts(self, context)
        view = root / "run/rescue-source"
        squashfs = media / "root.squashfs"
        try:
            mounts.bind(root, view)
            self.command(
                context,
                "mksquashfs",
                str(view),
                str(squashfs),
                "-noappend",
                "-comp",
                "xz",
                "-b",
                "1M",
                "-mem",
                "2G",
                "-wildcards",
                "-e",
                "boot/*",
                "dev/*",
                "proc/*",
                "sys/*",
                "run/*",
                "tmp/*",
                "var/tmp/*",
                ".rescue-media/*",
                ".rescue-esp/*",
            )
            self.command(context, "unsquashfs", "-s", str(squashfs))
        finally:
            mounts.close()
        names = ("vmlinuz", "initramfs.img", "root.squashfs")
        checksums = []
        for name in names:
            with (media / name).open("rb") as stream:
                digest = hashlib.sha256()
                for chunk in iter(lambda: stream.read(1024 * 1024), b""):
                    digest.update(chunk)
                checksums.append(f"{digest.hexdigest()}  {name}\n")
        write(media / "SHA256SUMS", "".join(checksums))
        self.command(context, "df", "-h", str(media))
        context.artifacts["payload"] = media


class RescueBootloaderStage(GentooStage):
    """Install BIOS and removable UEFI GRUB and publish verified rescue media."""

    requires = frozenset({"payload", "esp"})
    produces = frozenset({"image"})

    def validate(self, context: BuildContext) -> None:
        validate_disk(self.settings.disk)

    def execute(self, context: BuildContext) -> None:
        esp = Path(str(context.artifacts["esp"]))
        self.command(
            context,
            "grub-install",
            "--target=i386-pc",
            f"--boot-directory={esp!s}",
            str(self.settings.disk),
        )
        self.command(
            context,
            "grub-install",
            "--target=x86_64-efi",
            f"--boot-directory={esp!s}",
            f"--efi-directory={esp!s}",
            "--removable",
            "--no-nvram",
        )
        if not (esp / "EFI/BOOT/BOOTX64.EFI").is_file():
            raise ValueError("Missing removable UEFI bootloader")
        uuid = subprocess.check_output(
            [
                "blkid",
                "-s",
                "UUID",
                "-o",
                "value",
                str(context.artifacts["carrier-partition"]),
            ],
            text=True,
        ).strip()
        if not re.fullmatch(r"[0-9a-fA-F-]+", uuid):
            raise ValueError("Invalid carrier UUID")
        boot = (
            f"        linux /vmlinuz root=rescue:UUID={uuid} ro fsck.mode=skip rd.fstab=0 rd.luks=0 rd.lvm=0 rd.md=0 rd.dm=0 rd.auto=0 rd.systemd.gpt_auto=0 systemd.gpt_auto=0 vt.color=0x02 console=tty0 console=ttyS0,115200n8\n"
            "        initrd /initramfs.img\n"
        )
        config = esp / "grub/grub.cfg"
        write(
            config,
            (
                f"set timeout=300\n"
                f"set default=0\n"
                f"insmod part_gpt\n"
                f"insmod ext2\n"
                f"insmod search_fs_uuid\n"
                f"serial --unit=0 --speed=115200\n"
                f"terminal_input console serial\n"
                f"terminal_output console serial\n"
                "menuentry 'Gentoo rescue' {\n"
                f"    if search --no-floppy --fs-uuid --set=root {uuid}; then\n"
                f"{boot}"
                "    fi\n"
                "}\n"
                "menuentry 'Gentoo rescue (verify boot files)' {\n"
                "    echo 'Checking boot files against SHA256SUMS...'\n"
                "    set verified=0\n"
                "    insmod hashsum\n"
                "    insmod gcry_sha256\n"
                f"    if search --no-floppy --fs-uuid --set=root {uuid}; then\n"
                "        if test -s /SHA256SUMS; then\n"
                '            if hashsum --hash sha256 --check /SHA256SUMS --keep-going --prefix "($root)"; then\n'
                "                set verified=1\n"
                "            fi\n"
                "        fi\n"
                "    fi\n"
                '    if test "$verified" = 1; then\n'
                f"{boot}"
                "    else\n"
                "        echo 'Boot file verification failed. Boot canceled.'\n"
                "        sleep --interruptible 10\n"
                "    fi\n"
                "}\n"
            ),
        )
        self.command(context, "grub-script-check", str(config))
        self.command_string(context, "sync")
        context.artifacts["image"] = self.settings.disk
