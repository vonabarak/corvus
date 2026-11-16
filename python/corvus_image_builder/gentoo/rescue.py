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

POLICY = 'ACCEPT_KEYWORDS="amd64"\nGENTOO_MIRRORS="https://distfiles.gentoo.org"\nCOMMON_FLAGS="-O2 -pipe -march=x86-64 -mtune=generic"\nCFLAGS="${COMMON_FLAGS}"\nCXXFLAGS="${COMMON_FLAGS}"\nUSE="dbus wifi"\n'
USE = "sys-kernel/gentoo-kernel-bin -initramfs\nsys-kernel/linux-firmware -savedconfig\nsys-kernel/installkernel -dracut -grub -systemd\nsys-kernel/dracut systemd\nsys-fs/squashfs-tools zstd xattr\nsys-fs/lvm2 lvm -systemd -udev\n"
ATOMS = (
    "sys-kernel/gentoo-kernel-bin",
    "sys-kernel/linux-firmware",
    "sys-kernel/dracut",
    "sys-fs/squashfs-tools",
    "sys-fs/growpart",
    "sys-fs/e2fsprogs",
    "sys-fs/btrfs-progs",
    "sys-fs/xfsprogs",
    "sys-fs/f2fs-tools",
    "sys-fs/dosfstools",
    "sys-fs/ntfs3g",
    "sys-fs/cryptsetup",
    "sys-fs/mdadm",
    "sys-fs/lvm2",
    "sys-block/thin-provisioning-tools",
    "sys-apps/gptfdisk",
    "sys-block/parted",
    "sys-apps/util-linux",
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
    "net-misc/curl",
    "net-misc/wget",
    "net-misc/rsync",
    "net-misc/openssh",
    "net-misc/networkmanager",
    "net-wireless/iw",
    "net-wireless/wpa_supplicant",
    "app-editors/vim",
    "app-arch/tar",
    "app-arch/xz-utils",
    "app-arch/zstd",
    "app-arch/unzip",
)
RUNTIME_FILES = {
    "etc/systemd/system/rescue-grow.service": "[Unit]\nDescription=Expand the rescue storage partition when space is available\nDefaultDependencies=no\nBefore=rescue-home.service\nAfter=systemd-udev-trigger.service\nConflicts=shutdown.target\nBefore=shutdown.target\n\n[Service]\nType=oneshot\nExecStart=/usr/local/sbin/rescue-grow\nTimeoutStartSec=120\n",
    "etc/systemd/system/rescue-home.service": "[Unit]\nDescription=Persistent rescue home with empty RAM fallback\nDefaultDependencies=no\nWants=rescue-grow.service\nAfter=systemd-udev-trigger.service systemd-remount-fs.service rescue-grow.service\nBefore=local-fs.target systemd-user-sessions.service getty.target\nConflicts=shutdown.target\nBefore=shutdown.target\n\n[Service]\nType=oneshot\nRemainAfterExit=yes\nExecStart=/usr/local/sbin/rescue-home\nTimeoutStartSec=45\n\n[Install]\nWantedBy=local-fs.target\n",
    "etc/systemd/network/20-wired.network": "[Match]\nName=en* eth*\nType=ether\n\n[Network]\nDHCP=yes\n",
    "etc/motd": "Gentoo rescue: local root console, volatile system, persistent /root when usable.\nOnly rescue storage is mounted and expanded automatically. Other disks, swap, RAID and LVM require manual activation.\nUpdate kernel, initramfs and squashfs offline to preserve /root. Back up /root before reflashing.\nWired DHCP is enabled. Wi-Fi: iw, wpa_supplicant. SSH: ssh-keygen -A; systemctl unmask sshd; systemctl start sshd\nFor SSH login, install an authorized key in /root/.ssh/authorized_keys first.\n",
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

    atoms = (
        *ATOMS,
        "app-emulation/qemu-guest-agent",
        "mail-mta/msmtp",
        "app-admin/sudo",
        "sys-boot/efibootmgr",
        "net-fs/nfs-utils",
        "app-shells/bash-completion",
    )

    def execute(self, context: BuildContext) -> None:
        super().execute(context)
        PortageBackend(context.runner, context.root).install(
            InstallRequest(
                ("@world", "@system", *ATOMS),
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
        for path, content in (
            ("etc/fstab", ""),
            ("etc/crypttab", ""),
            ("etc/hostname", "gentoo-rescue\n"),
            ("etc/locale.conf", "LANG=C.UTF-8\n"),
            ("etc/hosts", "127.0.0.1 localhost\n::1 localhost\n"),
            (
                "etc/lvm/lvm.conf",
                "global { event_activation = 0 }\nactivation { auto_activation_volume_list = [] }\n",
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
        for name in ("home", "grow"):
            destination = root / f"usr/local/sbin/rescue-{name}"
            destination.parent.mkdir(parents=True, exist_ok=True)
            shutil.copyfile(ASSETS / f"{name}.sh", destination)
            destination.chmod(0o755)
        for name in ("getty@", "serial-getty@"):
            write(
                root / f"etc/systemd/system/{name}.service.d/autologin.conf",
                "[Unit]\nRequires=rescue-home.service\nAfter=rescue-home.service\n[Service]\nExecStart=\nExecStart=-/sbin/agetty --autologin root --noclear %I $TERM\n",
            )
        for name in (
            "sshd.service",
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
        ):
            self.command(context, "systemctl", f"--root={root!s}", "mask", str(name))
        for name in ("systemd-gpt-auto-generator", "systemd-ssh-generator"):
            link(root / "etc/systemd/system-generators" / name, "/dev/null")
        for pattern in ("*md-raid*", "*mdadm*", "*lvm*"):
            for rule in (root / "usr/lib/udev/rules.d").glob(pattern):
                link(root / "etc/udev/rules.d" / rule.name, "/dev/null")
        self.command(
            context,
            "systemctl",
            f"--root={root!s}",
            "enable",
            "systemd-networkd.service",
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
            "DEVTMPFS",
            "EXT4_FS",
            "OVERLAY_FS",
            "TMPFS",
            "SQUASHFS",
            "SQUASHFS_ZSTD",
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
            """hostonly="no"
hostonly_cmdline="no"
use_fstab="no"
add_dracutmodules+=" systemd systemd-initrd kernel-modules corvus-rescue shutdown "
omit_dracutmodules+=" lvm mdraid dmraid crypt resume network network-manager systemd-networkd "
filesystems+=" ext4 squashfs overlay "
add_drivers+=" xhci_pci usb_storage uas ahci nvme sd_mod virtio_pci virtio_blk virtio_scsi usbhid hid_generic "
""",
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
        for directory in ("boot", "rescue", "home/root"):
            (media / directory).mkdir(parents=True, exist_ok=True)
        (media / "home/root").chmod(0o700)
        shutil.copyfile(root / f"boot/vmlinuz-{kernel}", media / "boot/vmlinuz")
        shutil.copyfile(
            root / f"boot/initramfs-{kernel}.img", media / "boot/initramfs.img"
        )
        mounts = Mounts(self, context)
        view = root / "run/rescue-source"
        output = root / "run/rescue-output"
        try:
            mounts.bind(root, view)
            mounts.bind(media / "rescue", output)
            self.chroot(
                context,
                "mksquashfs",
                "/run/rescue-source",
                "/run/rescue-output/root.squashfs",
                "-noappend",
                "-comp",
                "zstd",
                "-Xcompression-level",
                "15",
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
            self.chroot_string(
                context, "unsquashfs -s /run/rescue-output/root.squashfs"
            )
        finally:
            mounts.close()
        names = ("boot/vmlinuz", "boot/initramfs.img", "rescue/root.squashfs")
        checksums = []
        for name in names:
            with (media / name).open("rb") as stream:
                digest = hashlib.sha256()
                for chunk in iter(lambda: stream.read(1024 * 1024), b""):
                    digest.update(chunk)
                checksums.append(f"{digest.hexdigest()}  {name}\n")
        write(media / "rescue/SHA256SUMS", "".join(checksums))
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
        write(
            esp / "grub/grub.cfg",
            f"""set timeout=300
set default=0
insmod part_gpt
insmod ext2
insmod search_fs_uuid
serial --unit=0 --speed=115200
terminal_input console serial
terminal_output console serial
menuentry 'Gentoo rescue' {{
    search --no-floppy --fs-uuid --set=root {uuid}
    linux /boot/vmlinuz root=rescue:UUID={uuid} ro fsck.mode=skip rd.fstab=0 rd.luks=0 rd.lvm=0 rd.md=0 rd.dm=0 rd.auto=0 rd.systemd.gpt_auto=0 systemd.gpt_auto=0 vt.color=0x02 console=tty0 console=ttyS0,115200n8
    initrd /boot/initramfs.img
}}
""",
        )
        self.command_string(context, "sync")
        context.artifacts["image"] = self.settings.disk
