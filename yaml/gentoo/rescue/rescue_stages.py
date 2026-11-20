"""Build removable rescue media from a RAM-backed Gentoo installation."""

import hashlib
import json
import os
import re
import shutil
import subprocess
from pathlib import Path

from corvus_image_builder.core import BuildContext
from corvus_image_builder.gentoo._files import link, remove, write
from corvus_image_builder.gentoo._validation import validate_disk, validate_kernel
from corvus_image_builder.gentoo.base import GentooStage
from corvus_image_builder.gentoo.finalize import FinalizeStage
from corvus_image_builder.gentoo.mounts import Mounts
from corvus_image_builder.gentoo.packages import GlibcPackageInstallStage
from corvus_image_builder.gentoo.repository import RepositoryStage
from corvus_image_builder.portage import InstallRequest, PortageBackend

GCC_RUNTIME = (
    "libgcc_s.so.1",
    "libstdc++.so.6",
    "libatomic.so.1",
    "libgomp.so.1",
)
ATOMS = (
    "media-fonts/terminus-font",
    "app-misc/cmatrix",
    "app-misc/neofetch",
    "app-text/tree",
    "app-admin/lnav",
    "sys-apps/ripgrep",
    "app-shells/fzf",
    "sys-libs/gpm",
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
ASSETS = Path("/run/rescue-assets")

RUNTIME_PATHS = (
    "etc/vconsole.conf",
    "etc/tmux.conf",
    "etc/systemd/system/rescue-grow.service",
    "etc/systemd/system/rescue-home.service",
    "etc/systemd/system/rescue-network-state.service",
    "usr/local/sbin/rescue-network-state",
    "etc/ssh/sshd_config.d/10-rescue-hostkeys.conf",
    "etc/motd",
    "etc/fstab",
    "etc/crypttab",
    "etc/hostname",
    "etc/locale.conf",
    "etc/hosts",
    "etc/lvm/lvm.conf",
    "etc/mdadm.conf",
    "usr/local/sbin/rescue-home",
    "usr/local/sbin/rescue-grow",
)
KERNEL_PATHS = (
    "etc/dracut.conf.d/90-rescue.conf",
    "usr/lib/dracut/modules.d/91corvus-rescue/generator.sh",
    "usr/lib/dracut/modules.d/91corvus-rescue/module-setup.sh",
    "usr/lib/dracut/modules.d/91corvus-rescue/parse-rescue.sh",
    "usr/lib/dracut/modules.d/91corvus-rescue/prepare-root.sh",
    "usr/lib/dracut/modules.d/91corvus-rescue/shutdown.sh",
)


def install_asset(source: str, destination: Path, mode: int = 0o644) -> None:
    destination.parent.mkdir(parents=True, exist_ok=True)
    shutil.copyfile(ASSETS / source, destination)
    destination.chmod(mode)


def install_portage_policy(root: Path) -> None:
    with (root / "etc/portage/make.conf").open("ab") as stream:
        stream.write((ASSETS / "portage/make.conf.fragment").read_bytes())
    install_asset("portage/package.use", root / "etc/portage/package.use/zz-rescue")


class RescueRepositoryStage(RepositoryStage):
    """Sync verified repositories and select stable generic rescue packages."""

    def validate(self, context: BuildContext) -> None:
        super().validate(context)
        for name in (
            *(f"root/{path}" for path in (*RUNTIME_PATHS, *KERNEL_PATHS)),
            "portage/make.conf.fragment",
            "portage/package.use",
            "systemd/network.conf",
            "systemd/autologin.conf",
            "grub/grub.cfg.in",
        ):
            if not (ASSETS / name).is_file():
                raise ValueError(f"Missing rescue asset: {name}")

    def execute(self, context: BuildContext) -> None:
        for directory in ("package.accept_keywords", "package.unmask"):
            remove(Path("/"), "etc/portage/" + directory)
            Path("/etc/portage", directory).mkdir(parents=True)
        link(
            Path("/etc/portage/make.profile"),
            "../../var/db/repos/gentoo/profiles/default/linux/amd64/23.0/no-multilib/systemd",
        )
        super().execute(context)
        install_portage_policy(Path("/"))
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
        install_portage_policy(context.root)

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

    def execute(self, context: BuildContext) -> None:
        root = context.root
        if not (root / "usr/share/consolefonts/ter-v16n.psf.gz").is_file():
            raise RuntimeError("Terminus console font ter-v16n is missing")
        for path in RUNTIME_PATHS:
            mode = 0o755 if path.startswith("usr/local/sbin/") else 0o644
            install_asset(f"root/{path}", root / path, mode)
        (root / "etc/NetworkManager/system-connections").mkdir(
            parents=True, exist_ok=True
        )
        for service in ("NetworkManager", "sshd"):
            install_asset(
                "systemd/network.conf",
                root / f"etc/systemd/system/{service}.service.d/rescue.conf",
            )
        (root / "root").mkdir(exist_ok=True)
        (root / "root").chmod(0o700)
        link(root / "etc/localtime", "../usr/share/zoneinfo/UTC")
        link(root / "etc/resolv.conf", "/run/systemd/resolve/stub-resolv.conf")
        (root / "usr/lib/locale").mkdir(parents=True, exist_ok=True)
        self.chroot_string(context, "localedef -i C -f UTF-8 C.UTF-8")
        self.chroot_string(context, "localedef -i en_US -f UTF-8 en_US.UTF-8")
        for name in ("getty@", "serial-getty@"):
            install_asset(
                "systemd/autologin.conf",
                root / f"etc/systemd/system/{name}.service.d/autologin.conf",
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
            "gpm.service",
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
            "INPUT_MOUSEDEV",
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
        for path in KERNEL_PATHS:
            mode = 0o755 if path.endswith(".sh") else 0o644
            install_asset(f"root/{path}", root / path, mode)
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
        config = esp / "grub/grub.cfg"
        template = (ASSETS / "grub/grub.cfg.in").read_text(encoding="utf-8")
        if "@CARRIER_UUID@" not in template:
            raise ValueError("Missing carrier UUID placeholder in GRUB template")
        write(config, template.replace("@CARRIER_UUID@", uuid))
        self.command(context, "grub-script-check", str(config))
        self.command_string(context, "sync")
        context.artifacts["image"] = self.settings.disk
