"""Native amd64/systemd image recipe executed inside a disposable Gentoo VM."""

import fcntl
import functools
import hashlib
import importlib
import json
import os
import shutil
import stat
import subprocess
import tempfile
import time
from dataclasses import dataclass
from pathlib import Path
from typing import Any

from .core import BuildContext, Stage
from .portage import InstallRequest, PortageBackend


def write(path: Path, value: str) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(value)


def link(path: Path, target: str) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.unlink(missing_ok=True)
    path.symlink_to(target)


@dataclass(frozen=True)
class Settings:
    disk: Path
    cache: Path
    kernel_config: Path
    use_binary: bool = True
    fetch_binary: bool = True
    name: str = "gentoo-image"


class GentooStage(Stage):
    def __init__(self, settings: Settings):
        self.settings = settings

    def command(self, context: BuildContext, *argv: str, **variables: str) -> None:
        environment = dict(os.environ)
        for key in ("ROOT", "SYSROOT", "PORTAGE_CONFIGROOT"):
            environment.pop(key, None)
        environment.update(variables)
        context.runner.run(argv, env=environment)

    def packages(
        self, context: BuildContext, atoms: tuple[str, ...], native: bool = False
    ) -> None:
        root = Path("/") if native else context.root
        PortageBackend(context.runner, root).install(
            InstallRequest(
                atoms, root, self.settings.use_binary, self.settings.fetch_binary
            )
        )


class WorkspaceStage(GentooStage):
    produces = frozenset({"workspace"})

    def validate(self, context: BuildContext) -> None:
        if os.geteuid() != 0:
            raise ValueError("Run inside a disposable Gentoo builder VM as root")
        importlib.import_module("portage")
        profile = Path("/etc/portage/make.profile").resolve()
        if "no-multilib/systemd" not in str(profile) or os.uname().machine != "x86_64":
            raise ValueError(
                "Builder must use the native amd64 no-multilib/systemd profile"
            )
        if context.root == Path("/") or context.root.is_symlink():
            raise ValueError("Target root must be a separate directory")
        if context.root.exists() and any(context.root.iterdir()):
            raise ValueError("Target mount directory must be empty")
        for tool in (
            "sgdisk",
            "wipefs",
            "partprobe",
            "mkfs.vfat",
            "mkfs.ext4",
            "mount",
            "umount",
            "grub-install",
            "make",
            "gcc",
            "gemato",
            "gpg",
            "gpgconf",
            "getuto",
            "emerge",
            "emerge-webrsync",
            "portageq",
        ):
            if not shutil.which(tool):
                raise ValueError(f"Missing prerequisite: {tool}")
        if not self.settings.kernel_config.is_file():
            raise ValueError("Kernel configuration is missing")
        disk = self.settings.disk.resolve()
        if not stat.S_ISBLK(disk.stat().st_mode):
            raise ValueError("Target disk must be a block device")
        # Reject every disk with a mounted filesystem, including the builder root.
        tree = json.loads(
            subprocess.check_output(
                ["lsblk", "--json", "--output", "PATH,MOUNTPOINTS", str(disk)],
                text=True,
            )
        )

        def mounted(device: dict[str, Any]) -> bool:
            return any(device.get("mountpoints") or []) or any(
                mounted(child) for child in device.get("children", [])
            )

        if any(mounted(device) for device in tree["blockdevices"]):
            raise ValueError("Target disk or a descendant is mounted")

    def execute(self, context: BuildContext) -> None:
        context.root.mkdir(parents=True, exist_ok=True)
        if not self.settings.cache.is_mount():
            self.settings.cache.mkdir(parents=True, exist_ok=True)
            self.command(
                context,
                "mount",
                "-t",
                "virtiofs",
                "corvus-cache",
                str(self.settings.cache),
            )
            context.own(
                lambda: self.command(context, "umount", str(self.settings.cache)),
                preserve_on_failure=True,
            )
        for relative in ("distfiles", "packages/x86-64", "binhost/x86-64", "kernels"):
            (self.settings.cache / relative).mkdir(parents=True, exist_ok=True)
        lock = (self.settings.cache / "python-build.lock").open("a")
        fcntl.flock(lock, fcntl.LOCK_EX)
        context.own(lock.close)
        context.artifacts["workspace"] = self.settings.cache


class RepositoryStage(GentooStage):
    requires = frozenset({"workspace"})
    produces = frozenset({"repository"})

    def execute(self, context: BuildContext) -> None:
        if not Path("/usr/share/openpgp-keys/gentoo-release.asc").is_file():
            raise ValueError("Builder lacks the Gentoo release verification key")
        config = Path("/etc/portage/make.conf")
        lines = [
            line
            for line in config.read_text().splitlines()
            if not line.lstrip().startswith(("FEATURES=", "DISTDIR=", "PKGDIR="))
        ]
        config.write_text(
            "\n".join(lines)
            + f'''\nDISTDIR="{self.settings.cache}/distfiles"
PKGDIR="{self.settings.cache}/packages/x86-64"
FEATURES="-userfetch -userpriv -getbinpkg -binpkg-request-signature -binpkg-ignore-signature"
GPG_VERIFY_USER_DROP=""
GPG_VERIFY_GROUP_DROP=""
'''
        )
        write(
            Path("/etc/portage/binrepos.conf/zz-corvus-python.conf"),
            f"""[binhost]
priority = 9999
location = {self.settings.cache}/binhost/x86-64
sync-uri = https://distfiles.gentoo.org/releases/amd64/binpackages/23.0/x86-64
verify-signature = true
""",
        )
        write(
            Path("/etc/portage/repos.conf/zz-corvus-python.conf"),
            f"""[gentoo]
sync-webrsync-verify-signature = true
sync-rsync-verify-metamanifest = true
sync-openpgp-key-package = sec-keys/openpgp-keys-gentoo-release
sync-openpgp-key-path = /usr/share/openpgp-keys/gentoo-release.asc
sync-openpgp-key-refresh = keyserver
sync-openpgp-keyserver = {os.environ.get("PORTAGE_KEYSERVER", "hkps://keys.gentoo.org")}
""",
        )
        helper = Path("/etc/portage/corvus-python-getuto")
        write(helper, "#!/bin/sh\nexec env -u ROOT -u SYSROOT getuto\n")
        helper.chmod(0o755)
        with config.open("a") as stream:
            stream.write('PORTAGE_TRUST_HELPER="/etc/portage/corvus-python-getuto"\n')
        self.command(context, "getuto")
        self.command(context, "emerge-webrsync", "-k")
        self.command(context, "emerge", "--sync")
        context.artifacts["repository"] = Path("/var/db/repos/gentoo")


class DiskStage(GentooStage):
    requires = frozenset({"repository"})
    produces = frozenset({"disk"})

    def execute(self, context: BuildContext) -> None:
        disk = str(self.settings.disk)
        self.command(context, "wipefs", "-a", disk)
        self.command(context, "sgdisk", "--zap-all", disk)
        for spec in (
            ("1:0:+1M", "1:ef02"),
            ("2:0:+512M", "2:ef00"),
            ("3:0:0", "3:8300"),
        ):
            self.command(context, "sgdisk", "-n", spec[0], "-t", spec[1], disk)
        self.command(context, "partprobe", disk)
        prefix = disk + ("p" if disk[-1].isdigit() else "")
        for _ in range(60):
            if all(Path(prefix + str(i)).exists() for i in (1, 2, 3)):
                break
            time.sleep(1)
        else:
            raise RuntimeError("Partition devices did not appear")
        self.command(context, "mkfs.vfat", "-F32", "-n", "CORVUS-EFI", prefix + "2")
        self.command(context, "mkfs.ext4", "-F", "-L", "corvus-root", prefix + "3")
        for device, destination in (
            (prefix + "3", context.root),
            (prefix + "2", context.root / "boot/efi"),
        ):
            destination.mkdir(parents=True, exist_ok=True)
            self.command(context, "mount", device, str(destination))
            context.own(
                functools.partial(self.command, context, "umount", str(destination)),
                preserve_on_failure=True,
            )
        context.artifacts["disk"] = self.settings.disk


class PackageInstallStage(GentooStage):
    requires = frozenset({"disk"})
    produces = frozenset({"system"})
    atoms: tuple[str, ...] = ()

    def execute(self, context: BuildContext) -> None:
        root = context.root
        for directory in (
            "usr/bin",
            "usr/lib",
            "usr/lib64",
            "etc",
            "var/tmp",
            "tmp",
            "proc",
            "sys",
            "dev",
            "run",
        ):
            (root / directory).mkdir(parents=True, exist_ok=True)
        for relative, target in (
            ("usr/sbin", "bin"),
            ("bin", "usr/bin"),
            ("sbin", "usr/sbin"),
            ("lib", "usr/lib"),
            ("lib64", "usr/lib64"),
        ):
            link(root / relative, target)
        for directory in ("tmp", "var/tmp"):
            (root / directory).chmod(0o1777)
        override = Path("/etc/portage/package.use/zz-corvus-python-bootstrap")
        if override.exists():
            raise RuntimeError(f"Bootstrap override already exists: {override}")
        write(override, "sys-apps/util-linux build\nsys-libs/pam -systemd\n")
        try:
            self.packages(context, ("@system",))
        finally:
            override.unlink()
        self.packages(context, ("sys-apps/util-linux", "sys-libs/pam"))
        self.packages(context, self.atoms)
        shutil.copytree(
            "/etc/portage", root / "etc/portage", symlinks=True, dirs_exist_ok=True
        )
        link(
            root / "etc/portage/make.profile",
            "../../var/db/repos/gentoo/profiles/default/linux/amd64/23.0/no-multilib/systemd",
        )
        write(
            root / "etc/portage/make.conf",
            """COMMON_FLAGS="-O2 -pipe"
CFLAGS="${COMMON_FLAGS}"
CXXFLAGS="${COMMON_FLAGS}"
ACCEPT_LICENSE="*"
DISTDIR="/var/cache/distfiles"
PKGDIR="/var/cache/binpkgs"
GPG_VERIFY_USER_DROP=""
GPG_VERIFY_GROUP_DROP=""
""",
        )
        write(
            root / "etc/portage/binrepos.conf/zz-corvus-python.conf",
            """[binhost]
location = /var/cache/binhost/x86-64
sync-uri = https://distfiles.gentoo.org/releases/amd64/binpackages/23.0/x86-64
verify-signature = true
""",
        )
        context.artifacts["system"] = root

    def verify(self, context: BuildContext) -> None:
        super().verify(context)
        backend = PortageBackend(context.runner, context.root)
        missing = [
            atom
            for atom in (*self.atoms, "sys-libs/glibc", "sys-apps/systemd")
            if not backend.installed(atom)
        ]
        if missing:
            raise RuntimeError(f"Target packages are missing: {missing}")


class KernelStage(GentooStage):
    requires = frozenset({"system"})
    produces = frozenset({"kernel"})

    def execute(self, context: BuildContext) -> None:
        env = dict(os.environ, ROOT="/", SYSROOT="/", ACCEPT_KEYWORDS="amd64")
        package = subprocess.check_output(
            ["portageq", "best_visible", "/", "ebuild", "sys-kernel/gentoo-sources"],
            env=env,
            text=True,
        ).strip()
        if not package.startswith("sys-kernel/gentoo-sources-"):
            raise RuntimeError("No visible Gentoo kernel sources")
        digest = hashlib.sha256(self.settings.kernel_config.read_bytes()).hexdigest()
        version = package.removeprefix("sys-kernel/gentoo-sources-")
        archive = self.settings.cache / "kernels" / f"kernel-{version}-{digest}.tar.gz"
        if archive.is_file():
            self.command(context, "tar", "-xzpf", str(archive), "-C", str(context.root))
            kernels = sorted((context.root / "boot").glob("vmlinuz-*"))
            if len(kernels) != 1:
                raise RuntimeError("Kernel cache must contain one kernel")
            context.artifacts["kernel"] = kernels[0].name
            return
        self.packages(context, ("=" + package,), native=True)
        version = package.removeprefix("sys-kernel/gentoo-sources-")
        base, separator, revision = version.partition("-r")
        source = Path("/usr/src") / (
            f"linux-{base}-gentoo" + (f"-r{revision}" if separator else "")
        )
        shutil.copyfile(self.settings.kernel_config, source / ".config")
        common = ("make", "-C", str(source), f"-j{os.cpu_count() or 1}", "ARCH=x86_64")
        self.command(context, *common, "olddefconfig")
        self.command(context, *common, "bzImage", "modules")
        release = subprocess.check_output(
            ["make", "-s", "-C", str(source), "ARCH=x86_64", "kernelrelease"], text=True
        ).strip()
        self.command(
            context, *common, f"INSTALL_MOD_PATH={context.root}", "modules_install"
        )
        for name, path in (
            ("vmlinuz", "arch/x86/boot/bzImage"),
            ("System.map", "System.map"),
            ("config", ".config"),
        ):
            shutil.copyfile(source / path, context.root / "boot" / f"{name}-{release}")
        descriptor, temporary = tempfile.mkstemp(dir=archive.parent, prefix=".kernel-")
        os.close(descriptor)
        try:
            self.command(
                context,
                "tar",
                "-czpf",
                temporary,
                "-C",
                str(context.root),
                f"boot/vmlinuz-{release}",
                f"boot/System.map-{release}",
                f"boot/config-{release}",
                f"lib/modules/{release}",
            )
            Path(temporary).replace(archive)
        finally:
            Path(temporary).unlink(missing_ok=True)
        context.artifacts["kernel"] = f"vmlinuz-{release}"


class ConfigurationStage(GentooStage):
    requires = frozenset({"kernel"})
    produces = frozenset({"configured"})

    def execute(self, context: BuildContext) -> None:
        root = context.root
        write(
            root / "etc/fstab",
            "LABEL=corvus-root / ext4 defaults,noatime 0 1\n"
            "LABEL=CORVUS-EFI /boot/efi vfat defaults,umask=0077 0 2\n",
        )
        write(root / "etc/hostname", self.settings.name + "\n")
        write(root / "etc/hosts", "127.0.0.1 localhost\n::1 localhost\n")
        link(root / "etc/localtime", "../usr/share/zoneinfo/UTC")
        write(root / "etc/locale.conf", "LANG=en_US.UTF-8\n")
        write(root / "etc/locale.gen", "en_US.UTF-8 UTF-8\nC.UTF-8 UTF-8\n")
        (root / "usr/lib/locale").mkdir(parents=True, exist_ok=True)
        for source, destination in (("en_US", "en_US.UTF-8"), ("C", "C.UTF-8")):
            self.command(
                context,
                "chroot",
                str(root),
                "localedef",
                "-i",
                source,
                "-f",
                "UTF-8",
                destination,
                LC_ALL="C",
            )
        write(
            root / "etc/systemd/network/20-wired.network",
            "[Match]\nName=en* eth*\n\n[Network]\nDHCP=yes\n",
        )
        write(
            root / "etc/systemd/system/sshd-hostkeys.service",
            """[Unit]
Description=Generate SSH host keys
DefaultDependencies=no
RequiresMountsFor=/etc/ssh
After=systemd-remount-fs.service
Conflicts=shutdown.target
Before=shutdown.target
[Service]
Type=oneshot
RemainAfterExit=yes
ExecStart=/usr/bin/ssh-keygen -A
""",
        )
        for consumer in ("sshd.service", "sshd-vsock.socket", "sshd-unix-local.socket"):
            write(
                root / f"etc/systemd/system/{consumer}.d/10-hostkeys.conf",
                "[Unit]\nRequires=sshd-hostkeys.service\nAfter=sshd-hostkeys.service\n",
            )
        self.command(
            context,
            "systemctl",
            f"--root={root}",
            "enable",
            "systemd-networkd.service",
            "systemd-resolved.service",
            "sshd.service",
            "qemu-guest-agent.service",
            "serial-getty@ttyS0.service",
        )
        link(root / "etc/resolv.conf", "/run/systemd/resolve/stub-resolv.conf")
        context.artifacts["configured"] = root


class BootloaderStage(GentooStage):
    requires = frozenset({"configured"})
    produces = frozenset({"bootloader"})

    def execute(self, context: BuildContext) -> None:
        root = context.root
        self.command(
            context,
            "grub-install",
            "--target=i386-pc",
            f"--boot-directory={root}/boot",
            str(self.settings.disk),
        )
        self.command(
            context,
            "grub-install",
            "--target=x86_64-efi",
            f"--boot-directory={root}/boot",
            f"--efi-directory={root}/boot/efi",
            "--removable",
            "--no-nvram",
        )
        kernel = context.artifacts["kernel"]
        link(root / "boot/vmlinuz", str(kernel))
        write(
            root / "boot/grub/grub.cfg",
            f"""set timeout=0
set default=0
insmod part_gpt
insmod ext2
insmod search_label
serial --unit=0 --speed=115200
terminal_input serial console
terminal_output serial console
menuentry 'Gentoo Linux' {{
    search --set=root --label corvus-root
    linux /boot/{kernel} root=/dev/vda3 rootfstype=ext4 console=tty0 console=ttyS0,115200n8
}}
""",
        )
        context.artifacts["bootloader"] = root / "boot/grub/grub.cfg"


class FinalizeStage(GentooStage):
    requires = frozenset({"bootloader"})
    produces = frozenset({"image"})

    def execute(self, context: BuildContext) -> None:
        root = context.root
        for key in (root / "etc/ssh").glob("ssh_host_*"):
            key.unlink()
        write(root / "etc/machine-id", "")
        self.command(context, "chroot", str(root), "passwd", "-l", "root")
        for relative in (
            "var/lib/cloud",
            "var/db/repos",
            "var/cache/binpkgs",
            "var/cache/binhost",
            "var/cache/distfiles",
            "var/tmp",
            "tmp",
            "var/log/portage",
        ):
            target = root / relative
            # Refuse ancestor symlinks before deleting build residue.
            if any(
                parent.is_symlink()
                for parent in (target, *target.parents)
                if parent != root and root in parent.parents
            ):
                raise RuntimeError(f"Unsafe residue path: {target}")
            if target.exists():
                shutil.rmtree(target)
                target.mkdir(mode=0o1777 if relative in ("tmp", "var/tmp") else 0o755)
                if relative in ("tmp", "var/tmp"):
                    target.chmod(0o1777)
        for path in (root / "var/log").glob("emerge*"):
            if path.is_file():
                path.unlink()
        write(
            root / "etc/corvus-image-info",
            json.dumps(
                {
                    "image": self.settings.name,
                    "kernel": context.artifacts["kernel"],
                    "built": time.time(),
                    "builder": "corvus-image-builder",
                }
            )
            + "\n",
        )
        self.command(context, "sync")
        context.artifacts["image"] = root
