"""Shared target population and libc-specific system bootstraps."""

import shutil
from abc import abstractmethod
from pathlib import Path

from ..core import BuildContext
from ..portage import PortageBackend
from ._files import link, remove, write
from .base import GentooStage


class PackageInstallStage(GentooStage):
    """Prepare a merged-/usr target and install flavor package atoms.

    Implement install_system for the native system bootstrap and override atoms
    to choose packages. Copy Portage configuration and verify installed packages.
    """

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
        self.install_system(context)
        if self.atoms:
            self.packages(context, self.atoms)
        self.configure_portage(context)
        context.artifacts["system"] = root

    def configure_portage(self, context: BuildContext) -> None:
        """Copy package policy while giving the target runtime cache paths."""
        root = context.root
        remove(root, "etc/portage")
        shutil.copytree(
            "/etc/portage", root / "etc/portage", symlinks=True, dirs_exist_ok=True
        )
        link(
            root / "etc/portage/make.profile",
            "../../"
            + str(Path("/etc/portage/make.profile").resolve().relative_to("/")),
        )
        write(
            root / "etc/portage/make.conf",
            f"""CHOST="x86_64-pc-linux-{"musl" if self.settings.libc == "musl" else "gnu"}"
USE="bash-completion"
COMMON_FLAGS="-O2 -pipe"
CFLAGS="${{COMMON_FLAGS}}"
CXXFLAGS="${{COMMON_FLAGS}}"
ACCEPT_LICENSE="*"
DISTDIR="/var/cache/distfiles"
PKGDIR="/var/cache/binpkgs"
GPG_VERIFY_USER_DROP=""
GPG_VERIFY_GROUP_DROP=""
""",
        )
        write(
            root / "etc/portage/binrepos.conf/zz-corvus-python.conf",
            f"""[binhost]
location = /var/cache/binhost/{self.settings.variant}
sync-uri = https://distfiles.gentoo.org/releases/amd64/binpackages/23.0/{self.settings.variant}
verify-signature = true
""",
        )

    @abstractmethod
    def install_system(self, context: BuildContext) -> None:
        """Bootstrap the target with the matching native system packages."""

    def verify(self, context: BuildContext) -> None:
        super().verify(context)
        backend = PortageBackend(context.runner, context.root)
        missing = [
            atom
            for atom in (*self.atoms, *self.settings.system_atoms)
            if not backend.installed(atom)
        ]
        if missing:
            raise RuntimeError(f"Target packages are missing: {missing}")


class GlibcPackageInstallStage(PackageInstallStage):
    """Bootstrap glibc/systemd, then restore full util-linux and PAM support."""

    def install_system(self, context: BuildContext) -> None:
        override = Path("/etc/portage/package.use/zz-corvus-python-bootstrap")
        if override.exists():
            raise RuntimeError(f"Bootstrap override already exists: {override}")
        write(override, "sys-apps/util-linux build\nsys-libs/pam -systemd\n")
        try:
            self.packages(context, ("@system",))
        finally:
            override.unlink()
        self.packages(context, ("sys-apps/util-linux", "sys-libs/pam"))


class MuslPackageInstallStage(PackageInstallStage):
    """Bootstrap musl/OpenRC using the matching native builder's system packages."""

    def install_system(self, context: BuildContext) -> None:
        write(
            Path("/etc/portage/package.use/corvus-musl-system"),
            "sys-apps/net-tools -hostname\n",
        )
        self.packages(context, ("@system", *self.settings.system_atoms))
