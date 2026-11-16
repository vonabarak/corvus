"""Prepare reusable native builders and bootstrap musl from a verified stage3."""

import hashlib
import json
import os
import re
import shutil
import subprocess
import tempfile
import time
import urllib.request
from pathlib import Path

from ..core import BuildContext
from ..portage import InstallRequest, PortageBackend
from ._files import link, remove, write
from .base import GentooStage
from .finalize import FinalizeStage
from .workspace import WorkspaceStage

TOOLS = (
    "sys-devel/gcc",
    "sys-devel/binutils",
    "sys-apps/portage",
    "dev-build/make",
    "sys-apps/gptfdisk",
    "dev-vcs/git",
    "dev-python/gpep517",
    "dev-python/setuptools",
    "sys-block/parted",
    "sys-fs/e2fsprogs",
    "sys-fs/dosfstools",
    "sys-boot/grub",
    "net-misc/curl",
    "app-arch/tar",
    "app-arch/xz-utils",
    "dev-libs/glib",
    "net-misc/openssh",
    "app-misc/ca-certificates",
    "app-emulation/qemu-guest-agent",
    "sys-apps/kbd",
    "net-misc/socat",
)


class BuilderWorkspaceStage(WorkspaceStage):
    """Prepare caches for an in-place glibc builder without selecting a target disk."""

    scratch_directory = "/run"

    def validate(self, context: BuildContext) -> None:
        if os.geteuid() != 0 or context.root != Path("/"):
            raise ValueError("Builder preparation requires root in a disposable VM")
        if not Path("/etc/gentoo-release").is_file():
            raise ValueError("Builder preparation requires Gentoo")


class BuilderToolsStage(GentooStage):
    """Select the glibc builder profile, install tools, and retry world transitions."""

    requires = frozenset({"repository"})
    produces = frozenset({"system"})

    def execute(self, context: BuildContext) -> None:
        link(
            Path("/etc/portage/make.profile"),
            "../../var/db/repos/gentoo/profiles/default/linux/amd64/23.0/no-multilib/systemd",
        )
        write(
            Path("/etc/portage/package.use/corvus-builder"),
            (
                "sys-boot/grub grub_platforms_pc grub_platforms_efi-64 -mount -nls -fonts -themes\n"
                "app-admin/sudo -sendmail\n"
            ),
        )
        with Path("/etc/portage/make.conf").open("a") as stream:
            stream.write(
                "ACCEPT_LICENSE=\"*\"\n"
                "USE=\"bash-completion\"\n"
            )
        self.packages(context, TOOLS, native=True, noreplace=True)
        backend = PortageBackend(context.runner)
        request = InstallRequest(
            ("@world",),
            Path("/"),
            self.settings.use_binary,
            self.settings.fetch_binary,
            update=True,
            deep=True,
            newuse=True,
        )
        try:
            backend.install(request)
        except subprocess.CalledProcessError:
            backend.install(request)
        context.artifacts["system"] = context.root


class BuilderIdentityStage(FinalizeStage):
    """Reset builder identity and clean its private disk, retaining tools and repositories.

    A non-recursive bind exposes disk contents without shared cache submounts.
    """

    retain_repositories = True
    requires = frozenset({"system"})
    produces = frozenset({"image"})

    def execute(self, context: BuildContext) -> None:
        self.reset_identity(context)
        view = Path(tempfile.mkdtemp(prefix="corvus-image-", dir="/run"))
        mounted = False
        try:
            self.command(context, "mount", "--bind", str(context.root), str(view))
            mounted = True
            self.command(context, "mount", "--make-private", str(view))
            self.clean(BuildContext(view))
        finally:
            if mounted:
                self.command(context, "umount", str(view))
            view.rmdir()
        write(
            context.root / "etc/corvus-image-info",
            json.dumps(
                {
                    "image": self.settings.name,
                    "built": time.time(),
                    "builder": "corvus-image-builder",
                }
            )
            + "\n",
        )
        self.command_string(context, "sync")
        for filesystem in (context.root, context.root / "boot/efi"):
            if filesystem.is_mount():
                try:
                    self.command(context, "fstrim", str(filesystem))
                except subprocess.CalledProcessError:
                    print(
                        f"Discard unavailable for {filesystem}; continuing publication.",
                        flush=True,
                    )
        context.artifacts["image"] = context.root


class Stage3Stage(GentooStage):
    """Verify an official musl stage3 in the shared cache and extract the target."""

    requires = frozenset({"disk"})
    produces = frozenset({"stage3"})

    def validate(self, context: BuildContext) -> None:
        digest = os.environ.get("MUSL_STAGE3_SHA256", "")
        url = os.environ.get("MUSL_STAGE3_URL", "")
        if not re.fullmatch(r"[0-9a-fA-F]{64}", digest):
            raise ValueError("Invalid musl stage3 SHA256")
        if not url.startswith("https://distfiles.gentoo.org/releases/amd64/"):
            raise ValueError("Expected official HTTPS musl stage3 URL")

    def execute(self, context: BuildContext) -> None:
        for relative in ("packages/x86-64_musl", "kernels/musl-openrc"):
            (self.settings.cache / relative).mkdir(parents=True, exist_ok=True)
        digest = os.environ["MUSL_STAGE3_SHA256"].lower()
        archive = self.settings.cache / "distfiles" / f"corvus-musl-{digest}.tar.xz"

        def verified(path: Path) -> bool:
            if not path.is_file():
                return False
            with path.open("rb") as stream:
                checksum = hashlib.sha256()
                for chunk in iter(lambda: stream.read(1024 * 1024), b""):
                    checksum.update(chunk)
                return checksum.hexdigest() == digest

        if not verified(archive):
            descriptor, name = tempfile.mkstemp(dir=archive.parent, prefix=".stage3-")
            os.close(descriptor)
            temporary = Path(name)
            try:
                with (
                    urllib.request.urlopen(
                        os.environ["MUSL_STAGE3_URL"], timeout=120
                    ) as source,
                    temporary.open("wb") as target,
                ):
                    shutil.copyfileobj(source, target)
                if not verified(temporary):
                    raise ValueError("Musl stage3 checksum mismatch")
                temporary.replace(archive)
            finally:
                temporary.unlink(missing_ok=True)
        self.command(
            context,
            "tar",
            "--xattrs",
            "--numeric-owner",
            "-xpf",
            str(archive),
            "-C",
            str(context.root),
        )
        resolver = context.root / "etc/resolv.conf"
        resolver.unlink(missing_ok=True)
        shutil.copyfile("/etc/resolv.conf", resolver)
        context.artifacts["stage3"] = archive


class MuslBuilderToolsStage(GentooStage):
    """Install native musl tools inside stage3 without using the helper's glibc policy."""

    requires = frozenset({"stage3", "interfaces"})
    produces = frozenset({"system"})

    def execute(self, context: BuildContext) -> None:
        root = context.root
        remove(root, "etc/portage/binrepos.conf")
        write(
            root / "etc/portage/binrepos.conf/corvus.conf",
            (
                "[binhost]\n"
                "location = /var/cache/binhost/x86-64_musl\n"
                "sync-uri = https://distfiles.gentoo.org/releases/amd64/binpackages/23.0/x86-64_musl\n"
                "verify-signature = true\n"
            ),
        )
        with (root / "etc/portage/make.conf").open("a") as stream:
            stream.write(
                'CHOST="x86_64-pc-linux-musl"\n'
                'COMMON_FLAGS="-Os -pipe"\n'
                'CFLAGS="${COMMON_FLAGS}"\n'
                'CXXFLAGS="${COMMON_FLAGS}"\n'
                'USE="-systemd -X -nls -doc -test"\n'
                'ACCEPT_LICENSE="*"\n'
                'DISTDIR="/var/cache/distfiles"\n'
                'PKGDIR="/var/cache/binpkgs"\n'
                'FEATURES="-userfetch -userpriv"\n'
                'GPG_VERIFY_USER_DROP=""\n'
                'GPG_VERIFY_GROUP_DROP=""\n'
            )
        write(
            root / "etc/portage/package.use/corvus-builder",
            (
                "dev-libs/glib -elf -introspection -mime\n"
                "sys-apps/openrc -netifrc\n"
                "sys-apps/coreutils hostname\n"
                "sys-boot/grub grub_platforms_pc grub_platforms_efi-64 -mount -nls -fonts -themes\n"
                "app-admin/sudo -sendmail\n"
            ),
        )
        self.chroot_string(context, "getuto")
        self.chroot(
            context,
            "emerge",
            "--ignore-default-opts",
            "--buildpkg",
            "--noreplace",
            "--binpkg-respect-use=y",
            "--usepkg=" + ("y" if self.settings.use_binary else "n"),
            "--getbinpkg=" + ("y" if self.settings.fetch_binary else "n"),
            "--",
            "@system",
            *TOOLS,
            "net-misc/dhcpcd",
            "app-admin/sysklogd",
        )
        context.artifacts["system"] = root


class BuilderFinalizeStage(FinalizeStage):
    """Publish a target builder with tools and a private copy of the source repository."""

    def clean(self, context: BuildContext) -> None:
        # Chroot mounts are already closed by FinalizeStage.execute.
        super().clean(context)
        shutil.copytree(
            "/var/db/repos",
            context.root / "var/db/repos",
            symlinks=True,
            dirs_exist_ok=True,
        )
