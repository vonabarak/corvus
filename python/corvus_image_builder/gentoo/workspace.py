"""Validate the builder and prepare shared caches."""

import fcntl
import importlib
import json
import os
import shutil
import stat
import subprocess
import tempfile
from pathlib import Path
from typing import Any

from ..core import BuildContext
from ..portage import PortageBackend
from .base import GentooStage


class WorkspaceStage(GentooStage):
    """Validate the native builder and target disk, then prepare and lock caches.

    Mount the corvus-cache virtiofs share when needed and register its cleanup.
    """

    produces = frozenset({"workspace"})

    def validate(self, context: BuildContext) -> None:
        if os.geteuid() != 0:
            raise ValueError("Run inside a disposable Gentoo builder VM as root")
        importlib.import_module("portage")
        profile = Path("/etc/portage/make.profile").resolve()
        expected = "musl" if self.settings.libc == "musl" else "no-multilib/systemd"
        if expected not in str(profile) or os.uname().machine != "x86_64":
            raise ValueError(f"Builder must use native amd64 {expected} profile")
        backend = PortageBackend(context.runner, Path("/"))
        if not all(backend.installed(atom) for atom in self.settings.system_atoms):
            raise ValueError("Builder libc/init does not match recipe")
        if self.settings.libc == "musl" and not shutil.which("glib-compile-schemas"):
            raise ValueError("Missing native installation hook: glib-compile-schemas")
        if context.root == Path("/") or context.root.is_symlink():
            raise ValueError("Target root must be a separate directory")
        if context.root.exists() and any(context.root.iterdir()):
            raise ValueError("Target mount directory must be empty")
        for tool in (
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
        # Provisioning transports may inherit umask 077. Directories created
        # before package merges must still be traversable by runtime users.
        previous_umask = os.umask(0o022)

        def restore_umask() -> None:
            os.umask(previous_umask)

        context.own(restore_umask)
        if shutil.which("cloud-init"):
            self.command(context, "cloud-init", "status", "--wait")
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
        for relative in (
            "distfiles",
            f"packages/{self.settings.variant}",
            f"binhost/{self.settings.variant}",
            f"kernels/{self.settings.libc}-{self.settings.init}",
        ):
            (self.settings.cache / relative).mkdir(parents=True, exist_ok=True)
        lock = (self.settings.cache / "python-build.lock").open("a")
        fcntl.flock(lock, fcntl.LOCK_EX)
        context.own(lock.close)
        scratch = Path(tempfile.mkdtemp(prefix="corvus-python-", dir="/var/tmp"))
        scratch.chmod(0o755)
        context.own(lambda: shutil.rmtree(scratch), preserve_on_failure=True)
        context.artifacts["scratch"] = scratch
        context.artifacts["workspace"] = self.settings.cache
