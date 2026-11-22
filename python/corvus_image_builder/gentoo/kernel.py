"""Compile and cache kernels for the target Gentoo flavor."""

import hashlib
import os
import re
import shutil
import subprocess
import tempfile
from pathlib import Path

from ..core import BuildContext
from ._validation import validate_kernel
from .base import GentooStage
from .settings import Settings


class KernelStage(GentooStage):
    """Build Gentoo kernel sources and install the kernel and modules in the image.

    Reuse archives keyed by flavor, source revision, and configuration digest.
    Kernel compilation and source package installation run in the builder VM.
    """

    requires = frozenset({"system"})
    produces = frozenset({"kernel"})

    def __init__(self, settings: Settings, *, cache_prefix: str = "kernel"):
        super().__init__(settings)
        if not re.fullmatch(r"[A-Za-z0-9][A-Za-z0-9._-]*", cache_prefix):
            raise ValueError("Invalid kernel cache prefix")
        self.cache_prefix = cache_prefix

    def validate(self, context: BuildContext) -> None:
        if (
            self.settings.kernel_config is None
            or not self.settings.kernel_config.is_file()
        ):
            raise ValueError("Kernel configuration is missing")

    def execute(self, context: BuildContext) -> None:
        assert self.settings.kernel_config is not None
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
        validate_kernel(version)
        archive = (
            self.settings.cache
            / "kernels"
            / f"{self.settings.libc}-{self.settings.init}"
            / f"{self.cache_prefix}-{version}-{digest}.tar.gz"
        )
        if archive.is_file():
            self.command(context, "tar", "-xzpf", str(archive), "-C", str(context.root))
            kernels = sorted((context.root / "boot").glob("vmlinuz-*"))
            if len(kernels) != 1:
                raise RuntimeError("Kernel cache must contain one kernel")
            validate_kernel(kernels[0].name.removeprefix("vmlinuz-"))
            context.artifacts["kernel"] = kernels[0].name
            return
        self.packages(context, ("=" + package,), native=True)
        version = package.removeprefix("sys-kernel/gentoo-sources-")
        base, separator, revision = version.partition("-r")
        source = Path("/usr/src") / (
            f"linux-{base}-gentoo" + (f"-r{revision}" if separator else "")
        )
        shutil.copyfile(self.settings.kernel_config, source / ".config")
        jobs = os.cpu_count() or 1
        common = ("make", "-C", str(source), f"-j{jobs}", "ARCH=x86_64")
        self.command(context, *common, "olddefconfig")
        self.command(context, *common, "bzImage", "modules")
        release = subprocess.check_output(
            ["make", "-s", "-C", str(source), "ARCH=x86_64", "kernelrelease"], text=True
        ).strip()
        validate_kernel(release)
        self.command(
            context, *common, f"INSTALL_MOD_PATH={context.root!s}", "modules_install"
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
                str(temporary),
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
