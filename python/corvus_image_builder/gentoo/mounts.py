"""Owned mounts and a private environment for individual target commands."""

from pathlib import Path

from ..core import BuildContext
from .base import GentooStage


class Mounts:
    """Acquire mounts and release them in reverse order, retaining failed unmounts.

    One idempotent callback owns the group. A failed build can preserve it for
    inspection; callers can explicitly close temporary mounts before cleanup.
    """

    def __init__(self, stage: GentooStage, context: BuildContext) -> None:
        self.stage = stage
        self.context = context
        self.paths: list[Path] = []
        context.own(self.close, preserve_on_failure=True)

    def mount(self, destination: Path, *options: str) -> None:
        from ._files import safe_path

        safe_path(self.context.root, str(destination.relative_to(self.context.root)))
        if destination.is_symlink():
            raise ValueError(f"Mount destination is a symlink: {destination}")
        destination.mkdir(parents=True, exist_ok=True)
        self.stage.command(self.context, "mount", *options, str(destination))
        self.paths.append(destination)

    def bind(self, source: Path, destination: Path) -> None:
        self.mount(destination, "--bind", str(source))
        self.stage.command(self.context, "mount", "--make-rprivate", str(destination))

    def close(self) -> None:
        while self.paths:
            self.stage.command(self.context, "umount", str(self.paths[-1]))
            self.paths.pop()


class ChrootEnvironmentStage(GentooStage):
    """Mount private runtime interfaces and shared package inputs beneath ROOT.

    Do not expose the builder's /run or init-system sockets. Compilation stays
    on the builder; only individual configuration commands run in the target.
    """

    requires = frozenset({"disk"})
    produces = frozenset({"interfaces"})

    def execute(self, context: BuildContext) -> None:
        root = context.root
        mounts = Mounts(self, context)
        context.artifacts["chroot-mounts"] = mounts
        mounts.bind(Path("/dev"), root / "dev")
        mounts.bind(Path("/dev/pts"), root / "dev/pts")
        mounts.bind(Path("/dev/shm"), root / "dev/shm")
        mounts.mount(root / "proc", "-t", "proc", "proc")
        mounts.bind(Path("/sys"), root / "sys")
        mounts.mount(root / "run", "-t", "tmpfs", "tmpfs")
        mounts.bind(Path("/var/db/repos"), root / "var/db/repos")
        for source, destination in (
            ("distfiles", "distfiles"),
            (f"packages/{self.settings.variant}", "binpkgs"),
            ("binhost", "binhost"),
        ):
            mounts.bind(self.settings.cache / source, root / "var/cache" / destination)
        context.artifacts["interfaces"] = root
