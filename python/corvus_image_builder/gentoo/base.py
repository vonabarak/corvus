"""Shared command and package operations for Gentoo stages."""

import os
from pathlib import Path

from ..core import BuildContext, Stage
from ..portage import InstallRequest, PortageBackend
from .settings import Settings


class GentooStage(Stage):
    """Provide shared settings, isolated command environments, and package installs.

    Packages target the image root by default; native=True installs build tools
    into the disposable builder VM instead.
    """

    def __init__(self, settings: Settings):
        self.settings = settings

    def command(
        self,
        context: BuildContext,
        *argv: str,
        **variables: str,
    ) -> None:
        environment = dict(os.environ)
        for key in ("ROOT", "SYSROOT", "PORTAGE_CONFIGROOT"):
            environment.pop(key, None)
        environment.update(variables)
        context.runner.run(argv, env=environment)

    def chroot(
        self, context: BuildContext, *argv: str, input: bytes | None = None
    ) -> None:
        """Run an individual target command without leaking builder cache paths."""
        environment = dict(os.environ)
        for key in (
            "ROOT",
            "SYSROOT",
            "PORTAGE_CONFIGROOT",
            "PORTAGE_TMPDIR",
            "DISTDIR",
            "PKGDIR",
        ):
            environment.pop(key, None)
        command = ("chroot", str(context.root), *argv)
        if input is None:
            context.runner.run(command, env=environment)
        else:
            context.runner.run(command, env=environment, input=input)

    def packages(
        self,
        context: BuildContext,
        atoms: tuple[str, ...],
        native: bool = False,
        *,
        noreplace: bool = False,
        usepkg_exclude: tuple[str, ...] = (),
        reinstall_atoms: tuple[str, ...] = (),
    ) -> None:
        root = Path("/") if native else context.root
        PortageBackend(context.runner, root).install(
            InstallRequest(
                atoms,
                root,
                self.settings.use_binary,
                self.settings.fetch_binary,
                noreplace=noreplace,
                usepkg_exclude=usepkg_exclude,
                reinstall_atoms=reinstall_atoms,
                tmpdir=Path(str(context.artifacts["scratch"]))
                if "scratch" in context.artifacts
                else None,
            )
        )
