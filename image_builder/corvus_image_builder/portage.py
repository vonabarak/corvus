"""Gentoo queries through Portage; installation through isolated emerge."""

import importlib
import os
from dataclasses import dataclass
from pathlib import Path
from typing import Any

from .core import CommandRunner


@dataclass(frozen=True)
class InstallRequest:
    atoms: tuple[str, ...]
    root: Path
    use_binary: bool = True
    fetch_binary: bool = True


class PortageBackend:
    def __init__(self, runner: CommandRunner, root: Path = Path("/")) -> None:
        self.runner = runner
        self.root = root
        self._trees: Any = None

    def refresh(self) -> None:
        self._trees = None

    def _database(self) -> Any:
        if self._trees is None:
            portage = importlib.import_module("portage")
            environment = dict(os.environ)
            environment.pop("SYSROOT", None)
            self._trees = portage.create_trees(
                config_root="/", target_root=str(self.root), env=environment
            )
        return self._trees[str(self.root).rstrip("/") + "/"]

    def installed(self, atom: str) -> list[str]:
        portage = importlib.import_module("portage")
        return list(self._database()["vartree"].dbapi.match(portage.dep.Atom(atom)))

    def metadata(self, package: str, keys: tuple[str, ...]) -> dict[str, str]:
        values = self._database()["vartree"].dbapi.aux_get(package, keys)
        return dict(zip(keys, values, strict=True))

    def install(self, request: InstallRequest) -> None:
        portage = importlib.import_module("portage")
        for atom in request.atoms:
            if atom != "@system":
                portage.dep.Atom(atom)
        if not request.atoms:
            raise ValueError("An installation requires package atoms")
        environment = dict(os.environ)
        environment.pop("SYSROOT", None)
        environment["ROOT"] = str(request.root)
        environment["FEATURES"] = environment.get("FEATURES", "") + (
            " -userfetch -userpriv binpkg-multi-instance parallel-fetch"
            " -binpkg-request-signature -binpkg-ignore-signature -getbinpkg"
        )
        argv = [
            "emerge",
            "--ignore-default-opts",
            "--buildpkg",
            "--binpkg-respect-use=y",
            "--binpkg-changed-deps=n",
            "--usepkg=" + ("y" if request.use_binary else "n"),
            "--getbinpkg=" + ("y" if request.fetch_binary else "n"),
            "--",
            *request.atoms,
        ]
        try:
            self.runner.run(argv, env=environment)
        finally:
            self.refresh()
