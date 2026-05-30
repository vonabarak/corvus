"""Locate the Haskell binaries (``corvus``, ``corvus-nodeagent``,
``corvus-netd``) on $PATH.

The same corvus-admin install needs to work whether the binaries
came from ``make install`` (~/.local/bin), a distro package
(/usr/bin), or a hand-copied install (/usr/local/bin). Resolving
via :func:`shutil.which` defers that choice to the user's
environment — corvus-admin doesn't need to know which install
mode produced the binaries.
"""

from __future__ import annotations

import shutil
from dataclasses import dataclass
from pathlib import Path


class BinaryNotFound(RuntimeError):
    """Raised when a required Haskell binary isn't on $PATH."""


@dataclass(frozen=True)
class BinaryPaths:
    """Resolved absolute paths for the four corvus binaries.
    ``netd`` and ``web`` are optional: a privesc-less host can skip
    netd; a host that's only an agent (no WebUI) can skip web."""

    corvus: Path
    nodeagent: Path
    netd: Path | None
    web: Path | None


_BIN_DAEMON = "corvus"
_BIN_NODEAGENT = "corvus-nodeagent"
_BIN_NETD = "corvus-netd"
_BIN_WEB = "corvus-web"


def find_all(*, require_netd: bool, require_web: bool = False) -> BinaryPaths:
    """Resolve binaries from $PATH. ``netd`` is included only when
    *require_netd* is True; ``web`` only when *require_web* is True.
    Optional binaries silently land as ``None`` when missing.

    Raises :class:`BinaryNotFound` when a required binary can't be
    located, with a diagnostic that lists the binary name and a
    hint about the usual install locations.
    """

    corvus = _which_required(_BIN_DAEMON)
    nodeagent = _which_required(_BIN_NODEAGENT)
    netd: Path | None = None
    if require_netd:
        netd = _which_required(_BIN_NETD)
    web: Path | None = None
    if require_web:
        web = _which_required(_BIN_WEB)
    return BinaryPaths(corvus=corvus, nodeagent=nodeagent, netd=netd, web=web)


def _which_required(name: str) -> Path:
    found = shutil.which(name)
    if found is None:
        raise BinaryNotFound(
            f"required binary {name!r} not found on $PATH. "
            f"Expected to find it under one of: "
            f"~/.local/bin, /usr/bin, /usr/local/bin. "
            f"Install Corvus via your package manager or `stack install`."
        )
    return Path(found).resolve()
