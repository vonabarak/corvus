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
    """Resolved absolute paths for the three Haskell binaries.
    ``netd`` is optional: a privesc-less host installs without it."""

    corvus: Path
    nodeagent: Path
    netd: Path | None


_BIN_DAEMON = "corvus"
_BIN_NODEAGENT = "corvus-nodeagent"
_BIN_NETD = "corvus-netd"


def find_all(*, require_netd: bool) -> BinaryPaths:
    """Resolve all three binaries from $PATH. ``netd`` is included
    only when *require_netd* is True; otherwise the field is None
    and a missing netd binary is silently tolerated.

    Raises :class:`BinaryNotFound` when a required binary can't be
    located, with a diagnostic that lists the binary name and a
    hint about the usual install locations.
    """

    corvus = _which_required(_BIN_DAEMON)
    nodeagent = _which_required(_BIN_NODEAGENT)
    netd: Path | None = None
    if require_netd:
        netd = _which_required(_BIN_NETD)
    return BinaryPaths(corvus=corvus, nodeagent=nodeagent, netd=netd)


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
