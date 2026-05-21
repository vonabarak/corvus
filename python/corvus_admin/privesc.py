"""Privilege-escalation detection.

corvus-admin needs root for a handful of operations (writing certs
under /etc/corvus, restarting system systemd units, installing the
netd unit file). Historically the deploy code hardcoded ``sudo -n``;
that breaks on hosts that ship ``doas`` instead (Alpine, OpenBSD,
some hardened Gentoo profiles).

This module probes the environment once and returns whatever
escalator is on $PATH. ``detect()`` is cached: corvus-admin is a
short-lived CLI, the answer doesn't change mid-run.
"""

from __future__ import annotations

import functools
import shutil
from dataclasses import dataclass


@dataclass(frozen=True)
class PrivEsc:
    """A resolved privilege-escalation tool. ``argv_prefix`` is the
    argv slice to prepend to a command so it runs as root without
    prompting (both sudo and doas accept ``-n`` to mean
    "non-interactive: fail if a password is required")."""

    tool: str
    argv_prefix: tuple[str, ...]


# Order matters: sudo is more common, check it first. Both are
# probed via shutil.which so PATH-only installs (no /usr/bin
# symlink) still get picked up.
_CANDIDATES: tuple[tuple[str, tuple[str, ...]], ...] = (
    ("sudo", ("sudo", "-n")),
    ("doas", ("doas", "-n")),
)


@functools.cache
def detect() -> PrivEsc | None:
    """Return the first available escalator, or None if neither sudo
    nor doas is on $PATH. Cached for the lifetime of the process."""

    for tool, prefix in _CANDIDATES:
        if shutil.which(tool) is not None:
            return PrivEsc(tool=tool, argv_prefix=prefix)
    return None


def reset_cache() -> None:
    """Test hook: drop the detect() cache between runs."""

    detect.cache_clear()
