"""Privilege-escalation detection.

corvus-admin needs root for a handful of operations (writing certs
under /etc/corvus, restarting system systemd units, installing the
netd unit file).

This module probes the environment once and returns whatever
escalator is on $PATH. The argv prefix is just ``sudo`` / ``doas``
— no ``-n``. That way an interactive run prompts on the
controlling terminal, while non-interactive contexts (CI, SSH
with ``BatchMode=yes``) still fail fast: both tools refuse to
prompt without a TTY and exit with a clear error. ``detect()``
is cached: corvus-admin is a short-lived CLI, the answer doesn't
change mid-run.
"""

from __future__ import annotations

import functools
import shutil
from dataclasses import dataclass


@dataclass(frozen=True)
class PrivEsc:
    """A resolved privilege-escalation tool. ``argv_prefix`` is the
    argv slice to prepend to a command so it runs as root. The
    underlying tool (sudo or doas) handles whether to prompt: with
    a TTY it asks for a password, without one it errors out."""

    tool: str
    argv_prefix: tuple[str, ...]


# Order matters: sudo is more common, check it first. Both are
# probed via shutil.which so PATH-only installs (no /usr/bin
# symlink) still get picked up.
_CANDIDATES: tuple[tuple[str, tuple[str, ...]], ...] = (
    ("sudo", ("sudo",)),
    ("doas", ("doas",)),
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
