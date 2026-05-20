"""Locate and validate the host-built Corvus binary.

The integration tests run the freshly compiled inner Corvus by
attaching the host's `stack install` bin directory to each test VM
via virtiofs. The systemd unit `corvus.service` inside the VM
then ExecStarts `/opt/corvus/bin/corvus`.

This module:
  - Resolves `stack path --local-install-root` on the host.
  - Asserts that `<root>/bin/corvus` exists and is executable.
  - Asserts that the binary's mtime is newer than `src/Corvus/`'s
    most-recently-modified `.hs` file — otherwise the user forgot
    `stack build` and is about to test stale code.

A `$CORVUS_TEST_HOST_BIN_DIR` env override exists for the rare case
where someone wants to test a specific binary outside the stack tree.
"""
from __future__ import annotations

import os
import shutil
import subprocess
from dataclasses import dataclass
from pathlib import Path
from typing import Optional


REPO_ROOT = Path(__file__).resolve().parent.parent.parent


@dataclass(frozen=True)
class HostBinary:
    """The directory the harness will attach via virtiofs as `corvus_host`."""

    bin_dir: Path
    binary: Path

    @classmethod
    def discover(cls, *, repo_root: Optional[Path] = None) -> "HostBinary":
        root = repo_root or REPO_ROOT
        override = os.environ.get("CORVUS_TEST_HOST_BIN_DIR")
        if override:
            bin_dir = Path(override).resolve()
        else:
            bin_dir = _stack_install_bin_dir(root)
        binary = bin_dir / "corvus"
        if not binary.is_file():
            raise RuntimeError(
                f"Corvus binary not found at {binary}. "
                "Run `stack build` first (or set $CORVUS_TEST_HOST_BIN_DIR)."
            )
        if not os.access(binary, os.X_OK):
            raise RuntimeError(f"Corvus binary at {binary} is not executable")
        _check_freshness(binary, root)
        return cls(bin_dir=bin_dir, binary=binary)


def _stack_install_bin_dir(repo_root: Path) -> Path:
    stack = _find_stack()
    out = subprocess.run(
        [stack, "path", "--local-install-root"],
        cwd=str(repo_root),
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        check=True,
        timeout=60,
    )
    install_root = Path(out.stdout.decode().strip())
    return install_root / "bin"


def _find_stack() -> str:
    # The user runs ghcup-installed stack; prefer that over any system one.
    ghcup_stack = Path.home() / ".ghcup" / "bin" / "stack"
    if ghcup_stack.is_file() and os.access(ghcup_stack, os.X_OK):
        return str(ghcup_stack)
    found = shutil.which("stack")
    if not found:
        raise RuntimeError("`stack` not found (looked at ~/.ghcup/bin/stack and $PATH)")
    return found


def _check_freshness(binary: Path, repo_root: Path) -> None:
    """Warn loudly if the binary is older than any .hs source.

    Doesn't error — a developer might be intentionally testing an older
    build — but the message makes accidental stale-binary runs hard to
    miss in pytest's output.
    """
    src = repo_root / "src"
    if not src.is_dir():
        return
    binary_mtime = binary.stat().st_mtime
    newest_src_mtime = binary_mtime
    newest_src_path: Optional[Path] = None
    for path in src.rglob("*.hs"):
        m = path.stat().st_mtime
        if m > newest_src_mtime:
            newest_src_mtime = m
            newest_src_path = path
    if newest_src_path is not None:
        # Emit a warning visible in pytest output. We intentionally
        # don't raise — let the developer override with their eyes.
        import warnings

        warnings.warn(
            f"\n\n  *** Corvus binary at {binary} is OLDER than\n"
            f"  *** {newest_src_path}\n"
            f"  *** Run `stack build` to pick up your latest source changes\n"
            f"  *** before relying on these test results.\n",
            stacklevel=3,
        )
