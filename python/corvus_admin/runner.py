"""Runner abstraction: every deploy step calls ``runner.copy(...)``
or ``runner.run(...)``, the runner translates that into either
local filesystem ops + ``subprocess.run`` or SSH/scp.

The plan's `local` ssh-target was the trigger for keeping these
behind a single interface: an all-in-one dev/single-host install
(admin's workstation == daemon host) should not require SSH at
all.
"""

from __future__ import annotations

import os
import shlex
import shutil
import subprocess
from abc import ABC, abstractmethod
from dataclasses import dataclass
from pathlib import Path


class RunnerError(RuntimeError):
    """Raised when a runner step fails (subprocess non-zero exit,
    SSH connection refused, etc.). Carries the failed command and
    its stderr so the caller can surface a useful diagnostic."""

    def __init__(self, message: str, *, stderr: str | None = None) -> None:
        super().__init__(message)
        self.stderr = stderr


@dataclass
class RunResult:
    returncode: int
    stdout: str
    stderr: str


# ---------------------------------------------------------------------------
# Interface


class Runner(ABC):
    """File-copy + command-execution surface used by deploy steps.
    Implementations live below; pick one with :func:`for_target`."""

    label: str

    @abstractmethod
    def copy_bytes(self, data: bytes, remote_path: str, *, mode: int) -> None:
        """Write *data* (as binary) to *remote_path* with the given
        Unix mode. Existing files are overwritten atomically.
        Intermediate directories must already exist (see
        :meth:`mkdir_p`)."""

    @abstractmethod
    def run(
        self,
        argv: list[str],
        *,
        check: bool = True,
        sudo: bool = False,
        capture: bool = False,
    ) -> RunResult:
        """Run *argv* on the target. When ``sudo`` is True, prepend
        ``sudo -n`` (non-interactive). ``capture=True`` makes
        stdout/stderr available on the result instead of streaming
        them through; the deploy probe step uses it."""

    @abstractmethod
    def mkdir_p(self, path: str, *, mode: int = 0o755, sudo: bool = False) -> None:
        """Equivalent of ``install -d -m <mode> <path>``."""


# ---------------------------------------------------------------------------
# Local


class LocalRunner(Runner):
    """File copies are ``shutil.copy2`` / direct writes; commands
    are ``subprocess.run``. Used when the admin's workstation is
    also the daemon host (the documented `local` ssh-target)."""

    label = "local"

    def __init__(self) -> None:
        # Pre-compute whether we're already root; sudo wrapping
        # uses this to avoid an unnecessary password prompt.
        try:
            self._is_root = os.geteuid() == 0
        except AttributeError:
            self._is_root = False

    def copy_bytes(self, data: bytes, remote_path: str, *, mode: int) -> None:
        path = Path(remote_path)
        tmp = path.with_suffix(path.suffix + ".tmp")
        # If the target needs root and we're not root, write to a
        # user-writable tmp then sudo-install. Keeps the high-mode
        # cert files arriving atomically without prompting for a
        # password twice.
        try:
            tmp.parent.mkdir(parents=True, exist_ok=True)
            fd = os.open(str(tmp), os.O_WRONLY | os.O_CREAT | os.O_TRUNC, mode)
            with os.fdopen(fd, "wb") as f:
                f.write(data)
            os.replace(tmp, path)
            os.chmod(path, mode)
        except PermissionError:
            # Tier 2: write into /tmp, then `install` with sudo.
            # We don't blindly try sudo on the original path —
            # that mixes user-id authority into a flow the admin
            # may not have authorised.
            import tempfile

            with tempfile.NamedTemporaryFile(delete=False, prefix="corvus-admin-") as t:
                t.write(data)
                staging = t.name
            try:
                self.run(
                    ["install", "-m", oct(mode)[2:], staging, remote_path],
                    sudo=True,
                )
            finally:
                try:
                    os.unlink(staging)
                except OSError:
                    pass

    def run(
        self,
        argv: list[str],
        *,
        check: bool = True,
        sudo: bool = False,
        capture: bool = False,
    ) -> RunResult:
        cmd = list(argv)
        if sudo and not self._is_root:
            cmd = ["sudo", "-n", *cmd]
        proc = subprocess.run(
            cmd,
            check=False,
            text=True,
            capture_output=capture,
        )
        result = RunResult(
            returncode=proc.returncode,
            stdout=proc.stdout or "",
            stderr=proc.stderr or "",
        )
        if check and proc.returncode != 0:
            raise RunnerError(
                f"command failed (rc={proc.returncode}): {shlex.join(cmd)}",
                stderr=result.stderr,
            )
        return result

    def mkdir_p(self, path: str, *, mode: int = 0o755, sudo: bool = False) -> None:
        if not sudo or self._is_root:
            os.makedirs(path, mode=mode, exist_ok=True)
            try:
                os.chmod(path, mode)
            except PermissionError:
                # Race: the dir existed and is owned by someone
                # else. Fall through to sudo-install -d.
                pass
            else:
                return
        self.run(
            ["install", "-d", "-m", oct(mode)[2:], path],
            sudo=True,
        )


# ---------------------------------------------------------------------------
# SSH


class SshRunner(Runner):
    """Thin wrapper over ``ssh`` / ``scp`` subprocesses. Avoids
    paramiko on purpose: the integration harness already speaks
    subprocess-SSH, and the operator's SSH config (jump hosts,
    proxy commands, signed keys, &c.) Just Works without us
    re-implementing it. Connection multiplexing is handled by
    OpenSSH's own ControlMaster — set it up in ~/.ssh/config if
    you care about latency."""

    def __init__(self, target: str) -> None:
        # Accept "user@host", "host", or "user@host:port" (with
        # the OpenSSH-style "[host]:port" form for IPv6). We do
        # not parse the target; ssh handles it.
        if not target or " " in target:
            raise ValueError(f"invalid ssh target {target!r}")
        self.target = target
        self.label = f"ssh:{target}"

    # Shared base args. -o BatchMode=yes refuses to prompt for a
    # password — the admin's key needs to be in agent before
    # corvus-admin runs.
    _SSH_BASE = ("ssh", "-o", "BatchMode=yes")
    _SCP_BASE = ("scp", "-q", "-o", "BatchMode=yes")

    def copy_bytes(self, data: bytes, remote_path: str, *, mode: int) -> None:
        import tempfile

        with tempfile.NamedTemporaryFile(delete=False, prefix="corvus-admin-") as t:
            t.write(data)
            staging = t.name
        try:
            # scp into a staging path the agent definitely owns,
            # then sudo-install into the real spot with the right
            # mode. Doing both in one shot via `ssh sudo tee` is
            # tempting but routes the bytes through sudo's stdin
            # and trips its tty heuristics on some configs.
            remote_staging = (
                f"/tmp/corvus-admin.{os.getpid()}.{os.path.basename(remote_path)}"
            )
            self._scp(staging, remote_staging)
            self.run(
                [
                    "install",
                    "-m",
                    oct(mode)[2:],
                    remote_staging,
                    remote_path,
                ],
                sudo=True,
            )
            self.run(["rm", "-f", remote_staging], sudo=False, check=False)
        finally:
            try:
                os.unlink(staging)
            except OSError:
                pass

    def run(
        self,
        argv: list[str],
        *,
        check: bool = True,
        sudo: bool = False,
        capture: bool = False,
    ) -> RunResult:
        cmd = list(argv)
        remote_cmd = shlex.join(cmd)
        if sudo:
            remote_cmd = f"sudo -n {remote_cmd}"
        full = [*self._SSH_BASE, self.target, remote_cmd]
        proc = subprocess.run(
            full,
            check=False,
            text=True,
            capture_output=capture,
        )
        result = RunResult(
            returncode=proc.returncode,
            stdout=proc.stdout or "",
            stderr=proc.stderr or "",
        )
        if check and proc.returncode != 0:
            raise RunnerError(
                f"ssh command failed on {self.target} "
                f"(rc={proc.returncode}): {remote_cmd}",
                stderr=result.stderr,
            )
        return result

    def mkdir_p(self, path: str, *, mode: int = 0o755, sudo: bool = False) -> None:
        self.run(
            ["install", "-d", "-m", oct(mode)[2:], path],
            sudo=sudo,
        )

    def _scp(self, local_src: str, remote_dest: str) -> None:
        full = [*self._SCP_BASE, local_src, f"{self.target}:{remote_dest}"]
        proc = subprocess.run(full, check=False, text=True, capture_output=True)
        if proc.returncode != 0:
            raise RunnerError(
                f"scp failed to {self.target}:{remote_dest} "
                f"(rc={proc.returncode}): {proc.stderr.strip()}",
                stderr=proc.stderr,
            )


# ---------------------------------------------------------------------------
# Factory


def for_target(target: str) -> Runner:
    """Pick the right runner for *target*.

    * ``"local"`` → :class:`LocalRunner` (no SSH).
    * anything else → :class:`SshRunner` (passes the string straight
      through to ``ssh`` so the operator's ~/.ssh/config wins for
      jumphosts, ports, identity files, etc.).
    """

    if target == "local":
        return LocalRunner()
    return SshRunner(target)


# Hook the imports above into the module namespace for cleaner test
# imports. shutil is referenced indirectly via shutil.copy2 in
# future deploy paths; pin the symbol so a slimming refactor
# doesn't trip mypy or linters.
_ = shutil
