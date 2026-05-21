"""Adapter from :class:`NodeShell` to :class:`corvus_admin.runner.Runner`.

The harness reaches every test-node via SSH-over-VSOCK
(:class:`integration_tests.corvus_test_harness.ssh.NodeShell`).
Corvus-admin's deploy code is written against a small
:class:`Runner` ABC with ``copy_bytes`` / ``run`` / ``mkdir_p``
methods; the production tool ships :class:`LocalRunner` and
:class:`SshRunner`. Wrapping ``NodeShell`` in a Runner-shaped
class lets the harness drive corvus-admin's existing
:func:`deploy_daemon` / :func:`deploy_node` / :func:`deploy_netd`
through its existing transport without paramiko or duplicate
plumbing.
"""

from __future__ import annotations

import base64
import shlex
from typing import Iterable

from corvus_admin.runner import Runner, RunResult, RunnerError

from .ssh import NodeShell


class NodeShellRunner(Runner):
    """``Runner`` implementation that pipes every operation through
    a :class:`NodeShell`.

    Construction is cheap; the underlying ``NodeShell`` is what
    actually holds the VSOCK CID + key path. One ``NodeShellRunner``
    per test-node per deploy pass.
    """

    def __init__(self, shell: NodeShell, *, label: str | None = None) -> None:
        self._shell = shell
        # The label flows through to ``IssuedRecord.deployed_to`` so
        # the admin store's index distinguishes per-node deploys.
        # Format mirrors :class:`corvus_admin.runner.SshRunner`'s
        # ``ssh:<target>`` for consistency: ``vsock:<cid>`` is what
        # an operator would type if there were a real CLI form.
        self.label = label or f"vsock:{shell.cid}"

    # ------------------------------------------------------------------
    # File copy

    def copy_bytes(self, data: bytes, remote_path: str, *, mode: int) -> None:
        """Push *data* to *remote_path* with the given Unix mode.

        Done in three SSH round-trips: base64-encode locally,
        decode into a staging file (the corvus user writes), then
        ``sudo install`` it atomically into the final path with
        the right mode + owner. The base64 step lets us ship the
        bytes through ``NodeShell.run``'s positional-string argv
        without worrying about quoting, and keeps key files
        intact even when OpenSSL writes 8-bit PEM.

        Owner is forced to ``corvus:corvus`` because every
        Corvus service in the test image runs as that user (see
        ``yaml/corvus-test-node/systemd/corvus-*.service``) — a
        naïve ``sudo install`` would leave the file owned by
        ``root:root`` and the daemon couldn't read its own
        0600-mode private key. The matching gap in
        :class:`corvus_admin.runner.SshRunner` is owed but not
        fatal for the production system-service path because
        ``corvus-netd`` is the only system service today and it
        runs as root.
        """

        if not isinstance(remote_path, str) or not remote_path:
            raise ValueError(f"invalid remote_path {remote_path!r}")
        encoded = base64.b64encode(data).decode("ascii")
        staging = f"/tmp/corvus-admin.{self._shell.cid}.{_safe_basename(remote_path)}"
        decode_cmd = (
            f"set -e; "
            f"printf '%s' {shlex.quote(encoded)} | base64 -d > {shlex.quote(staging)}"
        )
        self._shell.run(decode_cmd, check=True)
        try:
            self.run(
                [
                    "install",
                    "-o",
                    "corvus",
                    "-g",
                    "corvus",
                    "-m",
                    oct(mode)[2:],
                    staging,
                    remote_path,
                ],
                sudo=True,
                check=True,
            )
        finally:
            self.run(["rm", "-f", staging], check=False)

    # ------------------------------------------------------------------
    # Command execution

    def run(
        self,
        argv: list[str],
        *,
        check: bool = True,
        sudo: bool = False,
        capture: bool = False,
    ) -> RunResult:
        """Run *argv* in the test-node via SSH.

        ``capture`` is silently ignored — :class:`NodeShell` always
        captures, and the harness needs the output to diagnose
        deploy failures anyway. ``check=False`` mirrors
        :class:`LocalRunner`'s contract: don't raise on non-zero
        exit; the caller decides.
        """

        cmd_str = shlex.join(argv)
        if sudo:
            cmd_str = "sudo -n " + cmd_str
        proc = self._shell.run(cmd_str, check=False)
        stdout = proc.stdout.decode("utf-8", errors="replace") if proc.stdout else ""
        stderr = proc.stderr.decode("utf-8", errors="replace") if proc.stderr else ""
        result = RunResult(
            returncode=proc.returncode,
            stdout=stdout,
            stderr=stderr,
        )
        if check and proc.returncode != 0:
            # Bake stdout + stderr into the message because the
            # harness's pytest failure path surfaces the exception
            # repr, not the typed attributes — without this the
            # operator sees the rc but not the journalctl-style
            # systemd diagnostic that came back over the pipe.
            tail = ""
            if stderr.strip():
                tail += f"\nstderr:\n{stderr.strip()}"
            if stdout.strip():
                tail += f"\nstdout:\n{stdout.strip()}"
            raise RunnerError(
                f"command failed on {self.label} "
                f"(rc={proc.returncode}): {cmd_str}{tail}",
                stderr=stderr,
            )
        return result

    # ------------------------------------------------------------------
    # mkdir -p

    def mkdir_p(self, path: str, *, mode: int = 0o755, sudo: bool = False) -> None:
        """``install -d -m <mode> <path>``.

        Goes through :meth:`run` so the sudo wrapping + error
        translation match the rest of the Runner surface."""

        self.run(
            ["install", "-d", "-m", oct(mode)[2:], path],
            sudo=sudo,
        )


# ---------------------------------------------------------------------------
# helpers


def _safe_basename(path: str) -> str:
    """Turn a remote path into a staging-friendly basename. The
    path may have slashes; the basename keeps the last segment
    and strips anything that would mis-interpret as shell
    metacharacters (paranoid — Corvus' cert filenames are
    `[a-z\\-.]+`)."""

    last = path.rsplit("/", 1)[-1]
    safe: list[str] = []
    for ch in last:
        safe.append(ch if ch.isalnum() or ch in "-_." else "_")
    return "".join(safe) or "blob"


# Re-export for callers that want to assert the Runner contract
# without importing corvus_admin themselves.
__all__: Iterable[str] = ("NodeShellRunner",)
