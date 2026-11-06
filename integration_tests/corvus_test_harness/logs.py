"""Persistent, per-run exports of test-node service journals."""

from __future__ import annotations

import os
import secrets
import shlex
import subprocess
import sys
from datetime import datetime, timezone
from pathlib import Path

from .host_binary import REPO_ROOT
from .outer import Crv
from .ssh import HOST_TEST_KEY_PATH, NodeShell

SERVICES = ("corvus", "corvus-nodeagent", "corvus-netd")
_run_directory: Path | None = None


def configure_run_directory(directory: Path | None = None) -> Path:
    """Select a fresh invocation directory, or adopt the controller's path."""
    global _run_directory
    if directory is None:
        root = Path(
            os.environ.get("CORVUS_TEST_LOG_DIR", REPO_ROOT / "integration_tests/logs")
        )
        timestamp = datetime.now(timezone.utc).strftime("%Y%m%dT%H%M%S.%fZ")
        directory = root / f"{timestamp}-{secrets.token_hex(4)}"
    _run_directory = directory.expanduser().resolve()
    return _run_directory


def run_directory() -> Path:
    """Also support topologies constructed outside pytest."""
    return _run_directory if _run_directory is not None else configure_run_directory()


def _record_error(path: Path, error: Exception) -> None:
    detail = f"{type(error).__name__}: {error}"
    if isinstance(error, (subprocess.CalledProcessError, subprocess.TimeoutExpired)):
        stderr = error.stderr
        if isinstance(stderr, bytes):
            detail += "\n" + stderr.decode("utf-8", errors="replace")
    print(f"[harness] log collection failed ({path}): {detail}", file=sys.stderr)
    try:
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(detail + "\n")
    except OSError as write_error:
        print(
            f"[harness] cannot write log collection diagnostic: {write_error}",
            file=sys.stderr,
        )


def collect_node_logs(crv: Crv, directory: Path, name: str, cid: int | None) -> None:
    """Best-effort export independent of clients, relays, or deployment success."""
    try:
        directory.mkdir(parents=True, exist_ok=True)
        if cid is None:
            details = crv.run("vm", "show", name, timeout=15)
            candidate = details.get("vsock_cid") if isinstance(details, dict) else None
            if not isinstance(candidate, int) or candidate <= 2:
                raise RuntimeError(f"Node {name} has no usable VSOCK CID")
            cid = candidate
        shell = NodeShell(cid=cid, user="corvus", key_path=HOST_TEST_KEY_PATH)
    except Exception as error:
        _record_error(directory / "collection-error.txt", error)
        return

    for service in SERVICES:
        unit = shlex.quote(f"{service}.service")
        # '+' joins the system and user unit matches with OR. Do not restrict
        # boot, invocation, priority, or line count: restarts are diagnostic.
        command = (
            "sudo -n journalctl --no-pager --quiet --all --utc --output=short-iso-precise "
            f"_SYSTEMD_UNIT={unit} + _SYSTEMD_USER_UNIT={unit}"
        )
        try:
            shell.run_to_file(command, directory / f"{service}.log", timeout_sec=15)
        except Exception as error:
            _record_error(directory / f"{service}.collection-error.txt", error)
