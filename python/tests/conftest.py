"""Per-test Corvus daemon fixture.

Each test that needs RPC access pulls the `daemon_socket` fixture, which:

1. Creates a uniquely-named PostgreSQL database (`corvus_test_py_<rand>`).
2. Spawns `corvus` on a per-test temp Unix socket, pointing at that DB.
3. Waits for the socket to appear (or the process to die).
4. Yields the socket path.
5. On teardown: terminates the daemon, drops the database.

PostgreSQL connection: defaults to a local Unix-socket connection as the
current user. Override via `$CORVUS_PY_TEST_PG_USER`, `$CORVUS_PY_TEST_PG_HOST`.

The fixture is `module`-scoped so a single daemon serves all tests in a
module — fast enough for the size of our suite, and isolates state
between test files.
"""
from __future__ import annotations

import os
import secrets
import shutil
import signal
import socket
import subprocess
import time
from pathlib import Path
from typing import Iterator

import pytest


def _pg_user() -> str:
    # `corvus` is the project's convention for the Postgres role used by
    # integration tests; it owns every `corvus_test_*` database in dev
    # environments. Override via env if your local setup is different.
    return os.environ.get("CORVUS_PY_TEST_PG_USER", "corvus")


def _pg_host() -> str:
    return os.environ.get("CORVUS_PY_TEST_PG_HOST", "localhost")


def _pg_admin_db() -> str:
    return os.environ.get("CORVUS_PY_TEST_PG_ADMIN_DB", "postgres")


def _corvus_binary() -> str:
    override = os.environ.get("CORVUS_BIN")
    if override:
        return override
    # Prefer the user-installed binary (~/.local/bin/corvus) over any
    # system package: a system /usr/bin/corvus is typically older and
    # may speak a stale wire protocol.
    local = Path.home() / ".local/bin/corvus"
    if local.is_file() and os.access(local, os.X_OK):
        return str(local)
    found = shutil.which("corvus")
    if not found:
        raise RuntimeError("`corvus` not on PATH (set $CORVUS_BIN or `make install`)")
    return found


def _psql(args: list[str], *, db: str | None = None) -> None:
    cmd = ["psql", "-h", _pg_host(), "-U", _pg_user(), "-d", db or _pg_admin_db(), "-v", "ON_ERROR_STOP=1"]
    cmd.extend(args)
    subprocess.run(cmd, check=True, stdout=subprocess.DEVNULL, stderr=subprocess.PIPE)


def _create_db(name: str) -> None:
    _psql(["-c", f'CREATE DATABASE "{name}"'])


def _drop_db(name: str) -> None:
    # Terminate any lingering connections first.
    subprocess.run(
        [
            "psql", "-h", _pg_host(), "-U", _pg_user(),
            "-d", _pg_admin_db(), "-c",
            "SELECT pg_terminate_backend(pid) FROM pg_stat_activity "
            f"WHERE datname = '{name}' AND pid <> pg_backend_pid()",
        ],
        check=False, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL,
    )
    subprocess.run(
        [
            "psql", "-h", _pg_host(), "-U", _pg_user(),
            "-d", _pg_admin_db(), "-c", f'DROP DATABASE IF EXISTS "{name}"',
        ],
        check=False, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL,
    )


def _wait_for_socket(sock_path: Path, proc: subprocess.Popen, timeout: float = 15.0) -> None:
    """Poll until the daemon's socket is connectable or the process dies."""
    deadline = time.monotonic() + timeout
    while time.monotonic() < deadline:
        if proc.poll() is not None:
            try:
                _, err = proc.communicate(timeout=1)
            except Exception:
                err = b""
            raise RuntimeError(
                f"corvus daemon exited (code {proc.returncode}) before socket: "
                f"{err.decode(errors='replace')[-800:]}"
            )
        if sock_path.exists():
            try:
                s = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
                s.settimeout(0.5)
                s.connect(str(sock_path))
                s.close()
                return
            except OSError:
                pass
        time.sleep(0.1)
    raise RuntimeError(f"timed out waiting for corvus socket at {sock_path}")


@pytest.fixture(scope="module")
def daemon_socket(tmp_path_factory) -> Iterator[Path]:
    suffix = secrets.token_hex(4)
    db_name = f"corvus_test_py_{suffix}"
    sock_dir = tmp_path_factory.mktemp(f"corvus-{suffix}")
    sock = sock_dir / "corvus.sock"
    # The daemon writes disk images under $HOME/VMs (see
    # `Corvus.Qemu.Config.getEffectiveBasePath`). Point HOME at a temp
    # directory so each fixture run starts with empty disk storage and
    # can't collide with the user's daemon or earlier test runs.
    fake_home = sock_dir / "home"
    fake_home.mkdir()

    user = _pg_user()
    host = _pg_host()
    db_url = f"postgresql://{user}@{host}/{db_name}"

    log_file = sock_dir / "corvus.log"
    _create_db(db_name)
    proc: subprocess.Popen | None = None
    try:
        env = os.environ.copy()
        # Keep XDG_RUNTIME_DIR set so QEMU socket paths the daemon picks
        # for any spawned VMs land in a temp area, not in the user's runtime.
        env["XDG_RUNTIME_DIR"] = str(sock_dir)
        # Isolate $HOME/VMs (disk storage) per fixture run.
        env["HOME"] = str(fake_home)
        with open(log_file, "wb") as logf:
            proc = subprocess.Popen(
                [
                    _corvus_binary(),
                    "--socket", str(sock),
                    "--database", db_url,
                    "--log-level", "warn",
                ],
                stdout=logf,
                stderr=subprocess.STDOUT,
                env=env,
            )
            _wait_for_socket(sock, proc)
            yield sock
    finally:
        if proc and proc.poll() is None:
            proc.send_signal(signal.SIGTERM)
            try:
                proc.wait(timeout=5)
            except subprocess.TimeoutExpired:
                proc.kill()
                proc.wait(timeout=5)
        _drop_db(db_name)
