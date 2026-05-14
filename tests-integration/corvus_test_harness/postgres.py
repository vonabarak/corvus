"""In-VM Postgres readiness helper.

The integration-test image runs Postgres 18 as a systemd service; the
inner Corvus daemon connects to it over a unix socket. Most tests
don't need Postgres directly — the inner daemon does — but a few
inspect the database state via SSH. This module provides the helpers
those tests need.
"""
from __future__ import annotations

from .ssh import GuestShell


def wait_for_postgres(shell: GuestShell, *, timeout_sec: float = 60.0) -> None:
    """Block until `pg_isready` succeeds inside the VM.

    Raises `TimeoutError` on timeout, propagating the last stderr from
    `pg_isready` for diagnostics.
    """
    import time

    deadline = time.monotonic() + timeout_sec
    last_err: str = ""
    while time.monotonic() < deadline:
        out = shell.run("pg_isready -h /var/run/postgresql", check=False, timeout_sec=10)
        if out.returncode == 0:
            return
        last_err = out.stderr.decode(errors="replace").strip()
        time.sleep(0.5)
    raise TimeoutError(
        f"Postgres in VM (cid={shell.cid}) not ready after {timeout_sec:.0f}s; "
        f"last error: {last_err}"
    )


def psql(shell: GuestShell, sql: str, *, db: str = "corvus") -> str:
    """Run a SQL statement via `psql` in the VM; return stdout."""
    out = shell.run(
        f"sudo -u postgres psql -d {db} -tA -v ON_ERROR_STOP=1 -c {sql!r}"
    )
    return out.stdout.decode("utf-8", errors="replace").strip()
