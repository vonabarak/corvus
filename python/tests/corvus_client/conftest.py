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
from collections.abc import Iterator
from pathlib import Path

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


def _bin_search(name: str, env_override: str | None) -> str:
    """Common binary-discovery logic: env override → stack path →
    ~/.local/bin → $PATH. Used for both `corvus` and
    `corvus-nodeagent`."""
    if env_override:
        override = os.environ.get(env_override)
        if override:
            return override
    repo_root = Path(__file__).resolve().parent.parent.parent
    try:
        out = subprocess.run(
            [_find_stack(), "path", "--local-install-root"],
            cwd=str(repo_root),
            capture_output=True,
            check=True,
            timeout=60,
        )
        stack_bin = Path(out.stdout.decode().strip()) / "bin" / name
        if stack_bin.is_file() and os.access(stack_bin, os.X_OK):
            return str(stack_bin)
    except (FileNotFoundError, subprocess.SubprocessError, RuntimeError):
        pass
    local = Path.home() / ".local" / "bin" / name
    if local.is_file() and os.access(local, os.X_OK):
        return str(local)
    found = shutil.which(name)
    if not found:
        raise RuntimeError(
            f"`{name}` not found in .stack-work, ~/.local/bin, or $PATH "
            "(set $CORVUS_BIN / $CORVUS_NODEAGENT_BIN, run `stack build`, "
            "or `make install`)"
        )
    return found


def _corvus_binary() -> str:
    """Locate the `corvus` daemon binary."""
    return _bin_search("corvus", "CORVUS_BIN")


def _nodeagent_binary() -> str:
    """Locate the `corvus-nodeagent` binary."""
    return _bin_search("corvus-nodeagent", "CORVUS_NODEAGENT_BIN")


def _pick_free_port() -> int:
    """Bind 127.0.0.1:0, capture the kernel-assigned port, close.
    Races against another binder for the freshly released port are
    unlikely in serial-pytest setup; the daemon's reconnect loop
    masks transient collisions even if they happen."""
    s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    try:
        s.bind(("127.0.0.1", 0))
        return s.getsockname()[1]
    finally:
        s.close()


def _read_log_tail(log_path: Path) -> str:
    """Best-effort read of the last 800 bytes of a child process's
    redirected log file. Returns empty string on any IO error so
    the surrounding error message stays readable."""
    try:
        data = log_path.read_bytes()
    except OSError:
        return ""
    return data.decode(errors="replace")[-800:]


def _wait_for_tcp(
    host: str,
    port: int,
    proc: subprocess.Popen,
    log_path: Path,
    *,
    timeout: float = 15.0,
) -> None:
    """Poll until the agent's TCP port accepts a connection or the
    process dies."""
    deadline = time.monotonic() + timeout
    while time.monotonic() < deadline:
        if proc.poll() is not None:
            raise RuntimeError(
                f"corvus-nodeagent exited (code {proc.returncode}) before "
                f"listening on {host}:{port}:\n{_read_log_tail(log_path)}"
            )
        try:
            s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
            s.settimeout(0.5)
            s.connect((host, port))
            s.close()
            return
        except OSError:
            pass
        time.sleep(0.1)
    raise RuntimeError(f"timed out waiting for corvus-nodeagent at {host}:{port}")


def _find_stack() -> str:
    ghcup_stack = Path.home() / ".ghcup" / "bin" / "stack"
    if ghcup_stack.is_file() and os.access(ghcup_stack, os.X_OK):
        return str(ghcup_stack)
    found = shutil.which("stack")
    if not found:
        raise RuntimeError("`stack` not found (looked at ~/.ghcup/bin/stack and $PATH)")
    return found


def _psql(args: list[str], *, db: str | None = None) -> None:
    cmd = [
        "psql",
        "-h",
        _pg_host(),
        "-U",
        _pg_user(),
        "-d",
        db or _pg_admin_db(),
        "-v",
        "ON_ERROR_STOP=1",
    ]
    cmd.extend(args)
    subprocess.run(cmd, check=True, stdout=subprocess.DEVNULL, stderr=subprocess.PIPE)


def _create_db(name: str) -> None:
    _psql(["-c", f'CREATE DATABASE "{name}"'])


def _drop_db(name: str) -> None:
    # Terminate any lingering connections first.
    subprocess.run(
        [
            "psql",
            "-h",
            _pg_host(),
            "-U",
            _pg_user(),
            "-d",
            _pg_admin_db(),
            "-c",
            "SELECT pg_terminate_backend(pid) FROM pg_stat_activity "
            f"WHERE datname = '{name}' AND pid <> pg_backend_pid()",
        ],
        check=False,
        stdout=subprocess.DEVNULL,
        stderr=subprocess.DEVNULL,
    )
    subprocess.run(
        [
            "psql",
            "-h",
            _pg_host(),
            "-U",
            _pg_user(),
            "-d",
            _pg_admin_db(),
            "-c",
            f'DROP DATABASE IF EXISTS "{name}"',
        ],
        check=False,
        stdout=subprocess.DEVNULL,
        stderr=subprocess.DEVNULL,
    )


def _wait_for_socket(
    sock_path: Path,
    proc: subprocess.Popen,
    log_path: Path,
    *,
    timeout: float = 15.0,
) -> None:
    """Poll until the daemon's socket is connectable or the process dies."""
    deadline = time.monotonic() + timeout
    while time.monotonic() < deadline:
        if proc.poll() is not None:
            raise RuntimeError(
                f"corvus daemon exited (code {proc.returncode}) before socket "
                f"{sock_path}:\n{_read_log_tail(log_path)}"
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


def _ensure_self_node(sock: Path, agent_port: int) -> None:
    """Register a self-pointing 'Node' row so the daemon's
    per-node connection registry has somewhere to dial. Tests
    treat this node as the implicit default — every
    'vms.create' / 'disks.create' / 'networks.create' that
    omits 'node=' resolves to this row via the scheduler.

    Run against the freshly-booted daemon over a one-shot Unix
    socket client. Importing 'corvus_client' here (and not at
    module top) keeps test collection fast when the package
    isn't installed yet.
    """
    from corvus_client import Client
    from corvus_client.exceptions import CorvusError

    with Client(unix_socket=str(sock)) as c:
        c.nodes.create(
            "self",
            "127.0.0.1",
            node_agent_port=agent_port,
            # The conftest spawns no netd (it's not needed for
            # the python-level tests, which don't exercise
            # virtual networks). Point the netd port at the
            # nodeagent's port too — the per-node supervisor
            # will retry-and-warn on netd dial failure but the
            # nodeagent stays reachable.
            net_agent_port=agent_port,
        )
        # The daemon's per-node supervisor dials the agent
        # asynchronously from 'handleNodeAdd'; tests that hit
        # 'createImageViaAgent' immediately afterwards see
        # 'nodeagent for node 1 unavailable' until the dial
        # finishes. Poll a cheap agent-dependent op
        # ('disks.create' on a sentinel name, deleted
        # immediately) until it succeeds or we exhaust the
        # budget. Both calls are idempotent in the failing
        # case so retrying is safe.
        deadline = time.monotonic() + 10.0
        last_err: BaseException | None = None
        while time.monotonic() < deadline:
            try:
                d = c.disks.create("__conftest_probe__", size_mb=1)
                d.delete()
                return
            except CorvusError as e:
                last_err = e
                msg = str(e)
                if "unavailable" not in msg.lower():
                    # Real error — surface immediately.
                    raise
                time.sleep(0.05)
        raise RuntimeError(
            f"nodeagent never became reachable through the daemon's "
            f"registry: {last_err!r}"
        )


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
    agent_log = sock_dir / "corvus-nodeagent.log"
    _create_db(db_name)

    # Both processes share the per-fixture XDG_RUNTIME_DIR + HOME so
    # they agree on socket and disk-image paths. The nodeagent picks
    # a free port to avoid colliding with a developer's running
    # nodeagent on 9878 (or another module's fixture running in
    # parallel under pytest-xdist).
    env = os.environ.copy()
    env["XDG_RUNTIME_DIR"] = str(sock_dir)
    env["HOME"] = str(fake_home)
    agent_port = _pick_free_port()

    agent_proc: subprocess.Popen | None = None
    daemon_proc: subprocess.Popen | None = None
    try:
        # Phase 4 routes all disk / cloud-init / VM-lifecycle ops
        # through corvus-nodeagent; the daemon errors out with
        # "nodeagent unavailable" if it isn't reachable. Spawn it
        # first and wait for its TCP listener so the daemon's
        # initial dial succeeds on the first attempt.
        with open(agent_log, "wb") as alogf:
            agent_proc = subprocess.Popen(
                [
                    _nodeagent_binary(),
                    "--host",
                    "127.0.0.1",
                    "--port",
                    str(agent_port),
                    "--log-level",
                    "warn",
                    # The Python-client unit suite uses Unix
                    # sockets only and doesn't exercise the mTLS
                    # paths; --no-tls keeps the fixture
                    # cert-free. Integration tests have their
                    # own ephemeral CA (see
                    # `integration_tests/corvus_test_harness/pki.py`).
                    "--no-tls",
                ],
                stdout=alogf,
                stderr=subprocess.STDOUT,
                env=env,
            )
            _wait_for_tcp("127.0.0.1", agent_port, agent_proc, agent_log)

            with open(log_file, "wb") as logf:
                # Multi-node slice 1c dropped the legacy
                # '--node-agent-*' / '--no-netd' flags. The daemon
                # now learns its agent endpoints from rows in the
                # 'Node' table. We start the daemon with an empty
                # DB, then immediately register a self-pointing
                # node via the Cap'n Proto wire (see
                # '_ensure_self_node' below) so subsequent test
                # calls have a registry entry to route through.
                daemon_proc = subprocess.Popen(
                    [
                        _corvus_binary(),
                        "--socket",
                        str(sock),
                        "--database",
                        db_url,
                        "--log-level",
                        "warn",
                        # See the matching --no-tls on the
                        # nodeagent above; the daemon's outbound
                        # dial to it would also fail without one.
                        "--no-tls",
                        # Tests use the Unix socket exclusively;
                        # disable the TCP listener so the fixture
                        # doesn't collide with a developer's
                        # outer daemon already bound to 9876 (or
                        # with another module's fixture running
                        # under pytest-xdist).
                        "--no-tcp",
                    ],
                    stdout=logf,
                    stderr=subprocess.STDOUT,
                    env=env,
                )
                _wait_for_socket(sock, daemon_proc, log_file)
                _ensure_self_node(sock, agent_port)
                yield sock
    finally:
        for proc in (daemon_proc, agent_proc):
            if proc and proc.poll() is None:
                proc.send_signal(signal.SIGTERM)
                try:
                    proc.wait(timeout=5)
                except subprocess.TimeoutExpired:
                    proc.kill()
                    proc.wait(timeout=5)
        _drop_db(db_name)
