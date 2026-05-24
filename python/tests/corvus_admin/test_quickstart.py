"""quickstart end-to-end with subprocess/runner calls mocked."""

from __future__ import annotations

from dataclasses import dataclass

import pytest
from corvus_admin import privesc, quickstart, register


@dataclass
class _FakeRunResult:
    returncode: int = 0
    stdout: str = ""
    stderr: str = ""


class _FakeRunner:
    """Records every call so the test can assert on the order of
    operations. Doesn't touch the filesystem."""

    label = "local"

    def __init__(self) -> None:
        self.privesc = privesc.PrivEsc(tool="sudo", argv_prefix=("sudo",))
        self.calls: list[tuple[str, tuple, dict]] = []
        self.copies: list[tuple[str, int]] = []

    def copy_bytes(
        self,
        data: bytes,
        remote_path: str,
        *,
        mode: int,
        sudo: bool = True,
    ) -> None:
        del data, sudo
        self.copies.append((remote_path, mode))

    def run(
        self,
        argv: list[str],
        *,
        check: bool = True,
        sudo: bool = False,
        capture: bool = False,
    ) -> _FakeRunResult:
        self.calls.append(("run", tuple(argv), {"sudo": sudo}))
        return _FakeRunResult()

    def mkdir_p(self, path: str, *, mode: int = 0o755, sudo: bool = False) -> None:
        self.calls.append(("mkdir_p", (path,), {"mode": mode, "sudo": sudo}))

    def which(self, name: str) -> str | None:
        # Quickstart's binary resolution goes through binaries.find_all
        # (which uses shutil.which on the host), not the runner —
        # this stub only exists to satisfy the Runner ABC.
        return f"/opt/corvus/bin/{name}"


@pytest.fixture()
def patched_quickstart(monkeypatch, tmp_path, xdg_home):
    """Patch every external interaction quickstart performs so the
    test exercises only the recipe logic, not the surrounding
    plumbing."""

    privesc.reset_cache()

    # privesc.shutil and binaries.shutil are the same module, so a
    # combined which-table handles both lookups in one patch.
    def fake_which(name: str) -> str | None:
        return {
            "sudo": "/usr/bin/sudo",
            "corvus": "/usr/bin/corvus",
            "corvus-nodeagent": "/usr/bin/corvus-nodeagent",
            "corvus-netd": "/usr/bin/corvus-netd",
        }.get(name)

    monkeypatch.setattr(privesc.shutil, "which", fake_which)

    # Fake LocalRunner: don't touch the real filesystem outside tmp.
    fake_runner = _FakeRunner()
    monkeypatch.setattr(
        quickstart, "LocalRunner", lambda privesc_tool=None: fake_runner
    )

    # Pretend Postgres is reachable; the real probe would fail on
    # a CI host that doesn't run one.
    monkeypatch.setattr(quickstart, "_check_postgres", lambda url: None)

    # Skip register's subprocess work — the daemon isn't running.
    def fake_register(**kwargs):
        return register.RegisterResult(
            name=kwargs["name"],
            host=kwargs["host"],
            node_agent_port=register.DEFAULT_NODE_AGENT_PORT,
            net_agent_port=register.DEFAULT_NET_AGENT_PORT,
            healthy=True,
            show_stdout="",
        )

    monkeypatch.setattr(register, "register_node", fake_register)

    # Re-route both _drop_cert_trio and _install_and_restart to no-ops
    # so cert minting still happens (real CA gets generated) but no
    # systemctl is invoked.
    from corvus_admin import deploy

    monkeypatch.setattr(deploy, "_drop_cert_trio", lambda *args, **kwargs: None)
    monkeypatch.setattr(deploy, "_install_and_restart", lambda *args, **kwargs: None)

    return fake_runner


def test_quickstart_happy_path(tmp_path, patched_quickstart):
    log_lines: list[str] = []
    result = quickstart.run(
        node_name="primary",
        listen_ip="127.0.0.1",
        ca_dir=tmp_path / "admin",
        log_callback=log_lines.append,
    )
    assert result.node_name == "primary"
    assert result.privesc_tool == "sudo"
    assert result.netd_installed is True
    assert result.daemon_cert_cn.startswith("corvus-daemon:")
    assert result.node_cert_cn == "corvus-node:primary"
    assert result.netd_cert_cn == "corvus-netd:primary"
    assert result.healthy is True
    # Three unit files installed (daemon, nodeagent, netd).
    assert len(result.units_installed) == 3
    assert any(".service" in u for u in result.units_installed)


def test_quickstart_skips_netd_when_no_privesc(monkeypatch, tmp_path, xdg_home):
    privesc.reset_cache()

    def fake_which(name: str) -> str | None:
        # No sudo, no doas; daemon + nodeagent only.
        return {
            "corvus": "/usr/bin/corvus",
            "corvus-nodeagent": "/usr/bin/corvus-nodeagent",
        }.get(name)

    monkeypatch.setattr(privesc.shutil, "which", fake_which)
    monkeypatch.setattr(quickstart, "_check_postgres", lambda url: None)

    fake_runner = _FakeRunner()
    fake_runner.privesc = None
    monkeypatch.setattr(
        quickstart, "LocalRunner", lambda privesc_tool=None: fake_runner
    )

    def fake_register(**kwargs):
        return register.RegisterResult(
            name=kwargs["name"],
            host=kwargs["host"],
            node_agent_port=register.DEFAULT_NODE_AGENT_PORT,
            net_agent_port=register.DEFAULT_NET_AGENT_PORT,
            healthy=True,
            show_stdout="",
        )

    monkeypatch.setattr(register, "register_node", fake_register)
    from corvus_admin import deploy

    monkeypatch.setattr(deploy, "_drop_cert_trio", lambda *a, **k: None)
    monkeypatch.setattr(deploy, "_install_and_restart", lambda *a, **k: None)

    log_lines: list[str] = []
    result = quickstart.run(
        node_name="primary",
        ca_dir=tmp_path / "admin",
        log_callback=log_lines.append,
    )

    assert result.netd_installed is False
    assert result.netd_cert_cn is None
    assert result.privesc_tool is None
    # Two unit files installed (daemon + nodeagent only).
    assert len(result.units_installed) == 2
    # Warning text printed to caller.
    assert any("no privilege-escalation tool" in line for line in log_lines)
    # The warning mentions corvus-netd is being skipped.
    assert any("skipping corvus-netd" in line.lower() for line in log_lines)


def test_quickstart_skip_netd_flag(monkeypatch, tmp_path, patched_quickstart):
    result = quickstart.run(
        node_name="primary",
        ca_dir=tmp_path / "admin",
        skip_netd=True,
        log_callback=lambda _: None,
    )
    assert result.netd_installed is False
    assert result.netd_cert_cn is None


def test_quickstart_missing_binary_raises(monkeypatch, tmp_path, xdg_home):
    privesc.reset_cache()
    # sudo present but no corvus binaries — quickstart should fail
    # at the binary-discovery step.
    monkeypatch.setattr(
        privesc.shutil,
        "which",
        lambda name: "/usr/bin/sudo" if name == "sudo" else None,
    )

    with pytest.raises(quickstart.QuickstartError) as exc:
        quickstart.run(
            node_name="primary",
            ca_dir=tmp_path / "admin",
            log_callback=lambda _: None,
        )
    assert "corvus" in str(exc.value)


def test_quickstart_aborts_when_postgres_unreachable(monkeypatch, tmp_path, xdg_home):
    """If Postgres isn't up, quickstart should raise *before* it
    starts writing systemd units. Catches the operator before they
    spend 90s on a hung `systemctl --user enable --now`."""

    privesc.reset_cache()
    monkeypatch.setattr(
        privesc.shutil,
        "which",
        lambda name: {
            "sudo": "/usr/bin/sudo",
            "corvus": "/usr/bin/corvus",
            "corvus-nodeagent": "/usr/bin/corvus-nodeagent",
            "corvus-netd": "/usr/bin/corvus-netd",
        }.get(name),
    )
    monkeypatch.setattr(quickstart, "_check_postgres", lambda url: "Connection refused")

    with pytest.raises(quickstart.QuickstartError) as exc:
        quickstart.run(
            node_name="primary",
            ca_dir=tmp_path / "admin",
            log_callback=lambda _: None,
        )
    assert "PostgreSQL" in str(exc.value)
    assert "Connection refused" in str(exc.value)


def test_default_node_name_uses_hostname(monkeypatch):
    monkeypatch.setattr("socket.gethostname", lambda: "alpha.example.com")
    # Short form: strip the FQDN suffix.
    assert quickstart._default_node_name() == "alpha"


def test_default_node_name_falls_back_to_primary(monkeypatch):
    monkeypatch.setattr("socket.gethostname", lambda: "")
    assert quickstart._default_node_name() == "primary"


def test_check_postgres_rejects_non_postgres_url():
    err = quickstart._check_postgres("mysql://localhost/x")
    assert err is not None
    assert "not a postgres URL" in err


def test_check_postgres_tcp_unreachable(monkeypatch):
    import socket as _socket

    def fail_connect(*args, **kwargs):
        raise OSError("Connection refused")

    monkeypatch.setattr(_socket, "create_connection", fail_connect)
    err = quickstart._check_postgres("postgresql://localhost/corvus")
    assert err is not None
    assert "Connection refused" in err


def test_check_postgres_tcp_ok(monkeypatch):
    import socket as _socket

    class _FakeSock:
        def __enter__(self):
            return self

        def __exit__(self, *exc):
            return False

    monkeypatch.setattr(_socket, "create_connection", lambda *a, **k: _FakeSock())
    assert quickstart._check_postgres("postgresql://localhost/corvus") is None


def test_check_postgres_unix_socket_present(monkeypatch, tmp_path):
    # Re-route both common search dirs at /var/run/postgresql and
    # /tmp; the probe walks both. We pretend the socket file is
    # present by patching Path.exists for any path with the right
    # suffix.
    from pathlib import Path

    real_exists = Path.exists

    def fake_exists(self):
        if str(self).endswith(".s.PGSQL.5432"):
            return True
        return real_exists(self)

    monkeypatch.setattr(Path, "exists", fake_exists)
    assert quickstart._check_postgres("postgresql:///corvus") is None


def test_check_postgres_unix_socket_missing(monkeypatch):
    from pathlib import Path

    monkeypatch.setattr(Path, "exists", lambda self: False)
    err = quickstart._check_postgres("postgresql:///corvus")
    assert err is not None
    assert "Unix socket" in err
