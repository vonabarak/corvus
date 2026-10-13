"""Host PKI, deployment order, and retry behavior for development setup."""

from __future__ import annotations

import shlex
import subprocess
import tempfile
from pathlib import Path
from types import SimpleNamespace
from unittest.mock import MagicMock

import pytest
from corvus_admin import deploy
from corvus_test_harness import devnode_setup as setup
from corvus_test_harness import host_access as access
from corvus_test_harness.outer import Crv, JsonObject


@pytest.fixture
def environment(monkeypatch: pytest.MonkeyPatch, tmp_path: Path) -> SimpleNamespace:
    monkeypatch.setattr(tempfile, "gettempdir", lambda: str(tmp_path))
    key = tmp_path / "ssh-key"
    key.write_bytes(b"key")
    details: JsonObject = {
        "id": 12,
        "name": "custom",
        "status": "running",
        "vsock_cid": 44,
    }
    crv = MagicMock()
    crv.vm_show.return_value = details
    monkeypatch.setattr(Crv, "autodetect", lambda: crv)
    monkeypatch.setattr(setup, "inner_binary", lambda: Path("/built/crv"))
    shell = MagicMock()
    shell.outer_ip.return_value = "192.168.73.7"
    state = {"exec_start": "", "guest_ca": b"", "admin": False, "postgres": True}

    def run(command: str, **kwargs: object) -> subprocess.CompletedProcess[bytes]:
        if "property=ExecStart" in command:
            return subprocess.CompletedProcess(
                [], 0, str(state["exec_start"]).encode(), b""
            )
        if command.startswith("test -d") and command.endswith('/corvus/admin"'):
            return subprocess.CompletedProcess([], 0 if state["admin"] else 1, b"", b"")
        if command.startswith("test -e "):
            return subprocess.CompletedProcess(
                [], 0 if state["guest_ca"] else 1, b"", b""
            )
        if command.startswith("cat "):
            data = state["guest_ca"]
            assert isinstance(data, bytes)
            return subprocess.CompletedProcess([], 0 if data else 1, data, b"")
        if command.startswith("psql") and not state["postgres"]:
            raise subprocess.CalledProcessError(1, command)
        return subprocess.CompletedProcess([], 0, b"", b"")

    shell.run.side_effect = run
    monkeypatch.setattr(setup, "NodeShell", lambda **kwargs: shell)
    runner = MagicMock()
    runner.home = "/home/corvus"
    monkeypatch.setattr(setup, "DevRunner", lambda shell: runner)
    verification = MagicMock()
    monkeypatch.setattr(setup, "verify", verification)
    order: list[str] = []

    def deployment(role: str, **kwargs: object) -> None:
        order.append(role)
        if role == "daemon":
            state["exec_start"] = "--database " + (
                "postgresql:///corvus"
                if kwargs.get("database") == "postgresql:///corvus"
                else "/home/corvus/.local/share/corvus/corvus.db"
            )
            state["guest_ca"] = (
                access.vm_state(details) / "admin" / "ca.crt"
            ).read_bytes()

    for role in ("netd", "node", "daemon"):

        def callback(*args: object, _role: str = role, **kwargs: object) -> None:
            deployment(_role, **kwargs)

        monkeypatch.setattr(deploy, f"deploy_{role}", callback)
    return SimpleNamespace(
        key=key,
        details=details,
        shell=shell,
        runner=runner,
        state=state,
        order=order,
        verify=verification,
    )


@pytest.mark.parametrize("database", ["sqlite", "postgresql"])
def test_setup_host_pki_and_idempotency(
    environment: SimpleNamespace,
    tmp_path: Path,
    monkeypatch: pytest.MonkeyPatch,
    database: str,
) -> None:
    outer = tmp_path / "outer-credentials"
    outer.mkdir()
    (outer / "ca.crt").write_bytes(b"outer CA")
    monkeypatch.setenv("XDG_CONFIG_HOME", str(outer))
    setup.setup("custom", environment.key, database)
    details = environment.details
    directory = access.vm_state(details)
    client = access.certificate_directory(details)
    before = (client / "corvus-client.key").read_bytes()
    ca_before = (directory / "admin" / "ca.key").read_bytes()
    assert environment.order == ["netd", "node", "daemon"]
    assert directory.stat().st_mode & 0o777 == 0o700
    assert (directory / "admin" / "ca.key").stat().st_mode & 0o777 == 0o600
    assert (client / "corvus-client.key").stat().st_mode & 0o777 == 0o600
    assert all(
        "ca.key" not in call.args[1]
        for call in environment.runner.copy_bytes.call_args_list
    )
    assert (outer / "ca.crt").read_bytes() == b"outer CA"
    assert not (outer / "corvus").exists()
    setup.setup("12", environment.key, database)
    assert environment.order == ["netd", "node", "daemon"]
    assert (client / "corvus-client.key").read_bytes() == before
    assert (directory / "admin" / "ca.key").read_bytes() == ca_before
    assert environment.verify.call_count == 2
    assert (
        sum(
            call.args[0] == "sudo loginctl enable-linger corvus"
            for call in environment.shell.run.call_args_list
        )
        == 2
    )
    assert any(
        "register_node" in call.args[0]
        and "crv_path='/opt/corvus/bin/crv'" in shlex.split(call.args[0])[-1]
        for call in environment.shell.run.call_args_list
    )


def test_backend_mismatch_before_mutation(environment: SimpleNamespace) -> None:
    environment.state["exec_start"] = "--database postgresql:///corvus"
    with pytest.raises(RuntimeError, match="Configured backend"):
        setup.setup("custom", environment.key, "sqlite")
    assert not (access.state_root() / "12").exists()
    environment.runner.copy_bytes.assert_not_called()
    assert environment.order == []


def test_existing_guest_missing_host_state(environment: SimpleNamespace) -> None:
    environment.state["exec_start"] = "--database /db"
    with pytest.raises(RuntimeError, match="no matching host PKI"):
        setup.setup("custom", environment.key, "sqlite")
    assert environment.order == []


def test_postgres_preflight(environment: SimpleNamespace) -> None:
    environment.state["postgres"] = False
    with pytest.raises(subprocess.CalledProcessError):
        setup.setup("custom", environment.key, "postgresql")
    assert not (access.state_root() / "12").exists()
    environment.runner.copy_bytes.assert_not_called()
    assert environment.order == []


def test_failure_keeps_ca_for_retry(environment: SimpleNamespace) -> None:
    environment.verify.side_effect = RuntimeError("not ready")
    with pytest.raises(RuntimeError, match="not ready"):
        setup.setup("custom", environment.key, "sqlite")
    directory = access.vm_state(environment.details)
    key = (directory / "admin" / "ca.key").read_bytes()
    assert access.read_metadata(directory / "state.json")["ready"] is False
    with pytest.raises(RuntimeError, match="incomplete"):
        access.certificate_directory(environment.details)
    environment.verify.side_effect = None
    setup.setup("custom", environment.key, "sqlite")
    assert (directory / "admin" / "ca.key").read_bytes() == key
    assert access.certificate_directory(environment.details).exists()


def test_guest_ca_mismatch(environment: SimpleNamespace) -> None:
    setup.setup("custom", environment.key, "sqlite")
    environment.state["guest_ca"] = b"different CA"
    with pytest.raises(RuntimeError, match="CA does not match"):
        setup.setup("custom", environment.key, "sqlite")
    assert environment.order == ["netd", "node", "daemon"]


def test_runner_resolves_guest_home() -> None:
    shell = MagicMock()
    shell.run.return_value = subprocess.CompletedProcess([], 0, b"/home/corvus", b"")
    runner = setup.DevRunner(shell)
    runner.mkdir_p("~/.config/systemd/user")
    runner.copy_bytes(
        b"unit", "~/.config/systemd/user/corvus.service", mode=0o644, sudo=False
    )
    commands = [call.args[0] for call in shell.run.call_args_list]
    assert any("/home/corvus/.config/systemd/user" in command for command in commands)
    assert not any("'~/" in command for command in commands)


def test_manual_guest_credentials_are_not_overwritten(
    environment: SimpleNamespace,
) -> None:
    environment.state["guest_ca"] = b"manual CA"
    with pytest.raises(RuntimeError, match="credentials without matching host PKI"):
        setup.setup("custom", environment.key, "sqlite")
    assert not (access.state_root() / "12").exists()
    assert environment.order == []
