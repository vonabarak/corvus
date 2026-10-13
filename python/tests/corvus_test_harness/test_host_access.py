"""Connection and state lifecycle boundaries for host node helpers."""

from __future__ import annotations

import fcntl
import json
import os
import subprocess
import tempfile
from pathlib import Path
from unittest.mock import MagicMock

import pytest
from corvus_test_harness import host_access as access
from corvus_test_harness.outer import Crv, CrvError, JsonObject


@pytest.fixture
def details() -> JsonObject:
    return {
        "id": 42,
        "name": "custom-name",
        "status": "running",
        "net_ifs": [{"guest_ip_addresses": "fe80::1/64,192.168.89.2/22"}],
    }


@pytest.fixture
def private_tmp(monkeypatch: pytest.MonkeyPatch, tmp_path: Path) -> Path:
    monkeypatch.setattr(tempfile, "gettempdir", lambda: str(tmp_path))
    return tmp_path


def publish(details: JsonObject) -> Path:
    directory = access.vm_state(details, create=True)
    client = access.private_directory(directory / "client", create=True)
    for name in access.CERT_FILES:
        (client / name).write_bytes(b"credential")
        (client / name).chmod(0o600)
    access.publish_metadata(
        directory, {"vm_id": details["id"], "ready": True, "ssh_key": "/key"}
    )
    return client


def test_custom_name_id_and_replacement(details: JsonObject, private_tmp: Path) -> None:
    client = publish(details)
    assert access.certificate_directory(details) == client
    details["name"] = "renamed"
    assert access.certificate_directory(details) == client
    details["id"] = 43
    with pytest.raises(RuntimeError, match="No host certificates"):
        access.certificate_directory(details)


def test_integration_exact_mapping(details: JsonObject, private_tmp: Path) -> None:
    details["name"] = "corvus-it-Test-with-dashes-abcd1234-alpha"
    directory = (
        private_tmp / "corvus-it-pki-Test-with-dashes-abcd1234" / "host" / "alpha"
    )
    directory.mkdir(parents=True)
    for name in access.CERT_FILES:
        (directory / name).write_bytes(b"credential")
        (directory / name).chmod(0o600)
    assert access.certificate_directory(details) == directory
    details["name"] = "corvus-it-Test-with-dashes-abcd1235-alpha"
    with pytest.raises(FileNotFoundError):
        access.certificate_directory(details)


@pytest.mark.parametrize(
    "addresses", [None, "::1,127.0.0.1,169.254.1.2", "invalid", "0.0.0.0,224.0.0.1"]
)
def test_no_usable_ipv4(details: JsonObject, addresses: str | None) -> None:
    details["net_ifs"] = [{"guest_ip_addresses": addresses}]
    with pytest.raises(RuntimeError, match="IPv4"):
        access.ipv4(details)


def test_json_vm_resolution(details: JsonObject) -> None:
    crv = MagicMock()
    crv.vm_show.return_value = details
    assert access.resolve_vm(crv, "42") is details
    crv.vm_show.assert_called_once_with("42")
    assert access.ipv4(details) == "192.168.89.2"
    details["status"] = "stopped"
    with pytest.raises(RuntimeError, match="started"):
        access.resolve_vm(crv, "42")


def test_unsafe_root(details: JsonObject, private_tmp: Path) -> None:
    root = private_tmp / f"corvus-devnode-{os.getuid()}"
    root.symlink_to(private_tmp, target_is_directory=True)
    with pytest.raises(RuntimeError, match="Unsafe"):
        access.vm_state(details, create=True)


def test_permissions_and_symlinks(details: JsonObject, private_tmp: Path) -> None:
    client = publish(details)
    key = client / "corvus-client.key"
    key.chmod(0o644)
    with pytest.raises(RuntimeError, match="permissions"):
        access.certificate_directory(details)
    key.unlink()
    key.symlink_to(client / "ca.crt")
    with pytest.raises(RuntimeError, match="Unsafe"):
        access.certificate_directory(details)


def test_inner_binary_is_stack_build(
    monkeypatch: pytest.MonkeyPatch, tmp_path: Path
) -> None:
    binary = tmp_path / "install with spaces" / "bin" / "crv"
    binary.parent.mkdir(parents=True)
    binary.write_text("#!/bin/sh\n")
    binary.chmod(0o700)
    command = MagicMock(return_value=str(binary.parent.parent) + "\n")
    monkeypatch.setattr(subprocess, "check_output", command)
    monkeypatch.setenv("STACK_BUILD_FLAGS", "--system-ghc --no-install-ghc")
    assert access.inner_binary() == binary
    command.assert_called_once_with(
        ["stack", "--system-ghc", "--no-install-ghc", "path", "--local-install-root"],
        cwd=access.REPO_ROOT,
        text=True,
        timeout=60,
    )
    binary.unlink()
    with pytest.raises(RuntimeError, match="make build"):
        access.inner_binary()


def test_cli_argv_and_outer_binary(
    monkeypatch: pytest.MonkeyPatch, details: JsonObject, private_tmp: Path
) -> None:
    certs = publish(details)
    crv = MagicMock()
    crv.vm_show.return_value = details
    monkeypatch.setattr(Crv, "autodetect", lambda: crv)
    monkeypatch.setattr(access, "inner_binary", lambda: Path("/stack install/bin/crv"))
    execute = MagicMock(side_effect=SystemExit(17))
    monkeypatch.setattr(os, "execvp", execute)
    with pytest.raises(SystemExit) as error:
        access.main(["crv", "42", "vm", "create", "name with spaces"])
    assert error.value.code == 17
    execute.assert_called_once_with(
        "/stack install/bin/crv",
        [
            "/stack install/bin/crv",
            "--host",
            "192.168.89.2",
            "--tls-cert-dir",
            str(certs),
            "vm",
            "create",
            "name with spaces",
        ],
    )
    crv.vm_show.assert_called_once_with("42")


@pytest.mark.parametrize("fails", [False, True])
def test_clean_after_delete(
    monkeypatch: pytest.MonkeyPatch, details: JsonObject, private_tmp: Path, fails: bool
) -> None:
    client = publish(details)
    crv = MagicMock()
    crv.vm_show.return_value = details
    if fails:
        crv.run.side_effect = RuntimeError("delete failed")
    monkeypatch.setattr(Crv, "autodetect", lambda: crv)
    if fails:
        with pytest.raises(RuntimeError, match="delete failed"):
            access.clean("custom-name")
    else:
        access.clean("custom-name")
    assert client.exists() is fails
    crv.run.assert_called_once_with("vm", "delete", "--force", "42", timeout=120)


def test_clean_absent_does_not_remove_state(
    monkeypatch: pytest.MonkeyPatch, details: JsonObject, private_tmp: Path
) -> None:
    client = publish(details)
    crv = MagicMock()
    crv.vm_show.side_effect = CrvError([], 1, json.dumps({"code": "vm_not_found"}), "")
    monkeypatch.setattr(Crv, "autodetect", lambda: crv)
    access.clean("custom-name")
    assert client.exists()
    crv.run.assert_not_called()


def test_setup_lock_excludes_concurrent_writers(
    monkeypatch: pytest.MonkeyPatch, tmp_path: Path, details: JsonObject
) -> None:
    monkeypatch.setattr(tempfile, "gettempdir", lambda: str(tmp_path))
    with access.state_lock(details):
        with (access.state_root() / "42.lock").open("r+") as contender:
            with pytest.raises(BlockingIOError):
                fcntl.flock(contender, fcntl.LOCK_EX | fcntl.LOCK_NB)
    with access.state_lock(details):
        pass
