from pathlib import Path
from types import SimpleNamespace
from unittest.mock import MagicMock

import pytest
from corvus_client.exceptions import DiskNotFound
from corvus_test_harness import base_images, devnode_images
from corvus_test_harness.outer import Crv
from corvus_test_harness.transport import VsockTcpRelay


def image(path: str) -> base_images.BaseImage:
    return base_images.BaseImage(
        name="alpine",
        host_path=Path("/host/alpine.qcow2"),
        guest_path=Path(path),
        format="qcow2",
        outer_id=42,
    )


def disk(version: int, path: str) -> MagicMock:
    result = MagicMock()
    result.show.return_value = SimpleNamespace(
        id=version, placements=[SimpleNamespace(file_path=path)]
    )
    return result


@pytest.mark.parametrize(
    "existing,update,expected",
    [
        (None, False, 2),
        ("/new", False, 1),
        ("/old", True, 2),
    ],
)
def test_registration(
    monkeypatch: pytest.MonkeyPatch, existing: str | None, update: bool, expected: int
) -> None:
    selected = image("/new")
    monkeypatch.setattr(
        base_images, "discover", lambda *a: {"alpine": selected, "alias": selected}
    )
    client, crv = MagicMock(), MagicMock()
    crv.vm_exec.return_value = {"exitcode": 0}
    if existing is None:
        client.disks.get.side_effect = DiskNotFound("missing")
    else:
        client.disks.get.return_value = disk(1, existing)
    client.disks.register.return_value = disk(2, "/new")
    result = base_images.register_all(client, crv, "node", update_existing=update)
    assert result.disk_ids == {"alpine": expected}
    assert client.disks.register.call_count == (expected == 2)
    client.disks.get.return_value.delete.assert_not_called()


def test_strict_mismatch(monkeypatch: pytest.MonkeyPatch) -> None:
    monkeypatch.setattr(base_images, "discover", lambda *a: {"alpine": image("/new")})
    client, crv = MagicMock(), MagicMock()
    crv.vm_exec.return_value = {"exitcode": 0}
    client.disks.get.return_value = disk(1, "/old")
    with pytest.raises(ValueError, match="another file"):
        base_images.register_all(client, crv, "node")
    client.disks.register.assert_not_called()


def test_mount_failure(monkeypatch: pytest.MonkeyPatch) -> None:
    monkeypatch.setattr(base_images, "discover", lambda *a: {"alpine": image("/new")})
    client, crv = MagicMock(), MagicMock()
    crv.vm_exec.return_value = {"exitcode": 1, "stderr": "mount failed"}
    with pytest.raises(RuntimeError, match="mount failed"):
        base_images.register_all(client, crv, "node")
    client.disks.get.assert_not_called()


@pytest.mark.parametrize("failure", [None, "certs", "connect", "register"])
def test_command_cleanup(
    monkeypatch: pytest.MonkeyPatch,
    failure: str | None,
    capsys: pytest.CaptureFixture[str],
    tmp_path: Path,
) -> None:
    crv, relay, client = (MagicMock() for _ in range(3))
    crv.vm_show.return_value = {
        "id": 1,
        "name": "corvus-dev-node",
        "status": "running",
        "vsock_cid": 44,
    }
    monkeypatch.setattr(Crv, "autodetect", lambda: crv)
    certs = tmp_path / "client"
    certs.mkdir(mode=0o700)
    for name in ("ca.crt", "corvus-client.crt", "corvus-client.key"):
        (certs / name).write_bytes(b"credential")
        (certs / name).chmod(0o600)

    def certificate_directory(details: object) -> Path:
        if failure == "certs":
            raise RuntimeError("Missing host certificates; run make dev-node-vm-setup")
        return certs

    monkeypatch.setattr(devnode_images, "certificate_directory", certificate_directory)
    relay.__enter__.return_value = relay
    monkeypatch.setattr(VsockTcpRelay, "start", lambda cid: relay)
    paths: list[Path] = []

    def connect(
        r: VsockTcpRelay, *, cert_dir: Path, tls: bool, ensure_self_node: bool
    ) -> MagicMock:
        assert r is relay
        assert tls is True
        assert ensure_self_node is False
        directory = cert_dir
        paths.append(directory)
        assert directory.stat().st_mode & 0o777 == 0o700
        assert (directory / "corvus-client.key").stat().st_mode & 0o777 == 0o600
        if failure == "connect":
            raise RuntimeError("connection failed")
        return client

    monkeypatch.setattr(devnode_images, "open_client", connect)
    register = MagicMock(
        return_value=base_images.RegisteredImages(images={}, disk_ids={})
    )
    if failure == "register":
        register.side_effect = RuntimeError("registration failed")
    monkeypatch.setattr(base_images, "register_all", register)
    result = devnode_images.main([])
    assert result == (1 if failure else 0)
    assert all(path.exists() for path in paths)
    assert (certs / "corvus-client.key").read_bytes() == b"credential"
    if failure != "certs":
        relay.__exit__.assert_called_once()
    if failure not in ("certs", "connect"):
        client.close.assert_called_once()
        register.assert_called_once_with(
            client, crv, "corvus-dev-node", update_existing=True
        )
    if failure == "certs":
        assert "dev-node-vm-setup" in capsys.readouterr().err
    if failure is None:
        assert "No eligible" in capsys.readouterr().out


def test_stopped_vm(
    monkeypatch: pytest.MonkeyPatch, capsys: pytest.CaptureFixture[str]
) -> None:
    crv = MagicMock()
    crv.vm_show.return_value = {
        "id": 1,
        "name": "manual",
        "status": "stopped",
        "vsock_cid": 44,
    }
    monkeypatch.setattr(Crv, "autodetect", lambda: crv)
    assert devnode_images.main(["--vm", "manual"]) == 1
    assert "must be started" in capsys.readouterr().err
