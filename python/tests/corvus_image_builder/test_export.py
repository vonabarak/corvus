"""Exercise the rescue exporter without a live Corvus daemon."""

import hashlib
import runpy
from pathlib import Path
from types import SimpleNamespace
from unittest.mock import Mock

import pytest
from corvus_client import GuestAgentError
from corvus_client.types import GuestExecResult

EXPORT = runpy.run_path(
    str(Path(__file__).resolve().parents[3] / "yaml/gentoo/rescue/export.py")
)


@pytest.fixture
def corvus() -> tuple[Mock, Mock, Mock]:
    client = Mock()
    node = SimpleNamespace(id=1, name="first")
    client.nodes.list.return_value = [node, SimpleNamespace(id=2, name="second")]
    client.disks.get.return_value.show.return_value = SimpleNamespace(
        id=42, ephemeral=False, placements=[SimpleNamespace(node=node)]
    )
    template = client.templates.get.return_value
    template.show.return_value.drives = []
    vm = template.instantiate.return_value
    vm.guest_exec.return_value = GuestExecResult(0, "", "")
    return client, template, vm


@pytest.mark.parametrize(
    "failure", [None, "readiness", "mount", "download", "checksum"]
)
def test_export(
    tmp_path: Path, corvus: tuple[Mock, Mock, Mock], failure: str | None
) -> None:
    client, template, vm = corvus
    files = EXPORT["FILES"]
    manifest = "".join(
        f"{hashlib.sha256(name.encode()).hexdigest()}  {name}\n" for name in files[:-1]
    )
    expected = {name: name.encode() for name in files[:-1]}
    expected["SHA256SUMS"] = manifest.encode()
    if failure:
        for name in files:
            (tmp_path / name).write_bytes(b"previous")

    def download(source: str, target: Path) -> None:
        assert source == f"{EXPORT['MOUNT']}/{target.name}"
        if failure == "download":
            raise OSError("interrupted")
        data = expected[target.name]
        target.write_bytes(
            b"corrupt" if failure == "checksum" and target.name == files[0] else data
        )

    vm.download_to_file.side_effect = download
    if failure == "readiness":
        vm.guest_exec.side_effect = [
            GuestAgentError("booting"),
            GuestAgentError("booting"),
        ]
    elif failure == "mount":
        vm.guest_exec.side_effect = [
            GuestExecResult(0, "", ""),
            GuestExecResult(1, "", "bad disk"),
        ]

    if failure:
        with pytest.MonkeyPatch.context() as patch:
            patch.setattr(
                "time.monotonic",
                Mock(side_effect=[0, 121]) if failure == "readiness" else lambda: 0,
            )
            with pytest.raises((ValueError, OSError)):
                EXPORT["export_rescue_image"](client, tmp_path)
        assert all((tmp_path / name).read_bytes() == b"previous" for name in files)
    else:
        EXPORT["export_rescue_image"](client, tmp_path)
        assert {path.name: path.read_bytes() for path in tmp_path.iterdir()} == expected

    client.disks.get.assert_called_once_with("gentoo-rescue:latest")
    client.templates.get.assert_called_once_with("gentoo-musl-openrc-headless")
    assert template.instantiate.call_args.kwargs == {"node": "first"}
    assert [call[0] for call in vm.method_calls][:2] == ["attach_disk", "start"]
    vm.attach_disk.assert_called_once_with(42, interface="virtio", read_only=True)
    vm.stop.assert_called_once_with(wait=True, timeout_sec=30)
    vm.delete.assert_called_once_with(force=False)
    if failure not in {"readiness", "mount"}:
        assert vm.guest_exec.call_args.args == (f"umount {EXPORT['MOUNT']}",)
    assert not list(tmp_path.glob(".export-*"))


@pytest.mark.parametrize("empty", [True, False])
def test_export_requires_first_node_placement(
    tmp_path: Path, corvus: tuple[Mock, Mock, Mock], empty: bool
) -> None:
    client, template, _ = corvus
    nodes = client.nodes.list.return_value
    if empty:
        nodes.clear()
    else:
        client.disks.get.return_value.show.return_value.placements[0].node = nodes[1]

    with pytest.raises(
        ValueError, match=r"No Corvus nodes|no placement on the first node"
    ):
        EXPORT["export_rescue_image"](client, tmp_path)

    template.instantiate.assert_not_called()


def test_export_forces_cleanup_after_start_failure(
    tmp_path: Path, corvus: tuple[Mock, Mock, Mock]
) -> None:
    client, _, vm = corvus
    vm.start.side_effect = GuestAgentError("start failed")
    vm.stop.side_effect = GuestAgentError("stop failed")

    with pytest.raises(GuestAgentError, match="start failed"):
        EXPORT["export_rescue_image"](client, tmp_path)

    vm.delete.assert_called_once_with(force=True)
    vm.download_to_file.assert_not_called()
    assert not list(tmp_path.iterdir())


def test_export_reports_missing_helper_placement(
    tmp_path: Path, corvus: tuple[Mock, Mock, Mock]
) -> None:
    client, template, _ = corvus
    helper = SimpleNamespace(id=27, name="helper-boot", placements=[])
    template.show.return_value.drives = [SimpleNamespace(disk_image=helper)]
    rescue = client.disks.get.return_value
    client.disks.get.side_effect = [rescue, Mock(**{"show.return_value": helper})]

    with pytest.raises(ValueError, match=r"helper-boot .* has no registered placement"):
        EXPORT["export_rescue_image"](client, tmp_path)

    template.instantiate.assert_not_called()
