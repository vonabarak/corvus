"""Journal artifacts survive node cleanup and collection failures."""

from __future__ import annotations

import subprocess
from pathlib import Path
from unittest.mock import MagicMock

import pytest
from corvus_test_harness import logs
from corvus_test_harness.host_binary import HostBinary
from corvus_test_harness.images import ImageReady
from corvus_test_harness.outer import Crv
from corvus_test_harness.ssh import NodeShell
from corvus_test_harness.topology import Topology


@pytest.fixture(autouse=True)
def log_root(monkeypatch: pytest.MonkeyPatch, tmp_path: Path) -> None:
    monkeypatch.setattr(logs, "_run_directory", None)
    monkeypatch.setenv("CORVUS_TEST_LOG_DIR", str(tmp_path / "logs"))


def test_run_directory_shared_and_unique(tmp_path: Path) -> None:
    first = logs.configure_run_directory()
    assert first.parent == tmp_path / "logs"
    assert first.name[8] == "T"
    assert "Z-" in first.name
    assert logs.run_directory() == first
    second = logs.configure_run_directory()
    assert first != second
    # A worker adopts the controller's absolute path rather than creating one.
    assert logs.configure_run_directory(first) == first
    assert logs.run_directory() == first


def test_export_matches_system_and_user_units(
    monkeypatch: pytest.MonkeyPatch, tmp_path: Path
) -> None:
    calls: list[str] = []

    def export(
        shell: NodeShell, command: str, destination: Path, *, timeout_sec: float
    ) -> None:
        assert shell.cid == 42
        assert shell.user == "corvus"
        assert timeout_sec == 15
        service = destination.stem
        assert (
            f"_SYSTEMD_UNIT={service}.service + _SYSTEMD_USER_UNIT={service}.service"
            in command
        )
        assert "--utc --output=short-iso-precise" in command
        assert "sudo -n journalctl" in command
        calls.append(service)
        destination.write_bytes(
            b"" if service == "corvus" else b"precise timestamp process[42]: log\n"
        )

    monkeypatch.setattr(NodeShell, "run_to_file", export)
    crv = MagicMock(spec=Crv)
    logs.collect_node_logs(crv, tmp_path / "node", "outer-node", 42)
    assert calls == list(logs.SERVICES)
    assert (tmp_path / "node/corvus.log").read_bytes() == b""
    assert not list(tmp_path.rglob("*.txt"))
    crv.run.assert_not_called()


@pytest.mark.parametrize("timeout", [True, False])
def test_partial_export_continues(
    monkeypatch: pytest.MonkeyPatch, tmp_path: Path, timeout: bool
) -> None:
    def export(
        shell: NodeShell, command: str, destination: Path, *, timeout_sec: float
    ) -> None:
        destination.write_bytes(b"partial\n")
        if destination.stem == "corvus":
            if timeout:
                raise subprocess.TimeoutExpired(
                    command, timeout_sec, stderr=b"ssh timed out"
                )
            raise subprocess.CalledProcessError(
                1, command, stderr=b"journal unavailable"
            )

    monkeypatch.setattr(NodeShell, "run_to_file", export)
    logs.collect_node_logs(MagicMock(spec=Crv), tmp_path, "outer-node", 42)
    assert (tmp_path / "corvus.log").read_bytes() == b"partial\n"
    assert (tmp_path / "corvus-netd.log").exists()
    error = (tmp_path / "corvus.collection-error.txt").read_text()
    assert ("ssh timed out" if timeout else "journal unavailable") in error


def test_recover_cid_before_node_handle_exists(
    monkeypatch: pytest.MonkeyPatch, tmp_path: Path
) -> None:
    crv = MagicMock(spec=Crv)
    crv.run.return_value = {"vsock_cid": 42}
    export = MagicMock()
    monkeypatch.setattr(NodeShell, "run_to_file", export)
    logs.collect_node_logs(crv, tmp_path, "partially-booted", None)
    crv.run.assert_called_once_with("vm", "show", "partially-booted", timeout=15)
    assert export.call_count == 3


@pytest.mark.parametrize("details", [{}, {"vsock_cid": 2}, []])
def test_missing_cid_is_diagnostic(tmp_path: Path, details: object) -> None:
    crv = MagicMock(spec=Crv)
    crv.run.return_value = details
    logs.collect_node_logs(crv, tmp_path, "unstarted", None)
    assert "no usable VSOCK CID" in (tmp_path / "collection-error.txt").read_text()


def test_unwritable_destination_is_best_effort(tmp_path: Path) -> None:
    destination = tmp_path / "file"
    destination.write_text("not a directory")
    logs.collect_node_logs(MagicMock(spec=Crv), destination, "node", 42)
    assert destination.read_text() == "not a directory"


@pytest.mark.parametrize("leak", [False, True])
def test_topology_collects_before_cleanup_once(
    monkeypatch: pytest.MonkeyPatch, tmp_path: Path, leak: bool
) -> None:
    crv = MagicMock(spec=Crv)
    monkeypatch.setattr("tempfile.gettempdir", lambda: str(tmp_path))
    topology = Topology(
        crv,
        MagicMock(spec=ImageReady),
        MagicMock(spec=HostBinary),
        class_name="TestExample",
        network_name="network",
    )
    topology._log_nodes = {"node": 42, "partial-node": None}
    node = MagicMock()
    node.name = "node"
    topology._nodes = [node]
    events: list[str] = []
    node._client.close.side_effect = lambda: events.append("close")
    crv.vm_stop.side_effect = lambda *args, **kwargs: events.append("stop")

    def collect(crv: Crv, directory: Path, name: str, cid: int | None) -> None:
        assert directory == logs.run_directory() / "TestExample" / name
        events.append(name)

    monkeypatch.setattr(logs, "collect_node_logs", collect)
    topology.finalize(leak_on_failure=leak)
    topology.finalize(leak_on_failure=leak)
    assert events[:3] == ["node", "partial-node", "close"]
    assert events.count("node") == 1
    assert ("stop" in events) is not leak
    assert crv.vm_delete.called is not leak


def test_file_output_streams_and_keeps_partial_data(
    monkeypatch: pytest.MonkeyPatch, tmp_path: Path
) -> None:
    monkeypatch.setattr("shutil.which", lambda name: f"/bin/{name}")

    def run(argv: list[str], **kwargs: object) -> None:
        assert argv[-1] == "journal command"
        output = kwargs["stdout"]
        assert hasattr(output, "write")
        output.write(b"partial data")
        raise subprocess.TimeoutExpired(argv, 15)

    monkeypatch.setattr(subprocess, "run", run)
    destination = tmp_path / "corvus.log"
    with pytest.raises(subprocess.TimeoutExpired):
        NodeShell(cid=42).run_to_file("journal command", destination, timeout_sec=15)
    assert destination.read_bytes() == b"partial data"


def test_xdist_workers_share_controller_directory(tmp_path: Path) -> None:
    import shutil
    import sys

    from corvus_test_harness.host_binary import REPO_ROOT

    project = tmp_path / "pytest-project"
    project.mkdir()
    shutil.copyfile(
        REPO_ROOT / "integration_tests/conftest.py", project / "conftest.py"
    )
    (project / "test_workers.py").write_text(
        "from pathlib import Path\n"
        "import pytest\n"
        "from corvus_test_harness import logs\n"
        "@pytest.mark.parametrize('index', range(4))\n"
        "def test_directory(index):\n"
        f"    Path({str(project)!r}, f'worker-{{index}}.txt').write_text(str(logs.run_directory()))\n"
    )
    result = subprocess.run(
        [
            sys.executable,
            "-m",
            "pytest",
            "-c",
            str(REPO_ROOT / "pyproject.toml"),
            str(project),
            "-n",
            "2",
            "--dist=load",
            "-q",
        ],
        capture_output=True,
        text=True,
        timeout=60,
    )
    assert result.returncode == 0, result.stdout + result.stderr
    reports = list(project.glob("worker-*.txt"))
    assert len(reports) == 4
    directories = {report.read_text() for report in reports}
    assert len(directories) == 1
    directory = directories.pop()
    assert Path(directory).parent == tmp_path / "logs"
    assert f"test-node logs: {directory}" in result.stdout
