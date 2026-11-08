"""The netd tunnel waits for an authenticated read-only probe."""

import shutil
import subprocess
import time
from unittest.mock import Mock

import pytest

from . import test_netd_smoke as smoke


def test_retries_probe_and_returns_live_forward(
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    proc = Mock()
    proc.poll.return_value = None
    monkeypatch.setattr(shutil, "which", lambda binary: binary)
    monkeypatch.setattr(subprocess, "Popen", Mock(return_value=proc))
    monkeypatch.setattr(time, "sleep", lambda _: None)
    probe = Mock(side_effect=[ConnectionRefusedError("starting"), None])
    assert smoke._open_port_forward(42, 12345, probe) is proc
    assert probe.call_count == 2
    assert all(0 < call.args[0] <= 0.5 for call in probe.call_args_list)
    proc.terminate.assert_not_called()


def test_early_ssh_exit_closes_pipe(monkeypatch: pytest.MonkeyPatch) -> None:
    proc = Mock()
    proc.poll.return_value = 1
    proc.stderr.read.return_value = b"forward failed"
    monkeypatch.setattr(shutil, "which", lambda binary: binary)
    monkeypatch.setattr(subprocess, "Popen", Mock(return_value=proc))
    probe = Mock()
    with pytest.raises(RuntimeError, match="forward failed"):
        smoke._open_port_forward(42, 12345, probe)
    probe.assert_not_called()
    proc.stderr.close.assert_called_once()


def test_readiness_deadline_reaps_forward(monkeypatch: pytest.MonkeyPatch) -> None:
    proc = Mock()
    proc.poll.return_value = None
    monkeypatch.setattr(shutil, "which", lambda binary: binary)
    monkeypatch.setattr(subprocess, "Popen", Mock(return_value=proc))
    monkeypatch.setattr(time, "monotonic", Mock(side_effect=[0.0, 11.0]))
    with pytest.raises(RuntimeError, match="never ready"):
        smoke._open_port_forward(42, 12345, Mock())
    proc.terminate.assert_called_once()
    proc.wait.assert_called_once_with(timeout=2.0)
    proc.stderr.close.assert_called_once()
