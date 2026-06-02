"""Ring 3 smoke test for the build pipeline streaming window."""

from __future__ import annotations

import base64
from pathlib import Path
from typing import Any

import pytest
from corvus_client.types import (
    BuildStepEnd,
    BuildStepOutput,
    BuildStepStart,
)
from corvus_desktop.windows.build import BuildWidget, preprocess_build_yaml
from PySide6.QtCore import QObject, Signal

import yaml


class _MockBridge(QObject):
    operation_failed = Signal(str, str)
    build_started = Signal(int)
    build_event = Signal(object)
    build_finished = Signal(str)

    def __init__(self) -> None:
        super().__init__()
        self.run_calls: list[str] = []

    def build_run(self, yaml_text: str) -> None:
        self.run_calls.append(yaml_text)


@pytest.fixture
def bridge() -> _MockBridge:
    return _MockBridge()


def test_build_widget_calls_bridge_on_run(qapp: Any, bridge: _MockBridge) -> None:
    w = BuildWidget(bridge)
    try:
        w._editor.set_text("pipeline: []\n")
        w._run_btn.click()
        assert bridge.run_calls == ["pipeline: []\n"]
    finally:
        w.deleteLater()


def test_build_widget_appends_step_sections(qapp: Any, bridge: _MockBridge) -> None:
    w = BuildWidget(bridge)
    try:
        bridge.build_event.emit(
            BuildStepStart(step_index=0, name="apt-get", command="apt-get install ...")
        )
        bridge.build_event.emit(
            BuildStepOutput(step_index=0, line="Reading package lists…")
        )
        bridge.build_event.emit(
            BuildStepEnd(step_index=0, result="success", message=None)
        )
        # Section should exist and have the output line in its log.
        section = w._sections[0]
        assert "Reading package lists…" in section._log.toPlainText()
        assert "success" in section._status.text()
    finally:
        w.deleteLater()


def test_build_widget_invalid_yaml_disables_run(qapp: Any, bridge: _MockBridge) -> None:
    w = BuildWidget(bridge)
    try:
        w._editor.set_text("pipeline: [unterminated\n")
        assert not w._run_btn.isEnabled()
        w._editor.set_text("pipeline: []\n")
        assert w._run_btn.isEnabled()
    finally:
        w.deleteLater()


def test_build_widget_finish_resets_button(qapp: Any, bridge: _MockBridge) -> None:
    w = BuildWidget(bridge)
    try:
        w._run_btn.setEnabled(False)
        bridge.build_finished.emit("")
        assert w._run_btn.isEnabled()
    finally:
        w.deleteLater()


def test_preprocess_passes_through_when_nothing_to_inline(tmp_path: Path) -> None:
    body = "pipeline: []\n"
    assert preprocess_build_yaml(body, str(tmp_path)) == body


def test_preprocess_inlines_shell_script(tmp_path: Path) -> None:
    script = tmp_path / "do.sh"
    script.write_text("echo hi\n")
    body = (
        "pipeline:\n"
        "  - build:\n"
        "      provisioners:\n"
        "        - shell:\n"
        "            script: do.sh\n"
    )
    result = preprocess_build_yaml(body, str(tmp_path))
    doc = yaml.safe_load(result)
    shell = doc["pipeline"][0]["build"]["provisioners"][0]["shell"]
    assert "script" not in shell
    assert shell["inline"] == "echo hi\n"


def test_preprocess_inlines_file_from_as_base64(tmp_path: Path) -> None:
    pubkey = tmp_path / "k.pub"
    pubkey.write_bytes(b"ssh-ed25519 AAAA test\n")
    body = (
        "pipeline:\n"
        "  - build:\n"
        "      provisioners:\n"
        "        - file:\n"
        "            from: k.pub\n"
        "            path: /root/.ssh/authorized_keys\n"
    )
    result = preprocess_build_yaml(body, str(tmp_path))
    doc = yaml.safe_load(result)
    file_prov = doc["pipeline"][0]["build"]["provisioners"][0]["file"]
    assert "from" not in file_prov
    assert file_prov["content"] == base64.b64encode(b"ssh-ed25519 AAAA test\n").decode(
        "ascii"
    )
    assert file_prov["path"] == "/root/.ssh/authorized_keys"


def test_preprocess_inlines_floppy_with_default_filename(tmp_path: Path) -> None:
    src = tmp_path / "Autounattend.xml"
    src.write_bytes(b"<unattend/>")
    body = "pipeline:\n  - build:\n      floppy:\n        from: Autounattend.xml\n"
    result = preprocess_build_yaml(body, str(tmp_path))
    doc = yaml.safe_load(result)
    floppy = doc["pipeline"][0]["build"]["floppy"]
    assert "from" not in floppy
    assert floppy["filename"] == "Autounattend.xml"
    assert floppy["contentBase64"] == base64.b64encode(b"<unattend/>").decode("ascii")


def test_preprocess_floppy_keeps_user_supplied_filename(tmp_path: Path) -> None:
    src = tmp_path / "answer.xml"
    src.write_bytes(b"x")
    body = (
        "pipeline:\n"
        "  - build:\n"
        "      floppy:\n"
        "        from: answer.xml\n"
        "        filename: Autounattend.xml\n"
    )
    result = preprocess_build_yaml(body, str(tmp_path))
    floppy = yaml.safe_load(result)["pipeline"][0]["build"]["floppy"]
    assert floppy["filename"] == "Autounattend.xml"


def test_preprocess_resolves_relative_paths_from_base_dir(tmp_path: Path) -> None:
    nested = tmp_path / "sub"
    nested.mkdir()
    (nested / "a.sh").write_text("a\n")
    body = (
        "pipeline:\n"
        "  - build:\n"
        "      provisioners:\n"
        "        - shell:\n"
        "            script: sub/a.sh\n"
    )
    result = preprocess_build_yaml(body, str(tmp_path))
    shell = yaml.safe_load(result)["pipeline"][0]["build"]["provisioners"][0]["shell"]
    assert shell["inline"] == "a\n"


def test_preprocess_apply_steps_pass_through(tmp_path: Path) -> None:
    # apply: steps have no provisioners/floppy — must not be touched.
    body = (
        "pipeline:\n"
        "  - apply:\n"
        "      sshKeys:\n"
        "        - name: k\n"
        "          publicKey: ssh-ed25519 AAAA\n"
    )
    assert preprocess_build_yaml(body, str(tmp_path)) == body


def test_preprocess_missing_file_raises(tmp_path: Path) -> None:
    body = (
        "pipeline:\n"
        "  - build:\n"
        "      provisioners:\n"
        "        - file:\n"
        "            from: nope.bin\n"
    )
    with pytest.raises(OSError):
        preprocess_build_yaml(body, str(tmp_path))
