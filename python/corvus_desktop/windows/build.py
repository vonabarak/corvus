"""Build pipeline screen — full-page YAML editor + live event panel.

The pipeline YAML lives in a :class:`YamlEditor` on the left. Clicking
Run streams events through the bridge; each ``BuildStepStart`` opens
a foldable section that ``BuildStepOutput`` / ``BuildStepEnd`` fill.
The final ``BuildPipelineEnd`` shows the per-build artifact summary,
and the task id (if any) navigates to the task detail screen.

Before the YAML is shipped to the daemon, the same client-side
preprocessing that ``crv build`` does is applied: any
``pipeline[].build.provisioners[].shell.script`` /
``...file.from`` / ``pipeline[].build.floppy.from`` file-path
reference is read off disk (relative to the loaded YAML's directory,
falling back to ``cwd`` if the user has typed YAML by hand) and
embedded inline. See :func:`preprocess_build_yaml` for the exact
rewrite rules and ``src/Corvus/Client/Commands/Build.hs`` for the
canonical Haskell implementation we mirror.
"""

from __future__ import annotations

import base64
import os
from typing import TYPE_CHECKING, Any

from corvus_client.types import (
    BuildBuildEnd,
    BuildLogLine,
    BuildPipelineEnd,
    BuildStepEnd,
    BuildStepOutput,
    BuildStepStart,
)
from PySide6.QtCore import Signal
from PySide6.QtGui import QFontDatabase
from PySide6.QtWidgets import (
    QGroupBox,
    QHBoxLayout,
    QLabel,
    QPlainTextEdit,
    QPushButton,
    QScrollArea,
    QSplitter,
    QVBoxLayout,
    QWidget,
)

import yaml

from ..widgets.yaml_editor import YamlEditor, load_yaml_into

if TYPE_CHECKING:
    from ..client_bridge import CorvusBridge


_BUILD_SKELETON = """\
# Image build pipeline — see doc/image-builds.md for the schema.
pipeline:
  - apply:
      sshKeys:
        - name: corvus
          publicKey: "ssh-ed25519 AAAA…"
  - build:
      target:
        name: my-image
        ifExists: error
      strategy: overlay
      template: debian12
      provisioners: []
"""


def preprocess_build_yaml(text: str, base_dir: str) -> str:
    """Inline file/script references in a build pipeline YAML body.

    Mirrors the Haskell ``preprocessRoot`` in
    ``Corvus.Client.Commands.Build``:

    * ``pipeline[].build.provisioners[].shell.script: <path>`` →
      ``shell.inline: <file body as text>``
    * ``pipeline[].build.provisioners[].file.from: <path>`` →
      ``file.content: <base64 of file bytes>``
    * ``pipeline[].build.floppy.from: <path>`` →
      ``floppy.contentBase64: <base64 of file bytes>`` (and
      ``floppy.filename`` defaulted to the basename if unset)

    Paths are resolved relative to ``base_dir`` unless absolute.
    Read errors propagate as :class:`OSError` so the caller can
    surface them to the user.

    Returns the original ``text`` unchanged if there's nothing to
    inline — that way YAML the user typed by hand keeps its comments
    and exact formatting on the wire.
    """
    doc = yaml.safe_load(text)
    if not isinstance(doc, dict):
        return text
    steps = doc.get("pipeline")
    if not isinstance(steps, list):
        return text

    changed = False

    def resolve(rel: str) -> str:
        return rel if os.path.isabs(rel) else os.path.join(base_dir, rel)

    for step in steps:
        if not isinstance(step, dict):
            continue
        build = step.get("build")
        if not isinstance(build, dict):
            continue
        provs = build.get("provisioners")
        if isinstance(provs, list):
            for prov in provs:
                if not isinstance(prov, dict):
                    continue
                shell = prov.get("shell")
                if isinstance(shell, dict) and isinstance(shell.get("script"), str):
                    with open(resolve(shell["script"]), encoding="utf-8") as f:
                        body = f.read()
                    del shell["script"]
                    shell["inline"] = body
                    changed = True
                f_prov = prov.get("file")
                if isinstance(f_prov, dict) and isinstance(f_prov.get("from"), str):
                    with open(resolve(f_prov["from"]), "rb") as fh:
                        data = fh.read()
                    del f_prov["from"]
                    f_prov["content"] = base64.b64encode(data).decode("ascii")
                    changed = True
        floppy = build.get("floppy")
        if isinstance(floppy, dict) and isinstance(floppy.get("from"), str):
            rel = floppy["from"]
            with open(resolve(rel), "rb") as fh:
                data = fh.read()
            floppy.setdefault("filename", os.path.basename(rel))
            del floppy["from"]
            floppy["contentBase64"] = base64.b64encode(data).decode("ascii")
            changed = True

    if not changed:
        return text
    return yaml.safe_dump(doc, sort_keys=False)


class _StepSection(QGroupBox):
    """Foldable section per build step. Output is appended as it arrives."""

    def __init__(self, step_index: int, name: str, command: str) -> None:
        super().__init__(f"Step {step_index + 1}: {name}")
        self._log = QPlainTextEdit()
        self._log.setReadOnly(True)
        mono = QFontDatabase.systemFont(QFontDatabase.SystemFont.FixedFont)
        self._log.setFont(mono)
        self._log.setMaximumBlockCount(20000)
        self._status = QLabel("running…")
        layout = QVBoxLayout(self)
        layout.addWidget(QLabel(command))
        layout.addWidget(self._log, 1)
        layout.addWidget(self._status)

    def append(self, line: str) -> None:
        self._log.appendPlainText(line)

    def finish(self, result: str, message: str | None) -> None:
        text = result
        if message:
            text = f"{result}: {message}"
        self._status.setText(text)
        color = "#22863a" if result == "success" else "#991b1b"
        self._status.setStyleSheet(f"color: {color};")


class BuildWidget(QWidget):
    """Build screen. Bridge events come in through ``build_event`` /
    ``build_started`` / ``build_finished``."""

    # Emitted when the bridge tells us the task id — main_window opens
    # the task detail screen on it.
    task_started = Signal(int)

    def __init__(self, bridge: CorvusBridge, parent: QWidget | None = None) -> None:
        super().__init__(parent)
        self._bridge = bridge
        # Directory used to resolve relative `script:` / `from:`
        # references when preprocessing — set whenever the user loads
        # a YAML file. None means "use cwd" (the hand-typed case).
        self._base_dir: str | None = None

        self._editor = YamlEditor(_BUILD_SKELETON)
        self._editor.validation_changed.connect(self._on_validation)

        self._load_btn = QPushButton("Load file…")
        self._load_btn.clicked.connect(self._on_load)
        self._run_btn = QPushButton("Run")
        self._run_btn.clicked.connect(self._on_run)
        self._status = QLabel("")
        self._status.setStyleSheet("color: #6a737d;")

        toolbar = QHBoxLayout()
        toolbar.addWidget(self._load_btn)
        toolbar.addWidget(self._status, 1)
        toolbar.addWidget(self._run_btn)

        editor_pane = QVBoxLayout()
        editor_pane.addLayout(toolbar)
        editor_pane.addWidget(self._editor, 1)
        editor_widget = QWidget()
        editor_widget.setLayout(editor_pane)

        # Right pane: scrollable list of step sections.
        self._steps_container = QWidget()
        self._steps_layout = QVBoxLayout(self._steps_container)
        self._steps_layout.addStretch(1)
        self._scroll = QScrollArea()
        self._scroll.setWidget(self._steps_container)
        self._scroll.setWidgetResizable(True)

        self._global_log = QPlainTextEdit()
        self._global_log.setReadOnly(True)
        self._global_log.setMaximumBlockCount(5000)
        mono = QFontDatabase.systemFont(QFontDatabase.SystemFont.FixedFont)
        self._global_log.setFont(mono)

        right_pane = QVBoxLayout()
        right_pane.addWidget(self._scroll, 3)
        right_pane.addWidget(QLabel("Pipeline log"))
        right_pane.addWidget(self._global_log, 1)
        right_widget = QWidget()
        right_widget.setLayout(right_pane)

        split = QSplitter()
        split.addWidget(editor_widget)
        split.addWidget(right_widget)
        split.setStretchFactor(0, 2)
        split.setStretchFactor(1, 3)

        layout = QVBoxLayout(self)
        layout.addWidget(split, 1)

        self._sections: dict[int, _StepSection] = {}

        bridge.build_event.connect(self._on_event)
        bridge.build_started.connect(self._on_started)
        bridge.build_finished.connect(self._on_finished)
        self._on_validation(self._editor.validate())

    # ---------------------------------------------------- internals

    def _on_validation(self, err: str) -> None:
        self._run_btn.setEnabled(err == "")
        if err:
            self._status.setText(err)
            self._status.setStyleSheet("color: #991b1b;")
        else:
            self._status.setText("")

    def _on_load(self) -> None:
        path = load_yaml_into(self._editor, self)
        if path:
            self._base_dir = os.path.dirname(path)

    def _on_run(self) -> None:
        base_dir = self._base_dir or os.getcwd()
        try:
            yaml_text = preprocess_build_yaml(self._editor.text(), base_dir)
        except (OSError, yaml.YAMLError) as e:
            # File missing / unreadable, or somehow no longer valid
            # YAML — surface to the user and don't ship to the daemon.
            self._status.setText(f"preprocess error: {e}")
            self._status.setStyleSheet("color: #991b1b;")
            return
        # Reset the right pane.
        self._reset_sections()
        self._global_log.clear()
        self._status.setText("running…")
        self._status.setStyleSheet("color: #6a737d;")
        self._run_btn.setEnabled(False)
        self._bridge.build_run(yaml_text)

    def _on_event(self, event: Any) -> None:
        if isinstance(event, BuildStepStart):
            new_section = _StepSection(event.step_index, event.name, event.command)
            self._sections[event.step_index] = new_section
            # Insert above the trailing stretch.
            self._steps_layout.insertWidget(self._steps_layout.count() - 1, new_section)
        elif isinstance(event, BuildStepOutput):
            existing = self._sections.get(event.step_index)
            if existing is not None:
                existing.append(event.line)
        elif isinstance(event, BuildStepEnd):
            ending = self._sections.get(event.step_index)
            if ending is not None:
                ending.finish(event.result, event.message)
        elif isinstance(event, BuildLogLine):
            self._global_log.appendPlainText(event.line)
        elif isinstance(event, BuildBuildEnd):
            msg = "success" if event.success else f"failed: {event.error_message}"
            if event.artifact_disk_id is not None:
                msg += f" (artifact disk #{event.artifact_disk_id})"
            self._global_log.appendPlainText(f"[build end] {msg}")
        elif isinstance(event, BuildPipelineEnd):
            for b in event.builds:
                line = f"[pipeline] {b.name}: "
                if b.artifact_disk_id is not None:
                    line += f"artifact disk #{b.artifact_disk_id}"
                else:
                    line += b.error_message or "failed"
                self._global_log.appendPlainText(line)

    def _on_started(self, task_id: int) -> None:
        self._status.setText(f"task #{task_id}")
        self.task_started.emit(task_id)

    def _on_finished(self, error: str) -> None:
        self._run_btn.setEnabled(True)
        if error:
            self._status.setText(error)
            self._status.setStyleSheet("color: #991b1b;")
        else:
            self._status.setText("done")
            self._status.setStyleSheet("color: #22863a;")

    def _reset_sections(self) -> None:
        for section in list(self._sections.values()):
            self._steps_layout.removeWidget(section)
            section.deleteLater()
        self._sections.clear()
