"""YAML editor dialog used for template create / update."""

from __future__ import annotations

from typing import Any

from PySide6.QtWidgets import QDialog, QDialogButtonBox, QLabel, QVBoxLayout, QWidget

from ..widgets.yaml_editor import YamlEditor

_TEMPLATE_SKELETON = """\
name: "my-template"
description: "Edit this skeleton"
cpuCount: 2
ramMb: 2048
guestAgent: true
drives:
  - diskImageName: "ubuntu-24.04-server-base"
    interface: "virtio"
    strategy: "overlay"
    sizeMb: 10240
networkInterfaces:
  - type: "user"
sshKeys: []
"""


class TemplateYamlDialog(QDialog):
    """Modal YAML editor. Returns ``text()`` on Save (after validation)."""

    def __init__(
        self,
        title: str,
        initial: str = "",
        *,
        save_label: str = "Save",
        parent: QWidget | None = None,
    ) -> None:
        super().__init__(parent)
        self.setWindowTitle(title)
        self.setModal(True)
        self.resize(800, 600)

        if not initial:
            initial = _TEMPLATE_SKELETON
        self._editor = YamlEditor(initial)

        self._status = QLabel("")
        self._status.setStyleSheet("color: #991b1b;")

        buttons = QDialogButtonBox(QDialogButtonBox.StandardButton.Cancel)
        self._save_btn = buttons.addButton(
            save_label, QDialogButtonBox.ButtonRole.AcceptRole
        )
        self._save_btn.setDefault(True)
        buttons.accepted.connect(self._on_accept)
        buttons.rejected.connect(self.reject)

        self._editor.validation_changed.connect(self._on_validation)
        self._on_validation(self._editor.validate())

        layout = QVBoxLayout(self)
        layout.addWidget(self._editor, 1)
        layout.addWidget(self._status)
        layout.addWidget(buttons)

    # ---------------------------------------------------- API

    def text(self) -> str:
        return self._editor.text()

    # ---------------------------------------------------- internals

    def _on_validation(self, err: str) -> None:
        self._save_btn.setEnabled(err == "")
        self._status.setText(err)

    def _on_accept(self) -> None:
        if self._editor.validate():
            return  # disabled save anyway, defensive
        self.accept()


# Keep an attribute on the module so callers can introspect / override.
_: Any = _TEMPLATE_SKELETON
