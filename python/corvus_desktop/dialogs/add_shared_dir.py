"""Dialog for ``crv shared-dir add``."""

from __future__ import annotations

from typing import Any

from PySide6.QtWidgets import (
    QCheckBox,
    QComboBox,
    QFileDialog,
    QFormLayout,
    QHBoxLayout,
    QLineEdit,
    QPushButton,
    QWidget,
)

from .form_dialog import FormDialog

_CACHE = ("auto", "always", "never")


class AddSharedDirDialog(FormDialog):
    def __init__(self, parent: QWidget | None = None) -> None:
        self._path = QLineEdit()
        self._path.setPlaceholderText("/path on host")
        self._browse_btn = QPushButton("Browse…")
        self._browse_btn.clicked.connect(self._on_browse)
        path_row = QWidget()
        path_layout = QHBoxLayout(path_row)
        path_layout.setContentsMargins(0, 0, 0, 0)
        path_layout.addWidget(self._path, 1)
        path_layout.addWidget(self._browse_btn)
        self._path_row = path_row

        self._tag = QLineEdit()
        self._tag.setPlaceholderText("virtiofs tag, e.g. corvus_host")
        self._cache = QComboBox()
        for v in _CACHE:
            self._cache.addItem(v)
        self._read_only = QCheckBox()
        super().__init__("Add shared directory", save_label="Add", parent=parent)

    def build_form(self, form: QFormLayout) -> None:
        form.addRow("Path:", self._path_row)
        form.addRow("Tag:", self._tag)
        form.addRow("Cache:", self._cache)
        form.addRow("Read-only:", self._read_only)

    def _on_browse(self) -> None:
        path = QFileDialog.getExistingDirectory(self, "Select directory")
        if path:
            self._path.setText(path)

    def result_payload(self) -> dict[str, Any] | None:
        path = self._path.text().strip()
        tag = self._tag.text().strip()
        if not path:
            self.show_error("Path is required.")
            return None
        if not tag:
            self.show_error("Tag is required.")
            return None
        return {
            "path": path,
            "tag": tag,
            "cache": self._cache.currentText() or None,
            "read_only": self._read_only.isChecked(),
        }
