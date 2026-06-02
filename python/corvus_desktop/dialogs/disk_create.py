"""Tabbed dialog for the five disk-creation flows.

Mirrors the React DiskCreate stepper: Blank / Overlay / Clone /
Register / Import-URL. Each tab returns its own payload shape — the
caller routes to the matching ``CorvusBridge.disk_*`` slot based on
``payload()["mode"]``.
"""

from __future__ import annotations

from typing import TYPE_CHECKING, Any

from PySide6.QtWidgets import (
    QCheckBox,
    QComboBox,
    QDialog,
    QDialogButtonBox,
    QFormLayout,
    QLabel,
    QLineEdit,
    QSpinBox,
    QTabWidget,
    QVBoxLayout,
    QWidget,
)

from ..widgets.entity_combo import EntityCombo

if TYPE_CHECKING:
    from ..client_bridge import CorvusBridge


_FORMATS = ("qcow2", "raw", "vmdk", "vdi", "vpc", "vhdx")


def _format_combo(default: str = "qcow2") -> QComboBox:
    cb = QComboBox()
    for f in _FORMATS:
        cb.addItem(f)
    cb.setCurrentText(default)
    return cb


class _BlankTab(QWidget):
    def __init__(self, bridge: CorvusBridge) -> None:
        super().__init__()
        self.name = QLineEdit()
        self.size_mb = QSpinBox()
        self.size_mb.setRange(1, 4 * 1024 * 1024)
        self.size_mb.setValue(10 * 1024)
        self.size_mb.setSuffix(" MB")
        self.format = _format_combo()
        self.node = EntityCombo(
            bridge.request_node_list,
            bridge.node_list_ready,
            placeholder="— auto (scheduler picks) —",
        )
        self.ephemeral = QCheckBox()
        form = QFormLayout(self)
        form.addRow("Name:", self.name)
        form.addRow("Size:", self.size_mb)
        form.addRow("Format:", self.format)
        form.addRow("Node:", self.node)
        form.addRow("Ephemeral:", self.ephemeral)

    def payload(self) -> dict[str, Any] | None:
        if not self.name.text().strip():
            return None
        return {
            "mode": "blank",
            "name": self.name.text().strip(),
            "size_mb": self.size_mb.value(),
            "format": self.format.currentText(),
            "node": self.node.selected_id(),
            "ephemeral": self.ephemeral.isChecked(),
        }


class _OverlayTab(QWidget):
    def __init__(self, bridge: CorvusBridge) -> None:
        super().__init__()
        self.name = QLineEdit()
        self.backing = EntityCombo(
            bridge.request_disk_list,
            bridge.disk_list_ready,
            placeholder="— select backing disk —",
        )
        self.ephemeral = QCheckBox()
        form = QFormLayout(self)
        form.addRow("Name:", self.name)
        form.addRow("Backing disk:", self.backing)
        form.addRow("Ephemeral:", self.ephemeral)

    def payload(self) -> dict[str, Any] | None:
        if not self.name.text().strip() or self.backing.selected_id() is None:
            return None
        return {
            "mode": "overlay",
            "name": self.name.text().strip(),
            "backing_disk_ref": self.backing.selected_id(),
            "ephemeral": self.ephemeral.isChecked(),
        }


class _CloneTab(QWidget):
    def __init__(self, bridge: CorvusBridge) -> None:
        super().__init__()
        self.source = EntityCombo(
            bridge.request_disk_list,
            bridge.disk_list_ready,
            placeholder="— select source disk —",
        )
        self.new_name = QLineEdit()
        self.path = QLineEdit()
        self.path.setPlaceholderText("optional — destination path on daemon")
        self.ephemeral = QCheckBox()
        form = QFormLayout(self)
        form.addRow("Source disk:", self.source)
        form.addRow("New name:", self.new_name)
        form.addRow("Path:", self.path)
        form.addRow("Ephemeral:", self.ephemeral)

    def payload(self) -> dict[str, Any] | None:
        if self.source.selected_id() is None or not self.new_name.text().strip():
            return None
        return {
            "mode": "clone",
            "source_ref": self.source.selected_id(),
            "new_name": self.new_name.text().strip(),
            "path": self.path.text().strip() or None,
            "ephemeral": self.ephemeral.isChecked(),
        }


class _RegisterTab(QWidget):
    def __init__(self, bridge: CorvusBridge) -> None:
        super().__init__()
        self.name = QLineEdit()
        self.file_path = QLineEdit()
        self.file_path.setPlaceholderText("absolute path on the daemon host")
        self.format = _format_combo()
        self.node = EntityCombo(
            bridge.request_node_list,
            bridge.node_list_ready,
            placeholder="— auto (scheduler picks) —",
        )
        self.ephemeral = QCheckBox()
        form = QFormLayout(self)
        form.addRow("Name:", self.name)
        form.addRow("File path:", self.file_path)
        form.addRow("Format:", self.format)
        form.addRow("Node:", self.node)
        form.addRow("Ephemeral:", self.ephemeral)

    def payload(self) -> dict[str, Any] | None:
        if not self.name.text().strip() or not self.file_path.text().strip():
            return None
        return {
            "mode": "register",
            "name": self.name.text().strip(),
            "file_path": self.file_path.text().strip(),
            "format": self.format.currentText(),
            "node": self.node.selected_id(),
            "ephemeral": self.ephemeral.isChecked(),
        }


class _ImportUrlTab(QWidget):
    def __init__(self, bridge: CorvusBridge) -> None:
        super().__init__()
        self.name = QLineEdit()
        self.url = QLineEdit()
        self.url.setPlaceholderText("https://… cloud image URL")
        self.format = _format_combo()
        self.size_mb = QSpinBox()
        self.size_mb.setRange(0, 4 * 1024 * 1024)
        self.size_mb.setSpecialValueText("— don't resize —")
        self.size_mb.setSuffix(" MB")
        self.node = EntityCombo(
            bridge.request_node_list,
            bridge.node_list_ready,
            placeholder="— auto (scheduler picks) —",
        )
        self.ephemeral = QCheckBox()
        form = QFormLayout(self)
        form.addRow("Name:", self.name)
        form.addRow("URL:", self.url)
        form.addRow("Format:", self.format)
        form.addRow("Resize to:", self.size_mb)
        form.addRow("Node:", self.node)
        form.addRow("Ephemeral:", self.ephemeral)

    def payload(self) -> dict[str, Any] | None:
        if not self.name.text().strip() or not self.url.text().strip():
            return None
        size = self.size_mb.value() or None
        return {
            "mode": "import_url",
            "name": self.name.text().strip(),
            "url": self.url.text().strip(),
            "format": self.format.currentText(),
            "size_mb": size,
            "node": self.node.selected_id(),
            "ephemeral": self.ephemeral.isChecked(),
        }


class DiskCreateDialog(QDialog):
    """Tabbed dialog. Returns the payload of the active tab."""

    def __init__(self, bridge: CorvusBridge, parent: QWidget | None = None) -> None:
        super().__init__(parent)
        self.setWindowTitle("New disk")
        self.setModal(True)
        self.resize(540, 360)

        self._tabs = QTabWidget()
        self._blank = _BlankTab(bridge)
        self._overlay = _OverlayTab(bridge)
        self._clone = _CloneTab(bridge)
        self._register = _RegisterTab(bridge)
        self._import_url = _ImportUrlTab(bridge)
        self._tabs.addTab(self._blank, "Blank")
        self._tabs.addTab(self._overlay, "Overlay")
        self._tabs.addTab(self._clone, "Clone")
        self._tabs.addTab(self._register, "Register")
        self._tabs.addTab(self._import_url, "Import URL")

        self._error = QLabel("")
        self._error.setStyleSheet("color: #991b1b;")

        buttons = QDialogButtonBox(QDialogButtonBox.StandardButton.Cancel)
        save = buttons.addButton("Create", QDialogButtonBox.ButtonRole.AcceptRole)
        save.setDefault(True)
        buttons.accepted.connect(self._on_accept)
        buttons.rejected.connect(self.reject)

        layout = QVBoxLayout(self)
        layout.addWidget(self._tabs, 1)
        layout.addWidget(self._error)
        layout.addWidget(buttons)

        self._cached: dict[str, Any] = {}

    def _on_accept(self) -> None:
        tab = self._tabs.currentWidget()
        payload = getattr(tab, "payload", lambda: None)()
        if payload is None:
            self._error.setText("Please fill in the required fields.")
            return
        self._cached = payload
        self.accept()

    def payload(self) -> dict[str, Any]:
        return self._cached
