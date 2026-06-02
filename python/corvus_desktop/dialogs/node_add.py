"""Dialog for ``crv node add``."""

from __future__ import annotations

from typing import Any

from PySide6.QtWidgets import (
    QCheckBox,
    QComboBox,
    QFormLayout,
    QLineEdit,
    QSpinBox,
    QWidget,
)

from .form_dialog import FormDialog

_ADMIN_STATES = ("online", "draining", "maintenance")


class NodeAddDialog(FormDialog):
    def __init__(self, parent: QWidget | None = None) -> None:
        self._name = QLineEdit()
        self._host = QLineEdit()
        self._host.setPlaceholderText("hostname or IPv4 / IPv6")
        self._node_port = QSpinBox()
        self._node_port.setRange(1, 65535)
        self._node_port.setValue(9878)
        self._net_port = QSpinBox()
        self._net_port.setRange(1, 65535)
        self._net_port.setValue(9877)
        self._base_path = QLineEdit("/home/corvus/VMs")
        self._description = QLineEdit()
        self._admin_state = QComboBox()
        for s in _ADMIN_STATES:
            self._admin_state.addItem(s)
        self._netd_disabled = QCheckBox()
        super().__init__("Add node", save_label="Add", parent=parent)

    def build_form(self, form: QFormLayout) -> None:
        form.addRow("Name:", self._name)
        form.addRow("Host:", self._host)
        form.addRow("Node-agent port:", self._node_port)
        form.addRow("Net-agent port:", self._net_port)
        form.addRow("Base path:", self._base_path)
        form.addRow("Description:", self._description)
        form.addRow("Admin state:", self._admin_state)
        form.addRow("netd disabled:", self._netd_disabled)

    def result_payload(self) -> dict[str, Any] | None:
        name = self._name.text().strip()
        host = self._host.text().strip()
        if not name or not host:
            self.show_error("Name and host are required.")
            return None
        return {
            "name": name,
            "host": host,
            "node_agent_port": self._node_port.value(),
            "net_agent_port": self._net_port.value(),
            "base_path": self._base_path.text().strip() or "/home/corvus/VMs",
            "description": self._description.text().strip() or None,
            "admin_state": self._admin_state.currentText(),
            "netd_disabled": self._netd_disabled.isChecked(),
        }
