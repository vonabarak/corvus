"""Dialog for ``crv node edit``."""

from __future__ import annotations

from typing import Any

from corvus_client.types import NodeDetails
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


class NodeEditDialog(FormDialog):
    def __init__(self, node: NodeDetails, parent: QWidget | None = None) -> None:
        self._original = node
        self._name = QLineEdit(node.name)
        self._host = QLineEdit(node.host)
        self._node_port = QSpinBox()
        self._node_port.setRange(1, 65535)
        self._node_port.setValue(node.node_agent_port)
        self._net_port = QSpinBox()
        self._net_port.setRange(1, 65535)
        self._net_port.setValue(node.net_agent_port)
        self._base_path = QLineEdit(node.base_path or "")
        self._description = QLineEdit(node.description or "")
        self._admin_state = QComboBox()
        for s in _ADMIN_STATES:
            self._admin_state.addItem(s)
        self._admin_state.setCurrentText(node.admin_state)
        self._netd_disabled = QCheckBox()
        self._netd_disabled.setChecked(node.netd_disabled)
        super().__init__(f"Edit {node.name}", save_label="Save", parent=parent)

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
        payload: dict[str, Any] = {}
        if self._name.text() != self._original.name:
            payload["name"] = self._name.text()
        if self._host.text() != self._original.host:
            payload["host"] = self._host.text()
        if self._node_port.value() != self._original.node_agent_port:
            payload["node_agent_port"] = self._node_port.value()
        if self._net_port.value() != self._original.net_agent_port:
            payload["net_agent_port"] = self._net_port.value()
        if self._base_path.text() != (self._original.base_path or ""):
            payload["base_path"] = self._base_path.text()
        if self._description.text() != (self._original.description or ""):
            payload["description"] = self._description.text()
        if self._admin_state.currentText() != self._original.admin_state:
            payload["admin_state"] = self._admin_state.currentText()
        if self._netd_disabled.isChecked() != self._original.netd_disabled:
            payload["netd_disabled"] = self._netd_disabled.isChecked()
        if not payload:
            self.show_error("No changes to save.")
            return None
        return payload
