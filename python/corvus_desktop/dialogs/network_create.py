"""Dialog for ``crv network create``."""

from __future__ import annotations

from typing import TYPE_CHECKING, Any

from PySide6.QtWidgets import QCheckBox, QFormLayout, QLineEdit, QWidget

from ..widgets.entity_combo import EntityCombo
from .form_dialog import FormDialog

if TYPE_CHECKING:
    from ..client_bridge import CorvusBridge


class NetworkCreateDialog(FormDialog):
    """Name + subnet + DHCP / NAT / autostart flags + optional node pin."""

    def __init__(self, bridge: CorvusBridge, parent: QWidget | None = None) -> None:
        self._name = QLineEdit()
        self._name.setPlaceholderText("e.g. lab-net")
        self._subnet = QLineEdit()
        self._subnet.setPlaceholderText("e.g. 10.42.0.0/24")
        # Node picker — auto entry maps to None (scheduler picks).
        self._node = EntityCombo(
            bridge.request_node_list,
            bridge.node_list_ready,
            placeholder="— auto (scheduler picks) —",
        )
        self._dhcp = QCheckBox()
        self._nat = QCheckBox()
        self._autostart = QCheckBox()
        super().__init__("Create network", save_label="Create", parent=parent)

    def build_form(self, form: QFormLayout) -> None:
        form.addRow("Name:", self._name)
        form.addRow("Subnet (CIDR):", self._subnet)
        form.addRow("Node:", self._node)
        form.addRow("DHCP:", self._dhcp)
        form.addRow("NAT:", self._nat)
        form.addRow("Autostart:", self._autostart)

    def result_payload(self) -> dict[str, Any] | None:
        name = self._name.text().strip()
        subnet = self._subnet.text().strip()
        if not name:
            self.show_error("Name is required.")
            return None
        if "/" not in subnet:
            self.show_error("Subnet must be a CIDR string, e.g. 10.42.0.0/24.")
            return None
        return {
            "name": name,
            "subnet": subnet,
            "node": self._node.selected_id(),
            "dhcp": self._dhcp.isChecked(),
            "nat": self._nat.isChecked(),
            "autostart": self._autostart.isChecked(),
        }
