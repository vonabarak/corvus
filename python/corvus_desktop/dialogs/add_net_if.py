"""Dialog for ``crv net-if add``."""

from __future__ import annotations

from typing import TYPE_CHECKING, Any

from PySide6.QtWidgets import QComboBox, QFormLayout, QLineEdit, QWidget

from ..widgets.entity_combo import EntityCombo
from .form_dialog import FormDialog

if TYPE_CHECKING:
    from ..client_bridge import CorvusBridge


_TYPES = ("user", "tap", "bridge", "macvtap", "managed")


class AddNetIfDialog(FormDialog):
    def __init__(self, bridge: CorvusBridge, parent: QWidget | None = None) -> None:
        self._type = QComboBox()
        for t in _TYPES:
            self._type.addItem(t)
        self._type.setCurrentText("user")
        self._host_device = QLineEdit()
        self._host_device.setPlaceholderText("e.g. enp4s0 (bridge / tap / macvtap)")
        self._mac = QLineEdit()
        self._mac.setPlaceholderText("optional — generated if blank")
        self._network = EntityCombo(
            bridge.request_network_list,
            bridge.network_list_ready,
            placeholder="— optional managed network —",
        )
        super().__init__("Add network interface", save_label="Add", parent=parent)

    def build_form(self, form: QFormLayout) -> None:
        form.addRow("Type:", self._type)
        form.addRow("Host device:", self._host_device)
        form.addRow("MAC:", self._mac)
        form.addRow("Network:", self._network)

    def result_payload(self) -> dict[str, Any] | None:
        return {
            "type": self._type.currentText(),
            "host_device": self._host_device.text().strip() or None,
            "mac_address": self._mac.text().strip() or None,
            "network_ref": self._network.selected_id(),
        }
