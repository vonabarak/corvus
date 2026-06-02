"""Dialog for ``crv network edit``.

Patch-style: only fields the user actually changes are sent. The
network object is passed in at construction so the form pre-fills
with current values.
"""

from __future__ import annotations

from typing import Any

from corvus_client.types import NetworkInfo
from PySide6.QtWidgets import QCheckBox, QFormLayout, QLineEdit, QWidget

from .form_dialog import FormDialog


class NetworkEditDialog(FormDialog):
    def __init__(self, network: NetworkInfo, parent: QWidget | None = None) -> None:
        self._original = network
        self._subnet = QLineEdit(network.subnet)
        self._dhcp = QCheckBox()
        self._dhcp.setChecked(network.dhcp)
        self._nat = QCheckBox()
        self._nat.setChecked(network.nat)
        self._autostart = QCheckBox()
        self._autostart.setChecked(network.autostart)
        super().__init__(f"Edit {network.name}", save_label="Save", parent=parent)

    def build_form(self, form: QFormLayout) -> None:
        form.addRow("Subnet (CIDR):", self._subnet)
        form.addRow("DHCP:", self._dhcp)
        form.addRow("NAT:", self._nat)
        form.addRow("Autostart:", self._autostart)

    def result_payload(self) -> dict[str, Any] | None:
        subnet = self._subnet.text().strip()
        if "/" not in subnet:
            self.show_error("Subnet must be a CIDR string, e.g. 10.42.0.0/24.")
            return None
        # Send only the changed fields — None means "leave alone".
        payload: dict[str, Any] = {}
        if subnet != self._original.subnet:
            payload["subnet"] = subnet
        if self._dhcp.isChecked() != self._original.dhcp:
            payload["dhcp"] = self._dhcp.isChecked()
        if self._nat.isChecked() != self._original.nat:
            payload["nat"] = self._nat.isChecked()
        if self._autostart.isChecked() != self._original.autostart:
            payload["autostart"] = self._autostart.isChecked()
        if not payload:
            self.show_error("No changes to save.")
            return None
        return payload
