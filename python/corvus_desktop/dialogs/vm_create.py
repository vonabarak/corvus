"""Dialog for ``crv vm create``."""

from __future__ import annotations

from typing import Any

from PySide6.QtWidgets import (
    QCheckBox,
    QFormLayout,
    QLineEdit,
    QPlainTextEdit,
    QSpinBox,
    QWidget,
)

from .form_dialog import FormDialog


class VmCreateDialog(FormDialog):
    def __init__(self, parent: QWidget | None = None) -> None:
        self._name = QLineEdit()
        self._name.setPlaceholderText("e.g. web-1")
        self._node = QLineEdit()
        self._node.setPlaceholderText(
            "optional — pin to node by name / id (else scheduler picks)"
        )
        self._cpu = QSpinBox()
        self._cpu.setRange(1, 256)
        self._cpu.setValue(2)
        self._ram = QSpinBox()
        self._ram.setRange(64, 1024 * 1024)
        self._ram.setSuffix(" MB")
        self._ram.setValue(2048)
        self._cpu_model = QLineEdit("host")
        self._description = QPlainTextEdit()
        self._description.setMaximumHeight(80)
        self._headless = QCheckBox()
        self._guest_agent = QCheckBox()
        self._cloud_init = QCheckBox()
        self._autostart = QCheckBox()
        self._reboot_quirk = QCheckBox()
        super().__init__("New VM", save_label="Create", parent=parent)

    def build_form(self, form: QFormLayout) -> None:
        form.addRow("Name:", self._name)
        form.addRow("Node:", self._node)
        form.addRow("CPUs:", self._cpu)
        form.addRow("RAM:", self._ram)
        form.addRow("CPU model:", self._cpu_model)
        form.addRow("Description:", self._description)
        form.addRow("Headless:", self._headless)
        form.addRow("Guest agent:", self._guest_agent)
        form.addRow("Cloud-init:", self._cloud_init)
        form.addRow("Autostart:", self._autostart)
        form.addRow("Reboot quirk:", self._reboot_quirk)

    def result_payload(self) -> dict[str, Any] | None:
        name = self._name.text().strip()
        if not name:
            self.show_error("Name is required.")
            return None
        return {
            "name": name,
            "node": self._node.text().strip() or None,
            "cpu_count": self._cpu.value(),
            "ram_mb": self._ram.value(),
            "description": self._description.toPlainText().strip() or None,
            "cpu_model": self._cpu_model.text().strip() or "host",
            "headless": self._headless.isChecked(),
            "guest_agent": self._guest_agent.isChecked(),
            "cloud_init": self._cloud_init.isChecked(),
            "autostart": self._autostart.isChecked(),
            "reboot_quirk": self._reboot_quirk.isChecked(),
        }
