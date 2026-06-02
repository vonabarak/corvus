"""Dialog for ``crv vm edit``. Pre-fills with current :class:`VmDetails`."""

from __future__ import annotations

from typing import Any

from corvus_client.types import VmDetails
from PySide6.QtWidgets import (
    QCheckBox,
    QFormLayout,
    QLineEdit,
    QPlainTextEdit,
    QSpinBox,
    QWidget,
)

from .form_dialog import FormDialog


class VmEditDialog(FormDialog):
    def __init__(self, vm: VmDetails, parent: QWidget | None = None) -> None:
        self._original = vm
        self._name = QLineEdit(vm.name)
        self._cpu = QSpinBox()
        self._cpu.setRange(1, 256)
        self._cpu.setValue(vm.cpu_count)
        self._ram = QSpinBox()
        self._ram.setRange(64, 1024 * 1024)
        self._ram.setSuffix(" MB")
        self._ram.setValue(vm.ram_mb)
        self._cpu_model = QLineEdit(vm.cpu_model)
        self._description = QPlainTextEdit(vm.description or "")
        self._description.setMaximumHeight(80)
        self._headless = QCheckBox()
        self._headless.setChecked(vm.headless)
        self._guest_agent = QCheckBox()
        self._guest_agent.setChecked(vm.guest_agent)
        self._cloud_init = QCheckBox()
        self._cloud_init.setChecked(vm.cloud_init)
        self._autostart = QCheckBox()
        self._autostart.setChecked(vm.autostart)
        self._reboot_quirk = QCheckBox()
        self._reboot_quirk.setChecked(vm.reboot_quirk)
        super().__init__(f"Edit {vm.name}", save_label="Save", parent=parent)

    def build_form(self, form: QFormLayout) -> None:
        form.addRow("Name:", self._name)
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
        # Send only changed fields — daemon's edit() treats None as
        # "leave alone".
        payload: dict[str, Any] = {}
        if self._name.text() != self._original.name:
            payload["name"] = self._name.text()
        if self._cpu.value() != self._original.cpu_count:
            payload["cpu_count"] = self._cpu.value()
        if self._ram.value() != self._original.ram_mb:
            payload["ram_mb"] = self._ram.value()
        if self._cpu_model.text() != self._original.cpu_model:
            payload["cpu_model"] = self._cpu_model.text()
        new_desc = self._description.toPlainText()
        if new_desc != (self._original.description or ""):
            payload["description"] = new_desc
        for field_name, attr, widget in (
            ("headless", "headless", self._headless),
            ("guest_agent", "guest_agent", self._guest_agent),
            ("cloud_init", "cloud_init", self._cloud_init),
            ("autostart", "autostart", self._autostart),
            ("reboot_quirk", "reboot_quirk", self._reboot_quirk),
        ):
            if widget.isChecked() != getattr(self._original, attr):
                payload[field_name] = widget.isChecked()
        if not payload:
            self.show_error("No changes to save.")
            return None
        return payload
