"""Dialog for ``crv vm edit``. Pre-fills with current :class:`VmDetails`."""

from __future__ import annotations

from typing import cast

from corvus_client.types import VmDetails
from PySide6.QtWidgets import (
    QCheckBox,
    QComboBox,
    QFormLayout,
    QLineEdit,
    QPlainTextEdit,
    QSpinBox,
    QWidget,
)

from corvus_desktop.widgets.size_input import SizeInput

from .form_dialog import FormDialog, FormPayload
from .payload_types import VmEditPayload


class VmEditDialog(FormDialog):
    def payload(self) -> VmEditPayload:
        return cast(VmEditPayload, super().payload())

    def __init__(self, vm: VmDetails, parent: QWidget | None = None) -> None:
        self._original = vm
        self._name = QLineEdit(vm.name)
        self._cpu = QSpinBox()
        self._cpu.setRange(1, 256)
        self._cpu.setValue(vm.cpu_count)
        self._ram = SizeInput(ram=True)
        self._ram.setValue(vm.ram)
        self._cpu_model = QLineEdit(vm.cpu_model)
        self._graphics_adapter = QComboBox()
        self._graphics_adapter.addItems(
            [
                "virtio-vga",
                "qxl-vga",
                "vga",
                "virtio-gpu-pci",
                "virtio-vga-gl",
                "virtio-gpu-gl-pci",
            ]
        )
        self._graphics_adapter.setCurrentText(vm.graphics_adapter)
        self._description = QPlainTextEdit(vm.description or "")
        self._description.setMaximumHeight(80)
        self._headless = QCheckBox()
        self._headless.setChecked(vm.headless)
        self._guest_agent = QCheckBox()
        self._guest_agent.setChecked(vm.guest_agent)
        self._vsock = QCheckBox()
        self._vsock.setChecked(vm.vsock)
        self._balloon = QCheckBox()
        self._balloon.setChecked(vm.balloon)
        self._rng = QCheckBox()
        self._rng.setChecked(vm.rng)
        self._tpm = QCheckBox()
        self._tpm.setChecked(vm.tpm)
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
        form.addRow("Graphics adapter:", self._graphics_adapter)
        form.addRow("Description:", self._description)
        form.addRow("Headless:", self._headless)
        form.addRow("Guest agent:", self._guest_agent)
        form.addRow("VirtIO vsock:", self._vsock)
        form.addRow("VirtIO balloon:", self._balloon)
        form.addRow("VirtIO RNG:", self._rng)
        form.addRow("TPM 2.0:", self._tpm)
        form.addRow("Cloud-init:", self._cloud_init)
        form.addRow("Autostart:", self._autostart)
        form.addRow("Reboot quirk:", self._reboot_quirk)

    def result_payload(self) -> FormPayload | None:
        # Send only changed fields — daemon's edit() treats None as
        # "leave alone".
        payload: dict[str, str | int | bool | None] = {}
        if self._name.text() != self._original.name:
            payload["name"] = self._name.text()
        if self._cpu.value() != self._original.cpu_count:
            payload["cpu_count"] = self._cpu.value()
        if self._ram.value() != self._original.ram:
            payload["ram"] = self._ram.value()
        if self._cpu_model.text() != self._original.cpu_model:
            payload["cpu_model"] = self._cpu_model.text()
        if self._graphics_adapter.currentText() != self._original.graphics_adapter:
            payload["graphics_adapter"] = self._graphics_adapter.currentText()
        new_desc = self._description.toPlainText()
        if new_desc != (self._original.description or ""):
            payload["description"] = new_desc
        for field_name, attr, widget in (
            ("headless", "headless", self._headless),
            ("guest_agent", "guest_agent", self._guest_agent),
            ("vsock", "vsock", self._vsock),
            ("balloon", "balloon", self._balloon),
            ("rng", "rng", self._rng),
            ("tpm", "tpm", self._tpm),
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
