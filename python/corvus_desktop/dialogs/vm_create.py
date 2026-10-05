"""Dialog for ``crv vm create``."""

from __future__ import annotations

from typing import cast

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

from .form_dialog import FormDialog
from .payload_types import VmCreatePayload


class VmCreateDialog(FormDialog):
    def payload(self) -> VmCreatePayload:
        return cast(VmCreatePayload, super().payload())

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
        self._ram = SizeInput(ram=True)
        self._ram.setValue(2 * 1024**3)
        self._cpu_model = QLineEdit("host")
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
        self._description = QPlainTextEdit()
        self._description.setMaximumHeight(80)
        self._headless = QCheckBox()
        self._guest_agent = QCheckBox()
        self._vsock = QCheckBox()
        self._vsock.setChecked(True)
        self._balloon = QCheckBox()
        self._balloon.setChecked(True)
        self._rng = QCheckBox()
        self._rng.setChecked(True)
        self._tpm = QCheckBox()
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

    def result_payload(self) -> VmCreatePayload | None:
        name = self._name.text().strip()
        if not name:
            self.show_error("Name is required.")
            return None
        return {
            "name": name,
            "node": self._node.text().strip() or None,
            "cpu_count": self._cpu.value(),
            "ram": self._ram.value(),
            "description": self._description.toPlainText().strip() or None,
            "cpu_model": self._cpu_model.text().strip() or "host",
            "graphics_adapter": self._graphics_adapter.currentText(),
            "headless": self._headless.isChecked(),
            "guest_agent": self._guest_agent.isChecked(),
            "vsock": self._vsock.isChecked(),
            "balloon": self._balloon.isChecked(),
            "rng": self._rng.isChecked(),
            "tpm": self._tpm.isChecked(),
            "cloud_init": self._cloud_init.isChecked(),
            "autostart": self._autostart.isChecked(),
            "reboot_quirk": self._reboot_quirk.isChecked(),
        }
