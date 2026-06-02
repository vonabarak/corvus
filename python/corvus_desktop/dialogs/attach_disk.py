"""Dialog for ``crv disk attach`` — pick a disk + interface options."""

from __future__ import annotations

from typing import TYPE_CHECKING, Any

from PySide6.QtWidgets import (
    QCheckBox,
    QComboBox,
    QFormLayout,
    QWidget,
)

from ..widgets.entity_combo import EntityCombo
from .form_dialog import FormDialog

if TYPE_CHECKING:
    from ..client_bridge import CorvusBridge


_INTERFACES = ("virtio", "scsi", "ide", "sata")
_MEDIA = ("disk", "cdrom")
_CACHE = ("default", "writeback", "none", "writethrough", "directsync", "unsafe")


class AttachDiskDialog(FormDialog):
    def __init__(self, bridge: CorvusBridge, parent: QWidget | None = None) -> None:
        self._bridge = bridge
        self._disk = EntityCombo(
            bridge.request_disk_list,
            bridge.disk_list_ready,
            placeholder="— select disk —",
        )
        self._interface = _make_combo(_INTERFACES, default="virtio")
        self._media = _make_combo(_MEDIA, default="disk")
        self._cache = _make_combo(_CACHE, default="default")
        self._read_only = QCheckBox()
        self._discard = QCheckBox()
        super().__init__("Attach disk", save_label="Attach", parent=parent)

    def build_form(self, form: QFormLayout) -> None:
        form.addRow("Disk:", self._disk)
        form.addRow("Interface:", self._interface)
        form.addRow("Media:", self._media)
        form.addRow("Cache:", self._cache)
        form.addRow("Read-only:", self._read_only)
        form.addRow("Discard:", self._discard)

    def result_payload(self) -> dict[str, Any] | None:
        disk_id = self._disk.selected_id()
        if disk_id is None:
            self.show_error("Pick a disk.")
            return None
        return {
            "disk_ref": disk_id,
            "interface": self._interface.currentText(),
            "media": self._media.currentText(),
            "read_only": self._read_only.isChecked(),
            "cache_type": (
                None
                if self._cache.currentText() == "default"
                else self._cache.currentText()
            ),
            "discard": self._discard.isChecked(),
        }


def _make_combo(values: tuple[str, ...], *, default: str) -> QComboBox:
    cb = QComboBox()
    for v in values:
        cb.addItem(v)
    if default in values:
        cb.setCurrentText(default)
    return cb
