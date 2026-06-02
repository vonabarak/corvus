"""Template detail — summary fields + Instantiate / Edit YAML / Delete."""

from __future__ import annotations

from typing import TYPE_CHECKING, Any

from corvus_client.types import TemplateDetails
from PySide6.QtCore import Signal
from PySide6.QtGui import QFont
from PySide6.QtWidgets import (
    QFormLayout,
    QGroupBox,
    QHBoxLayout,
    QLabel,
    QMessageBox,
    QPushButton,
    QTableWidget,
    QTableWidgetItem,
    QVBoxLayout,
    QWidget,
)

from ..dialogs.template_instantiate import TemplateInstantiateDialog
from ..dialogs.template_yaml import TemplateYamlDialog

if TYPE_CHECKING:
    from ..client_bridge import CorvusBridge


class TemplateDetailWidget(QWidget):
    back_requested = Signal()

    def __init__(self, bridge: CorvusBridge, parent: QWidget | None = None) -> None:
        super().__init__(parent)
        self._bridge = bridge
        self._template_id: int | None = None
        self._template_name: str = ""

        self._back_btn = QPushButton("← Back to templates")
        self._back_btn.clicked.connect(self.back_requested)
        self._title = QLabel("(no template)")
        f = self._title.font()
        f.setPointSize(f.pointSize() + 4)
        f.setWeight(QFont.Weight.DemiBold)
        self._title.setFont(f)

        self._btn_instantiate = QPushButton("Instantiate…")
        self._btn_edit = QPushButton("Edit YAML…")
        self._btn_delete = QPushButton("Delete")
        self._btn_instantiate.clicked.connect(self._on_instantiate)
        self._btn_edit.clicked.connect(self._on_edit)
        self._btn_delete.clicked.connect(self._on_delete)
        actions = QHBoxLayout()
        for b in (self._btn_instantiate, self._btn_edit, self._btn_delete):
            actions.addWidget(b)
        actions.addStretch(1)

        self._cpu = QLabel("—")
        self._ram = QLabel("—")
        self._created = QLabel("—")
        self._description = QLabel("—")
        self._description.setWordWrap(True)
        self._flags = QLabel("—")
        form = QFormLayout()
        form.addRow("CPU:", self._cpu)
        form.addRow("RAM:", self._ram)
        form.addRow("Flags:", self._flags)
        form.addRow("Created:", self._created)
        form.addRow("Description:", self._description)
        summary = QGroupBox("Summary")
        sl = QVBoxLayout(summary)
        sl.addLayout(form)

        self._drives_table = QTableWidget(0, 5)
        self._drives_table.setHorizontalHeaderLabels(
            ["Disk image", "Interface", "Strategy", "Format", "Size MB"]
        )
        self._drives_table.verticalHeader().setVisible(False)
        self._drives_table.horizontalHeader().setStretchLastSection(True)
        self._drives_table.setEditTriggers(QTableWidget.EditTrigger.NoEditTriggers)
        drives_box = QGroupBox("Drives")
        dl = QVBoxLayout(drives_box)
        dl.addWidget(self._drives_table)

        layout = QVBoxLayout(self)
        layout.addWidget(self._back_btn)
        layout.addWidget(self._title)
        layout.addLayout(actions)
        layout.addWidget(summary)
        layout.addWidget(drives_box, 1)

        bridge.template_detail_ready.connect(self._on_detail)
        bridge.template_action_completed.connect(self._on_action_completed)

    def set_template_id(self, template_id: int) -> None:
        self.clear()
        self._template_id = template_id
        self._title.setText(f"Template #{template_id}")
        self._bridge.request_template_detail(template_id)

    def clear(self) -> None:
        self._template_id = None
        self._template_name = ""
        self._title.setText("(no template)")
        for label in (
            self._cpu,
            self._ram,
            self._created,
            self._description,
            self._flags,
        ):
            label.setText("—")
        self._drives_table.setRowCount(0)

    def _on_detail(self, info: Any) -> None:
        if not isinstance(info, TemplateDetails) or info.id != self._template_id:
            return
        self._template_name = info.name
        self._title.setText(info.name)
        self._cpu.setText(str(info.cpu_count))
        self._ram.setText(f"{info.ram_mb} MB")
        self._created.setText(info.created_at.isoformat(sep=" ", timespec="seconds"))
        self._description.setText(getattr(info, "description", "") or "—")
        flags: list[str] = []
        if info.guest_agent:
            flags.append("QGA")
        if info.cloud_init:
            flags.append("cloud-init")
        if info.autostart:
            flags.append("autostart")
        if info.headless:
            flags.append("headless")
        self._flags.setText(", ".join(flags) or "—")
        self._drives_table.setRowCount(len(info.drives))
        for row, d in enumerate(info.drives):
            disk_name = d.disk_image.name if d.disk_image is not None else ""
            self._drives_table.setItem(row, 0, QTableWidgetItem(disk_name))
            self._drives_table.setItem(row, 1, QTableWidgetItem(d.interface))
            self._drives_table.setItem(
                row, 2, QTableWidgetItem(getattr(d, "clone_strategy", ""))
            )
            self._drives_table.setItem(row, 3, QTableWidgetItem(d.format or ""))
            size = getattr(d, "size_mb", None)
            self._drives_table.setItem(
                row, 4, QTableWidgetItem(str(size) if size is not None else "")
            )
        self._drives_table.resizeColumnsToContents()

    def _on_action_completed(self, template_id: int, action: str) -> None:
        if template_id != self._template_id:
            return
        if action == "delete":
            self.back_requested.emit()
            return
        # Update / instantiate → refresh detail.
        self._bridge.request_template_detail(template_id)

    def _on_instantiate(self) -> None:
        if self._template_id is None:
            return
        dlg = TemplateInstantiateDialog(self._bridge, self._template_name, self)
        if dlg.exec():
            p = dlg.payload()
            self._bridge.template_instantiate(self._template_id, p["name"], p["node"])

    def _on_edit(self) -> None:
        if self._template_id is None:
            return
        dlg = TemplateYamlDialog(
            f"Edit {self._template_name}",
            "",  # blank: user re-enters / pastes the full YAML
            save_label="Replace",
            parent=self,
        )
        if dlg.exec():
            self._bridge.template_update(self._template_id, dlg.text())

    def _on_delete(self) -> None:
        if self._template_id is None:
            return
        reply = QMessageBox.question(
            self,
            "Delete template",
            f"Delete template '{self._template_name}'?",
            QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No,
            QMessageBox.StandardButton.No,
        )
        if reply == QMessageBox.StandardButton.Yes:
            self._bridge.template_delete(self._template_id)
