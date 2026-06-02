"""Disk list screen — table + refresh."""

from __future__ import annotations

from typing import TYPE_CHECKING, Any

from PySide6.QtCore import Signal
from PySide6.QtWidgets import (
    QAbstractItemView,
    QHBoxLayout,
    QHeaderView,
    QPushButton,
    QTableView,
    QVBoxLayout,
    QWidget,
)

from ..dialogs.disk_create import DiskCreateDialog
from ..models.disk_table import DiskTableModel

if TYPE_CHECKING:
    from ..client_bridge import CorvusBridge


class DiskListWidget(QWidget):
    disk_activated = Signal(int)  # disk_id

    def __init__(self, bridge: CorvusBridge, parent: QWidget | None = None) -> None:
        super().__init__(parent)
        self._bridge = bridge
        self._model = DiskTableModel(self)

        self._new_btn = QPushButton("+ New disk")
        self._new_btn.clicked.connect(self._on_new)
        self._refresh_btn = QPushButton("Refresh")
        self._refresh_btn.clicked.connect(self.refresh)
        toolbar = QHBoxLayout()
        toolbar.addWidget(self._new_btn)
        toolbar.addStretch(1)
        toolbar.addWidget(self._refresh_btn)

        self._table = QTableView()
        self._table.setModel(self._model)
        self._table.setSelectionBehavior(QAbstractItemView.SelectionBehavior.SelectRows)
        self._table.setSelectionMode(QAbstractItemView.SelectionMode.SingleSelection)
        self._table.setEditTriggers(QAbstractItemView.EditTrigger.NoEditTriggers)
        self._table.verticalHeader().setVisible(False)
        self._table.horizontalHeader().setSectionResizeMode(
            QHeaderView.ResizeMode.ResizeToContents
        )
        self._table.horizontalHeader().setSectionResizeMode(
            DiskTableModel.COL_NAME, QHeaderView.ResizeMode.Stretch
        )
        self._table.doubleClicked.connect(self._on_double_click)

        layout = QVBoxLayout(self)
        layout.addLayout(toolbar)
        layout.addWidget(self._table, 1)

        bridge.disk_list_ready.connect(self._on_disks)
        # When a disk-level action completes, refresh so the size /
        # attachment columns reflect the new state.
        bridge.disk_action_completed.connect(lambda *_: self.refresh())

    def refresh(self) -> None:
        self._bridge.request_disk_list()

    def _on_new(self) -> None:
        dlg = DiskCreateDialog(self._bridge, self)
        if dlg.exec():
            p = dlg.payload()
            mode = p["mode"]
            if mode == "blank":
                self._bridge.disk_create(
                    p["name"],
                    p["size_mb"],
                    format=p["format"],
                    ephemeral=p["ephemeral"],
                    node=p["node"],
                )
            elif mode == "overlay":
                self._bridge.disk_overlay(
                    p["name"],
                    p["backing_disk_ref"],
                    ephemeral=p["ephemeral"],
                )
            elif mode == "clone":
                self._bridge.disk_clone(
                    p["source_ref"],
                    p["new_name"],
                    path=p["path"],
                    ephemeral=p["ephemeral"],
                )
            elif mode == "register":
                self._bridge.disk_register(
                    p["name"],
                    p["file_path"],
                    format=p["format"],
                    ephemeral=p["ephemeral"],
                    node=p["node"],
                )
            elif mode == "import_url":
                self._bridge.disk_import_url(
                    p["name"],
                    p["url"],
                    format=p["format"],
                    size_mb=p["size_mb"],
                    ephemeral=p["ephemeral"],
                    node=p["node"],
                )

    def _on_disks(self, disks: Any) -> None:
        self._model.set_disks(disks)

    def _on_double_click(self, index: Any) -> None:
        disk = self._model.disk_at(index.row())
        if disk is not None:
            self.disk_activated.emit(disk.id)
