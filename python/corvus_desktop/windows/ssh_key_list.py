"""SSH-key list screen — table + Add / Refresh."""

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

from ..dialogs.ssh_key_add import SshKeyAddDialog
from ..models.ssh_key_table import SshKeyTableModel

if TYPE_CHECKING:
    from ..client_bridge import CorvusBridge


class SshKeyListWidget(QWidget):
    key_activated = Signal(int)  # key_id

    def __init__(self, bridge: CorvusBridge, parent: QWidget | None = None) -> None:
        super().__init__(parent)
        self._bridge = bridge
        self._model = SshKeyTableModel(self)

        self._add_btn = QPushButton("+ Add SSH key")
        self._add_btn.clicked.connect(self._on_add)
        self._refresh_btn = QPushButton("Refresh")
        self._refresh_btn.clicked.connect(self.refresh)

        toolbar = QHBoxLayout()
        toolbar.addWidget(self._add_btn)
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
            SshKeyTableModel.COL_PUBLIC_KEY, QHeaderView.ResizeMode.Stretch
        )
        self._table.doubleClicked.connect(self._on_double_click)

        layout = QVBoxLayout(self)
        layout.addLayout(toolbar)
        layout.addWidget(self._table, 1)

        bridge.ssh_key_list_ready.connect(self._on_keys)
        bridge.ssh_key_action_completed.connect(lambda *_: self.refresh())

    def refresh(self) -> None:
        self._bridge.request_ssh_key_list()

    def _on_add(self) -> None:
        dlg = SshKeyAddDialog(self)
        if dlg.exec():
            payload = dlg.payload()
            self._bridge.ssh_key_create(payload["name"], payload["public_key"])

    def _on_keys(self, keys: Any) -> None:
        self._model.set_keys(keys)

    def _on_double_click(self, index: Any) -> None:
        key = self._model.key_at(index.row())
        if key is not None:
            self.key_activated.emit(key.id)
