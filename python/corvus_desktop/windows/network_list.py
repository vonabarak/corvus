"""Network list screen — table + New / Refresh."""

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

from ..dialogs.network_create import NetworkCreateDialog
from ..models.network_table import NetworkTableModel

if TYPE_CHECKING:
    from ..client_bridge import CorvusBridge


class NetworkListWidget(QWidget):
    network_activated = Signal(int)  # network_id

    def __init__(self, bridge: CorvusBridge, parent: QWidget | None = None) -> None:
        super().__init__(parent)
        self._bridge = bridge
        self._model = NetworkTableModel(self)

        self._new_btn = QPushButton("+ New network")
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
            NetworkTableModel.COL_NAME, QHeaderView.ResizeMode.Stretch
        )
        self._table.doubleClicked.connect(self._on_double_click)

        layout = QVBoxLayout(self)
        layout.addLayout(toolbar)
        layout.addWidget(self._table, 1)

        bridge.network_list_ready.connect(self._on_networks)
        bridge.network_action_completed.connect(lambda *_: self.refresh())

    def refresh(self) -> None:
        self._bridge.request_network_list()

    def _on_new(self) -> None:
        dlg = NetworkCreateDialog(self._bridge, self)
        if dlg.exec():
            p = dlg.payload()
            self._bridge.network_create(
                p["name"],
                p["subnet"],
                node=p["node"],
                dhcp=p["dhcp"],
                nat=p["nat"],
                autostart=p["autostart"],
            )

    def _on_networks(self, networks: Any) -> None:
        self._model.set_networks(networks)

    def _on_double_click(self, index: Any) -> None:
        net = self._model.network_at(index.row())
        if net is not None:
            self.network_activated.emit(net.id)
