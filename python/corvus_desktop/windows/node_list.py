"""Node list screen — table + Add / Refresh."""

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

from ..dialogs.node_add import NodeAddDialog
from ..models.node_table import NodeTableModel

if TYPE_CHECKING:
    from ..client_bridge import CorvusBridge


class NodeListWidget(QWidget):
    node_activated = Signal(int)

    def __init__(self, bridge: CorvusBridge, parent: QWidget | None = None) -> None:
        super().__init__(parent)
        self._bridge = bridge
        self._model = NodeTableModel(self)

        self._add_btn = QPushButton("+ Add node")
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
            NodeTableModel.COL_NAME, QHeaderView.ResizeMode.Stretch
        )
        self._table.doubleClicked.connect(self._on_double_click)

        layout = QVBoxLayout(self)
        layout.addLayout(toolbar)
        layout.addWidget(self._table, 1)

        bridge.node_list_ready.connect(self._on_nodes)
        bridge.node_action_completed.connect(lambda *_: self.refresh())

    def refresh(self) -> None:
        self._bridge.request_node_list()

    def _on_add(self) -> None:
        dlg = NodeAddDialog(self)
        if dlg.exec():
            self._bridge.node_create(**dlg.payload())

    def _on_nodes(self, nodes: Any) -> None:
        self._model.set_nodes(nodes)

    def _on_double_click(self, index: Any) -> None:
        node = self._model.node_at(index.row())
        if node is not None:
            self.node_activated.emit(node.id)
