"""VM list screen: table + refresh button.

Bound to :class:`CorvusBridge.vm_list_ready`. Like the task list, we
refresh on open and on user request rather than auto-polling.
"""

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

from ..dialogs.vm_create import VmCreateDialog
from ..models.vm_table import VmTableModel

if TYPE_CHECKING:
    from ..client_bridge import CorvusBridge


class VmListWidget(QWidget):
    """The VMs panel — table + refresh."""

    vm_activated = Signal(int)  # vm_id

    def __init__(self, bridge: CorvusBridge, parent: QWidget | None = None) -> None:
        super().__init__(parent)
        self._bridge = bridge
        self._model = VmTableModel(self)

        self._new_btn = QPushButton("+ New VM")
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
            VmTableModel.COL_NAME, QHeaderView.ResizeMode.Stretch
        )
        self._table.doubleClicked.connect(self._on_double_click)

        layout = QVBoxLayout(self)
        layout.addLayout(toolbar)
        layout.addWidget(self._table, 1)

        bridge.vm_list_ready.connect(self._on_vms)
        # When an action succeeds, refresh so the status badges
        # reflect the new state.
        bridge.vm_action_completed.connect(lambda *_: self.refresh())
        bridge.vm_edit_completed.connect(lambda *_: self.refresh())

    def refresh(self) -> None:
        self._bridge.request_vm_list()

    def _on_new(self) -> None:
        dlg = VmCreateDialog(self)
        if dlg.exec():
            self._bridge.vm_create(**dlg.payload())

    def _on_vms(self, vms: Any) -> None:
        self._model.set_vms(vms)

    def _on_double_click(self, index: Any) -> None:
        vm = self._model.vm_at(index.row())
        if vm is not None:
            self.vm_activated.emit(vm.id)
