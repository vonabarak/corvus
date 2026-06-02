"""Template list screen — table + New / Refresh."""

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

from ..dialogs.template_yaml import TemplateYamlDialog
from ..models.template_table import TemplateTableModel

if TYPE_CHECKING:
    from ..client_bridge import CorvusBridge


class TemplateListWidget(QWidget):
    template_activated = Signal(int)

    def __init__(self, bridge: CorvusBridge, parent: QWidget | None = None) -> None:
        super().__init__(parent)
        self._bridge = bridge
        self._model = TemplateTableModel(self)

        self._new_btn = QPushButton("+ New template")
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
            TemplateTableModel.COL_DESCRIPTION, QHeaderView.ResizeMode.Stretch
        )
        self._table.doubleClicked.connect(self._on_double_click)

        layout = QVBoxLayout(self)
        layout.addLayout(toolbar)
        layout.addWidget(self._table, 1)

        bridge.template_list_ready.connect(self._on_templates)
        bridge.template_action_completed.connect(lambda *_: self.refresh())

    def refresh(self) -> None:
        self._bridge.request_template_list()

    def _on_new(self) -> None:
        dlg = TemplateYamlDialog("New template", save_label="Create", parent=self)
        if dlg.exec():
            self._bridge.template_create(dlg.text())

    def _on_templates(self, templates: Any) -> None:
        self._model.set_templates(templates)

    def _on_double_click(self, index: Any) -> None:
        t = self._model.template_at(index.row())
        if t is not None:
            self.template_activated.emit(t.id)
