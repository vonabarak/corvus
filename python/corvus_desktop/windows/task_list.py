"""Task list screen: filterable table + manual refresh.

Bound to :class:`CorvusBridge` via the ``task_list_ready`` signal. The
web UI auto-polls every 2 s; the desktop refreshes on open + on
``Refresh`` click, which is enough for the workstation use case and
much friendlier to laptop battery.
"""

from __future__ import annotations

from typing import TYPE_CHECKING, Any

from PySide6.QtCore import Signal
from PySide6.QtWidgets import (
    QAbstractItemView,
    QComboBox,
    QHBoxLayout,
    QHeaderView,
    QLabel,
    QPushButton,
    QTableView,
    QVBoxLayout,
    QWidget,
)

from ..models.task_table import TaskTableModel

if TYPE_CHECKING:
    from ..client_bridge import CorvusBridge


# The web UI's subsystem filter dropdown values, mirrored 1:1 from
# frontend/src/pages/TaskList.tsx so the experience is consistent.
_SUBSYSTEMS = (
    "vm",
    "disk",
    "network",
    "ssh-key",
    "template",
    "shared-dir",
    "snapshot",
    "system",
    "apply",
)
_RESULTS = ("running", "success", "error")


class TaskListWidget(QWidget):
    """The Tasks panel — list + filters + refresh."""

    # Forwarded when the user double-clicks a row. The main window
    # listens and opens the task detail screen.
    task_activated = Signal(int)  # task_id

    def __init__(self, bridge: CorvusBridge, parent: QWidget | None = None) -> None:
        super().__init__(parent)
        self._bridge = bridge
        self._model = TaskTableModel(self)

        # ---------------- filters row ----------------
        self._subsystem_combo = QComboBox()
        self._subsystem_combo.addItem("All subsystems", userData=None)
        for s in _SUBSYSTEMS:
            self._subsystem_combo.addItem(s, userData=s)

        self._result_combo = QComboBox()
        self._result_combo.addItem("All results", userData=None)
        for r in _RESULTS:
            self._result_combo.addItem(r, userData=r)

        self._refresh_btn = QPushButton("Refresh")
        self._refresh_btn.clicked.connect(self.refresh)

        filter_row = QHBoxLayout()
        filter_row.addWidget(QLabel("Subsystem:"))
        filter_row.addWidget(self._subsystem_combo)
        filter_row.addSpacing(12)
        filter_row.addWidget(QLabel("Result:"))
        filter_row.addWidget(self._result_combo)
        filter_row.addStretch(1)
        filter_row.addWidget(self._refresh_btn)

        # ---------------- table ----------------
        self._table = QTableView()
        self._table.setModel(self._model)
        self._table.setSelectionBehavior(QAbstractItemView.SelectionBehavior.SelectRows)
        self._table.setSelectionMode(QAbstractItemView.SelectionMode.SingleSelection)
        self._table.setEditTriggers(QAbstractItemView.EditTrigger.NoEditTriggers)
        self._table.setSortingEnabled(False)
        self._table.verticalHeader().setVisible(False)
        self._table.horizontalHeader().setStretchLastSection(False)
        self._table.horizontalHeader().setSectionResizeMode(
            QHeaderView.ResizeMode.ResizeToContents
        )
        # The Command column is variable-length: stretch it so the row
        # uses all the width.
        self._table.horizontalHeader().setSectionResizeMode(
            TaskTableModel.COL_COMMAND, QHeaderView.ResizeMode.Stretch
        )
        self._table.doubleClicked.connect(self._on_double_click)

        # ---------------- layout ----------------
        layout = QVBoxLayout(self)
        layout.addLayout(filter_row)
        layout.addWidget(self._table, 1)

        # Bridge wiring
        bridge.task_list_ready.connect(self._on_tasks)
        # Refresh whenever the user changes a filter.
        self._subsystem_combo.currentIndexChanged.connect(self.refresh)
        self._result_combo.currentIndexChanged.connect(self.refresh)

    # ----------------------------------------------------- public hooks

    def refresh(self) -> None:
        """Re-request the task list with the current filter values."""
        self._bridge.request_task_list(
            limit=50,
            subsystem=self._subsystem_combo.currentData(),
            result=self._result_combo.currentData(),
        )

    # ----------------------------------------------------- slots

    def _on_tasks(self, tasks: Any) -> None:
        # The signal carries `list[TaskInfo]` but is typed `object`.
        # Defer typing checks to the model.
        self._model.set_tasks(tasks)

    def _on_double_click(self, index: Any) -> None:
        task = self._model.task_at(index.row())
        if task is not None:
            self.task_activated.emit(task.id)
