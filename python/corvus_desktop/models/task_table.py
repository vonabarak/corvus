"""``QAbstractTableModel`` over ``list[corvus_client.types.TaskInfo]``.

Column layout mirrors the web UI's TaskList (frontend/src/pages/TaskList.tsx).
The model is read-only — task creation/cancellation happens elsewhere; this
just renders a snapshot fed by ``CorvusBridge.task_list_ready``.
"""

from __future__ import annotations

from datetime import datetime, timezone
from typing import Any

from corvus_client.types import TaskInfo
from PySide6.QtCore import QAbstractTableModel, QModelIndex, QPersistentModelIndex, Qt

# Module-level sentinel so ``rowCount(parent=...)`` / ``columnCount(parent=...)``
# can keep the Qt default-argument shape without violating B008 (no QObject
# construction in the def-line default).
_INVALID_PARENT: QModelIndex = QModelIndex()


def _fmt_duration_s(seconds: float) -> str:
    """Compact duration: "12s", "3m", "1h 04m", "2d 03h"."""
    if seconds < 0:
        seconds = 0
    s = int(seconds)
    if s < 60:
        return f"{s}s"
    m, s = divmod(s, 60)
    if m < 60:
        return f"{m}m {s:02d}s" if s else f"{m}m"
    h, m = divmod(m, 60)
    if h < 24:
        return f"{h}h {m:02d}m" if m else f"{h}h"
    d, h = divmod(h, 24)
    return f"{d}d {h:02d}h" if h else f"{d}d"


def _fmt_age(dt: datetime) -> str:
    """Relative-age string like ``"5m ago"`` or ``"2025-01-03 11:04"``
    when the timestamp is more than a day old."""
    now = datetime.now(timezone.utc)
    if dt.tzinfo is None:
        # Daemon timestamps are UTC; assume so when naive.
        dt = dt.replace(tzinfo=timezone.utc)
    delta = (now - dt).total_seconds()
    if delta < 86400:
        return f"{_fmt_duration_s(delta)} ago"
    return dt.strftime("%Y-%m-%d %H:%M")


# Column indices kept as class-level constants so the view layer can
# refer to them by name (model.COL_RESULT) without magic numbers.
class TaskTableModel(QAbstractTableModel):
    """Renders a flat list of :class:`TaskInfo` for ``QTableView``."""

    COL_ID = 0
    COL_STARTED = 1
    COL_SUBSYSTEM = 2
    COL_COMMAND = 3
    COL_ENTITY = 4
    COL_RESULT = 5
    COL_DURATION = 6
    COL_CLIENT = 7
    COLS: tuple[str, ...] = (
        "ID",
        "Started",
        "Subsystem",
        "Command",
        "Entity",
        "Result",
        "Duration",
        "Client",
    )

    def __init__(self, parent: Any = None) -> None:
        super().__init__(parent)
        self._tasks: list[TaskInfo] = []

    def set_tasks(self, tasks: list[TaskInfo]) -> None:
        """Replace the entire backing list. Phase 2 doesn't try
        incremental diffs — a fresh snapshot every refresh is good
        enough at our list sizes (≤500 daemon-side cap)."""
        self.beginResetModel()
        self._tasks = list(tasks)
        self.endResetModel()

    def task_at(self, row: int) -> TaskInfo | None:
        if 0 <= row < len(self._tasks):
            return self._tasks[row]
        return None

    # ------------------------------------------------------------ Qt API

    def rowCount(
        self, parent: QModelIndex | QPersistentModelIndex = _INVALID_PARENT
    ) -> int:
        return 0 if parent.isValid() else len(self._tasks)

    def columnCount(
        self, parent: QModelIndex | QPersistentModelIndex = _INVALID_PARENT
    ) -> int:
        return 0 if parent.isValid() else len(self.COLS)

    def headerData(
        self,
        section: int,
        orientation: Qt.Orientation,
        role: int = Qt.ItemDataRole.DisplayRole,
    ) -> Any:
        if (
            orientation == Qt.Orientation.Horizontal
            and role == Qt.ItemDataRole.DisplayRole
            and 0 <= section < len(self.COLS)
        ):
            return self.COLS[section]
        return None

    def data(
        self,
        index: QModelIndex | QPersistentModelIndex,
        role: int = Qt.ItemDataRole.DisplayRole,
    ) -> Any:
        if not index.isValid():
            return None
        task = self.task_at(index.row())
        if task is None:
            return None
        col = index.column()
        if role == Qt.ItemDataRole.DisplayRole:
            return self._display(task, col)
        if role == Qt.ItemDataRole.TextAlignmentRole and col in (
            self.COL_ID,
            self.COL_DURATION,
        ):
            return int(Qt.AlignmentFlag.AlignRight | Qt.AlignmentFlag.AlignVCenter)
        return None

    # --------------------------------------------------------- internals

    def _display(self, task: TaskInfo, col: int) -> Any:
        if col == self.COL_ID:
            return task.id
        if col == self.COL_STARTED:
            return _fmt_age(task.started_at)
        if col == self.COL_SUBSYSTEM:
            return task.subsystem
        if col == self.COL_COMMAND:
            return task.command
        if col == self.COL_ENTITY:
            return task.entity.name if task.entity else ""
        if col == self.COL_RESULT:
            return task.result
        if col == self.COL_DURATION:
            if task.finished_at is None:
                return ""
            started = task.started_at
            finished = task.finished_at
            if started.tzinfo is None:
                started = started.replace(tzinfo=timezone.utc)
            if finished.tzinfo is None:
                finished = finished.replace(tzinfo=timezone.utc)
            return _fmt_duration_s((finished - started).total_seconds())
        if col == self.COL_CLIENT:
            return task.client_name
        return None
