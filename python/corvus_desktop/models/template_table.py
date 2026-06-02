"""``QAbstractTableModel`` over ``list[corvus_client.types.TemplateVmInfo]``."""

from __future__ import annotations

from typing import Any

from corvus_client.types import TemplateVmInfo
from PySide6.QtCore import QAbstractTableModel, QModelIndex, QPersistentModelIndex, Qt

_INVALID_PARENT: QModelIndex = QModelIndex()


def _flags(t: TemplateVmInfo) -> str:
    parts: list[str] = []
    if t.guest_agent:
        parts.append("QGA")
    if t.autostart:
        parts.append("auto")
    if t.headless:
        parts.append("headless")
    return " ".join(parts)


class TemplateTableModel(QAbstractTableModel):
    COL_NAME = 0
    COL_CPU = 1
    COL_RAM = 2
    COL_FLAGS = 3
    COL_DESCRIPTION = 4
    COLS: tuple[str, ...] = ("Name", "CPU", "RAM (MB)", "Flags", "Description")

    def __init__(self, parent: Any = None) -> None:
        super().__init__(parent)
        self._templates: list[TemplateVmInfo] = []

    def set_templates(self, templates: list[TemplateVmInfo]) -> None:
        self.beginResetModel()
        self._templates = list(templates)
        self.endResetModel()

    def template_at(self, row: int) -> TemplateVmInfo | None:
        if 0 <= row < len(self._templates):
            return self._templates[row]
        return None

    def rowCount(
        self, parent: QModelIndex | QPersistentModelIndex = _INVALID_PARENT
    ) -> int:
        return 0 if parent.isValid() else len(self._templates)

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
        t = self.template_at(index.row())
        if t is None:
            return None
        col = index.column()
        if role == Qt.ItemDataRole.DisplayRole:
            if col == self.COL_NAME:
                return t.name
            if col == self.COL_CPU:
                return t.cpu_count
            if col == self.COL_RAM:
                return t.ram_mb
            if col == self.COL_FLAGS:
                return _flags(t)
            if col == self.COL_DESCRIPTION:
                return t.description or ""
        if role == Qt.ItemDataRole.TextAlignmentRole and col in (
            self.COL_CPU,
            self.COL_RAM,
        ):
            return int(Qt.AlignmentFlag.AlignRight | Qt.AlignmentFlag.AlignVCenter)
        return None
