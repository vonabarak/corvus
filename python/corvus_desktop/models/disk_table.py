"""``QAbstractTableModel`` over ``list[corvus_client.types.DiskImageInfo]``."""

from __future__ import annotations

from typing import Any

from corvus_client.types import DiskImageInfo
from PySide6.QtCore import QAbstractTableModel, QModelIndex, QPersistentModelIndex, Qt

_INVALID_PARENT: QModelIndex = QModelIndex()


def _fmt_size_mb(size_mb: int | None) -> str:
    if size_mb is None:
        return "—"
    if size_mb < 1024:
        return f"{size_mb} MB"
    return f"{size_mb / 1024:.1f} GB"


class DiskTableModel(QAbstractTableModel):
    """Renders a flat list of :class:`DiskImageInfo` for ``QTableView``."""

    COL_NAME = 0
    COL_FORMAT = 1
    COL_SIZE = 2
    COL_PLACEMENT = 3
    COL_BACKING = 4
    COL_ATTACHED = 5
    COL_EPHEMERAL = 6
    COLS: tuple[str, ...] = (
        "Name",
        "Format",
        "Size",
        "Placement",
        "Backing",
        "Attached",
        "Ephemeral",
    )

    def __init__(self, parent: Any = None) -> None:
        super().__init__(parent)
        self._disks: list[DiskImageInfo] = []

    def set_disks(self, disks: list[DiskImageInfo]) -> None:
        self.beginResetModel()
        self._disks = list(disks)
        self.endResetModel()

    def disk_at(self, row: int) -> DiskImageInfo | None:
        if 0 <= row < len(self._disks):
            return self._disks[row]
        return None

    # ------------------------------------------------------------ Qt API

    def rowCount(
        self, parent: QModelIndex | QPersistentModelIndex = _INVALID_PARENT
    ) -> int:
        return 0 if parent.isValid() else len(self._disks)

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
        disk = self.disk_at(index.row())
        if disk is None:
            return None
        col = index.column()
        if role == Qt.ItemDataRole.DisplayRole:
            if col == self.COL_NAME:
                return disk.name
            if col == self.COL_FORMAT:
                return disk.format
            if col == self.COL_SIZE:
                return _fmt_size_mb(disk.size_mb)
            if col == self.COL_PLACEMENT:
                return ", ".join(p.node.name for p in disk.placements)
            if col == self.COL_BACKING:
                return disk.backing_image.name if disk.backing_image else ""
            if col == self.COL_ATTACHED:
                return ", ".join(a.vm.name for a in disk.attached_to)
            if col == self.COL_EPHEMERAL:
                return "yes" if disk.ephemeral else ""
        if role == Qt.ItemDataRole.TextAlignmentRole and col == self.COL_SIZE:
            return int(Qt.AlignmentFlag.AlignRight | Qt.AlignmentFlag.AlignVCenter)
        return None
