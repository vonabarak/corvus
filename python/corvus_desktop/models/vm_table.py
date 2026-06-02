"""``QAbstractTableModel`` over ``list[corvus_client.types.VmInfo]``."""

from __future__ import annotations

from typing import Any

from corvus_client.types import VmInfo
from PySide6.QtCore import QAbstractTableModel, QModelIndex, QPersistentModelIndex, Qt

_INVALID_PARENT: QModelIndex = QModelIndex()


def _flags(vm: VmInfo) -> str:
    """Compact flag column mirroring the web UI."""
    parts: list[str] = []
    if vm.guest_agent:
        parts.append("QGA")
    if vm.cloud_init:
        parts.append("cloud-init")
    if vm.autostart:
        parts.append("auto")
    if vm.headless:
        parts.append("headless")
    return " ".join(parts)


class VmTableModel(QAbstractTableModel):
    """Renders a flat list of :class:`VmInfo` for ``QTableView``."""

    COL_NAME = 0
    COL_NODE = 1
    COL_STATUS = 2
    COL_CPU = 3
    COL_RAM = 4
    COL_FLAGS = 5
    COLS: tuple[str, ...] = ("Name", "Node", "Status", "CPU", "RAM (MB)", "Flags")

    def __init__(self, parent: Any = None) -> None:
        super().__init__(parent)
        self._vms: list[VmInfo] = []

    def set_vms(self, vms: list[VmInfo]) -> None:
        self.beginResetModel()
        self._vms = list(vms)
        self.endResetModel()

    def vm_at(self, row: int) -> VmInfo | None:
        if 0 <= row < len(self._vms):
            return self._vms[row]
        return None

    # ------------------------------------------------------------ Qt API

    def rowCount(
        self, parent: QModelIndex | QPersistentModelIndex = _INVALID_PARENT
    ) -> int:
        return 0 if parent.isValid() else len(self._vms)

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
        vm = self.vm_at(index.row())
        if vm is None:
            return None
        col = index.column()
        if role == Qt.ItemDataRole.DisplayRole:
            if col == self.COL_NAME:
                return vm.name
            if col == self.COL_NODE:
                return vm.node.name if vm.node else ""
            if col == self.COL_STATUS:
                return vm.status
            if col == self.COL_CPU:
                return vm.cpu_count
            if col == self.COL_RAM:
                return vm.ram_mb
            if col == self.COL_FLAGS:
                return _flags(vm)
        if role == Qt.ItemDataRole.TextAlignmentRole and col in (
            self.COL_CPU,
            self.COL_RAM,
        ):
            return int(Qt.AlignmentFlag.AlignRight | Qt.AlignmentFlag.AlignVCenter)
        return None
