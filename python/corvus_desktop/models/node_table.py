"""``QAbstractTableModel`` over ``list[corvus_client.types.NodeInfo]``."""

from __future__ import annotations

from typing import Any

from corvus_client.types import NodeInfo
from PySide6.QtCore import QAbstractTableModel, QModelIndex, QPersistentModelIndex, Qt

_INVALID_PARENT: QModelIndex = QModelIndex()


def _bytes_human(b: int | None) -> str:
    if b is None or b <= 0:
        return "—"
    gb = b / (1024**3)
    if gb >= 1:
        return f"{gb:.1f} GiB"
    mb = b / (1024**2)
    return f"{mb:.0f} MiB"


class NodeTableModel(QAbstractTableModel):
    COL_NAME = 0
    COL_HOST = 1
    COL_STATE = 2
    COL_CPU = 3
    COL_RAM = 4
    COL_STORAGE = 5
    COL_LOAD = 6
    COL_AGENTS = 7
    COLS: tuple[str, ...] = (
        "Name",
        "Host:port",
        "State",
        "CPU",
        "RAM (free/total)",
        "Storage (free/total)",
        "Load 1m",
        "Agents",
    )

    def __init__(self, parent: Any = None) -> None:
        super().__init__(parent)
        self._nodes: list[NodeInfo] = []

    def set_nodes(self, nodes: list[NodeInfo]) -> None:
        self.beginResetModel()
        self._nodes = list(nodes)
        self.endResetModel()

    def node_at(self, row: int) -> NodeInfo | None:
        if 0 <= row < len(self._nodes):
            return self._nodes[row]
        return None

    def rowCount(
        self, parent: QModelIndex | QPersistentModelIndex = _INVALID_PARENT
    ) -> int:
        return 0 if parent.isValid() else len(self._nodes)

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
        node = self.node_at(index.row())
        if node is None:
            return None
        col = index.column()
        if role == Qt.ItemDataRole.DisplayRole:
            if col == self.COL_NAME:
                return node.name
            if col == self.COL_HOST:
                return f"{node.host}:{node.node_agent_port}"
            if col == self.COL_STATE:
                return node.admin_state
            if col == self.COL_CPU:
                return node.cpu_count
            if col == self.COL_RAM:
                free = node.ram_mb_free
                total = node.ram_mb_total
                if not total:
                    return "—"
                return f"{free} / {total} MB"
            if col == self.COL_STORAGE:
                return (
                    f"{_bytes_human(node.storage_bytes_free)} / "
                    f"{_bytes_human(node.storage_bytes_total)}"
                )
            if col == self.COL_LOAD:
                la = node.load_avg1
                return f"{la:.2f}" if la is not None else "—"
            if col == self.COL_AGENTS:
                pieces = []
                pieces.append("nodeagent" if node.last_node_agent_push_at else "—")
                if not node.netd_disabled:
                    pieces.append("netd" if node.netd_connected else "netd?")
                else:
                    pieces.append("(netd off)")
                return " · ".join(pieces)
        return None
