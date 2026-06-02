"""``QAbstractTableModel`` over ``list[corvus_client.types.NetworkInfo]``."""

from __future__ import annotations

from typing import Any

from corvus_client.types import NetworkInfo
from PySide6.QtCore import QAbstractTableModel, QModelIndex, QPersistentModelIndex, Qt

_INVALID_PARENT: QModelIndex = QModelIndex()


def _flags(net: NetworkInfo) -> str:
    parts: list[str] = []
    if net.dhcp:
        parts.append("DHCP")
    if net.nat:
        parts.append("NAT")
    if net.autostart:
        parts.append("auto")
    return " ".join(parts)


class NetworkTableModel(QAbstractTableModel):
    COL_NAME = 0
    COL_SUBNET = 1
    COL_STATE = 2
    COL_FLAGS = 3
    COL_PEERS = 4
    COLS: tuple[str, ...] = ("Name", "Subnet", "State", "Flags", "Peer nodes")

    def __init__(self, parent: Any = None) -> None:
        super().__init__(parent)
        self._networks: list[NetworkInfo] = []

    def set_networks(self, networks: list[NetworkInfo]) -> None:
        self.beginResetModel()
        self._networks = list(networks)
        self.endResetModel()

    def network_at(self, row: int) -> NetworkInfo | None:
        if 0 <= row < len(self._networks):
            return self._networks[row]
        return None

    def rowCount(
        self, parent: QModelIndex | QPersistentModelIndex = _INVALID_PARENT
    ) -> int:
        return 0 if parent.isValid() else len(self._networks)

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
        net = self.network_at(index.row())
        if net is None:
            return None
        col = index.column()
        if role == Qt.ItemDataRole.DisplayRole:
            if col == self.COL_NAME:
                return net.name
            if col == self.COL_SUBNET:
                return net.subnet
            if col == self.COL_STATE:
                return "running" if net.running else "stopped"
            if col == self.COL_FLAGS:
                return _flags(net)
            if col == self.COL_PEERS:
                return str(len(net.peer_node_ids))
        return None
