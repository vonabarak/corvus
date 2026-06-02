"""``QAbstractTableModel`` over ``list[corvus_client.types.SshKeyInfo]``."""

from __future__ import annotations

from typing import Any

from corvus_client.types import SshKeyInfo
from PySide6.QtCore import QAbstractTableModel, QModelIndex, QPersistentModelIndex, Qt

_INVALID_PARENT: QModelIndex = QModelIndex()


def _short_pubkey(public_key: str) -> str:
    """Truncate the middle of a key body so the type + comment stay
    visible in a single table cell."""
    parts = public_key.split()
    if len(parts) < 2:
        return public_key
    kind = parts[0]
    body = parts[1]
    comment = " ".join(parts[2:])
    if len(body) > 16:
        body = body[:8] + "…" + body[-6:]
    suffix = f" {comment}" if comment else ""
    return f"{kind} {body}{suffix}"


class SshKeyTableModel(QAbstractTableModel):
    COL_NAME = 0
    COL_PUBLIC_KEY = 1
    COL_ATTACHED = 2
    COLS: tuple[str, ...] = ("Name", "Public key", "Attached to")

    def __init__(self, parent: Any = None) -> None:
        super().__init__(parent)
        self._keys: list[SshKeyInfo] = []

    def set_keys(self, keys: list[SshKeyInfo]) -> None:
        self.beginResetModel()
        self._keys = list(keys)
        self.endResetModel()

    def key_at(self, row: int) -> SshKeyInfo | None:
        if 0 <= row < len(self._keys):
            return self._keys[row]
        return None

    def rowCount(
        self, parent: QModelIndex | QPersistentModelIndex = _INVALID_PARENT
    ) -> int:
        return 0 if parent.isValid() else len(self._keys)

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
        key = self.key_at(index.row())
        if key is None:
            return None
        col = index.column()
        if role == Qt.ItemDataRole.DisplayRole:
            if col == self.COL_NAME:
                return key.name
            if col == self.COL_PUBLIC_KEY:
                return _short_pubkey(key.public_key)
            if col == self.COL_ATTACHED:
                return ", ".join(v.vm.name for v in key.attached_vms)
        return None
