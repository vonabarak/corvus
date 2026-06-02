"""``QComboBox`` bound to a bridge entity list.

Pattern: a dialog wants to pick a node / disk / network / ssh-key /
template. Each such picker needs to:

1. Trigger the bridge's ``request_<thing>_list`` slot on construction.
2. Receive the resulting list via the matching ``<thing>_list_ready``
   signal.
3. Render entity names but carry the id as ``userData``.

EntityCombo wraps that dance once so every dialog can just construct
one. Adds an optional first item with ``id=None`` for "auto" /
"unset" placement.
"""

from __future__ import annotations

from typing import TYPE_CHECKING, Any

from PySide6.QtCore import QObject, Signal
from PySide6.QtWidgets import QComboBox, QWidget

if TYPE_CHECKING:
    pass


class EntityCombo(QComboBox):
    """``QComboBox`` whose items are entity-id-keyed.

    Pass a callable that triggers the list refresh, and the signal that
    delivers the resulting ``list[X]`` (where each item has ``.id`` and
    ``.name`` attributes). The combo auto-fetches once on construction.
    """

    selection_changed = Signal(object)  # int | None — newly-selected id

    def __init__(
        self,
        list_request: Any,  # bound bridge method; called with no args
        list_signal: Any,  # PySide6.QtCore.SignalInstance — bound bridge signal
        *,
        placeholder: str | None = None,
        parent: QWidget | None = None,
    ) -> None:
        super().__init__(parent)
        self._placeholder = placeholder
        self._list_request = list_request
        if placeholder is not None:
            self.addItem(placeholder, userData=None)
        # Connect before kicking off the request so we don't miss the
        # signal if the worker returns synchronously fast.
        list_signal.connect(self._on_list)
        self.currentIndexChanged.connect(self._on_index_changed)
        list_request()

    # ---------------------------------------------------- public

    def selected_id(self) -> int | None:
        return self.currentData()

    def selected_name(self) -> str | None:
        idx = self.currentIndex()
        if idx < 0:
            return None
        text = self.itemText(idx)
        if self._placeholder is not None and text == self._placeholder:
            return None
        return text

    def select_id(self, entity_id: int | None) -> None:
        for i in range(self.count()):
            if self.itemData(i) == entity_id:
                self.setCurrentIndex(i)
                return

    # ---------------------------------------------------- slots

    def _on_list(self, items: Any) -> None:
        # Preserve the currently-selected id across refresh so the user
        # doesn't lose their pick when a dialog refreshes underneath.
        previous = self.currentData()
        self.blockSignals(True)
        self.clear()
        if self._placeholder is not None:
            self.addItem(self._placeholder, userData=None)
        for item in items:
            name = getattr(item, "name", None) or str(getattr(item, "id", "?"))
            entity_id = getattr(item, "id", None)
            self.addItem(name, userData=entity_id)
        if previous is not None:
            self.select_id(previous)
        self.blockSignals(False)

    def _on_index_changed(self, _index: int) -> None:
        self.selection_changed.emit(self.currentData())


# Helper to expose ``QObject`` as part of the public surface for type
# checkers — keeps the import from being flagged as unused if no other
# code in this module needs it directly.
_ = QObject
