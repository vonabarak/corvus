"""Dialogs for ``crv disk rebase`` / ``copy`` / ``move``.

Each is a small form with the destination input + relevant flags.
``Rebase`` takes another disk (the new backing image). ``Copy`` and
``move`` take a destination node (free-text by id or name in this
phase; Phase 10 swaps in a node combo) and optional destination path.
"""

from __future__ import annotations

from typing import TYPE_CHECKING, Any

from PySide6.QtWidgets import (
    QCheckBox,
    QFormLayout,
    QLineEdit,
    QWidget,
)

from ..widgets.entity_combo import EntityCombo
from .form_dialog import FormDialog

if TYPE_CHECKING:
    from ..client_bridge import CorvusBridge


class DiskRebaseDialog(FormDialog):
    def __init__(self, bridge: CorvusBridge, parent: QWidget | None = None) -> None:
        self._backing = EntityCombo(
            bridge.request_disk_list,
            bridge.disk_list_ready,
            placeholder="— pick new backing disk —",
        )
        super().__init__("Rebase disk", save_label="Rebase", parent=parent)

    def build_form(self, form: QFormLayout) -> None:
        form.addRow("New backing:", self._backing)

    def result_payload(self) -> dict[str, Any] | None:
        if self._backing.selected_id() is None:
            self.show_error("Pick a backing disk.")
            return None
        return {"new_backing_disk_ref": self._backing.selected_id()}


class DiskCopyMoveDialog(FormDialog):
    """One class drives both `copy` and `move` — the mode just changes
    the title + the bridge slot the caller invokes."""

    def __init__(
        self,
        mode: str,
        parent: QWidget | None = None,
    ) -> None:
        assert mode in ("copy", "move")
        self._mode = mode
        self._to_node = QLineEdit()
        self._to_node.setPlaceholderText("destination node name or id")
        self._to_path = QLineEdit()
        self._to_path.setPlaceholderText("optional destination path")
        self._with_backing = QCheckBox("With backing chain")
        super().__init__(
            f"{mode.title()} disk",
            save_label=mode.title(),
            parent=parent,
        )

    def build_form(self, form: QFormLayout) -> None:
        form.addRow("To node:", self._to_node)
        form.addRow("To path:", self._to_path)
        form.addRow("", self._with_backing)

    def result_payload(self) -> dict[str, Any] | None:
        node = self._to_node.text().strip()
        if not node:
            self.show_error("Destination node is required.")
            return None
        return {
            "to_node_ref": node,
            "to_path": self._to_path.text().strip() or None,
            "with_backing_chain": self._with_backing.isChecked(),
        }
