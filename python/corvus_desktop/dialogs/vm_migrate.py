"""Dialog for ``crv vm migrate``.

Picks the destination node from the bridge's node list. Returns
``{"to_node_ref": <id>}`` on Save.
"""

from __future__ import annotations

from typing import TYPE_CHECKING, Any

from PySide6.QtWidgets import QFormLayout, QWidget

from ..widgets.entity_combo import EntityCombo
from .form_dialog import FormDialog

if TYPE_CHECKING:
    from ..client_bridge import CorvusBridge


class VmMigrateDialog(FormDialog):
    def __init__(self, bridge: CorvusBridge, parent: QWidget | None = None) -> None:
        self._node = EntityCombo(
            bridge.request_node_list,
            bridge.node_list_ready,
            placeholder="— select destination node —",
        )
        super().__init__("Migrate VM", save_label="Migrate", parent=parent)

    def build_form(self, form: QFormLayout) -> None:
        form.addRow("To node:", self._node)

    def result_payload(self) -> dict[str, Any] | None:
        if self._node.selected_id() is None:
            self.show_error("Pick a destination node.")
            return None
        return {"to_node_ref": self._node.selected_id()}
