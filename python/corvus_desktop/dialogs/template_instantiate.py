"""Dialog for ``crv template instantiate``: name + optional node picker."""

from __future__ import annotations

from typing import TYPE_CHECKING, Any

from PySide6.QtWidgets import QFormLayout, QLineEdit, QWidget

from ..widgets.entity_combo import EntityCombo
from .form_dialog import FormDialog

if TYPE_CHECKING:
    from ..client_bridge import CorvusBridge


class TemplateInstantiateDialog(FormDialog):
    def __init__(
        self,
        bridge: CorvusBridge,
        template_name: str,
        parent: QWidget | None = None,
    ) -> None:
        self._template_name = template_name
        self._vm_name = QLineEdit()
        self._vm_name.setPlaceholderText("VM name, e.g. web-1")
        # Node picker — fetches from the daemon on construction. The
        # "auto" placeholder lets the scheduler pick.
        self._node = EntityCombo(
            bridge.request_node_list,
            bridge.node_list_ready,
            placeholder="— auto (scheduler picks) —",
        )
        super().__init__(
            f"Instantiate {template_name}", save_label="Create", parent=parent
        )

    def build_form(self, form: QFormLayout) -> None:
        form.addRow("VM name:", self._vm_name)
        form.addRow("Node:", self._node)

    def result_payload(self) -> dict[str, Any] | None:
        name = self._vm_name.text().strip()
        if not name:
            self.show_error("VM name is required.")
            return None
        return {"name": name, "node": self._node.selected_id()}
