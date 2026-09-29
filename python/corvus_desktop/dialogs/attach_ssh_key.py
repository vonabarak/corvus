"""Dialog: pick an SSH key from the daemon and attach it to a VM."""

from __future__ import annotations

from typing import TYPE_CHECKING, cast

from PySide6.QtWidgets import QFormLayout, QWidget

from ..widgets.entity_combo import EntityCombo
from .form_dialog import FormDialog
from .payload_types import AttachSshKeyPayload

if TYPE_CHECKING:
    from ..client_bridge import CorvusBridge


class AttachSshKeyDialog(FormDialog):
    def payload(self) -> AttachSshKeyPayload:
        return cast(AttachSshKeyPayload, super().payload())

    def __init__(self, bridge: CorvusBridge, parent: QWidget | None = None) -> None:
        self._key = EntityCombo(
            bridge.request_ssh_key_list,
            bridge.ssh_key_list_ready,
            placeholder="— select SSH key —",
        )
        super().__init__("Attach SSH key", save_label="Attach", parent=parent)

    def build_form(self, form: QFormLayout) -> None:
        form.addRow("Key:", self._key)

    def result_payload(self) -> AttachSshKeyPayload | None:
        key_id = self._key.selected_id()
        if key_id is None:
            self.show_error("Pick a key.")
            return None
        return {"key_ref": key_id}
