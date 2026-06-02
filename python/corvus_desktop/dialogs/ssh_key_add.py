"""Dialog for ``crv ssh-key create``."""

from __future__ import annotations

from typing import Any

from PySide6.QtWidgets import QFormLayout, QLineEdit, QPlainTextEdit, QWidget

from .form_dialog import FormDialog


class SshKeyAddDialog(FormDialog):
    """Name + public-key textarea. Validates non-empty fields."""

    def __init__(self, parent: QWidget | None = None) -> None:
        self._name = QLineEdit()
        self._name.setPlaceholderText("e.g. workstation")
        self._public_key = QPlainTextEdit()
        self._public_key.setPlaceholderText(
            "ssh-ed25519 AAAA... user@host  (paste the full public key)"
        )
        self._public_key.setMinimumHeight(120)
        super().__init__("Add SSH key", save_label="Add", parent=parent)

    def build_form(self, form: QFormLayout) -> None:
        form.addRow("Name:", self._name)
        form.addRow("Public key:", self._public_key)

    def result_payload(self) -> dict[str, Any] | None:
        name = self._name.text().strip()
        pubkey = self._public_key.toPlainText().strip()
        if not name:
            self.show_error("Name is required.")
            return None
        if not pubkey:
            self.show_error("Public key is required.")
            return None
        if not (pubkey.startswith("ssh-") or pubkey.startswith("ecdsa-")):
            self.show_error(
                "Public key should start with ssh-rsa / ssh-ed25519 / ecdsa-…"
            )
            return None
        return {"name": name, "public_key": pubkey}
