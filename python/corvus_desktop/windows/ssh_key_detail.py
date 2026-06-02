"""SSH-key detail screen — full public key + attached VMs + Delete."""

from __future__ import annotations

from typing import TYPE_CHECKING, Any

from corvus_client.types import SshKeyInfo
from PySide6.QtCore import Signal
from PySide6.QtGui import QFont, QFontDatabase
from PySide6.QtWidgets import (
    QFormLayout,
    QHBoxLayout,
    QLabel,
    QMessageBox,
    QPlainTextEdit,
    QPushButton,
    QVBoxLayout,
    QWidget,
)

if TYPE_CHECKING:
    from ..client_bridge import CorvusBridge


class SshKeyDetailWidget(QWidget):
    back_requested = Signal()

    def __init__(self, bridge: CorvusBridge, parent: QWidget | None = None) -> None:
        super().__init__(parent)
        self._bridge = bridge
        self._key_id: int | None = None

        self._back_btn = QPushButton("← Back to SSH keys")
        self._back_btn.clicked.connect(self.back_requested)
        self._title = QLabel("(no key)")
        f = self._title.font()
        f.setPointSize(f.pointSize() + 4)
        f.setWeight(QFont.Weight.DemiBold)
        self._title.setFont(f)

        self._created = QLabel("—")
        self._attached = QLabel("—")
        self._attached.setWordWrap(True)
        form = QFormLayout()
        form.addRow("Created:", self._created)
        form.addRow("Attached to:", self._attached)

        self._public_key = QPlainTextEdit()
        self._public_key.setReadOnly(True)
        mono = QFontDatabase.systemFont(QFontDatabase.SystemFont.FixedFont)
        self._public_key.setFont(mono)
        self._public_key.setMaximumHeight(120)

        self._delete_btn = QPushButton("Delete key")
        self._delete_btn.clicked.connect(self._on_delete)
        action_row = QHBoxLayout()
        action_row.addStretch(1)
        action_row.addWidget(self._delete_btn)

        layout = QVBoxLayout(self)
        layout.addWidget(self._back_btn)
        layout.addWidget(self._title)
        layout.addLayout(form)
        layout.addWidget(QLabel("Public key:"))
        layout.addWidget(self._public_key)
        layout.addLayout(action_row)
        layout.addStretch(1)

        bridge.ssh_key_list_ready.connect(self._on_keys)
        bridge.ssh_key_action_completed.connect(self._on_action_completed)

    def set_key_id(self, key_id: int) -> None:
        self.clear()
        self._key_id = key_id
        self._title.setText(f"SSH key #{key_id}")
        # The bridge has no per-key fetch; the list carries everything we
        # need, so just trigger a refresh and the list handler fills in.
        self._bridge.request_ssh_key_list()

    def clear(self) -> None:
        self._key_id = None
        self._title.setText("(no key)")
        self._created.setText("—")
        self._attached.setText("—")
        self._public_key.clear()

    def _on_keys(self, keys: Any) -> None:
        if self._key_id is None:
            return
        for key in keys:
            if key.id == self._key_id:
                self._populate(key)
                return

    def _populate(self, key: SshKeyInfo) -> None:
        self._title.setText(key.name)
        self._created.setText(key.created_at.isoformat(sep=" ", timespec="seconds"))
        self._attached.setText(", ".join(v.vm.name for v in key.attached_vms) or "—")
        self._public_key.setPlainText(key.public_key)
        # Can't delete a key that's still attached to a VM.
        self._delete_btn.setEnabled(not key.attached_vms)

    def _on_action_completed(self, key_id: int, action: str) -> None:
        if action == "delete" and key_id == self._key_id:
            self.back_requested.emit()

    def _on_delete(self) -> None:
        if self._key_id is None:
            return
        reply = QMessageBox.question(
            self,
            "Delete SSH key",
            "Delete this SSH key? Detach it from any VM first.",
            QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No,
            QMessageBox.StandardButton.No,
        )
        if reply == QMessageBox.StandardButton.Yes:
            self._bridge.ssh_key_delete(self._key_id)
