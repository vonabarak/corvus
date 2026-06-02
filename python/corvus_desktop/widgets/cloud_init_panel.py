"""Cloud-init panel — drops into VM detail as a tab.

Shows the current ``user_data`` / ``network_config`` blocks in
read-only YAML editors and offers Edit (replace) / Delete actions.
"""

from __future__ import annotations

from typing import TYPE_CHECKING, Any

from corvus_client.types import CloudInitInfo
from PySide6.QtWidgets import (
    QCheckBox,
    QDialog,
    QDialogButtonBox,
    QHBoxLayout,
    QLabel,
    QMessageBox,
    QPushButton,
    QSplitter,
    QTabWidget,
    QVBoxLayout,
    QWidget,
)

from .yaml_editor import YamlEditor

if TYPE_CHECKING:
    from ..client_bridge import CorvusBridge


class _CloudInitEditDialog(QDialog):
    """Modal editor for the user-data + network-config pair."""

    def __init__(
        self,
        current: CloudInitInfo,
        parent: QWidget | None = None,
    ) -> None:
        super().__init__(parent)
        self.setWindowTitle("Edit cloud-init")
        self.setModal(True)
        self.resize(900, 600)

        self._user_data = YamlEditor(current.user_data or "")
        self._network_config = YamlEditor(current.network_config or "")
        self._inject_ssh_keys = QCheckBox("Inject attached SSH keys")
        self._inject_ssh_keys.setChecked(current.inject_ssh_keys)

        tabs = QTabWidget()
        tabs.addTab(self._user_data, "user-data")
        tabs.addTab(self._network_config, "network-config")

        buttons = QDialogButtonBox(QDialogButtonBox.StandardButton.Cancel)
        save = buttons.addButton("Save", QDialogButtonBox.ButtonRole.AcceptRole)
        save.setDefault(True)
        buttons.accepted.connect(self.accept)
        buttons.rejected.connect(self.reject)

        layout = QVBoxLayout(self)
        layout.addWidget(tabs, 1)
        layout.addWidget(self._inject_ssh_keys)
        layout.addWidget(buttons)

    def payload(self) -> dict[str, Any]:
        return {
            "user_data": self._user_data.text() or None,
            "network_config": self._network_config.text() or None,
            "inject_ssh_keys": self._inject_ssh_keys.isChecked(),
        }


class CloudInitPanel(QWidget):
    """Inline view of a VM's cloud-init config + Edit / Delete buttons."""

    def __init__(self, bridge: CorvusBridge, parent: QWidget | None = None) -> None:
        super().__init__(parent)
        self._bridge = bridge
        self._vm_id: int | None = None
        self._current = CloudInitInfo()

        self._user_data = YamlEditor("")
        self._user_data.setReadOnly(True)
        self._network_config = YamlEditor("")
        self._network_config.setReadOnly(True)

        split = QSplitter()
        split.addWidget(_labeled("user-data", self._user_data))
        split.addWidget(_labeled("network-config", self._network_config))

        self._inject = QLabel("inject SSH keys: —")

        self._edit_btn = QPushButton("Edit…")
        self._edit_btn.clicked.connect(self._on_edit)
        self._delete_btn = QPushButton("Delete")
        self._delete_btn.clicked.connect(self._on_delete)
        actions = QHBoxLayout()
        actions.addWidget(self._inject)
        actions.addStretch(1)
        actions.addWidget(self._edit_btn)
        actions.addWidget(self._delete_btn)

        layout = QVBoxLayout(self)
        layout.addWidget(split, 1)
        layout.addLayout(actions)

        bridge.cloud_init_ready.connect(self._on_cloud_init)
        bridge.cloud_init_action_completed.connect(self._on_action)
        # Surface specific cloud-init errors so the user sees what failed.
        bridge.operation_failed.connect(self._on_op_failed)

    # ---------------------------------------------------- public

    def set_vm_id(self, vm_id: int) -> None:
        self._vm_id = vm_id
        self._current = CloudInitInfo()
        self._user_data.set_text("")
        self._network_config.set_text("")
        self._inject.setText("inject SSH keys: —")
        self._bridge.request_cloud_init(vm_id)

    def clear(self) -> None:
        self._vm_id = None

    # ---------------------------------------------------- bridge slots

    def _on_cloud_init(self, vm_id: int, info: Any) -> None:
        if vm_id != self._vm_id:
            return
        if isinstance(info, CloudInitInfo):
            self._current = info
            self._user_data.set_text(info.user_data or "")
            self._network_config.set_text(info.network_config or "")
            self._inject.setText(
                f"inject SSH keys: {'yes' if info.inject_ssh_keys else 'no'}"
            )

    def _on_action(self, vm_id: int, action: str) -> None:
        if vm_id != self._vm_id:
            return
        # Refresh after every mutation.
        self._bridge.request_cloud_init(vm_id)

    def _on_op_failed(self, op: str, message: str) -> None:
        if not op.startswith("cloud_init"):
            return
        # Surface as a small status replacement so the user knows
        # something went wrong; the main window's status bar already
        # carries a transient toast too.

    def _on_edit(self) -> None:
        if self._vm_id is None:
            return
        dlg = _CloudInitEditDialog(self._current, self)
        if dlg.exec():
            p = dlg.payload()
            self._bridge.cloud_init_set(
                self._vm_id,
                user_data=p["user_data"],
                network_config=p["network_config"],
                inject_ssh_keys=p["inject_ssh_keys"],
            )

    def _on_delete(self) -> None:
        if self._vm_id is None:
            return
        reply = QMessageBox.question(
            self,
            "Delete cloud-init",
            "Clear this VM's cloud-init configuration?",
            QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No,
            QMessageBox.StandardButton.No,
        )
        if reply == QMessageBox.StandardButton.Yes:
            self._bridge.cloud_init_delete(self._vm_id)


def _labeled(title: str, widget: QWidget) -> QWidget:
    """Wrap ``widget`` in a vertical layout with a small caption above."""
    holder = QWidget()
    layout = QVBoxLayout(holder)
    layout.setContentsMargins(0, 0, 0, 0)
    label = QLabel(title)
    layout.addWidget(label)
    layout.addWidget(widget, 1)
    return holder
