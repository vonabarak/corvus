"""Apply screen — full-page YAML editor + Apply / Apply (skip existing).

Submits the YAML body to the daemon via :class:`CorvusBridge.apply_yaml`
and renders the result (one section per entity type) on success.
"""

from __future__ import annotations

from typing import TYPE_CHECKING, Any

from corvus_client.types import ApplyResult
from PySide6.QtCore import Signal
from PySide6.QtWidgets import (
    QCheckBox,
    QGroupBox,
    QHBoxLayout,
    QLabel,
    QPushButton,
    QSplitter,
    QVBoxLayout,
    QWidget,
)

from ..widgets.yaml_editor import YamlEditor, load_yaml_into

if TYPE_CHECKING:
    from ..client_bridge import CorvusBridge


_APPLY_SKELETON = """\
# crv-apply YAML — declarative environment.
# Edit and click Apply. The daemon runs each section in dependency order.
sshKeys: []
disks: []
networks: []
vms: []
templates: []
"""


class ApplyWidget(QWidget):
    """Public signal — emitted when the apply finishes so the host
    can pop a task-detail view on the returned id (Phase 11 will
    actually do this; for now we just log + render the result list)."""

    task_started = Signal(int)

    def __init__(self, bridge: CorvusBridge, parent: QWidget | None = None) -> None:
        super().__init__(parent)
        self._bridge = bridge
        self._last_task_id: int | None = None

        self._editor = YamlEditor(_APPLY_SKELETON)
        self._status = QLabel("")
        self._status.setStyleSheet("color: #6a737d;")

        self._skip_existing = QCheckBox("Skip existing")
        self._skip_existing.setToolTip(
            "Don't fail if an entity (disk / network / VM / …) already exists by name."
        )
        self._load_btn = QPushButton("Load file…")
        self._load_btn.clicked.connect(self._on_load)
        self._apply_btn = QPushButton("Apply")
        self._apply_btn.clicked.connect(self._on_apply)

        toolbar = QHBoxLayout()
        toolbar.addWidget(self._load_btn)
        toolbar.addWidget(self._skip_existing)
        toolbar.addStretch(1)
        toolbar.addWidget(self._apply_btn)

        editor_pane = QVBoxLayout()
        editor_pane.addLayout(toolbar)
        editor_pane.addWidget(self._editor, 1)
        editor_pane.addWidget(self._status)
        editor_widget = QWidget()
        editor_widget.setLayout(editor_pane)

        self._result_panel = _ApplyResultPanel()

        split = QSplitter()
        split.addWidget(editor_widget)
        split.addWidget(self._result_panel)
        split.setStretchFactor(0, 3)
        split.setStretchFactor(1, 2)

        layout = QVBoxLayout(self)
        layout.addWidget(split, 1)

        self._editor.validation_changed.connect(self._on_validation)
        self._on_validation(self._editor.validate())

        bridge.apply_completed.connect(self._on_apply_completed)
        bridge.operation_failed.connect(self._on_op_failed)

    # ---------------------------------------------------- internals

    def _on_validation(self, err: str) -> None:
        self._apply_btn.setEnabled(err == "")
        self._status.setText(err)
        self._status.setStyleSheet("color: #991b1b;" if err else "color: #6a737d;")

    def _on_load(self) -> None:
        load_yaml_into(self._editor, self)

    def _on_apply(self) -> None:
        self._status.setText("Applying…")
        self._status.setStyleSheet("color: #6a737d;")
        self._apply_btn.setEnabled(False)
        self._result_panel.clear()
        self._bridge.apply_yaml(
            self._editor.text(),
            skip_existing=self._skip_existing.isChecked(),
        )

    def _on_apply_completed(self, result: Any, task_id: int) -> None:
        self._apply_btn.setEnabled(True)
        self._last_task_id = task_id
        if isinstance(result, ApplyResult):
            self._result_panel.set_result(result, task_id)
        self._status.setText(f"Applied — task #{task_id}")
        self._status.setStyleSheet("color: #22863a;")
        if task_id:
            self.task_started.emit(task_id)

    def _on_op_failed(self, op: str, message: str) -> None:
        if op != "apply":
            return
        self._apply_btn.setEnabled(True)
        self._status.setText(message)
        self._status.setStyleSheet("color: #991b1b;")


class _ApplyResultPanel(QWidget):
    """Renders an :class:`ApplyResult` as grouped per-entity-type lists."""

    def __init__(self, parent: QWidget | None = None) -> None:
        super().__init__(parent)
        self._task_label = QLabel("(no apply yet)")
        self._ssh_keys = _GroupList("SSH keys")
        self._disks = _GroupList("Disks")
        self._networks = _GroupList("Networks")
        self._vms = _GroupList("VMs")
        self._templates = _GroupList("Templates")

        layout = QVBoxLayout(self)
        layout.addWidget(self._task_label)
        for w in (
            self._ssh_keys,
            self._disks,
            self._networks,
            self._vms,
            self._templates,
        ):
            layout.addWidget(w)
        layout.addStretch(1)

    def clear(self) -> None:
        self._task_label.setText("(applying…)")
        for w in (
            self._ssh_keys,
            self._disks,
            self._networks,
            self._vms,
            self._templates,
        ):
            w.clear()

    def set_result(self, result: ApplyResult, task_id: int) -> None:
        self._task_label.setText(f"Task #{task_id}")
        self._ssh_keys.populate(result.ssh_keys)
        self._disks.populate(result.disks)
        self._networks.populate(result.networks)
        self._vms.populate(result.vms)
        self._templates.populate(result.templates)


class _GroupList(QGroupBox):
    def __init__(self, title: str) -> None:
        super().__init__(title)
        self._content = QLabel("(none)")
        self._content.setWordWrap(True)
        layout = QVBoxLayout(self)
        layout.addWidget(self._content)

    def clear(self) -> None:
        self._content.setText("(none)")

    def populate(self, items: Any) -> None:
        if not items:
            self.clear()
            return
        # Items are ``ApplyCreated`` with ``.name`` and ``.id``.
        lines = [f"#{c.id}  {c.name}" for c in items]
        self._content.setText("\n".join(lines))
