"""Node detail — summary + edit / drain / delete."""

from __future__ import annotations

from typing import TYPE_CHECKING, Any

from corvus_client.types import NodeDetails
from PySide6.QtCore import Signal
from PySide6.QtGui import QFont
from PySide6.QtWidgets import (
    QFormLayout,
    QGroupBox,
    QHBoxLayout,
    QLabel,
    QMessageBox,
    QPushButton,
    QVBoxLayout,
    QWidget,
)

from ..dialogs.node_edit import NodeEditDialog
from ..widgets.status_badge import StatusBadge

if TYPE_CHECKING:
    from ..client_bridge import CorvusBridge


class NodeDetailWidget(QWidget):
    back_requested = Signal()

    def __init__(self, bridge: CorvusBridge, parent: QWidget | None = None) -> None:
        super().__init__(parent)
        self._bridge = bridge
        self._node_id: int | None = None
        self._node: NodeDetails | None = None

        self._back_btn = QPushButton("← Back to nodes")
        self._back_btn.clicked.connect(self.back_requested)
        self._title = QLabel("(no node)")
        f = self._title.font()
        f.setPointSize(f.pointSize() + 4)
        f.setWeight(QFont.Weight.DemiBold)
        self._title.setFont(f)
        self._badge = StatusBadge("vm")

        self._btn_edit = QPushButton("Edit…")
        self._btn_drain = QPushButton("Drain")
        self._btn_delete = QPushButton("Delete")
        self._btn_edit.clicked.connect(self._on_edit)
        self._btn_drain.clicked.connect(self._on_drain)
        self._btn_delete.clicked.connect(self._on_delete)
        actions = QHBoxLayout()
        actions.addWidget(self._btn_edit)
        actions.addWidget(self._btn_drain)
        actions.addWidget(self._btn_delete)
        actions.addStretch(1)

        self._host = QLabel("—")
        self._base_path = QLabel("—")
        self._cpu = QLabel("—")
        self._ram = QLabel("—")
        self._storage = QLabel("—")
        self._load = QLabel("—")
        self._agents = QLabel("—")
        self._kernel = QLabel("—")
        self._description = QLabel("—")
        form = QFormLayout()
        form.addRow("Host:", self._host)
        form.addRow("Base path:", self._base_path)
        form.addRow("CPU:", self._cpu)
        form.addRow("RAM:", self._ram)
        form.addRow("Storage:", self._storage)
        form.addRow("Load (1/5/15):", self._load)
        form.addRow("Agents:", self._agents)
        form.addRow("Kernel:", self._kernel)
        form.addRow("Description:", self._description)
        summary = QGroupBox("Summary")
        sl = QVBoxLayout(summary)
        sl.addLayout(form)

        header = QHBoxLayout()
        header.addWidget(self._title)
        header.addSpacing(12)
        header.addWidget(self._badge)
        header.addStretch(1)

        layout = QVBoxLayout(self)
        layout.addWidget(self._back_btn)
        layout.addLayout(header)
        layout.addLayout(actions)
        layout.addWidget(summary)
        layout.addStretch(1)

        bridge.node_detail_ready.connect(self._on_detail)
        bridge.node_action_completed.connect(self._on_action_completed)

    def set_node_id(self, node_id: int) -> None:
        self.clear()
        self._node_id = node_id
        self._title.setText(f"Node #{node_id}")
        self._bridge.request_node_detail(node_id)

    def clear(self) -> None:
        self._node_id = None
        self._node = None
        self._title.setText("(no node)")
        self._badge.set_status("")
        for label in (
            self._host,
            self._base_path,
            self._cpu,
            self._ram,
            self._storage,
            self._load,
            self._agents,
            self._kernel,
            self._description,
        ):
            label.setText("—")

    def _on_detail(self, info: Any) -> None:
        if not isinstance(info, NodeDetails) or info.id != self._node_id:
            return
        self._node = info
        self._title.setText(info.name)
        self._badge.set_status(info.admin_state)
        self._host.setText(
            f"{info.host}:{info.node_agent_port} (netd {info.net_agent_port})"
        )
        self._base_path.setText(info.base_path or "—")
        self._cpu.setText(str(info.cpu_count))
        if info.ram_mb_total:
            self._ram.setText(f"{info.ram_mb_free or 0} / {info.ram_mb_total} MB free")
        else:
            self._ram.setText("—")
        if info.storage_bytes_total:
            free_gib = (info.storage_bytes_free or 0) / (1024**3)
            total_gib = info.storage_bytes_total / (1024**3)
            self._storage.setText(f"{free_gib:.1f} / {total_gib:.1f} GiB free")
        else:
            self._storage.setText("—")
        loads = [info.load_avg1, info.load_avg5, info.load_avg15]
        self._load.setText(
            " / ".join(f"{x:.2f}" if x is not None else "—" for x in loads)
        )
        agents = []
        agents.append(
            f"nodeagent {info.agent_version or ''} "
            f"({'connected' if info.last_node_agent_push_at else 'offline'})"
        )
        if info.netd_disabled:
            agents.append("netd disabled")
        else:
            agents.append("netd " + ("connected" if info.netd_connected else "offline"))
        self._agents.setText(" · ".join(agents))
        self._kernel.setText(info.kernel_release or "—")
        self._description.setText(info.description or "—")

    def _on_action_completed(self, node_id: int, action: str) -> None:
        if node_id != self._node_id:
            return
        if action == "delete":
            self.back_requested.emit()
            return
        self._bridge.request_node_detail(node_id)

    def _on_edit(self) -> None:
        if self._node_id is None or self._node is None:
            return
        dlg = NodeEditDialog(self._node, self)
        if dlg.exec():
            self._bridge.node_edit(self._node_id, **dlg.payload())

    def _on_drain(self) -> None:
        if self._node_id is None:
            return
        self._bridge.node_drain(self._node_id)

    def _on_delete(self) -> None:
        if self._node_id is None:
            return
        reply = QMessageBox.question(
            self,
            "Delete node",
            "Delete this node? Drain it first (no VMs / disks left on it).",
            QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No,
            QMessageBox.StandardButton.No,
        )
        if reply == QMessageBox.StandardButton.Yes:
            self._bridge.node_delete(self._node_id)
