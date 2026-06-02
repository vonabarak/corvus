"""Network detail — summary + start/stop/edit/delete."""

from __future__ import annotations

from typing import TYPE_CHECKING, Any

from corvus_client.types import NetworkInfo
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

from ..dialogs.network_edit import NetworkEditDialog
from ..widgets.status_badge import StatusBadge

if TYPE_CHECKING:
    from ..client_bridge import CorvusBridge


# Map running/stopped onto the StatusBadge palette (which keys off VM
# status labels). We pick `running` / `stopped` from that palette.
def _state_label(running: bool) -> str:
    return "running" if running else "stopped"


class NetworkDetailWidget(QWidget):
    back_requested = Signal()

    def __init__(self, bridge: CorvusBridge, parent: QWidget | None = None) -> None:
        super().__init__(parent)
        self._bridge = bridge
        self._network_id: int | None = None
        self._network: NetworkInfo | None = None

        self._back_btn = QPushButton("← Back to networks")
        self._back_btn.clicked.connect(self.back_requested)
        self._title = QLabel("(no network)")
        f = self._title.font()
        f.setPointSize(f.pointSize() + 4)
        f.setWeight(QFont.Weight.DemiBold)
        self._title.setFont(f)
        self._badge = StatusBadge("vm")  # palette covers "running" / "stopped"

        self._btn_start = QPushButton("Start")
        self._btn_stop = QPushButton("Stop")
        self._btn_edit = QPushButton("Edit…")
        self._btn_delete = QPushButton("Delete")
        self._btn_start.clicked.connect(self._on_start)
        self._btn_stop.clicked.connect(self._on_stop)
        self._btn_edit.clicked.connect(self._on_edit)
        self._btn_delete.clicked.connect(self._on_delete)

        actions = QHBoxLayout()
        for b in (self._btn_start, self._btn_stop, self._btn_edit, self._btn_delete):
            actions.addWidget(b)
        actions.addStretch(1)

        self._subnet = QLabel("—")
        self._flags = QLabel("—")
        self._peers = QLabel("—")
        self._created = QLabel("—")
        self._vni = QLabel("—")
        self._dnsmasq_pid = QLabel("—")
        form = QFormLayout()
        form.addRow("Subnet:", self._subnet)
        form.addRow("Flags:", self._flags)
        form.addRow("Peer nodes:", self._peers)
        form.addRow("Created:", self._created)
        form.addRow("VNI:", self._vni)
        form.addRow("dnsmasq PID:", self._dnsmasq_pid)
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

        bridge.network_detail_ready.connect(self._on_detail)
        bridge.network_action_completed.connect(self._on_action_completed)
        self._refresh_actions()

    # ---------------------------------------------------- public

    def set_network_id(self, network_id: int) -> None:
        self.clear()
        self._network_id = network_id
        self._title.setText(f"Network #{network_id}")
        self._bridge.request_network_detail(network_id)

    def clear(self) -> None:
        self._network_id = None
        self._network = None
        self._title.setText("(no network)")
        self._badge.set_status("")
        for label in (
            self._subnet,
            self._flags,
            self._peers,
            self._created,
            self._vni,
            self._dnsmasq_pid,
        ):
            label.setText("—")
        self._refresh_actions()

    # ---------------------------------------------------- bridge slots

    def _on_detail(self, info: Any) -> None:
        if not isinstance(info, NetworkInfo) or info.id != self._network_id:
            return
        self._network = info
        self._title.setText(info.name)
        self._badge.set_status(_state_label(info.running))
        self._subnet.setText(info.subnet)
        flags: list[str] = []
        if info.dhcp:
            flags.append("DHCP")
        if info.nat:
            flags.append("NAT")
        if info.autostart:
            flags.append("autostart")
        self._flags.setText(", ".join(flags) or "—")
        self._peers.setText(str(len(info.peer_node_ids)))
        self._created.setText(info.created_at.isoformat(sep=" ", timespec="seconds"))
        self._vni.setText(str(info.vni) if info.vni is not None else "—")
        self._dnsmasq_pid.setText(
            str(info.dnsmasq_pid) if info.dnsmasq_pid is not None else "—"
        )
        self._refresh_actions()

    def _on_action_completed(self, network_id: int, action: str) -> None:
        if network_id != self._network_id:
            return
        if action == "delete":
            self.back_requested.emit()
            return
        self._bridge.request_network_detail(network_id)

    # ---------------------------------------------------- ui actions

    def _on_start(self) -> None:
        if self._network_id is not None:
            self._bridge.network_start(self._network_id)

    def _on_stop(self) -> None:
        if self._network_id is None:
            return
        # Mirror crv's --force option via a confirm with two buttons; we
        # ask plain stop first and let the bridge surface errors.
        self._bridge.network_stop(self._network_id, force=False)

    def _on_edit(self) -> None:
        if self._network_id is None or self._network is None:
            return
        dlg = NetworkEditDialog(self._network, self)
        if dlg.exec():
            p = dlg.payload()
            self._bridge.network_edit(
                self._network_id,
                subnet=p.get("subnet"),
                dhcp=p.get("dhcp"),
                nat=p.get("nat"),
                autostart=p.get("autostart"),
            )

    def _on_delete(self) -> None:
        if self._network_id is None:
            return
        reply = QMessageBox.question(
            self,
            "Delete network",
            "Delete this network? Stop it first if it's running.",
            QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No,
            QMessageBox.StandardButton.No,
        )
        if reply == QMessageBox.StandardButton.Yes:
            self._bridge.network_delete(self._network_id)

    def _refresh_actions(self) -> None:
        running = self._network.running if self._network is not None else False
        loaded = self._network is not None
        self._btn_start.setEnabled(loaded and not running)
        self._btn_stop.setEnabled(loaded and running)
        self._btn_edit.setEnabled(loaded)
        # Daemon rejects delete on a running network — gate locally for
        # immediate feedback.
        self._btn_delete.setEnabled(loaded and not running)
