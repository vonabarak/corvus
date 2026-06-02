"""VM detail screen: summary, drives/NICs tables, lifecycle, serial.

Action buttons gate on the current status using the same logic the
React UI does (see :mod:`Corvus.Model.VmState` in the Haskell tree for
the authoritative state machine). Phase 8 adds the editing surface:
Edit / Delete / Guest exec buttons in the action row, an Attach /
Remove control on every Drive / NIC / SSH-key / Shared-dir tab.
"""

from __future__ import annotations

from typing import TYPE_CHECKING, Any

from corvus_client.types import (
    GuestAgentStatus,
    SshKeyInfo,
    ViewGrant,
    VmDetails,
    VmStats,
)
from PySide6.QtCore import Signal
from PySide6.QtGui import QFont
from PySide6.QtWidgets import (
    QFormLayout,
    QHBoxLayout,
    QLabel,
    QMessageBox,
    QPushButton,
    QTableWidget,
    QTableWidgetItem,
    QTabWidget,
    QVBoxLayout,
    QWidget,
)

from ..dialogs.add_net_if import AddNetIfDialog
from ..dialogs.add_shared_dir import AddSharedDirDialog
from ..dialogs.attach_disk import AttachDiskDialog
from ..dialogs.attach_ssh_key import AttachSshKeyDialog
from ..dialogs.guest_exec import GuestExecDialog
from ..dialogs.vm_edit import VmEditDialog
from ..dialogs.vm_migrate import VmMigrateDialog
from ..widgets.cloud_init_panel import CloudInitPanel
from ..widgets.serial_console import SerialConsoleWidget
from ..widgets.spice_launcher import launch_remote_viewer
from ..widgets.stats_chart import StatsSparkline
from ..widgets.status_badge import StatusBadge

if TYPE_CHECKING:
    from ..client_bridge import CorvusBridge


# Permitted lifecycle transitions, mirroring the daemon state machine.
_ALLOWED_ACTIONS: dict[str, set[str]] = {
    "stopped": {"start"},
    "starting": {"stop", "reset"},
    "running": {"stop", "pause", "reset", "save", "send_ctrl_alt_del"},
    "stopping": {"reset"},
    "paused": {"start", "reset", "save"},
    "saved": {"start", "reset"},
    "error": {"reset"},
}


class VmDetailWidget(QWidget):
    """Live view of a single VM."""

    back_requested = Signal()

    def __init__(self, bridge: CorvusBridge, parent: QWidget | None = None) -> None:
        super().__init__(parent)
        self._bridge = bridge
        self._vm_id: int | None = None
        self._vm_status: str = ""
        self._vm_details: VmDetails | None = None
        self._serial_open = False

        # ---------------- header ----------------
        self._back_btn = QPushButton("← Back to VMs")
        self._back_btn.clicked.connect(self.back_requested)

        self._title = QLabel("(no vm)")
        title_font = self._title.font()
        title_font.setPointSize(title_font.pointSize() + 4)
        title_font.setWeight(QFont.Weight.DemiBold)
        self._title.setFont(title_font)

        self._status_badge = StatusBadge("vm")
        # Live guest-agent reachability pill — updated by subscribe_guest_agent.
        self._guest_badge = StatusBadge("vm")
        self._guest_badge.set_status("unknown")

        # ---------------- lifecycle buttons ----------------
        self._btn_start = QPushButton("Start")
        self._btn_stop = QPushButton("Stop")
        self._btn_pause = QPushButton("Pause")
        self._btn_reset = QPushButton("Reset")
        self._btn_save = QPushButton("Save")
        self._btn_cad = QPushButton("Ctrl+Alt+Del")
        self._actions: dict[str, QPushButton] = {
            "start": self._btn_start,
            "stop": self._btn_stop,
            "pause": self._btn_pause,
            "reset": self._btn_reset,
            "save": self._btn_save,
            "send_ctrl_alt_del": self._btn_cad,
        }
        for action, btn in self._actions.items():
            btn.clicked.connect(lambda _checked=False, a=action: self._on_action(a))

        # Editing buttons (always visible; Delete gates on status).
        self._btn_edit = QPushButton("Edit…")
        self._btn_edit.clicked.connect(self._on_edit)
        self._btn_guest_exec = QPushButton("Guest exec…")
        self._btn_guest_exec.clicked.connect(self._on_guest_exec)
        self._btn_migrate = QPushButton("Migrate…")
        self._btn_migrate.clicked.connect(self._on_migrate)
        self._btn_view = QPushButton("View (SPICE)…")
        self._btn_view.clicked.connect(self._on_view)
        self._btn_delete = QPushButton("Delete")
        self._btn_delete.clicked.connect(self._on_delete)

        actions_row = QHBoxLayout()
        for btn in self._actions.values():
            actions_row.addWidget(btn)
        actions_row.addSpacing(12)
        actions_row.addWidget(self._btn_edit)
        actions_row.addWidget(self._btn_guest_exec)
        actions_row.addWidget(self._btn_migrate)
        actions_row.addWidget(self._btn_view)
        actions_row.addWidget(self._btn_delete)
        actions_row.addStretch(1)

        # ---------------- summary fields ----------------
        self._cpu = QLabel("—")
        self._ram = QLabel("—")
        self._node = QLabel("—")
        self._created = QLabel("—")
        self._error = QLabel("")
        self._error.setStyleSheet("color: #991b1b;")
        self._error.setWordWrap(True)
        # Live stats sparklines — populated from subscribe_vm_stats.
        # Rate-style charts (CPU% / disk-IO / net-IO) need the previous
        # sample to compute a delta; _prev_sample holds it.
        self._prev_sample: VmStats | None = None
        self._chart_cpu = StatsSparkline(y_unit="%")
        self._chart_ram = StatsSparkline(y_unit=" MiB")
        self._chart_disk = StatsSparkline(y_unit=" KiB/s")
        self._chart_net = StatsSparkline(y_unit=" KiB/s")
        form = QFormLayout()
        form.addRow("Node:", self._node)
        form.addRow("CPU:", self._cpu)
        form.addRow("RAM:", self._ram)
        form.addRow("Created:", self._created)

        # Summary pane is its own tab (no QGroupBox — the tab header
        # already labels it).
        summary_pane = QWidget()
        sum_layout = QVBoxLayout(summary_pane)
        sum_layout.addLayout(form)
        sum_layout.addWidget(self._error)
        sum_layout.addStretch(1)

        # Live stats pane — four sparklines in a QFormLayout, also a tab.
        stats_pane = QWidget()
        charts_layout = QFormLayout(stats_pane)
        charts_layout.addRow("CPU:", self._chart_cpu)
        charts_layout.addRow("RAM (RSS):", self._chart_ram)
        charts_layout.addRow("Disk I/O:", self._chart_disk)
        charts_layout.addRow("Net I/O:", self._chart_net)

        # ---------------- drives ----------------
        self._drives_table = QTableWidget(0, 6)
        self._drives_table.setHorizontalHeaderLabels(
            ["Disk", "Interface", "Format", "Media", "Read-only", ""]
        )
        self._drives_table.verticalHeader().setVisible(False)
        self._drives_table.horizontalHeader().setStretchLastSection(False)
        self._drives_table.setEditTriggers(QTableWidget.EditTrigger.NoEditTriggers)
        drives_pane = _table_with_action(
            self._drives_table, "+ Attach disk", self._on_attach_disk
        )

        # ---------------- NICs ----------------
        self._netifs_table = QTableWidget(0, 5)
        self._netifs_table.setHorizontalHeaderLabels(
            ["Type", "Network", "MAC", "Guest IPs", ""]
        )
        self._netifs_table.verticalHeader().setVisible(False)
        self._netifs_table.horizontalHeader().setStretchLastSection(False)
        self._netifs_table.setEditTriggers(QTableWidget.EditTrigger.NoEditTriggers)
        netifs_pane = _table_with_action(
            self._netifs_table, "+ Add NIC", self._on_add_net_if
        )

        # ---------------- SSH keys ----------------
        self._ssh_table = QTableWidget(0, 2)
        self._ssh_table.setHorizontalHeaderLabels(["Key", ""])
        self._ssh_table.verticalHeader().setVisible(False)
        self._ssh_table.horizontalHeader().setStretchLastSection(False)
        self._ssh_table.setEditTriggers(QTableWidget.EditTrigger.NoEditTriggers)
        ssh_pane = _table_with_action(
            self._ssh_table, "+ Attach SSH key", self._on_attach_ssh_key
        )

        # ---------------- shared dirs ----------------
        self._shared_table = QTableWidget(0, 5)
        self._shared_table.setHorizontalHeaderLabels(
            ["Path", "Tag", "Cache", "Read-only", ""]
        )
        self._shared_table.verticalHeader().setVisible(False)
        self._shared_table.horizontalHeader().setStretchLastSection(False)
        self._shared_table.setEditTriggers(QTableWidget.EditTrigger.NoEditTriggers)
        shared_pane = _table_with_action(
            self._shared_table, "+ Add shared dir", self._on_add_shared_dir
        )

        self._serial = SerialConsoleWidget()
        self._serial.input_bytes.connect(self._on_serial_input)
        self._hmp = SerialConsoleWidget()
        self._hmp.input_bytes.connect(self._on_hmp_input)
        self._hmp_open = False

        self._cloud_init = CloudInitPanel(bridge)

        self._tabs = QTabWidget()
        self._tabs.addTab(summary_pane, "Summary")
        self._tabs.addTab(stats_pane, "Live stats")
        self._tabs.addTab(drives_pane, "Drives")
        self._tabs.addTab(netifs_pane, "Network")
        self._tabs.addTab(ssh_pane, "SSH keys")
        self._tabs.addTab(shared_pane, "Shared dirs")
        self._tabs.addTab(self._cloud_init, "Cloud-init")
        self._tabs.addTab(self._serial, "Serial console")
        self._tabs.addTab(self._hmp, "HMP monitor")
        self._tabs.currentChanged.connect(self._on_tab_changed)

        # ---------------- root layout ----------------
        header_row = QHBoxLayout()
        header_row.addWidget(self._title)
        header_row.addSpacing(12)
        header_row.addWidget(self._status_badge)
        header_row.addSpacing(8)
        header_row.addWidget(QLabel("Guest agent:"))
        header_row.addWidget(self._guest_badge)
        header_row.addStretch(1)

        layout = QVBoxLayout(self)
        layout.addWidget(self._back_btn)
        layout.addLayout(header_row)
        layout.addLayout(actions_row)
        layout.addWidget(self._tabs, 1)

        bridge.vm_detail_ready.connect(self._on_detail)
        bridge.vm_action_completed.connect(self._on_action_completed)
        bridge.vm_edit_completed.connect(self._on_edit_completed)
        bridge.vm_shared_dirs_ready.connect(self._on_shared_dirs)
        bridge.serial_data.connect(self._on_serial_data)
        bridge.serial_closed.connect(self._on_serial_closed)
        bridge.vm_stats_event.connect(self._on_stats)
        bridge.vm_stats_history_ready.connect(self._on_stats_history)
        bridge.vm_ssh_keys_ready.connect(self._on_ssh_keys)
        bridge.guest_agent_event.connect(self._on_guest_agent)
        bridge.view_grant_ready.connect(self._on_view_grant)
        bridge.hmp_data.connect(self._on_hmp_data)
        bridge.hmp_closed.connect(self._on_hmp_closed)

        self._refresh_actions()

    # ---------------------------------------------------- public

    def set_vm_id(self, vm_id: int) -> None:
        self.clear()
        self._vm_id = vm_id
        self._title.setText(f"VM #{vm_id}")
        self._bridge.request_vm_detail(vm_id)
        self._bridge.request_vm_shared_dirs(vm_id)
        self._bridge.request_vm_ssh_keys(vm_id)
        self._bridge.request_vm_stats_history(vm_id)
        self._bridge.subscribe_vm_stats(vm_id)
        self._bridge.subscribe_guest_agent(vm_id)
        self._cloud_init.set_vm_id(vm_id)

    def clear(self) -> None:
        if self._vm_id is not None:
            if self._serial_open:
                self._bridge.close_serial(self._vm_id)
                self._serial_open = False
            if self._hmp_open:
                self._bridge.close_hmp(self._vm_id)
                self._hmp_open = False
            self._bridge.unsubscribe_vm_stats(self._vm_id)
            self._bridge.unsubscribe_guest_agent(self._vm_id)
        self._serial.reset_screen()
        self._hmp.reset_screen()
        self._cloud_init.clear()
        self._guest_badge.set_status("unknown")
        self._prev_sample = None
        for chart in (
            self._chart_cpu,
            self._chart_ram,
            self._chart_disk,
            self._chart_net,
        ):
            chart.clear()
        self._vm_id = None
        self._vm_status = ""
        self._vm_details = None
        self._title.setText("(no vm)")
        self._status_badge.set_status("")
        for label in (self._cpu, self._ram, self._node, self._created):
            label.setText("—")
        self._error.setText("")
        self._drives_table.setRowCount(0)
        self._netifs_table.setRowCount(0)
        self._ssh_table.setRowCount(0)
        self._shared_table.setRowCount(0)
        self._refresh_actions()
        self._tabs.setCurrentIndex(0)

    # ---------------------------------------------------- bridge slots

    def _on_detail(self, info: Any) -> None:
        if not isinstance(info, VmDetails) or info.id != self._vm_id:
            return
        self._vm_details = info
        self._vm_status = info.status
        self._title.setText(info.name)
        self._status_badge.set_status(info.status)
        self._node.setText(info.node.name if info.node else "—")
        self._cpu.setText(f"{info.cpu_count} ({info.cpu_model})")
        self._ram.setText(f"{info.ram_mb} MB")
        self._created.setText(info.created_at.isoformat(sep=" ", timespec="seconds"))
        self._error.setText(info.error_message or "")
        self._fill_drives(info.drives)
        self._fill_netifs(info.net_ifs)
        # SSH keys: VmDetails doesn't carry the list directly; the
        # daemon attaches them as part of `Vm.listSshKeys`. For now we
        # surface the empty table — Phase 12 can subscribe properly.
        self._refresh_actions()

    def _on_action_completed(self, vm_id: int, action: str, new_status: str) -> None:
        if vm_id != self._vm_id:
            return
        if new_status:
            self._vm_status = new_status
            self._status_badge.set_status(new_status)
            self._refresh_actions()
        self._bridge.request_vm_detail(vm_id)

    def _on_edit_completed(self, vm_id: int, action: str) -> None:
        if vm_id != self._vm_id and action != "create":
            return
        if action == "delete" and vm_id == self._vm_id:
            self.back_requested.emit()
            return
        # Refetch detail + shared dirs + ssh keys after any mutation.
        if self._vm_id is not None:
            self._bridge.request_vm_detail(self._vm_id)
            self._bridge.request_vm_shared_dirs(self._vm_id)
            if action in ("attach_ssh_key", "detach_ssh_key"):
                self._bridge.request_vm_ssh_keys(self._vm_id)

    def _on_shared_dirs(self, vm_id: int, dirs: Any) -> None:
        if vm_id != self._vm_id:
            return
        self._fill_shared_dirs(dirs)

    def _on_serial_data(self, vm_id: int, chunk: bytes) -> None:
        if vm_id != self._vm_id:
            return
        self._serial.feed_bytes(chunk)

    def _on_serial_closed(self, vm_id: int, _reason: str) -> None:
        if vm_id != self._vm_id:
            return
        self._serial_open = False

    def _on_hmp_data(self, vm_id: int, chunk: bytes) -> None:
        if vm_id != self._vm_id:
            return
        self._hmp.feed_bytes(chunk)

    def _on_hmp_closed(self, vm_id: int, _reason: str) -> None:
        if vm_id != self._vm_id:
            return
        self._hmp_open = False

    def _on_hmp_input(self, chunk: bytes) -> None:
        if self._vm_id is None or not self._hmp_open:
            return
        self._bridge.send_hmp(self._vm_id, chunk)

    def _on_stats(self, vm_id: int, sample: object) -> None:
        if vm_id != self._vm_id or not isinstance(sample, VmStats):
            return
        self._append_sample(sample)

    def _on_stats_history(self, vm_id: int, history: object) -> None:
        """Daemon ring buffer (oldest first) — seeds the charts so they
        aren't empty when VM detail opens."""
        if vm_id != self._vm_id or not isinstance(history, list):
            return
        self._prev_sample = None
        for chart in (
            self._chart_cpu,
            self._chart_ram,
            self._chart_disk,
            self._chart_net,
        ):
            chart.clear()
        for sample in history:
            if isinstance(sample, VmStats):
                self._append_sample(sample)

    def _append_sample(self, sample: VmStats) -> None:
        """Push one ``VmStats`` row into the four charts. CPU / disk /
        net are rates relative to ``_prev_sample``; RAM is an instant
        gauge.

        Rate units: CPU is a percentage (multi-core summed; can exceed
        100%). Disk and net are KiB/s aggregated across drives / TAPs.
        Memory is host-side RSS in MiB.
        """
        # RAM — instant gauge.
        self._chart_ram.add_point(sample.host_rss_bytes / (1024 * 1024))

        prev = self._prev_sample
        if prev is not None and sample.interval_millis > 0:
            seconds = sample.interval_millis / 1000.0
            d_jiffies = sample.cpu_jiffies_total - prev.cpu_jiffies_total
            if sample.clk_tck > 0:
                cpu_pct = max(0.0, d_jiffies) / (seconds * sample.clk_tck) * 100.0
                self._chart_cpu.add_point(cpu_pct)
            disk_delta = _sum_drive_delta(sample.drives, prev.drives)
            self._chart_disk.add_point(max(0.0, disk_delta) / seconds / 1024.0)
            net_delta = _sum_net_delta(sample.nets, prev.nets)
            self._chart_net.add_point(max(0.0, net_delta) / seconds / 1024.0)
        self._prev_sample = sample

    def _on_ssh_keys(self, vm_id: int, keys: object) -> None:
        if vm_id != self._vm_id or not isinstance(keys, list):
            return
        self._fill_ssh_keys(keys)

    def _fill_ssh_keys(self, keys: list[SshKeyInfo]) -> None:
        self._ssh_table.setRowCount(len(keys))
        for row, key in enumerate(keys):
            self._ssh_table.setItem(row, 0, QTableWidgetItem(key.name))
            self._ssh_table.setCellWidget(
                row,
                1,
                _detach_button("Detach", lambda _v=key.id: self._detach_ssh_key(_v)),
            )
        self._ssh_table.resizeColumnsToContents()

    def _detach_ssh_key(self, key_id: int) -> None:
        if self._vm_id is None:
            return
        self._bridge.vm_detach_ssh_key(self._vm_id, key_id)

    def _on_guest_agent(self, vm_id: int, status: object) -> None:
        if vm_id != self._vm_id or not isinstance(status, GuestAgentStatus):
            return
        if not status.enabled:
            self._guest_badge.set_status("disabled")
        elif status.reachable:
            self._guest_badge.set_status("running")
        else:
            self._guest_badge.set_status("error")

    def _on_view_grant(self, vm_id: int, grant: object) -> None:
        if vm_id != self._vm_id or not isinstance(grant, ViewGrant):
            return
        launch_remote_viewer(grant, self)

    def _on_view(self) -> None:
        if self._vm_id is None:
            return
        self._bridge.request_view_grant(self._vm_id)

    # ---------------------------------------------------- internals

    def _on_action(self, action: str) -> None:
        if self._vm_id is None:
            return
        self._bridge.vm_action(self._vm_id, action)

    def _on_edit(self) -> None:
        if self._vm_id is None or self._vm_details is None:
            return
        dlg = VmEditDialog(self._vm_details, self)
        if dlg.exec():
            self._bridge.vm_edit(self._vm_id, **dlg.payload())

    def _on_delete(self) -> None:
        if self._vm_id is None:
            return
        reply = QMessageBox.question(
            self,
            "Delete VM",
            "Delete this VM? Ephemeral disks attached to it will also "
            "be removed (use Edit instead to detach disks you want to keep).",
            QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No,
            QMessageBox.StandardButton.No,
        )
        if reply == QMessageBox.StandardButton.Yes:
            self._bridge.vm_delete(self._vm_id)

    def _on_guest_exec(self) -> None:
        if self._vm_id is None:
            return
        dlg = GuestExecDialog(self._bridge, self._vm_id, self)
        dlg.exec()

    def _on_migrate(self) -> None:
        if self._vm_id is None:
            return
        dlg = VmMigrateDialog(self._bridge, self)
        if dlg.exec():
            p = dlg.payload()
            self._bridge.vm_migrate(self._vm_id, p["to_node_ref"])

    def _on_attach_disk(self) -> None:
        if self._vm_id is None:
            return
        dlg = AttachDiskDialog(self._bridge, self)
        if dlg.exec():
            p = dlg.payload()
            self._bridge.vm_attach_disk(
                self._vm_id,
                p["disk_ref"],
                interface=p["interface"],
                media=p["media"],
                read_only=p["read_only"],
                cache_type=p["cache_type"],
                discard=p["discard"],
            )

    def _on_add_net_if(self) -> None:
        if self._vm_id is None:
            return
        dlg = AddNetIfDialog(self._bridge, self)
        if dlg.exec():
            p = dlg.payload()
            self._bridge.vm_add_net_if(
                self._vm_id,
                type=p["type"],
                host_device=p["host_device"],
                mac_address=p["mac_address"],
                network_ref=p["network_ref"],
            )

    def _on_attach_ssh_key(self) -> None:
        if self._vm_id is None:
            return
        dlg = AttachSshKeyDialog(self._bridge, self)
        if dlg.exec():
            p = dlg.payload()
            self._bridge.vm_attach_ssh_key(self._vm_id, p["key_ref"])

    def _on_add_shared_dir(self) -> None:
        if self._vm_id is None:
            return
        dlg = AddSharedDirDialog(self)
        if dlg.exec():
            p = dlg.payload()
            self._bridge.vm_add_shared_dir(
                self._vm_id,
                p["path"],
                p["tag"],
                cache=p["cache"],
                read_only=p["read_only"],
            )

    def _on_tab_changed(self, index: int) -> None:
        widget = self._tabs.widget(index)
        if widget is self._serial:
            if self._vm_id is not None and not self._serial_open:
                self._bridge.open_serial(self._vm_id)
                self._serial_open = True
                self._serial.setFocus()
        elif self._vm_id is not None and self._serial_open:
            self._bridge.close_serial(self._vm_id)
            self._serial_open = False

        if widget is self._hmp:
            if self._vm_id is not None and not self._hmp_open:
                self._bridge.open_hmp(self._vm_id)
                self._hmp_open = True
                self._hmp.setFocus()
        elif self._vm_id is not None and self._hmp_open:
            self._bridge.close_hmp(self._vm_id)
            self._hmp_open = False

    def _on_serial_input(self, chunk: bytes) -> None:
        if self._vm_id is None or not self._serial_open:
            return
        self._bridge.send_serial(self._vm_id, chunk)

    def _refresh_actions(self) -> None:
        allowed = _ALLOWED_ACTIONS.get(self._vm_status, set())
        for action, btn in self._actions.items():
            btn.setEnabled(action in allowed)
        loaded = self._vm_details is not None
        self._btn_edit.setEnabled(loaded)
        # Migrate is valid for any settled state — the daemon pauses
        # running VMs automatically before transferring memory. Gate
        # locally only against transitional / error states; the
        # daemon's pre-flight checks have the final say.
        self._btn_migrate.setEnabled(
            loaded and self._vm_status in ("stopped", "saved", "paused", "running")
        )
        # View (SPICE) only works for graphical (non-headless) running VMs.
        self._btn_view.setEnabled(
            loaded
            and self._vm_details is not None
            and not self._vm_details.headless
            and self._vm_status == "running"
        )
        # Guest exec requires guest agent + running VM.
        guest_ok = (
            loaded
            and (self._vm_details is not None)
            and self._vm_details.guest_agent
            and self._vm_status == "running"
        )
        self._btn_guest_exec.setEnabled(guest_ok)
        self._btn_delete.setEnabled(loaded and self._vm_status == "stopped")

    def _fill_drives(self, drives: Any) -> None:
        self._drives_table.setRowCount(len(drives))
        for row, d in enumerate(drives):
            self._drives_table.setItem(
                row,
                0,
                QTableWidgetItem(d.disk_image.name if d.disk_image else ""),
            )
            self._drives_table.setItem(row, 1, QTableWidgetItem(d.interface))
            self._drives_table.setItem(row, 2, QTableWidgetItem(d.format))
            self._drives_table.setItem(row, 3, QTableWidgetItem(d.media))
            self._drives_table.setItem(
                row, 4, QTableWidgetItem("yes" if d.read_only else "no")
            )
            self._drives_table.setCellWidget(
                row, 5, _detach_button("Detach", lambda _v=d.id: self._detach_disk(_v))
            )
        self._drives_table.resizeColumnsToContents()

    def _detach_disk(self, drive_id: int) -> None:
        if self._vm_id is None:
            return
        self._bridge.vm_detach_disk(self._vm_id, drive_id)

    def _fill_netifs(self, netifs: Any) -> None:
        self._netifs_table.setRowCount(len(netifs))
        for row, n in enumerate(netifs):
            self._netifs_table.setItem(row, 0, QTableWidgetItem(n.type))
            net_name = n.network.name if n.network else (n.host_device or "")
            self._netifs_table.setItem(row, 1, QTableWidgetItem(net_name))
            self._netifs_table.setItem(row, 2, QTableWidgetItem(n.mac_address or ""))
            # ``guest_ip_addresses`` is a single string (typically a
            # space- or comma-separated list assembled daemon-side);
            # render it verbatim — joining over a str would split it
            # into individual characters.
            self._netifs_table.setItem(
                row, 3, QTableWidgetItem(n.guest_ip_addresses or "")
            )
            self._netifs_table.setCellWidget(
                row,
                4,
                _detach_button("Remove", lambda _v=n.id: self._remove_net_if(_v)),
            )
        self._netifs_table.resizeColumnsToContents()

    def _remove_net_if(self, net_if_id: int) -> None:
        if self._vm_id is None:
            return
        self._bridge.vm_remove_net_if(self._vm_id, net_if_id)

    def _fill_shared_dirs(self, dirs: Any) -> None:
        self._shared_table.setRowCount(len(dirs))
        for row, d in enumerate(dirs):
            self._shared_table.setItem(row, 0, QTableWidgetItem(d.path))
            self._shared_table.setItem(row, 1, QTableWidgetItem(d.tag))
            self._shared_table.setItem(row, 2, QTableWidgetItem(d.cache or ""))
            self._shared_table.setItem(
                row, 3, QTableWidgetItem("yes" if d.read_only else "no")
            )
            self._shared_table.setCellWidget(
                row,
                4,
                _detach_button("Remove", lambda _v=d.id: self._remove_shared_dir(_v)),
            )
        self._shared_table.resizeColumnsToContents()

    def _remove_shared_dir(self, shared_dir_id: int) -> None:
        if self._vm_id is None:
            return
        self._bridge.vm_remove_shared_dir(self._vm_id, shared_dir_id)


def _table_with_action(
    table: QTableWidget, action_label: str, action_cb: Any
) -> QWidget:
    """Wrap a table with an action button row below it."""
    container = QWidget()
    layout = QVBoxLayout(container)
    layout.setContentsMargins(0, 0, 0, 0)
    layout.addWidget(table, 1)
    btn = QPushButton(action_label)
    btn.clicked.connect(action_cb)
    row = QHBoxLayout()
    row.addWidget(btn)
    row.addStretch(1)
    layout.addLayout(row)
    return container


def _detach_button(label: str, cb: Any) -> QPushButton:
    btn = QPushButton(label)
    btn.clicked.connect(cb)
    return btn


def _sum_drive_delta(curr: Any, prev: Any) -> float:
    """Total read + write bytes since the previous sample across all
    drives. Matched by ``DriveIo.name`` (new drives or detached ones
    contribute zero rather than spiking the chart)."""
    prev_by_name = {d.name: d for d in prev}
    delta = 0.0
    for d in curr:
        p = prev_by_name.get(d.name)
        if p is None:
            continue
        delta += (d.read_bytes_total - p.read_bytes_total) + (
            d.write_bytes_total - p.write_bytes_total
        )
    return delta


def _sum_net_delta(curr: Any, prev: Any) -> float:
    """Total rx + tx bytes since the previous sample across all TAPs."""
    prev_by_name = {n.tap_name: n for n in prev}
    delta = 0.0
    for n in curr:
        p = prev_by_name.get(n.tap_name)
        if p is None:
            continue
        delta += (n.rx_bytes_total - p.rx_bytes_total) + (
            n.tx_bytes_total - p.tx_bytes_total
        )
    return delta
