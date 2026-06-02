"""Top-level :class:`QMainWindow` with sidebar nav + central stack.

Phase 6 grows the sidebar with SSH keys and Networks (alongside Tasks,
VMs, Disks) and routes their list / detail pairs through the same
nav pattern used since Phase 2.
"""

from __future__ import annotations

from typing import TYPE_CHECKING

from PySide6.QtGui import QAction
from PySide6.QtWidgets import (
    QApplication,
    QFrame,
    QHBoxLayout,
    QLabel,
    QListWidget,
    QListWidgetItem,
    QMainWindow,
    QMessageBox,
    QStackedWidget,
    QStatusBar,
    QWidget,
)

from .apply import ApplyWidget
from .build import BuildWidget
from .disk_detail import DiskDetailWidget
from .disk_list import DiskListWidget
from .network_detail import NetworkDetailWidget
from .network_list import NetworkListWidget
from .node_detail import NodeDetailWidget
from .node_list import NodeListWidget
from .ssh_key_detail import SshKeyDetailWidget
from .ssh_key_list import SshKeyListWidget
from .task_detail import TaskDetailWidget
from .task_list import TaskListWidget
from .template_detail import TemplateDetailWidget
from .template_list import TemplateListWidget
from .vm_detail import VmDetailWidget
from .vm_list import VmListWidget

if TYPE_CHECKING:
    from ..client_bridge import CorvusBridge


class MainWindow(QMainWindow):
    """The application window. Owns the sidebar + central stack."""

    PAGE_TASKS = 0
    PAGE_TASK_DETAIL = 1
    PAGE_VMS = 2
    PAGE_VM_DETAIL = 3
    PAGE_DISKS = 4
    PAGE_DISK_DETAIL = 5
    PAGE_NETWORKS = 6
    PAGE_NETWORK_DETAIL = 7
    PAGE_SSH_KEYS = 8
    PAGE_SSH_KEY_DETAIL = 9
    PAGE_TEMPLATES = 10
    PAGE_TEMPLATE_DETAIL = 11
    PAGE_APPLY = 12
    PAGE_NODES = 13
    PAGE_NODE_DETAIL = 14
    PAGE_BUILD = 15

    NAV_TASKS = 0
    NAV_VMS = 1
    NAV_DISKS = 2
    NAV_NETWORKS = 3
    NAV_SSH_KEYS = 4
    NAV_TEMPLATES = 5
    NAV_APPLY = 6
    NAV_NODES = 7
    NAV_BUILD = 8

    def __init__(self, bridge: CorvusBridge, target: str) -> None:
        super().__init__()
        self._bridge = bridge
        self.setWindowTitle("corvus-desktop")
        self.resize(1100, 700)

        # ---------------- sidebar ----------------
        self._sidebar = QListWidget()
        self._sidebar.setFrameShape(QFrame.Shape.NoFrame)
        self._sidebar.setFixedWidth(180)
        for label in (
            "Tasks",
            "VMs",
            "Disks",
            "Networks",
            "SSH keys",
            "Templates",
            "Apply",
            "Nodes",
            "Build",
        ):
            self._sidebar.addItem(QListWidgetItem(label))
        self._sidebar.currentRowChanged.connect(self._on_sidebar_changed)

        # ---------------- central stack ----------------
        self._stack = QStackedWidget()

        self._task_list = TaskListWidget(bridge)
        self._task_detail = TaskDetailWidget(bridge)
        self._task_list.task_activated.connect(self._open_task_detail)
        self._task_detail.back_requested.connect(self._close_task_detail)

        self._vm_list = VmListWidget(bridge)
        self._vm_detail = VmDetailWidget(bridge)
        self._vm_list.vm_activated.connect(self._open_vm_detail)
        self._vm_detail.back_requested.connect(self._close_vm_detail)

        self._disk_list = DiskListWidget(bridge)
        self._disk_detail = DiskDetailWidget(bridge)
        self._disk_list.disk_activated.connect(self._open_disk_detail)
        self._disk_detail.back_requested.connect(self._close_disk_detail)

        self._network_list = NetworkListWidget(bridge)
        self._network_detail = NetworkDetailWidget(bridge)
        self._network_list.network_activated.connect(self._open_network_detail)
        self._network_detail.back_requested.connect(self._close_network_detail)

        self._ssh_key_list = SshKeyListWidget(bridge)
        self._ssh_key_detail = SshKeyDetailWidget(bridge)
        self._ssh_key_list.key_activated.connect(self._open_ssh_key_detail)
        self._ssh_key_detail.back_requested.connect(self._close_ssh_key_detail)

        self._template_list = TemplateListWidget(bridge)
        self._template_detail = TemplateDetailWidget(bridge)
        self._template_list.template_activated.connect(self._open_template_detail)
        self._template_detail.back_requested.connect(self._close_template_detail)

        self._apply = ApplyWidget(bridge)
        # Apply emits task_started for the result task. Open task detail
        # on it so the user can watch progress.
        self._apply.task_started.connect(self._open_task_detail)

        self._node_list = NodeListWidget(bridge)
        self._node_detail = NodeDetailWidget(bridge)
        self._node_list.node_activated.connect(self._open_node_detail)
        self._node_detail.back_requested.connect(self._close_node_detail)

        self._build = BuildWidget(bridge)
        self._build.task_started.connect(self._open_task_detail)

        for index, widget in (
            (self.PAGE_TASKS, self._task_list),
            (self.PAGE_TASK_DETAIL, self._task_detail),
            (self.PAGE_VMS, self._vm_list),
            (self.PAGE_VM_DETAIL, self._vm_detail),
            (self.PAGE_DISKS, self._disk_list),
            (self.PAGE_DISK_DETAIL, self._disk_detail),
            (self.PAGE_NETWORKS, self._network_list),
            (self.PAGE_NETWORK_DETAIL, self._network_detail),
            (self.PAGE_SSH_KEYS, self._ssh_key_list),
            (self.PAGE_SSH_KEY_DETAIL, self._ssh_key_detail),
            (self.PAGE_TEMPLATES, self._template_list),
            (self.PAGE_TEMPLATE_DETAIL, self._template_detail),
            (self.PAGE_APPLY, self._apply),
            (self.PAGE_NODES, self._node_list),
            (self.PAGE_NODE_DETAIL, self._node_detail),
            (self.PAGE_BUILD, self._build),
        ):
            self._stack.insertWidget(index, widget)

        # ---------------- root layout ----------------
        root = QWidget()
        root_layout = QHBoxLayout(root)
        root_layout.setContentsMargins(0, 0, 0, 0)
        root_layout.setSpacing(0)
        root_layout.addWidget(self._sidebar)
        root_layout.addWidget(self._stack, 1)
        self.setCentralWidget(root)

        # ---------------- status bar ----------------
        self._status_bar = QStatusBar()
        self.setStatusBar(self._status_bar)
        self._daemon_label = QLabel(f"daemon: {target} (connecting…)")
        self._status_bar.addPermanentWidget(self._daemon_label)
        bridge.connected.connect(self._on_connected)
        bridge.connection_failed.connect(self._on_connection_failed)
        bridge.status_ready.connect(self._on_status_ready)
        bridge.operation_failed.connect(self._on_operation_failed)
        # Disk import / copy / move return a task id. Open the task
        # detail screen so the user can watch progress.
        bridge.disk_import_started.connect(self._open_task_detail)
        # vm_migrate_started carries (vm_id, task_id); open task detail.
        bridge.vm_migrate_started.connect(
            lambda _vm_id, task_id: self._open_task_detail(task_id)
        )

        self._build_menu_bar()

        self._sidebar.setCurrentRow(self.NAV_TASKS)

    # ---------------------------------------------------- menu

    def _build_menu_bar(self) -> None:
        bar = self.menuBar()
        file_menu = bar.addMenu("&File")
        # Close window — hides this window but leaves the QApplication
        # running (the tray icon keeps the event loop alive). Right-click
        # the tray to reopen, or use the bigger hammer below.
        close_action = QAction("&Close window", self)
        close_action.setShortcut("Ctrl+W")
        close_action.triggered.connect(self.close)
        file_menu.addAction(close_action)
        # Quit — really terminate the application. Necessary because
        # the tray icon would otherwise keep us running with no windows
        # shown.
        quit_action = QAction("&Quit", self)
        quit_action.setShortcut("Ctrl+Q")
        quit_action.triggered.connect(QApplication.quit)
        file_menu.addAction(quit_action)

        daemon_menu = bar.addMenu("&Daemon")
        shutdown_action = QAction("&Shutdown daemon…", self)
        shutdown_action.triggered.connect(self._on_daemon_shutdown)
        daemon_menu.addAction(shutdown_action)

        help_menu = bar.addMenu("&Help")
        about_action = QAction("&About corvus-desktop…", self)
        about_action.triggered.connect(self._on_about)
        help_menu.addAction(about_action)

    def _on_daemon_shutdown(self) -> None:
        reply = QMessageBox.warning(
            self,
            "Shutdown daemon",
            "Tell the corvus daemon to shut down? Any running VMs and "
            "in-flight tasks will be affected.",
            QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No,
            QMessageBox.StandardButton.No,
        )
        if reply == QMessageBox.StandardButton.Yes:
            self._bridge.daemon_shutdown()

    def _on_about(self) -> None:
        QMessageBox.about(
            self,
            "corvus-desktop",
            "<b>corvus-desktop</b><br>"
            "Native PySide6 GUI for the corvus VM management daemon.<br><br>"
            "See <code>doc/</code> in the repo for protocol + apply / build "
            "documentation.",
        )

    # ---------------------------------------------------- navigation

    def _on_sidebar_changed(self, row: int) -> None:
        if row == self.NAV_TASKS:
            self._stack.setCurrentIndex(self.PAGE_TASKS)
            self._task_list.refresh()
        elif row == self.NAV_VMS:
            self._stack.setCurrentIndex(self.PAGE_VMS)
            self._vm_list.refresh()
        elif row == self.NAV_DISKS:
            self._stack.setCurrentIndex(self.PAGE_DISKS)
            self._disk_list.refresh()
        elif row == self.NAV_NETWORKS:
            self._stack.setCurrentIndex(self.PAGE_NETWORKS)
            self._network_list.refresh()
        elif row == self.NAV_SSH_KEYS:
            self._stack.setCurrentIndex(self.PAGE_SSH_KEYS)
            self._ssh_key_list.refresh()
        elif row == self.NAV_TEMPLATES:
            self._stack.setCurrentIndex(self.PAGE_TEMPLATES)
            self._template_list.refresh()
        elif row == self.NAV_APPLY:
            self._stack.setCurrentIndex(self.PAGE_APPLY)
        elif row == self.NAV_NODES:
            self._stack.setCurrentIndex(self.PAGE_NODES)
            self._node_list.refresh()
        elif row == self.NAV_BUILD:
            self._stack.setCurrentIndex(self.PAGE_BUILD)

    def _open_task_detail(self, task_id: int) -> None:
        self._task_detail.set_task_id(task_id)
        self._stack.setCurrentIndex(self.PAGE_TASK_DETAIL)

    def _close_task_detail(self) -> None:
        self._task_detail.clear()
        self._stack.setCurrentIndex(self.PAGE_TASKS)
        self._task_list.refresh()

    def _open_vm_detail(self, vm_id: int) -> None:
        self._vm_detail.set_vm_id(vm_id)
        self._stack.setCurrentIndex(self.PAGE_VM_DETAIL)

    def _close_vm_detail(self) -> None:
        self._vm_detail.clear()
        self._stack.setCurrentIndex(self.PAGE_VMS)
        self._vm_list.refresh()

    def _open_disk_detail(self, disk_id: int) -> None:
        self._disk_detail.set_disk_id(disk_id)
        self._stack.setCurrentIndex(self.PAGE_DISK_DETAIL)

    def _close_disk_detail(self) -> None:
        self._disk_detail.clear()
        self._stack.setCurrentIndex(self.PAGE_DISKS)
        self._disk_list.refresh()

    def _open_network_detail(self, network_id: int) -> None:
        self._network_detail.set_network_id(network_id)
        self._stack.setCurrentIndex(self.PAGE_NETWORK_DETAIL)

    def _close_network_detail(self) -> None:
        self._network_detail.clear()
        self._stack.setCurrentIndex(self.PAGE_NETWORKS)
        self._network_list.refresh()

    def _open_ssh_key_detail(self, key_id: int) -> None:
        self._ssh_key_detail.set_key_id(key_id)
        self._stack.setCurrentIndex(self.PAGE_SSH_KEY_DETAIL)

    def _close_ssh_key_detail(self) -> None:
        self._ssh_key_detail.clear()
        self._stack.setCurrentIndex(self.PAGE_SSH_KEYS)
        self._ssh_key_list.refresh()

    def _open_template_detail(self, template_id: int) -> None:
        self._template_detail.set_template_id(template_id)
        self._stack.setCurrentIndex(self.PAGE_TEMPLATE_DETAIL)

    def _close_template_detail(self) -> None:
        self._template_detail.clear()
        self._stack.setCurrentIndex(self.PAGE_TEMPLATES)
        self._template_list.refresh()

    def _open_node_detail(self, node_id: int) -> None:
        self._node_detail.set_node_id(node_id)
        self._stack.setCurrentIndex(self.PAGE_NODE_DETAIL)

    def _close_node_detail(self) -> None:
        self._node_detail.clear()
        self._stack.setCurrentIndex(self.PAGE_NODES)
        self._node_list.refresh()

    # ---------------------------------------------------- bridge slots

    def _on_connected(self) -> None:
        self._daemon_label.setText("connected — fetching status…")
        self._bridge.request_status()
        self._task_list.refresh()
        self._vm_list.refresh()
        self._disk_list.refresh()
        self._network_list.refresh()
        self._ssh_key_list.refresh()
        self._template_list.refresh()
        self._node_list.refresh()

    def _on_connection_failed(self, msg: str) -> None:
        self._daemon_label.setText(f"connection failed: {msg}")
        self._status_bar.showMessage(f"Daemon connection failed: {msg}", 0)

    def _on_status_ready(self, info: object) -> None:
        version = getattr(info, "version", "?")
        uptime = getattr(info, "uptime_seconds", 0)
        self._daemon_label.setText(f"daemon {version}    uptime {uptime}s")

    def _on_operation_failed(self, op: str, message: str) -> None:
        self._status_bar.showMessage(f"{op}: {message}", 5000)
