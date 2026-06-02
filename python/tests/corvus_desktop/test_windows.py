"""Ring 3 smoke tests: each window constructs against a mock bridge,
shows without exceptions, and exposes a sentinel widget by class.

Pixel comparisons are deliberately out of scope — these tests catch
import errors, missing signal wiring, and surprise dependencies.
"""

from __future__ import annotations

from collections.abc import Iterator
from datetime import datetime, timezone
from typing import Any

import pytest
from corvus_client.types import (
    DiskAttachment,
    DiskImageInfo,
    DiskImagePlacement,
    NamedRef,
    SnapshotInfo,
    TaskInfo,
    TaskProgressFinished,
    TaskProgressProgress,
    TaskProgressStarted,
    VmDetails,
    VmInfo,
)
from corvus_desktop.windows.disk_detail import DiskDetailWidget
from corvus_desktop.windows.disk_list import DiskListWidget
from corvus_desktop.windows.main_window import MainWindow
from corvus_desktop.windows.task_detail import TaskDetailWidget
from corvus_desktop.windows.task_list import TaskListWidget
from corvus_desktop.windows.vm_detail import VmDetailWidget
from corvus_desktop.windows.vm_list import VmListWidget
from PySide6.QtCore import QObject, Signal
from PySide6.QtWidgets import QProgressBar, QTableView


class _MockBridge(QObject):
    """Mirrors :class:`CorvusBridge`'s signal surface; slots are
    plain recorders so tests can assert what the UI asked for."""

    connected = Signal()
    connection_failed = Signal(str)
    status_ready = Signal(object)
    operation_failed = Signal(str, str)
    task_list_ready = Signal(object)
    task_event = Signal(int, object)
    task_detail_ready = Signal(object)
    vm_list_ready = Signal(object)
    vm_detail_ready = Signal(object)
    vm_action_completed = Signal(int, str, str)
    serial_opened = Signal(int)
    serial_data = Signal(int, bytes)
    serial_closed = Signal(int, str)
    disk_list_ready = Signal(object)
    disk_detail_ready = Signal(object)
    snapshot_list_ready = Signal(int, object)
    disk_action_completed = Signal(int, str)
    snapshot_action_completed = Signal(int, str, str)
    ssh_key_list_ready = Signal(object)
    ssh_key_action_completed = Signal(int, str)
    network_list_ready = Signal(object)
    network_detail_ready = Signal(object)
    network_action_completed = Signal(int, str)
    template_list_ready = Signal(object)
    template_detail_ready = Signal(object)
    template_action_completed = Signal(int, str)
    cloud_init_ready = Signal(int, object)
    cloud_init_action_completed = Signal(int, str)
    apply_completed = Signal(object, int)
    vm_edit_completed = Signal(int, str)
    vm_shared_dirs_ready = Signal(int, object)
    guest_exec_result = Signal(int, object)
    disk_import_started = Signal(int)
    node_list_ready = Signal(object)
    node_detail_ready = Signal(object)
    node_action_completed = Signal(int, str)
    vm_migrate_started = Signal(int, int)
    build_started = Signal(int)
    build_event = Signal(object)
    build_finished = Signal(str)
    vm_stats_event = Signal(int, object)
    vm_stats_history_ready = Signal(int, object)
    vm_ssh_keys_ready = Signal(int, object)
    guest_agent_event = Signal(int, object)
    view_grant_ready = Signal(int, object)
    daemon_shutdown_completed = Signal()
    hmp_data = Signal(int, bytes)
    hmp_closed = Signal(int, str)

    def __init__(self) -> None:
        super().__init__()
        self.status_calls = 0
        self.task_list_calls: list[dict[str, Any]] = []
        self.subscribed: list[int] = []
        self.unsubscribed: list[int] = []
        self.vm_list_calls = 0
        self.vm_detail_calls: list[int] = []
        self.vm_actions: list[tuple[int, str]] = []
        self.serial_opens: list[int] = []
        self.serial_closes: list[int] = []
        self.serial_sends: list[tuple[int, bytes]] = []
        self.disk_list_calls = 0
        self.disk_detail_calls: list[int] = []
        self.snapshot_list_calls: list[int] = []
        self.disk_resizes: list[tuple[int, int]] = []
        self.disk_deletes: list[int] = []
        self.snapshot_creates: list[tuple[int, str]] = []
        self.snapshot_actions: list[tuple[int, str, str]] = []
        self.ssh_key_list_calls = 0
        self.network_list_calls = 0
        self.network_detail_calls: list[int] = []

    def request_status(self) -> None:
        self.status_calls += 1

    def request_task_list(
        self,
        limit: int | None = 50,
        subsystem: str | None = None,
        result: str | None = None,
    ) -> None:
        self.task_list_calls.append(
            {"limit": limit, "subsystem": subsystem, "result": result}
        )

    def subscribe_task(self, task_id: int) -> None:
        self.subscribed.append(task_id)

    def unsubscribe_task(self, task_id: int) -> None:
        self.unsubscribed.append(task_id)

    def request_task_detail(self, task_id: int) -> None: ...

    def request_vm_list(self) -> None:
        self.vm_list_calls += 1

    def request_vm_detail(self, vm_id: int) -> None:
        self.vm_detail_calls.append(vm_id)

    def vm_action(self, vm_id: int, action: str) -> None:
        self.vm_actions.append((vm_id, action))

    def open_serial(self, vm_id: int) -> None:
        self.serial_opens.append(vm_id)

    def close_serial(self, vm_id: int) -> None:
        self.serial_closes.append(vm_id)

    def send_serial(self, vm_id: int, chunk: bytes) -> None:
        self.serial_sends.append((vm_id, chunk))

    def request_disk_list(self) -> None:
        self.disk_list_calls += 1

    def request_disk_detail(self, disk_id: int) -> None:
        self.disk_detail_calls.append(disk_id)

    def request_snapshot_list(self, disk_id: int) -> None:
        self.snapshot_list_calls.append(disk_id)

    def disk_resize(self, disk_id: int, new_size_mb: int) -> None:
        self.disk_resizes.append((disk_id, new_size_mb))

    def disk_delete(self, disk_id: int) -> None:
        self.disk_deletes.append(disk_id)

    def snapshot_create(self, disk_id: int, name: str) -> None:
        self.snapshot_creates.append((disk_id, name))

    def request_ssh_key_list(self) -> None:
        self.ssh_key_list_calls += 1

    def request_network_list(self) -> None:
        self.network_list_calls += 1

    def request_network_detail(self, network_id: int) -> None:
        self.network_detail_calls.append(network_id)

    def network_create(self, name: str, subnet: str, **kwargs: Any) -> None: ...
    def network_start(self, network_id: int) -> None: ...
    def network_stop(self, network_id: int, *, force: bool = False) -> None: ...
    def network_delete(self, network_id: int) -> None: ...
    def network_edit(self, network_id: int, **kwargs: Any) -> None: ...
    def ssh_key_create(self, name: str, public_key: str) -> None: ...
    def ssh_key_delete(self, key_id: int) -> None: ...

    def request_template_list(self) -> None: ...
    def request_template_detail(self, template_id: int) -> None: ...
    def template_create(self, yaml_text: str) -> None: ...
    def template_update(self, template_id: int, yaml_text: str) -> None: ...
    def template_delete(self, template_id: int) -> None: ...
    def template_instantiate(
        self, template_id: int, vm_name: str, node: str | None = None
    ) -> None: ...

    def request_cloud_init(self, vm_id: int) -> None: ...
    def cloud_init_set(self, vm_id: int, **kwargs: Any) -> None: ...
    def cloud_init_delete(self, vm_id: int) -> None: ...

    def apply_yaml(self, yaml_text: str, *, skip_existing: bool = False) -> None: ...

    def vm_create(self, **kwargs: Any) -> None: ...
    def vm_edit(self, vm_id: int, **kwargs: Any) -> None: ...
    def vm_delete(self, vm_id: int, *, keep_disks: bool = False) -> None: ...
    def vm_attach_disk(self, vm_id: int, disk_ref: Any, **kwargs: Any) -> None: ...
    def vm_detach_disk(self, vm_id: int, drive_id: int) -> None: ...
    def vm_add_net_if(self, vm_id: int, **kwargs: Any) -> None: ...
    def vm_remove_net_if(self, vm_id: int, net_if_id: int) -> None: ...
    def vm_attach_ssh_key(self, vm_id: int, key_ref: Any) -> None: ...
    def vm_detach_ssh_key(self, vm_id: int, key_ref: Any) -> None: ...
    def vm_add_shared_dir(
        self, vm_id: int, path: str, tag: str, **kwargs: Any
    ) -> None: ...
    def vm_remove_shared_dir(self, vm_id: int, shared_dir_id: int) -> None: ...
    def request_vm_shared_dirs(self, vm_id: int) -> None: ...
    def vm_guest_exec(self, vm_id: int, command: str) -> None: ...

    def disk_create(self, name: str, size_mb: int, **kwargs: Any) -> None: ...
    def disk_register(self, name: str, file_path: str, **kwargs: Any) -> None: ...
    def disk_overlay(self, name: str, backing_disk_ref: Any, **kwargs: Any) -> None: ...
    def disk_clone(self, source_ref: Any, new_name: str, **kwargs: Any) -> None: ...
    def disk_import_url(self, name: str, url: str, **kwargs: Any) -> None: ...
    def disk_rebase(self, disk_id: int, new_backing_disk_ref: Any) -> None: ...
    def disk_flatten(self, disk_id: int) -> None: ...
    def disk_copy(self, disk_id: int, to_node_ref: Any, **kwargs: Any) -> None: ...
    def disk_move(self, disk_id: int, to_node_ref: Any, **kwargs: Any) -> None: ...

    def request_node_list(self) -> None: ...
    def request_node_detail(self, node_id: int) -> None: ...
    def node_create(self, **kwargs: Any) -> None: ...
    def node_edit(self, node_id: int, **kwargs: Any) -> None: ...
    def node_drain(self, node_id: int) -> None: ...
    def node_delete(self, node_id: int) -> None: ...
    def vm_migrate(self, vm_id: int, to_node_ref: Any) -> None: ...
    def network_attach_node(self, network_id: int, node_ref: Any) -> None: ...
    def network_detach_node(self, network_id: int, node_ref: Any) -> None: ...

    def build_run(self, yaml_text: str) -> None: ...

    def subscribe_vm_stats(self, vm_id: int) -> None: ...
    def unsubscribe_vm_stats(self, vm_id: int) -> None: ...
    def request_vm_stats_history(self, vm_id: int) -> None: ...
    def request_vm_ssh_keys(self, vm_id: int) -> None: ...
    def subscribe_guest_agent(self, vm_id: int) -> None: ...
    def unsubscribe_guest_agent(self, vm_id: int) -> None: ...
    def request_view_grant(self, vm_id: int) -> None: ...
    def daemon_shutdown(self) -> None: ...
    def open_hmp(self, vm_id: int) -> None: ...
    def send_hmp(self, vm_id: int, chunk: bytes) -> None: ...
    def close_hmp(self, vm_id: int) -> None: ...

    def snapshot_action(self, disk_id: int, snap_name: str, action: str) -> None:
        self.snapshot_actions.append((disk_id, snap_name, action))


@pytest.fixture
def bridge() -> _MockBridge:
    return _MockBridge()


@pytest.fixture
def task_list_widget(qapp: Any, bridge: _MockBridge) -> Iterator[TaskListWidget]:
    widget = TaskListWidget(bridge)
    yield widget
    widget.deleteLater()


@pytest.fixture
def task_detail_widget(qapp: Any, bridge: _MockBridge) -> Iterator[TaskDetailWidget]:
    widget = TaskDetailWidget(bridge)
    yield widget
    widget.deleteLater()


@pytest.fixture
def vm_list_widget(qapp: Any, bridge: _MockBridge) -> Iterator[VmListWidget]:
    widget = VmListWidget(bridge)
    yield widget
    widget.deleteLater()


@pytest.fixture
def vm_detail_widget(qapp: Any, bridge: _MockBridge) -> Iterator[VmDetailWidget]:
    widget = VmDetailWidget(bridge)
    yield widget
    widget.deleteLater()


@pytest.fixture
def disk_list_widget(qapp: Any, bridge: _MockBridge) -> Iterator[DiskListWidget]:
    widget = DiskListWidget(bridge)
    yield widget
    widget.deleteLater()


@pytest.fixture
def disk_detail_widget(qapp: Any, bridge: _MockBridge) -> Iterator[DiskDetailWidget]:
    widget = DiskDetailWidget(bridge)
    yield widget
    widget.deleteLater()


def _task(task_id: int = 1, result: str = "running") -> TaskInfo:
    return TaskInfo(
        id=task_id,
        started_at=datetime.now(timezone.utc),
        subsystem="vm",
        command="start",
        result=result,
    )


def _vm(vm_id: int = 1, name: str = "web-1", status: str = "running") -> VmInfo:
    return VmInfo(
        id=vm_id,
        name=name,
        node=NamedRef(id=99, name="node-a"),
        status=status,
        cpu_count=2,
        ram_mb=1024,
        headless=False,
        guest_agent=True,
        cloud_init=True,
        autostart=False,
    )


def _disk(disk_id: int = 1, name: str = "root", size_mb: int = 4096) -> DiskImageInfo:
    return DiskImageInfo(
        id=disk_id,
        name=name,
        format="qcow2",
        created_at=datetime.now(timezone.utc),
        placements=[
            DiskImagePlacement(node=NamedRef(id=1, name="node-a"), file_path="/x")
        ],
        attached_to=[],
        size_mb=size_mb,
    )


def _attached_disk(disk_id: int = 1, vm_name: str = "web-1") -> DiskImageInfo:
    return DiskImageInfo(
        id=disk_id,
        name="attached",
        format="qcow2",
        created_at=datetime.now(timezone.utc),
        placements=[
            DiskImagePlacement(node=NamedRef(id=1, name="node-a"), file_path="/x")
        ],
        attached_to=[DiskAttachment(vm=NamedRef(id=9, name=vm_name))],
        size_mb=1024,
    )


def _snap(name: str, snap_id: int = 1, size_mb: int = 64) -> SnapshotInfo:
    return SnapshotInfo(
        id=snap_id,
        name=name,
        created_at=datetime.now(timezone.utc),
        size_mb=size_mb,
    )


def _vm_details(vm_id: int = 1, status: str = "running") -> VmDetails:
    return VmDetails(
        id=vm_id,
        name=f"vm-{vm_id}",
        node=NamedRef(id=99, name="node-a"),
        created_at=datetime.now(timezone.utc),
        status=status,
        cpu_count=2,
        ram_mb=1024,
        headless=False,
        monitor_socket="",
        serial_socket="",
        guest_agent_socket="",
        guest_agent=True,
        cloud_init=True,
        autostart=False,
    )


# ---------------------------------------------------------- TaskListWidget


def test_task_list_renders_and_refresh_calls_bridge(
    task_list_widget: TaskListWidget, bridge: _MockBridge
) -> None:
    task_list_widget.show()
    bridge.task_list_calls.clear()
    task_list_widget.refresh()
    assert len(bridge.task_list_calls) == 1
    call = bridge.task_list_calls[0]
    assert call["subsystem"] is None
    assert call["result"] is None


def test_task_list_ready_populates_model(
    task_list_widget: TaskListWidget, bridge: _MockBridge
) -> None:
    bridge.task_list_ready.emit([_task(1), _task(2), _task(3)])
    table = task_list_widget.findChild(QTableView)
    assert table is not None
    assert table.model().rowCount() == 3


# ---------------------------------------------------------- TaskDetailWidget


def test_task_detail_subscribes_on_set_task_id(
    task_detail_widget: TaskDetailWidget, bridge: _MockBridge
) -> None:
    task_detail_widget.set_task_id(99)
    assert bridge.subscribed == [99]


def test_task_detail_progress_event_updates_bar(
    task_detail_widget: TaskDetailWidget, bridge: _MockBridge
) -> None:
    task_detail_widget.set_task_id(7)
    bar = task_detail_widget.findChild(QProgressBar)
    assert bar is not None
    bridge.task_event.emit(
        7, TaskProgressStarted(task_id=7, command="start", subsystem="vm")
    )
    bridge.task_event.emit(
        7, TaskProgressProgress(task_id=7, completed=3, total=10, label="step")
    )
    assert bar.maximum() == 10
    assert bar.value() == 3
    bridge.task_event.emit(
        7, TaskProgressFinished(task_id=7, result="success", message=None)
    )
    assert bar.maximum() == 1


def test_task_detail_filters_events_by_task_id(
    task_detail_widget: TaskDetailWidget, bridge: _MockBridge
) -> None:
    task_detail_widget.set_task_id(11)
    bar = task_detail_widget.findChild(QProgressBar)
    assert bar is not None
    bridge.task_event.emit(
        99, TaskProgressProgress(task_id=99, completed=5, total=5, label="x")
    )
    assert bar.value() == 0
    assert bar.maximum() == 0


def test_task_detail_clear_unsubscribes(
    task_detail_widget: TaskDetailWidget, bridge: _MockBridge
) -> None:
    task_detail_widget.set_task_id(3)
    task_detail_widget.clear()
    assert bridge.unsubscribed == [3]


# ---------------------------------------------------------- VmListWidget


def test_vm_list_refresh_calls_bridge(
    vm_list_widget: VmListWidget, bridge: _MockBridge
) -> None:
    vm_list_widget.refresh()
    assert bridge.vm_list_calls == 1


def test_vm_list_ready_populates_model(
    vm_list_widget: VmListWidget, bridge: _MockBridge
) -> None:
    bridge.vm_list_ready.emit([_vm(1, "a"), _vm(2, "b")])
    table = vm_list_widget.findChild(QTableView)
    assert table is not None
    assert table.model().rowCount() == 2


def test_vm_list_refreshes_after_action_completes(
    vm_list_widget: VmListWidget, bridge: _MockBridge
) -> None:
    bridge.vm_list_calls = 0
    bridge.vm_action_completed.emit(1, "start", "running")
    assert bridge.vm_list_calls == 1


# ---------------------------------------------------------- VmDetailWidget


def test_vm_detail_set_id_requests_detail(
    vm_detail_widget: VmDetailWidget, bridge: _MockBridge
) -> None:
    vm_detail_widget.set_vm_id(42)
    assert bridge.vm_detail_calls == [42]


def test_vm_detail_action_buttons_gate_on_status(
    vm_detail_widget: VmDetailWidget, bridge: _MockBridge
) -> None:
    vm_detail_widget.set_vm_id(5)
    bridge.vm_detail_ready.emit(_vm_details(5, status="running"))
    # When running, start should be disabled; stop should be enabled.
    assert vm_detail_widget._btn_start.isEnabled() is False
    assert vm_detail_widget._btn_stop.isEnabled() is True
    assert vm_detail_widget._btn_pause.isEnabled() is True


def test_vm_detail_action_button_emits_bridge_call(
    vm_detail_widget: VmDetailWidget, bridge: _MockBridge
) -> None:
    vm_detail_widget.set_vm_id(5)
    bridge.vm_detail_ready.emit(_vm_details(5, status="stopped"))
    assert vm_detail_widget._btn_start.isEnabled()
    vm_detail_widget._btn_start.click()
    assert bridge.vm_actions == [(5, "start")]


def test_vm_detail_serial_tab_opens_session(
    vm_detail_widget: VmDetailWidget, bridge: _MockBridge
) -> None:
    vm_detail_widget.set_vm_id(7)
    bridge.vm_detail_ready.emit(_vm_details(7, status="running"))
    # Activate the Serial tab — the widget should ask the bridge to
    # open a session, and close it when we switch away.
    serial_index = vm_detail_widget._tabs.indexOf(vm_detail_widget._serial)
    vm_detail_widget._tabs.setCurrentIndex(serial_index)
    assert bridge.serial_opens == [7]
    vm_detail_widget._tabs.setCurrentIndex(0)
    assert bridge.serial_closes == [7]


def test_vm_detail_serial_data_feeds_console(
    vm_detail_widget: VmDetailWidget, bridge: _MockBridge
) -> None:
    vm_detail_widget.set_vm_id(8)
    bridge.vm_detail_ready.emit(_vm_details(8, status="running"))
    # Pretend the session is open: switch to serial tab to wire it up.
    serial_index = vm_detail_widget._tabs.indexOf(vm_detail_widget._serial)
    vm_detail_widget._tabs.setCurrentIndex(serial_index)
    bridge.serial_data.emit(8, b"hello\n")
    first_row = vm_detail_widget._serial._screen.display[0]
    assert first_row.startswith("hello")


def test_vm_detail_clear_closes_serial(
    vm_detail_widget: VmDetailWidget, bridge: _MockBridge
) -> None:
    vm_detail_widget.set_vm_id(9)
    bridge.vm_detail_ready.emit(_vm_details(9, status="running"))
    # Switch to serial tab (opens), then clear → should close.
    serial_index = vm_detail_widget._tabs.indexOf(vm_detail_widget._serial)
    vm_detail_widget._tabs.setCurrentIndex(serial_index)
    bridge.serial_closes.clear()
    vm_detail_widget.clear()
    assert bridge.serial_closes == [9]


# ---------------------------------------------------------- MainWindow


def test_main_window_constructs_and_shows(qapp: Any, bridge: _MockBridge) -> None:
    win = MainWindow(bridge, target="/tmp/socket")
    try:
        win.show()
        bridge.task_list_calls.clear()
        bridge.vm_list_calls = 0
        bridge.connected.emit()
        assert bridge.status_calls == 1
        assert len(bridge.task_list_calls) == 1
        assert bridge.vm_list_calls == 1
    finally:
        win.deleteLater()


def test_main_window_opens_task_detail_on_activation(
    qapp: Any, bridge: _MockBridge
) -> None:
    win = MainWindow(bridge, target="/tmp/socket")
    try:
        win._task_list.task_activated.emit(123)
        assert win._stack.currentIndex() == MainWindow.PAGE_TASK_DETAIL
        assert bridge.subscribed == [123]
        win._task_detail.back_requested.emit()
        assert win._stack.currentIndex() == MainWindow.PAGE_TASKS
        assert bridge.unsubscribed == [123]
    finally:
        win.deleteLater()


def test_main_window_opens_vm_detail_on_activation(
    qapp: Any, bridge: _MockBridge
) -> None:
    win = MainWindow(bridge, target="/tmp/socket")
    try:
        win._vm_list.vm_activated.emit(55)
        assert win._stack.currentIndex() == MainWindow.PAGE_VM_DETAIL
        assert 55 in bridge.vm_detail_calls
        win._vm_detail.back_requested.emit()
        assert win._stack.currentIndex() == MainWindow.PAGE_VMS
    finally:
        win.deleteLater()


# ---------------------------------------------------------- DiskListWidget


def test_disk_list_refresh_calls_bridge(
    disk_list_widget: DiskListWidget, bridge: _MockBridge
) -> None:
    disk_list_widget.refresh()
    assert bridge.disk_list_calls == 1


def test_disk_list_ready_populates_model(
    disk_list_widget: DiskListWidget, bridge: _MockBridge
) -> None:
    bridge.disk_list_ready.emit([_disk(1), _disk(2)])
    table = disk_list_widget.findChild(QTableView)
    assert table is not None
    assert table.model().rowCount() == 2


def test_disk_list_refreshes_after_action(
    disk_list_widget: DiskListWidget, bridge: _MockBridge
) -> None:
    bridge.disk_list_calls = 0
    bridge.disk_action_completed.emit(1, "resize")
    assert bridge.disk_list_calls == 1


# ---------------------------------------------------------- DiskDetailWidget


def test_disk_detail_set_id_requests_detail_and_snapshots(
    disk_detail_widget: DiskDetailWidget, bridge: _MockBridge
) -> None:
    disk_detail_widget.set_disk_id(7)
    assert bridge.disk_detail_calls == [7]
    assert bridge.snapshot_list_calls == [7]


def test_disk_detail_delete_button_disabled_when_attached(
    disk_detail_widget: DiskDetailWidget, bridge: _MockBridge
) -> None:
    disk_detail_widget.set_disk_id(1)
    bridge.disk_detail_ready.emit(_attached_disk(1))
    assert disk_detail_widget._btn_delete.isEnabled() is False


def test_disk_detail_delete_button_enabled_when_free(
    disk_detail_widget: DiskDetailWidget, bridge: _MockBridge
) -> None:
    disk_detail_widget.set_disk_id(2)
    bridge.disk_detail_ready.emit(_disk(2))
    assert disk_detail_widget._btn_delete.isEnabled() is True


def test_disk_detail_create_snapshot_with_blank_name_is_noop(
    disk_detail_widget: DiskDetailWidget, bridge: _MockBridge
) -> None:
    disk_detail_widget.set_disk_id(3)
    disk_detail_widget._new_snap_name.setText("")
    disk_detail_widget._on_create_snapshot()
    assert bridge.snapshot_creates == []


def test_disk_detail_create_snapshot_sends_to_bridge(
    disk_detail_widget: DiskDetailWidget, bridge: _MockBridge
) -> None:
    disk_detail_widget.set_disk_id(4)
    disk_detail_widget._new_snap_name.setText("v1")
    disk_detail_widget._on_create_snapshot()
    assert bridge.snapshot_creates == [(4, "v1")]
    # The field should clear after submission.
    assert disk_detail_widget._new_snap_name.text() == ""


def test_disk_detail_snapshots_populate(
    disk_detail_widget: DiskDetailWidget, bridge: _MockBridge
) -> None:
    disk_detail_widget.set_disk_id(5)
    bridge.snapshot_list_ready.emit(5, [_snap("a"), _snap("b", snap_id=2)])
    assert disk_detail_widget._snap_table.rowCount() == 2


def test_disk_detail_delete_action_signals_back(
    disk_detail_widget: DiskDetailWidget, bridge: _MockBridge
) -> None:
    back_signals: list[None] = []
    disk_detail_widget.back_requested.connect(lambda: back_signals.append(None))
    disk_detail_widget.set_disk_id(6)
    bridge.disk_action_completed.emit(6, "delete")
    # On delete completion, the detail widget asks the host to take us
    # back to the list.
    assert len(back_signals) == 1


# ---------------------------------------------------------- MainWindow / disks


def test_main_window_opens_disk_detail_on_activation(
    qapp: Any, bridge: _MockBridge
) -> None:
    win = MainWindow(bridge, target="/tmp/socket")
    try:
        win._disk_list.disk_activated.emit(77)
        assert win._stack.currentIndex() == MainWindow.PAGE_DISK_DETAIL
        assert 77 in bridge.disk_detail_calls
        win._disk_detail.back_requested.emit()
        assert win._stack.currentIndex() == MainWindow.PAGE_DISKS
    finally:
        win.deleteLater()
