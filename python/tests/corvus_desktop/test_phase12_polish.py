"""Phase 12 tests: SPICE launcher fallback + guest-agent badge wiring."""

from __future__ import annotations

from typing import Any

import pytest
from corvus_client.types import GuestAgentStatus, ViewGrant
from corvus_desktop.widgets import spice_launcher
from PySide6.QtCore import QObject, Signal


class _MockBridge(QObject):
    """Subset of CorvusBridge surface needed for VmDetail construction."""

    operation_failed = Signal(str, str)
    vm_detail_ready = Signal(object)
    vm_action_completed = Signal(int, str, str)
    vm_edit_completed = Signal(int, str)
    vm_shared_dirs_ready = Signal(int, object)
    serial_data = Signal(int, bytes)
    serial_closed = Signal(int, str)
    vm_stats_event = Signal(int, object)
    vm_stats_history_ready = Signal(int, object)
    vm_ssh_keys_ready = Signal(int, object)
    guest_agent_event = Signal(int, object)
    view_grant_ready = Signal(int, object)
    daemon_shutdown_completed = Signal()
    hmp_data = Signal(int, bytes)
    hmp_closed = Signal(int, str)
    cloud_init_ready = Signal(int, object)
    cloud_init_action_completed = Signal(int, str)

    def __init__(self) -> None:
        super().__init__()
        self.shutdown_calls = 0

    def daemon_shutdown(self) -> None:
        self.shutdown_calls += 1

    def request_vm_detail(self, vm_id: int) -> None: ...
    def request_vm_shared_dirs(self, vm_id: int) -> None: ...
    def subscribe_vm_stats(self, vm_id: int) -> None: ...
    def unsubscribe_vm_stats(self, vm_id: int) -> None: ...
    def request_vm_stats_history(self, vm_id: int) -> None: ...
    def request_vm_ssh_keys(self, vm_id: int) -> None: ...
    def subscribe_guest_agent(self, vm_id: int) -> None: ...
    def unsubscribe_guest_agent(self, vm_id: int) -> None: ...
    def open_serial(self, vm_id: int) -> None: ...
    def close_serial(self, vm_id: int) -> None: ...
    def open_hmp(self, vm_id: int) -> None: ...
    def close_hmp(self, vm_id: int) -> None: ...
    def send_serial(self, vm_id: int, chunk: bytes) -> None: ...
    def send_hmp(self, vm_id: int, chunk: bytes) -> None: ...
    def request_cloud_init(self, vm_id: int) -> None: ...
    def cloud_init_set(self, vm_id: int, **kwargs: object) -> None: ...
    def cloud_init_delete(self, vm_id: int) -> None: ...
    def vm_action(self, vm_id: int, action: str) -> None: ...
    def vm_edit(self, vm_id: int, **kwargs: object) -> None: ...
    def vm_delete(self, vm_id: int, *, keep_disks: bool = False) -> None: ...
    def vm_attach_disk(
        self, vm_id: int, disk_ref: object, **kwargs: object
    ) -> None: ...
    def vm_detach_disk(self, vm_id: int, drive_id: int) -> None: ...
    def vm_add_net_if(self, vm_id: int, **kwargs: object) -> None: ...
    def vm_remove_net_if(self, vm_id: int, net_if_id: int) -> None: ...
    def vm_attach_ssh_key(self, vm_id: int, key_ref: object) -> None: ...
    def vm_detach_ssh_key(self, vm_id: int, key_ref: object) -> None: ...
    def vm_add_shared_dir(
        self, vm_id: int, path: str, tag: str, **kwargs: object
    ) -> None: ...
    def vm_remove_shared_dir(self, vm_id: int, shared_dir_id: int) -> None: ...
    def vm_guest_exec(self, vm_id: int, command: str) -> None: ...
    def vm_migrate(self, vm_id: int, to_node_ref: object) -> None: ...
    def request_view_grant(self, vm_id: int) -> None: ...
    def request_disk_list(self) -> None: ...
    def request_network_list(self) -> None: ...
    def request_ssh_key_list(self) -> None: ...
    def request_node_list(self) -> None: ...


@pytest.fixture
def bridge() -> _MockBridge:
    return _MockBridge()


# ----------------------------------------------------- SPICE launcher


def test_remote_viewer_falls_back_to_dialog(monkeypatch: Any, qapp: Any) -> None:
    """When remote-viewer isn't on PATH, the launcher pops an info dialog
    rather than crashing."""
    monkeypatch.setattr(spice_launcher.shutil, "which", lambda _: None)
    captured: list[tuple[Any, str, str]] = []
    monkeypatch.setattr(
        spice_launcher.QMessageBox,
        "information",
        lambda parent, title, body: captured.append((parent, title, body)),
    )
    spice_launcher.launch_remote_viewer(
        ViewGrant(host="h", port=1234, password="p", ttl_seconds=60), None
    )
    assert len(captured) == 1
    assert "remote-viewer is not installed" in captured[0][2]
    assert "1234" in captured[0][2]


def test_remote_viewer_spawns_when_present(monkeypatch: Any, qapp: Any) -> None:
    monkeypatch.setattr(
        spice_launcher.shutil, "which", lambda _: "/usr/bin/remote-viewer"
    )
    calls: list[list[str]] = []

    def _fake_popen(argv: list[str]) -> Any:
        calls.append(argv)
        return None

    monkeypatch.setattr(spice_launcher.subprocess, "Popen", _fake_popen)
    spice_launcher.launch_remote_viewer(
        ViewGrant(host="h", port=1234, password="p", ttl_seconds=60), None
    )
    assert len(calls) == 1
    assert calls[0][0] == "/usr/bin/remote-viewer"


# ----------------------------------------------------- guest-agent badge


def test_guest_agent_badge_states(qapp: Any, bridge: _MockBridge) -> None:
    # Construct a VmDetailWidget — small smoke that the new wiring
    # accepts GuestAgentStatus payloads.
    from corvus_desktop.windows.vm_detail import VmDetailWidget

    w = VmDetailWidget(bridge)
    try:
        # Pretend we're viewing VM #5 so the filter passes.
        w._vm_id = 5
        bridge.guest_agent_event.emit(
            5,
            GuestAgentStatus(vm_id=5, enabled=True, reachable=True),
        )
        assert "running" in w._guest_badge.text()
        bridge.guest_agent_event.emit(
            5,
            GuestAgentStatus(vm_id=5, enabled=True, reachable=False),
        )
        assert "error" in w._guest_badge.text()
        bridge.guest_agent_event.emit(
            5,
            GuestAgentStatus(vm_id=5, enabled=False, reachable=False),
        )
        assert "disabled" in w._guest_badge.text()
    finally:
        w.deleteLater()
