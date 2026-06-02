"""Ring 2 tests: :class:`StatsSparkline` + the VM-detail stats wiring."""

from __future__ import annotations

from typing import Any

from corvus_client.types import DriveIo, NetIo, SshKeyInfo, VmStats
from corvus_desktop.widgets.stats_chart import StatsSparkline
from corvus_desktop.windows.vm_detail import (
    _sum_drive_delta,
    _sum_net_delta,
)
from PySide6.QtCore import QObject, Signal


def test_sparkline_rolling_window(qapp: Any) -> None:
    spark = StatsSparkline(max_points=3)
    for v in (1.0, 2.0, 3.0, 4.0):
        spark.add_point(v)
    assert spark.latest() == 4.0
    # After 4 adds with a 3-point window, the oldest must've been dropped.
    assert spark._values == [2.0, 3.0, 4.0]


def test_sparkline_clear(qapp: Any) -> None:
    spark = StatsSparkline()
    spark.add_point(5.0)
    spark.clear()
    assert spark.latest() is None
    assert spark._values == []


def test_sum_drive_delta_handles_name_match() -> None:
    prev = [DriveIo("vda", 100, 200, 5, 10)]
    curr = [DriveIo("vda", 250, 350, 7, 12)]
    # +150 reads, +150 writes → 300 bytes total.
    assert _sum_drive_delta(curr, prev) == 300


def test_sum_drive_delta_ignores_new_drive() -> None:
    """A drive present in curr but not prev contributes zero (rather
    than spiking the chart from a fake 0→big delta)."""
    prev: list[DriveIo] = []
    curr = [DriveIo("vda", 100, 0, 1, 0)]
    assert _sum_drive_delta(curr, prev) == 0


def test_sum_net_delta() -> None:
    prev = [NetIo("tap0", 1000, 2000)]
    curr = [NetIo("tap0", 1500, 2500)]
    # +500 rx + +500 tx = 1000.
    assert _sum_net_delta(curr, prev) == 1000


class _MockBridge(QObject):
    """Subset of the bridge surface VmDetail's SSH keys path touches."""

    operation_failed = Signal(str, str)
    vm_detail_ready = Signal(object)
    vm_action_completed = Signal(int, str, str)
    vm_edit_completed = Signal(int, str)
    vm_shared_dirs_ready = Signal(int, object)
    vm_ssh_keys_ready = Signal(int, object)
    serial_data = Signal(int, bytes)
    serial_closed = Signal(int, str)
    vm_stats_event = Signal(int, object)
    vm_stats_history_ready = Signal(int, object)
    guest_agent_event = Signal(int, object)
    view_grant_ready = Signal(int, object)
    daemon_shutdown_completed = Signal()
    hmp_data = Signal(int, bytes)
    hmp_closed = Signal(int, str)
    cloud_init_ready = Signal(int, object)
    cloud_init_action_completed = Signal(int, str)

    def __init__(self) -> None:
        super().__init__()
        self.ssh_key_list_calls: list[int] = []
        self.detach_ssh_key_calls: list[tuple[int, int]] = []
        self.stats_history_calls: list[int] = []

    def request_vm_detail(self, vm_id: int) -> None: ...
    def request_vm_shared_dirs(self, vm_id: int) -> None: ...

    def request_vm_ssh_keys(self, vm_id: int) -> None:
        self.ssh_key_list_calls.append(vm_id)

    def request_vm_stats_history(self, vm_id: int) -> None:
        self.stats_history_calls.append(vm_id)

    def subscribe_vm_stats(self, vm_id: int) -> None: ...
    def unsubscribe_vm_stats(self, vm_id: int) -> None: ...
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

    def vm_detach_ssh_key(self, vm_id: int, key_ref: object) -> None:
        assert isinstance(key_ref, int)
        self.detach_ssh_key_calls.append((vm_id, key_ref))

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


def test_vm_detail_renders_ssh_key_list(qapp: Any) -> None:
    from datetime import datetime, timezone

    from corvus_desktop.windows.vm_detail import VmDetailWidget

    bridge = _MockBridge()
    w = VmDetailWidget(bridge)
    try:
        w.set_vm_id(7)
        assert bridge.ssh_key_list_calls == [7]
        bridge.vm_ssh_keys_ready.emit(
            7,
            [
                SshKeyInfo(
                    id=11,
                    name="workstation",
                    public_key="ssh-ed25519 AAAA",
                    created_at=datetime.now(timezone.utc),
                ),
                SshKeyInfo(
                    id=12,
                    name="laptop",
                    public_key="ssh-ed25519 BBBB",
                    created_at=datetime.now(timezone.utc),
                ),
            ],
        )
        assert w._ssh_table.rowCount() == 2
        assert w._ssh_table.item(0, 0).text() == "workstation"
    finally:
        w.deleteLater()


def test_vm_detail_stats_appends_chart_point(qapp: Any) -> None:
    from corvus_desktop.windows.vm_detail import VmDetailWidget

    bridge = _MockBridge()
    w = VmDetailWidget(bridge)
    try:
        w._vm_id = 9
        # Two samples 1 s apart with a CPU jiffies delta of 100.
        prev = VmStats(
            sampled_at_nanos=0,
            interval_millis=1000,
            cpu_jiffies_total=1000,
            clk_tck=100,
            host_rss_bytes=200 * 1024 * 1024,
            balloon_actual_bytes=0,
            balloon_max_bytes=0,
        )
        curr = VmStats(
            sampled_at_nanos=1_000_000_000,
            interval_millis=1000,
            cpu_jiffies_total=1100,
            clk_tck=100,
            host_rss_bytes=210 * 1024 * 1024,
            balloon_actual_bytes=0,
            balloon_max_bytes=0,
        )
        bridge.vm_stats_event.emit(9, prev)
        bridge.vm_stats_event.emit(9, curr)
        # RAM is an instant gauge → two points; CPU is a rate → one point.
        assert w._chart_ram.latest() is not None
        # delta_jiffies (100) / (1s * clk_tck (100)) * 100 = 100% on one core.
        cpu_latest = w._chart_cpu.latest()
        assert cpu_latest is not None
        assert 99.0 < cpu_latest < 101.0
    finally:
        w.deleteLater()
