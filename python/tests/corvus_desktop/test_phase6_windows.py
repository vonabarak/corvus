"""Ring 3 smokes for Phase 6 windows + dialog payload validation."""

from __future__ import annotations

from collections.abc import Iterator
from datetime import datetime, timezone
from typing import Any

import pytest
from corvus_client.types import (
    NamedRef,
    NetworkInfo,
    SshKeyInfo,
    VmAttachment,
)
from corvus_desktop.dialogs.network_create import NetworkCreateDialog
from corvus_desktop.dialogs.network_edit import NetworkEditDialog
from corvus_desktop.dialogs.ssh_key_add import SshKeyAddDialog
from corvus_desktop.windows.network_detail import NetworkDetailWidget
from corvus_desktop.windows.network_list import NetworkListWidget
from corvus_desktop.windows.ssh_key_detail import SshKeyDetailWidget
from corvus_desktop.windows.ssh_key_list import SshKeyListWidget
from PySide6.QtCore import QObject, Signal
from PySide6.QtWidgets import QTableView


class _MockBridge(QObject):
    """Minimal mock with Phase 6 surface."""

    connected = Signal()
    connection_failed = Signal(str)
    status_ready = Signal(object)
    operation_failed = Signal(str, str)
    ssh_key_list_ready = Signal(object)
    ssh_key_action_completed = Signal(int, str)
    network_list_ready = Signal(object)
    network_detail_ready = Signal(object)
    network_action_completed = Signal(int, str)
    node_list_ready = Signal(object)

    def __init__(self) -> None:
        super().__init__()
        self.ssh_key_list_calls = 0
        self.network_list_calls = 0
        self.network_detail_calls: list[int] = []
        self.ssh_key_creates: list[dict[str, str]] = []
        self.ssh_key_deletes: list[int] = []
        self.network_creates: list[dict[str, Any]] = []
        self.network_starts: list[int] = []
        self.network_stops: list[tuple[int, bool]] = []
        self.network_deletes: list[int] = []
        self.network_edits: list[tuple[int, dict[str, Any]]] = []

    def request_ssh_key_list(self) -> None:
        self.ssh_key_list_calls += 1

    def ssh_key_create(self, name: str, public_key: str) -> None:
        self.ssh_key_creates.append({"name": name, "public_key": public_key})

    def ssh_key_delete(self, key_id: int) -> None:
        self.ssh_key_deletes.append(key_id)

    def request_network_list(self) -> None:
        self.network_list_calls += 1

    def request_network_detail(self, network_id: int) -> None:
        self.network_detail_calls.append(network_id)

    def network_create(self, name: str, subnet: str, **kwargs: Any) -> None:
        self.network_creates.append({"name": name, "subnet": subnet, **kwargs})

    def request_node_list(self) -> None: ...

    def network_start(self, network_id: int) -> None:
        self.network_starts.append(network_id)

    def network_stop(self, network_id: int, *, force: bool = False) -> None:
        self.network_stops.append((network_id, force))

    def network_delete(self, network_id: int) -> None:
        self.network_deletes.append(network_id)

    def network_edit(self, network_id: int, **kwargs: Any) -> None:
        self.network_edits.append((network_id, kwargs))


@pytest.fixture
def bridge() -> _MockBridge:
    return _MockBridge()


# --------------------------------------------------------- dialog payloads


def test_ssh_key_add_dialog_validates(qapp: Any) -> None:
    dlg = SshKeyAddDialog()
    # blank name → rejects
    dlg._name.setText("")
    dlg._public_key.setPlainText("ssh-ed25519 AAAA bob@host")
    assert dlg.result_payload() is None

    # blank key → rejects
    dlg._name.setText("ws")
    dlg._public_key.setPlainText("")
    assert dlg.result_payload() is None

    # not an ssh-* / ecdsa-* key → rejects
    dlg._public_key.setPlainText("garbage value")
    assert dlg.result_payload() is None

    # well-formed → accepts
    dlg._public_key.setPlainText("ssh-ed25519 AAAA bob@host")
    payload = dlg.result_payload()
    assert payload == {"name": "ws", "public_key": "ssh-ed25519 AAAA bob@host"}


def test_network_create_dialog_validates(qapp: Any, bridge: _MockBridge) -> None:
    dlg = NetworkCreateDialog(bridge)
    dlg._name.setText("")
    dlg._subnet.setText("10.0.0.0/24")
    assert dlg.result_payload() is None

    dlg._name.setText("lab")
    dlg._subnet.setText("not-a-cidr")
    assert dlg.result_payload() is None

    dlg._subnet.setText("10.0.0.0/24")
    dlg._dhcp.setChecked(True)
    dlg._nat.setChecked(True)
    dlg._autostart.setChecked(False)
    payload = dlg.result_payload()
    assert payload == {
        "name": "lab",
        "subnet": "10.0.0.0/24",
        "node": None,
        "dhcp": True,
        "nat": True,
        "autostart": False,
    }


def test_network_edit_dialog_returns_only_changes(qapp: Any) -> None:
    net = NetworkInfo(
        id=1,
        name="lab",
        subnet="10.0.0.0/24",
        dhcp=False,
        nat=True,
        running=False,
        autostart=False,
        created_at=datetime.now(timezone.utc),
    )
    dlg = NetworkEditDialog(net)
    # No changes — rejects.
    assert dlg.result_payload() is None
    # Flip dhcp; only that field is in the patch.
    dlg._dhcp.setChecked(True)
    payload = dlg.result_payload()
    assert payload == {"dhcp": True}


# --------------------------------------------------------- list smokes


@pytest.fixture
def ssh_list(qapp: Any, bridge: _MockBridge) -> Iterator[SshKeyListWidget]:
    w = SshKeyListWidget(bridge)
    yield w
    w.deleteLater()


@pytest.fixture
def ssh_detail(qapp: Any, bridge: _MockBridge) -> Iterator[SshKeyDetailWidget]:
    w = SshKeyDetailWidget(bridge)
    yield w
    w.deleteLater()


@pytest.fixture
def net_list(qapp: Any, bridge: _MockBridge) -> Iterator[NetworkListWidget]:
    w = NetworkListWidget(bridge)
    yield w
    w.deleteLater()


@pytest.fixture
def net_detail(qapp: Any, bridge: _MockBridge) -> Iterator[NetworkDetailWidget]:
    w = NetworkDetailWidget(bridge)
    yield w
    w.deleteLater()


def test_ssh_list_refresh_and_populate(
    ssh_list: SshKeyListWidget, bridge: _MockBridge
) -> None:
    ssh_list.refresh()
    assert bridge.ssh_key_list_calls == 1
    bridge.ssh_key_list_ready.emit(
        [
            SshKeyInfo(
                id=i,
                name=f"k{i}",
                public_key=f"ssh-ed25519 AAAA{i}",
                created_at=datetime.now(timezone.utc),
            )
            for i in (1, 2)
        ]
    )
    table = ssh_list.findChild(QTableView)
    assert table is not None
    assert table.model().rowCount() == 2


def test_ssh_list_refreshes_after_action(
    ssh_list: SshKeyListWidget, bridge: _MockBridge
) -> None:
    bridge.ssh_key_list_calls = 0
    bridge.ssh_key_action_completed.emit(0, "create")
    assert bridge.ssh_key_list_calls == 1


def test_ssh_detail_delete_gates_on_attachments(
    ssh_detail: SshKeyDetailWidget, bridge: _MockBridge
) -> None:
    ssh_detail.set_key_id(5)
    bridge.ssh_key_list_ready.emit(
        [
            SshKeyInfo(
                id=5,
                name="k5",
                public_key="ssh-ed25519 AAAA",
                created_at=datetime.now(timezone.utc),
                attached_vms=[VmAttachment(vm=NamedRef(id=1, name="web"))],
            )
        ]
    )
    assert ssh_detail._delete_btn.isEnabled() is False


def test_net_list_refresh_and_populate(
    net_list: NetworkListWidget, bridge: _MockBridge
) -> None:
    net_list.refresh()
    assert bridge.network_list_calls == 1
    bridge.network_list_ready.emit(
        [
            NetworkInfo(
                id=i,
                name=f"n{i}",
                subnet="10.0.0.0/24",
                dhcp=True,
                nat=False,
                running=True,
                autostart=False,
                created_at=datetime.now(timezone.utc),
            )
            for i in (1, 2, 3)
        ]
    )
    table = net_list.findChild(QTableView)
    assert table is not None
    assert table.model().rowCount() == 3


def test_net_detail_action_gating(
    net_detail: NetworkDetailWidget, bridge: _MockBridge
) -> None:
    # running → Start disabled, Stop enabled, Delete disabled
    net_detail.set_network_id(9)
    bridge.network_detail_ready.emit(
        NetworkInfo(
            id=9,
            name="lab",
            subnet="10.0.0.0/24",
            dhcp=False,
            nat=False,
            running=True,
            autostart=False,
            created_at=datetime.now(timezone.utc),
        )
    )
    assert net_detail._btn_start.isEnabled() is False
    assert net_detail._btn_stop.isEnabled() is True
    assert net_detail._btn_delete.isEnabled() is False

    # stopped → opposite gating
    bridge.network_detail_ready.emit(
        NetworkInfo(
            id=9,
            name="lab",
            subnet="10.0.0.0/24",
            dhcp=False,
            nat=False,
            running=False,
            autostart=False,
            created_at=datetime.now(timezone.utc),
        )
    )
    assert net_detail._btn_start.isEnabled() is True
    assert net_detail._btn_stop.isEnabled() is False
    assert net_detail._btn_delete.isEnabled() is True


def test_net_detail_start_button_calls_bridge(
    net_detail: NetworkDetailWidget, bridge: _MockBridge
) -> None:
    net_detail.set_network_id(9)
    bridge.network_detail_ready.emit(
        NetworkInfo(
            id=9,
            name="lab",
            subnet="10.0.0.0/24",
            dhcp=False,
            nat=False,
            running=False,
            autostart=False,
            created_at=datetime.now(timezone.utc),
        )
    )
    net_detail._btn_start.click()
    assert bridge.network_starts == [9]


def test_net_detail_action_routes_back_on_delete(
    net_detail: NetworkDetailWidget, bridge: _MockBridge
) -> None:
    backs: list[None] = []
    net_detail.back_requested.connect(lambda: backs.append(None))
    net_detail.set_network_id(3)
    bridge.network_action_completed.emit(3, "delete")
    assert len(backs) == 1
