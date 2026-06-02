"""Ring 2 tests: ``NetworkTableModel``."""

from __future__ import annotations

from datetime import datetime, timezone

import pytest
from corvus_client.types import NetworkInfo
from corvus_desktop.models.network_table import NetworkTableModel
from PySide6.QtCore import Qt


def _net(
    *,
    net_id: int = 1,
    name: str = "lab",
    subnet: str = "10.42.0.0/24",
    dhcp: bool = True,
    nat: bool = False,
    running: bool = True,
    autostart: bool = False,
    peer_node_ids: tuple[int, ...] = (),
) -> NetworkInfo:
    return NetworkInfo(
        id=net_id,
        name=name,
        subnet=subnet,
        dhcp=dhcp,
        nat=nat,
        running=running,
        autostart=autostart,
        created_at=datetime.now(timezone.utc),
        peer_node_ids=peer_node_ids,
    )


@pytest.fixture
def model() -> NetworkTableModel:
    return NetworkTableModel()


def test_columns_and_empty(model: NetworkTableModel) -> None:
    assert model.rowCount() == 0
    assert model.columnCount() == len(NetworkTableModel.COLS)


def test_display_values(model: NetworkTableModel) -> None:
    model.set_networks([_net(running=True, dhcp=True, nat=True, autostart=True)])
    role = Qt.ItemDataRole.DisplayRole

    def cell(col: int) -> object:
        return model.data(model.index(0, col), role)

    assert cell(NetworkTableModel.COL_NAME) == "lab"
    assert cell(NetworkTableModel.COL_SUBNET) == "10.42.0.0/24"
    assert cell(NetworkTableModel.COL_STATE) == "running"
    flags = cell(NetworkTableModel.COL_FLAGS)
    assert isinstance(flags, str)
    assert "DHCP" in flags
    assert "NAT" in flags
    assert "auto" in flags
    assert cell(NetworkTableModel.COL_PEERS) == "0"


def test_stopped_state(model: NetworkTableModel) -> None:
    model.set_networks([_net(running=False)])
    assert (
        model.data(
            model.index(0, NetworkTableModel.COL_STATE),
            Qt.ItemDataRole.DisplayRole,
        )
        == "stopped"
    )


def test_peer_count(model: NetworkTableModel) -> None:
    model.set_networks([_net(peer_node_ids=(1, 2, 3))])
    assert (
        model.data(
            model.index(0, NetworkTableModel.COL_PEERS),
            Qt.ItemDataRole.DisplayRole,
        )
        == "3"
    )


def test_network_at_round_trip(model: NetworkTableModel) -> None:
    model.set_networks([_net(net_id=i, name=f"n{i}") for i in (3, 7)])
    assert model.network_at(0).name == "n3"
    assert model.network_at(1).name == "n7"
    assert model.network_at(9) is None
