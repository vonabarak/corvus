"""Ring 2 tests: ``NodeTableModel``."""

from __future__ import annotations

from datetime import datetime, timezone

import pytest
from corvus_client.types import NodeInfo
from corvus_desktop.models.node_table import NodeTableModel
from PySide6.QtCore import Qt


def _node(
    *,
    node_id: int = 1,
    name: str = "node-a",
    host: str = "10.0.0.1",
    node_agent_port: int = 9878,
    net_agent_port: int = 9877,
    admin_state: str = "online",
    cpu_count: int = 8,
    ram_mb_total: int | None = 16384,
    ram_mb_free: int | None = 8000,
    storage_bytes_total: int | None = 500 * 1024**3,
    storage_bytes_free: int | None = 200 * 1024**3,
    load_avg1: float | None = 0.42,
    last_node_agent_push_at: datetime | None = None,
    last_net_agent_push_at: datetime | None = None,
    netd_disabled: bool = False,
    netd_connected: bool = True,
) -> NodeInfo:
    return NodeInfo(
        id=node_id,
        name=name,
        host=host,
        node_agent_port=node_agent_port,
        net_agent_port=net_agent_port,
        admin_state=admin_state,
        created_at=datetime.now(timezone.utc),
        cpu_count=cpu_count,
        ram_mb_total=ram_mb_total,
        ram_mb_free=ram_mb_free,
        storage_bytes_total=storage_bytes_total,
        storage_bytes_free=storage_bytes_free,
        load_avg1=load_avg1,
        last_node_agent_push_at=last_node_agent_push_at or datetime.now(timezone.utc),
        last_net_agent_push_at=last_net_agent_push_at,
        netd_disabled=netd_disabled,
        netd_connected=netd_connected,
    )


@pytest.fixture
def model() -> NodeTableModel:
    return NodeTableModel()


def test_columns_and_empty(model: NodeTableModel) -> None:
    assert model.rowCount() == 0
    assert model.columnCount() == len(NodeTableModel.COLS)


def test_display_values(model: NodeTableModel) -> None:
    model.set_nodes([_node()])
    role = Qt.ItemDataRole.DisplayRole

    def cell(col: int) -> object:
        return model.data(model.index(0, col), role)

    assert cell(NodeTableModel.COL_NAME) == "node-a"
    host = cell(NodeTableModel.COL_HOST)
    assert isinstance(host, str)
    assert "10.0.0.1" in host and "9878" in host
    assert cell(NodeTableModel.COL_STATE) == "online"
    assert cell(NodeTableModel.COL_CPU) == 8
    ram = cell(NodeTableModel.COL_RAM)
    assert isinstance(ram, str)
    assert "8000" in ram and "16384" in ram


def test_netd_disabled_marker(model: NodeTableModel) -> None:
    model.set_nodes([_node(netd_disabled=True)])
    agents = model.data(
        model.index(0, NodeTableModel.COL_AGENTS), Qt.ItemDataRole.DisplayRole
    )
    assert isinstance(agents, str)
    assert "off" in agents


def test_node_at_round_trip(model: NodeTableModel) -> None:
    model.set_nodes([_node(node_id=i, name=f"n{i}") for i in (1, 2)])
    assert model.node_at(0).name == "n1"
    assert model.node_at(1).name == "n2"
    assert model.node_at(9) is None
