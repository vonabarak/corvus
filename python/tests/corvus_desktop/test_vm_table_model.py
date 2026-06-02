"""Ring 2 tests: ``VmTableModel``."""

from __future__ import annotations

from datetime import datetime, timezone

import pytest
from corvus_client.types import NamedRef, VmInfo
from corvus_desktop.models.vm_table import VmTableModel
from PySide6.QtCore import Qt


def _vm(
    *,
    vm_id: int = 1,
    name: str = "web-1",
    node_name: str = "node-a",
    status: str = "running",
    cpu_count: int = 2,
    ram_mb: int = 1024,
    headless: bool = False,
    guest_agent: bool = True,
    cloud_init: bool = True,
    autostart: bool = False,
) -> VmInfo:
    return VmInfo(
        id=vm_id,
        name=name,
        node=NamedRef(id=99, name=node_name),
        status=status,
        cpu_count=cpu_count,
        ram_mb=ram_mb,
        headless=headless,
        guest_agent=guest_agent,
        cloud_init=cloud_init,
        autostart=autostart,
        last_healthcheck=datetime.now(timezone.utc),
    )


@pytest.fixture
def model() -> VmTableModel:
    return VmTableModel()


def test_columns_and_count(model: VmTableModel) -> None:
    assert model.rowCount() == 0
    assert model.columnCount() == len(VmTableModel.COLS)


def test_display_values_for_each_column(model: VmTableModel) -> None:
    model.set_vms(
        [
            _vm(
                name="alpha",
                node_name="node-1",
                status="running",
                cpu_count=4,
                ram_mb=8192,
                headless=False,
                guest_agent=True,
                cloud_init=True,
                autostart=True,
            )
        ]
    )
    role = Qt.ItemDataRole.DisplayRole

    def cell(col: int) -> object:
        return model.data(model.index(0, col), role)

    assert cell(VmTableModel.COL_NAME) == "alpha"
    assert cell(VmTableModel.COL_NODE) == "node-1"
    assert cell(VmTableModel.COL_STATUS) == "running"
    assert cell(VmTableModel.COL_CPU) == 4
    assert cell(VmTableModel.COL_RAM) == 8192
    flags = cell(VmTableModel.COL_FLAGS)
    assert isinstance(flags, str)
    assert "QGA" in flags
    assert "cloud-init" in flags
    assert "auto" in flags
    assert "headless" not in flags


def test_headless_flag(model: VmTableModel) -> None:
    model.set_vms([_vm(headless=True, guest_agent=False, cloud_init=False)])
    flags = model.data(
        model.index(0, VmTableModel.COL_FLAGS), Qt.ItemDataRole.DisplayRole
    )
    assert flags == "headless"


def test_vm_at_round_trip(model: VmTableModel) -> None:
    vms = [_vm(vm_id=i, name=f"v{i}") for i in (1, 3, 5)]
    model.set_vms(vms)
    assert model.vm_at(0).name == "v1"
    assert model.vm_at(2).name == "v5"
    assert model.vm_at(7) is None
