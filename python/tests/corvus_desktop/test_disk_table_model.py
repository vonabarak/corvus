"""Ring 2 tests: ``DiskTableModel``."""

from __future__ import annotations

from datetime import datetime, timezone

import pytest
from corvus_client.types import (
    DiskAttachment,
    DiskImageInfo,
    DiskImagePlacement,
    NamedRef,
)
from corvus_desktop.models.disk_table import DiskTableModel
from PySide6.QtCore import Qt


def _disk(
    *,
    disk_id: int = 1,
    name: str = "root",
    format: str = "qcow2",
    size_mb: int | None = 4096,
    placements: list[DiskImagePlacement] | None = None,
    attached_to: list[DiskAttachment] | None = None,
    backing: NamedRef | None = None,
    ephemeral: bool = False,
) -> DiskImageInfo:
    return DiskImageInfo(
        id=disk_id,
        name=name,
        format=format,
        created_at=datetime.now(timezone.utc),
        placements=placements
        or [DiskImagePlacement(node=NamedRef(id=1, name="node-a"), file_path="/x")],
        attached_to=attached_to or [],
        size_mb=size_mb,
        backing_image=backing,
        ephemeral=ephemeral,
    )


@pytest.fixture
def model() -> DiskTableModel:
    return DiskTableModel()


def test_columns(model: DiskTableModel) -> None:
    assert model.rowCount() == 0
    assert model.columnCount() == len(DiskTableModel.COLS)


def test_display_values(model: DiskTableModel) -> None:
    model.set_disks(
        [
            _disk(
                name="root",
                format="qcow2",
                size_mb=2048,
                backing=NamedRef(id=5, name="ubuntu24"),
                attached_to=[DiskAttachment(vm=NamedRef(id=10, name="web-1"))],
                ephemeral=True,
            )
        ]
    )
    role = Qt.ItemDataRole.DisplayRole

    def cell(col: int) -> object:
        return model.data(model.index(0, col), role)

    assert cell(DiskTableModel.COL_NAME) == "root"
    assert cell(DiskTableModel.COL_FORMAT) == "qcow2"
    assert cell(DiskTableModel.COL_SIZE) == "2.0 GB"
    assert cell(DiskTableModel.COL_PLACEMENT) == "node-a"
    assert cell(DiskTableModel.COL_BACKING) == "ubuntu24"
    assert cell(DiskTableModel.COL_ATTACHED) == "web-1"
    assert cell(DiskTableModel.COL_EPHEMERAL) == "yes"


def test_small_size_uses_mb(model: DiskTableModel) -> None:
    model.set_disks([_disk(size_mb=512)])
    assert (
        model.data(model.index(0, DiskTableModel.COL_SIZE), Qt.ItemDataRole.DisplayRole)
        == "512 MB"
    )


def test_disk_at_round_trip(model: DiskTableModel) -> None:
    model.set_disks([_disk(disk_id=i, name=f"d{i}") for i in (1, 2, 3)])
    assert model.disk_at(0).name == "d1"
    assert model.disk_at(2).name == "d3"
    assert model.disk_at(99) is None
