"""Ring 2 tests: ``TaskTableModel`` against fixed ``list[TaskInfo]``.

No Qt event loop, no bridge, no daemon. Just exercises the model's
``rowCount`` / ``columnCount`` / ``data`` / ``set_tasks`` contract."""

from __future__ import annotations

from datetime import datetime, timedelta, timezone

import pytest
from corvus_client.types import NamedRef, TaskInfo
from corvus_desktop.models.task_table import TaskTableModel
from PySide6.QtCore import QModelIndex, Qt


def _task(
    *,
    task_id: int = 1,
    started_at: datetime | None = None,
    finished_at: datetime | None = None,
    subsystem: str = "vm",
    command: str = "start",
    result: str = "success",
    entity: NamedRef | None = None,
    client_name: str = "local",
) -> TaskInfo:
    started_at = started_at or datetime.now(timezone.utc)
    return TaskInfo(
        id=task_id,
        started_at=started_at,
        subsystem=subsystem,
        command=command,
        result=result,
        client_name=client_name,
        finished_at=finished_at,
        entity=entity,
    )


@pytest.fixture
def model() -> TaskTableModel:
    return TaskTableModel()


def test_empty_model_has_no_rows(model: TaskTableModel) -> None:
    assert model.rowCount() == 0
    assert model.columnCount() == len(TaskTableModel.COLS)


def test_set_tasks_populates_rows(model: TaskTableModel) -> None:
    tasks = [_task(task_id=i) for i in range(3)]
    model.set_tasks(tasks)
    assert model.rowCount() == 3


def test_display_role_for_each_column(model: TaskTableModel) -> None:
    started = datetime(2026, 1, 1, 12, 0, 0, tzinfo=timezone.utc)
    finished = started + timedelta(seconds=42)
    model.set_tasks(
        [
            _task(
                task_id=7,
                started_at=started,
                finished_at=finished,
                subsystem="disk",
                command="snapshot-create",
                result="success",
                entity=NamedRef(id=11, name="root-disk"),
                client_name="alice",
            )
        ]
    )
    role = Qt.ItemDataRole.DisplayRole

    def cell(row: int, col: int) -> object:
        return model.data(model.index(row, col), role)

    assert cell(0, TaskTableModel.COL_ID) == 7
    assert cell(0, TaskTableModel.COL_SUBSYSTEM) == "disk"
    assert cell(0, TaskTableModel.COL_COMMAND) == "snapshot-create"
    assert cell(0, TaskTableModel.COL_ENTITY) == "root-disk"
    assert cell(0, TaskTableModel.COL_RESULT) == "success"
    assert cell(0, TaskTableModel.COL_CLIENT) == "alice"
    # Duration covers 42 seconds — shows up as "42s".
    assert cell(0, TaskTableModel.COL_DURATION) == "42s"


def test_running_task_has_no_duration(model: TaskTableModel) -> None:
    model.set_tasks([_task(result="running", finished_at=None)])
    assert (
        model.data(
            model.index(0, TaskTableModel.COL_DURATION), Qt.ItemDataRole.DisplayRole
        )
        == ""
    )


def test_missing_entity_renders_blank(model: TaskTableModel) -> None:
    model.set_tasks([_task(entity=None)])
    assert (
        model.data(
            model.index(0, TaskTableModel.COL_ENTITY), Qt.ItemDataRole.DisplayRole
        )
        == ""
    )


def test_header_data(model: TaskTableModel) -> None:
    for col, expected in enumerate(TaskTableModel.COLS):
        assert (
            model.headerData(
                col, Qt.Orientation.Horizontal, Qt.ItemDataRole.DisplayRole
            )
            == expected
        )
    # No vertical headers.
    assert (
        model.headerData(0, Qt.Orientation.Vertical, Qt.ItemDataRole.DisplayRole)
        is None
    )


def test_out_of_range_index_returns_none(model: TaskTableModel) -> None:
    model.set_tasks([_task()])
    assert model.data(model.index(5, 0), Qt.ItemDataRole.DisplayRole) is None
    assert model.data(QModelIndex(), Qt.ItemDataRole.DisplayRole) is None


def test_task_at_round_trip(model: TaskTableModel) -> None:
    tasks = [_task(task_id=i) for i in (3, 5, 7)]
    model.set_tasks(tasks)
    assert model.task_at(0).id == 3
    assert model.task_at(2).id == 7
    assert model.task_at(99) is None
