"""Ring 2 tests: ``TemplateTableModel``."""

from __future__ import annotations

import pytest
from corvus_client.types import TemplateVmInfo
from corvus_desktop.models.template_table import TemplateTableModel
from PySide6.QtCore import Qt


def _t(
    *,
    tid: int = 1,
    name: str = "alma-10",
    cpu_count: int = 2,
    ram: int = 2147483648,
    headless: bool = False,
    guest_agent: bool = True,
    tpm: bool = False,
    autostart: bool = False,
    description: str | None = None,
) -> TemplateVmInfo:
    return TemplateVmInfo(
        id=tid,
        name=name,
        cpu_count=cpu_count,
        ram=ram,
        headless=headless,
        guest_agent=guest_agent,
        tpm=tpm,
        autostart=autostart,
        description=description,
    )


@pytest.fixture
def model() -> TemplateTableModel:
    return TemplateTableModel()


def test_columns_and_empty(model: TemplateTableModel) -> None:
    assert model.rowCount() == 0
    assert model.columnCount() == len(TemplateTableModel.COLS)


def test_display_values(model: TemplateTableModel) -> None:
    model.set_templates(
        [
            _t(
                name="web",
                cpu_count=4,
                ram=4294967296,
                guest_agent=True,
                headless=True,
                description="web stack",
            )
        ]
    )
    role = Qt.ItemDataRole.DisplayRole

    def cell(col: int) -> object:
        return model.data(model.index(0, col), role)

    assert cell(TemplateTableModel.COL_NAME) == "web"
    assert cell(TemplateTableModel.COL_CPU) == 4
    assert cell(TemplateTableModel.COL_RAM) == "4G"
    flags = cell(TemplateTableModel.COL_FLAGS)
    assert isinstance(flags, str)
    assert "QGA" in flags
    assert "headless" in flags
    assert cell(TemplateTableModel.COL_DESCRIPTION) == "web stack"


def test_template_at_round_trip(model: TemplateTableModel) -> None:
    model.set_templates([_t(tid=i, name=f"t{i}") for i in (3, 4)])
    first = model.template_at(0)
    second = model.template_at(1)
    assert first is not None
    assert second is not None
    assert first.name == "t3"
    assert second.name == "t4"
    assert model.template_at(9) is None
