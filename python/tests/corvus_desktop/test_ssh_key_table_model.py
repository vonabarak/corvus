"""Ring 2 tests: ``SshKeyTableModel``."""

from __future__ import annotations

from datetime import datetime, timezone

import pytest
from corvus_client.types import NamedRef, SshKeyInfo, VmAttachment
from corvus_desktop.models.ssh_key_table import SshKeyTableModel
from PySide6.QtCore import Qt


def _key(
    *,
    key_id: int = 1,
    name: str = "ws",
    public_key: str = "ssh-ed25519 AAAA0000000000000000ABCDEF1234 alice@laptop",
    attached: list[VmAttachment] | None = None,
) -> SshKeyInfo:
    return SshKeyInfo(
        id=key_id,
        name=name,
        public_key=public_key,
        created_at=datetime.now(timezone.utc),
        attached_vms=attached or [],
    )


@pytest.fixture
def model() -> SshKeyTableModel:
    return SshKeyTableModel()


def test_columns_and_empty(model: SshKeyTableModel) -> None:
    assert model.rowCount() == 0
    assert model.columnCount() == len(SshKeyTableModel.COLS)


def test_display_truncates_long_body(model: SshKeyTableModel) -> None:
    model.set_keys([_key(public_key="ssh-rsa AAAA" + "X" * 200 + " bob@host")])
    text = model.data(
        model.index(0, SshKeyTableModel.COL_PUBLIC_KEY), Qt.ItemDataRole.DisplayRole
    )
    assert text.startswith("ssh-rsa ")
    assert "bob@host" in text
    assert "…" in text  # middle truncation marker


def test_attached_column_lists_vms(model: SshKeyTableModel) -> None:
    model.set_keys(
        [
            _key(
                attached=[
                    VmAttachment(vm=NamedRef(id=1, name="web-1")),
                    VmAttachment(vm=NamedRef(id=2, name="db-1")),
                ]
            )
        ]
    )
    assert (
        model.data(
            model.index(0, SshKeyTableModel.COL_ATTACHED),
            Qt.ItemDataRole.DisplayRole,
        )
        == "web-1, db-1"
    )


def test_key_at_round_trip(model: SshKeyTableModel) -> None:
    model.set_keys([_key(key_id=i, name=f"k{i}") for i in (1, 5)])
    assert model.key_at(0).name == "k1"
    assert model.key_at(1).name == "k5"
    assert model.key_at(9) is None
