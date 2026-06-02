"""Ring 2 tests for Phase 8 dialogs (offscreen)."""

from __future__ import annotations

from datetime import datetime, timezone
from typing import Any

import pytest
from corvus_client.types import NamedRef, VmDetails
from corvus_desktop.dialogs.add_shared_dir import AddSharedDirDialog
from corvus_desktop.dialogs.vm_create import VmCreateDialog
from corvus_desktop.dialogs.vm_edit import VmEditDialog


@pytest.fixture
def vm_details() -> VmDetails:
    return VmDetails(
        id=1,
        name="web-1",
        node=NamedRef(id=1, name="node-a"),
        created_at=datetime.now(timezone.utc),
        status="stopped",
        cpu_count=2,
        ram_mb=2048,
        headless=False,
        monitor_socket="",
        serial_socket="",
        guest_agent_socket="",
        guest_agent=True,
        cloud_init=False,
        autostart=False,
        cpu_model="host",
    )


def test_vm_create_requires_name(qapp: Any) -> None:
    dlg = VmCreateDialog()
    assert dlg.result_payload() is None  # name blank
    dlg._name.setText("web-1")
    payload = dlg.result_payload()
    assert payload is not None
    assert payload["name"] == "web-1"
    assert payload["cpu_count"] == 2
    assert payload["ram_mb"] == 2048


def test_vm_edit_no_changes_returns_none(qapp: Any, vm_details: VmDetails) -> None:
    dlg = VmEditDialog(vm_details)
    assert dlg.result_payload() is None


def test_vm_edit_returns_diff(qapp: Any, vm_details: VmDetails) -> None:
    dlg = VmEditDialog(vm_details)
    dlg._cpu.setValue(4)
    dlg._guest_agent.setChecked(False)  # original True
    p = dlg.result_payload()
    assert p is not None
    assert p == {"cpu_count": 4, "guest_agent": False}


def test_shared_dir_dialog_validates(qapp: Any) -> None:
    dlg = AddSharedDirDialog()
    assert dlg.result_payload() is None  # blank
    dlg._path.setText("/srv")
    assert dlg.result_payload() is None  # tag still blank
    dlg._tag.setText("srv_tag")
    p = dlg.result_payload()
    assert p is not None
    assert p["path"] == "/srv"
    assert p["tag"] == "srv_tag"
