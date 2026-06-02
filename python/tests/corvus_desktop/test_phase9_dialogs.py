"""Ring 2 tests for Phase 9 disk dialogs."""

from __future__ import annotations

from typing import Any

from corvus_desktop.dialogs.disk_create import DiskCreateDialog, _BlankTab
from corvus_desktop.dialogs.disk_rebase_copy_move import DiskCopyMoveDialog
from PySide6.QtCore import QObject, Signal


class _Bridge(QObject):
    disk_list_ready = Signal(object)
    node_list_ready = Signal(object)

    def __init__(self) -> None:
        super().__init__()
        self.list_calls = 0

    def request_disk_list(self) -> None:
        self.list_calls += 1

    def request_node_list(self) -> None: ...


def test_blank_tab_requires_name(qapp: Any) -> None:
    bridge = _Bridge()
    tab = _BlankTab(bridge)
    tab.name.setText("")
    assert tab.payload() is None
    tab.name.setText("d1")
    p = tab.payload()
    assert p is not None
    assert p["mode"] == "blank"
    assert p["size_mb"] == 10 * 1024


def test_disk_create_dialog_blank_payload(qapp: Any) -> None:
    bridge = _Bridge()
    dlg = DiskCreateDialog(bridge)
    # Blank tab is selected by default.
    dlg._blank.name.setText("d2")
    dlg._on_accept()
    p = dlg.payload()
    assert p["mode"] == "blank"
    assert p["name"] == "d2"


def test_disk_create_dialog_overlay_requires_backing(qapp: Any) -> None:
    bridge = _Bridge()
    dlg = DiskCreateDialog(bridge)
    dlg._tabs.setCurrentWidget(dlg._overlay)
    dlg._overlay.name.setText("d3")
    # Backing not selected.
    dlg._on_accept()
    assert dlg.payload() == {}


def test_copy_dialog_validates_node(qapp: Any) -> None:
    dlg = DiskCopyMoveDialog("copy")
    dlg._to_node.setText("")
    assert dlg.result_payload() is None
    dlg._to_node.setText("node-b")
    p = dlg.result_payload()
    assert p == {
        "to_node_ref": "node-b",
        "to_path": None,
        "with_backing_chain": False,
    }
