"""Ring 3 smokes for Phase 7 windows + dialogs."""

from __future__ import annotations

from collections.abc import Iterator
from typing import Any

import pytest
from corvus_client.types import (
    ApplyCreated,
    ApplyResult,
    CloudInitInfo,
    TemplateVmInfo,
)
from corvus_desktop.dialogs.template_instantiate import TemplateInstantiateDialog
from corvus_desktop.dialogs.template_yaml import TemplateYamlDialog
from corvus_desktop.widgets.cloud_init_panel import CloudInitPanel
from corvus_desktop.windows.apply import ApplyWidget
from corvus_desktop.windows.template_list import TemplateListWidget
from PySide6.QtCore import QObject, Signal
from PySide6.QtWidgets import QTableView


class _MockBridge(QObject):
    operation_failed = Signal(str, str)
    template_list_ready = Signal(object)
    template_detail_ready = Signal(object)
    template_action_completed = Signal(int, str)
    cloud_init_ready = Signal(int, object)
    cloud_init_action_completed = Signal(int, str)
    apply_completed = Signal(object, int)
    node_list_ready = Signal(object)

    def __init__(self) -> None:
        super().__init__()
        self.template_list_calls = 0
        self.template_creates: list[str] = []
        self.template_updates: list[tuple[int, str]] = []
        self.template_deletes: list[int] = []
        self.template_instantiates: list[tuple[int, str, str | None]] = []
        self.cloud_init_calls: list[int] = []
        self.cloud_init_sets: list[tuple[int, dict[str, Any]]] = []
        self.cloud_init_deletes: list[int] = []
        self.apply_calls: list[tuple[str, bool]] = []

    def request_template_list(self) -> None:
        self.template_list_calls += 1

    def template_create(self, yaml_text: str) -> None:
        self.template_creates.append(yaml_text)

    def template_update(self, template_id: int, yaml_text: str) -> None:
        self.template_updates.append((template_id, yaml_text))

    def template_delete(self, template_id: int) -> None:
        self.template_deletes.append(template_id)

    def template_instantiate(
        self, template_id: int, vm_name: str, node: str | None = None
    ) -> None:
        self.template_instantiates.append((template_id, vm_name, node))

    def request_cloud_init(self, vm_id: int) -> None:
        self.cloud_init_calls.append(vm_id)

    def cloud_init_set(self, vm_id: int, **kwargs: Any) -> None:
        self.cloud_init_sets.append((vm_id, kwargs))

    def cloud_init_delete(self, vm_id: int) -> None:
        self.cloud_init_deletes.append(vm_id)

    def apply_yaml(self, yaml_text: str, *, skip_existing: bool = False) -> None:
        self.apply_calls.append((yaml_text, skip_existing))

    def request_node_list(self) -> None: ...


@pytest.fixture
def bridge() -> _MockBridge:
    return _MockBridge()


# ----------------------------------------------------- template dialogs


def test_template_yaml_dialog_disables_save_on_invalid(qapp: Any) -> None:
    dlg = TemplateYamlDialog("Test", initial="a: [unterminated\n")
    assert not dlg._save_btn.isEnabled()
    dlg._editor.set_text("a: 1\n")
    assert dlg._save_btn.isEnabled()


def test_template_yaml_dialog_text_round_trip(qapp: Any) -> None:
    dlg = TemplateYamlDialog("Test", initial="foo: bar\n")
    assert dlg.text() == "foo: bar\n"


def test_template_instantiate_dialog_validates(qapp: Any, bridge: _MockBridge) -> None:
    dlg = TemplateInstantiateDialog(bridge, "alma-10")
    dlg._vm_name.setText("")
    assert dlg.result_payload() is None
    dlg._vm_name.setText("web-1")
    # Seed the node combo and pick one.
    from datetime import datetime, timezone

    from corvus_client.types import NodeInfo

    bridge.node_list_ready.emit(
        [
            NodeInfo(
                id=11,
                name="node-a",
                host="h",
                node_agent_port=9878,
                net_agent_port=9877,
                admin_state="online",
                created_at=datetime.now(timezone.utc),
                cpu_count=1,
            )
        ]
    )
    dlg._node.select_id(11)
    assert dlg.result_payload() == {"name": "web-1", "node": 11}


# ----------------------------------------------------- template list


@pytest.fixture
def tpl_list(qapp: Any, bridge: _MockBridge) -> Iterator[TemplateListWidget]:
    w = TemplateListWidget(bridge)
    yield w
    w.deleteLater()


def test_template_list_refresh_and_populate(
    tpl_list: TemplateListWidget, bridge: _MockBridge
) -> None:
    tpl_list.refresh()
    assert bridge.template_list_calls == 1
    bridge.template_list_ready.emit(
        [
            TemplateVmInfo(
                id=i,
                name=f"t{i}",
                cpu_count=2,
                ram_mb=1024,
                headless=False,
                guest_agent=True,
                autostart=False,
            )
            for i in (1, 2)
        ]
    )
    table = tpl_list.findChild(QTableView)
    assert table is not None
    assert table.model().rowCount() == 2


# ----------------------------------------------------- cloud-init panel


@pytest.fixture
def ci_panel(qapp: Any, bridge: _MockBridge) -> Iterator[CloudInitPanel]:
    w = CloudInitPanel(bridge)
    yield w
    w.deleteLater()


def test_cloud_init_panel_requests_on_set(
    ci_panel: CloudInitPanel, bridge: _MockBridge
) -> None:
    ci_panel.set_vm_id(42)
    assert bridge.cloud_init_calls == [42]


def test_cloud_init_panel_renders_payload(
    ci_panel: CloudInitPanel, bridge: _MockBridge
) -> None:
    ci_panel.set_vm_id(7)
    bridge.cloud_init_ready.emit(
        7,
        CloudInitInfo(
            user_data="#cloud-config\nhostname: a\n",
            network_config="version: 2\n",
            inject_ssh_keys=True,
        ),
    )
    assert "hostname" in ci_panel._user_data.text()
    assert "version" in ci_panel._network_config.text()
    assert "yes" in ci_panel._inject.text()


# ----------------------------------------------------- apply widget


@pytest.fixture
def apply_widget(qapp: Any, bridge: _MockBridge) -> Iterator[ApplyWidget]:
    w = ApplyWidget(bridge)
    yield w
    w.deleteLater()


def test_apply_button_disabled_on_invalid(
    apply_widget: ApplyWidget, bridge: _MockBridge
) -> None:
    apply_widget._editor.set_text("a: [unterminated\n")
    assert not apply_widget._apply_btn.isEnabled()
    apply_widget._editor.set_text("sshKeys: []\n")
    assert apply_widget._apply_btn.isEnabled()


def test_apply_button_calls_bridge(
    apply_widget: ApplyWidget, bridge: _MockBridge
) -> None:
    apply_widget._editor.set_text("vms: []\n")
    apply_widget._apply_btn.click()
    assert len(bridge.apply_calls) == 1
    assert bridge.apply_calls[0][0] == "vms: []\n"


def test_apply_result_populates_result_panel(
    apply_widget: ApplyWidget, bridge: _MockBridge
) -> None:
    bridge.apply_completed.emit(
        ApplyResult(
            ssh_keys=[ApplyCreated(name="k1", id=11)],
            vms=[ApplyCreated(name="web", id=20)],
        ),
        99,
    )
    # Status label should mention the task id.
    assert "99" in apply_widget._status.text()
