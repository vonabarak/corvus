"""Development VM network allocation and lifecycle."""

import json
import tempfile
from pathlib import Path
from unittest.mock import MagicMock

import pytest
from corvus_test_harness import devnode_create as creation
from corvus_test_harness import host_access as access
from corvus_test_harness.outer import Crv, CrvError, JsonObject


def test_subnet_skips_overlapping_ranges() -> None:
    assert (
        creation.pick_subnet([{"subnet": "10.91.0.0/23"}, {"subnet": "::/0"}])
        == "10.91.2.0/24"
    )
    with pytest.raises(RuntimeError, match="No free"):
        creation.pick_subnet([{"subnet": "10.91.0.0/16"}])


def test_create_replaces_template_interface_before_start(
    monkeypatch: pytest.MonkeyPatch, tmp_path: Path
) -> None:
    monkeypatch.setattr(tempfile, "gettempdir", lambda: str(tmp_path))
    crv = MagicMock()
    details: JsonObject = {"id": 42, "name": "dev", "net_ifs": [{"id": 7}]}
    crv.vm_show.side_effect = [
        CrvError([], 1, json.dumps({"code": "vm_not_found"}), ""),
        details,
    ]
    crv.network_list.return_value = [{"subnet": "10.91.0.0/24"}]
    monkeypatch.setattr(Crv, "autodetect", lambda: crv)
    monkeypatch.setattr(creation, "inner_binary", lambda: tmp_path / "bin/crv")
    documents = []
    crv.apply.side_effect = lambda path, **kwargs: documents.append(
        json.loads(Path(path).read_text())
    )
    creation.create("dev")
    assert documents[0]["networks"][0]["subnet"] == "10.91.1.0/24"
    calls = [call.args for call in crv.run.call_args_list]
    assert (
        calls.index(("net-if", "remove", "dev", "7"))
        < calls.index(("net-if", "add", "dev", "--network", "corvus-dev-node-42"))
        < calls.index(("vm", "start", "dev", "--wait"))
    )
    assert (
        access.read_metadata(access.vm_state(details) / "network.json")["network"]
        == "corvus-dev-node-42"
    )


def test_cleanup_retries_network_after_vm_deleted(
    monkeypatch: pytest.MonkeyPatch, tmp_path: Path
) -> None:
    monkeypatch.setattr(tempfile, "gettempdir", lambda: str(tmp_path))
    state = access.vm_state({"id": 42}, create=True)
    access.publish_metadata(
        state,
        {"vm_id": 42, "vm_name": "dev", "network": "corvus-dev-node-42"},
        filename="network.json",
    )
    crv = MagicMock()
    crv.vm_show.side_effect = CrvError([], 1, json.dumps({"code": "vm_not_found"}), "")
    crv.network_list.return_value = [
        {"name": "corvus-dev-node-42", "running": True},
        {"name": "corvus", "running": True},
    ]
    monkeypatch.setattr(Crv, "autodetect", lambda: crv)
    crv.network_delete.side_effect = RuntimeError("failed")
    with pytest.raises(RuntimeError, match="failed"):
        access.clean("dev")
    assert state.exists()
    crv.network_delete.side_effect = None
    access.clean("dev")
    assert not state.exists()
    crv.network_stop.assert_called_with("corvus-dev-node-42")
    crv.network_delete.assert_called_with("corvus-dev-node-42")
