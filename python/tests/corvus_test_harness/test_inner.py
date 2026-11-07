"""Readiness waits use the existing read-only node capability."""

import time
from typing import cast
from unittest.mock import MagicMock

import pytest
from corvus_client._sync.node import SyncNode
from corvus_client.types import NodePingResult
from corvus_test_harness import inner


def test_readiness_retries_unreachable_nodes(monkeypatch: pytest.MonkeyPatch) -> None:
    node = MagicMock(spec=SyncNode)
    node.ping.side_effect = [
        NodePingResult(False, "supervisor connecting"),
        NodePingResult(True, "reachable"),
    ]
    monkeypatch.setattr(time, "sleep", lambda _: None)
    inner._wait_for_self_node_ready(cast(SyncNode, node))
    assert node.ping.call_count == 2
    assert [call[0] for call in node.mock_calls] == ["ping", "ping"]


def test_readiness_timeout_keeps_last_diagnostic(
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    node = MagicMock(spec=SyncNode)
    node.ping.return_value = NodePingResult(False, "agent ping timed out")
    times = iter([0.0, 0.0, 2.0])
    monkeypatch.setattr(time, "monotonic", lambda: next(times))
    monkeypatch.setattr(time, "sleep", lambda _: None)
    with pytest.raises(RuntimeError, match="agent ping timed out"):
        inner._wait_for_self_node_ready(cast(SyncNode, node), timeout_sec=1)


def test_readiness_propagates_unexpected_errors() -> None:
    node = MagicMock(spec=SyncNode)
    node.ping.side_effect = ValueError("bad response")
    with pytest.raises(ValueError, match="bad response"):
        inner._wait_for_self_node_ready(cast(SyncNode, node))
