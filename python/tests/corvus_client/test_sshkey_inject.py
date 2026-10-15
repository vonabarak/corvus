"""Assigned-key injection bindings and reference handling."""

from __future__ import annotations

import asyncio
from types import SimpleNamespace
from unittest.mock import AsyncMock, Mock

import pytest
from corvus_client._async.sshkey import AsyncSshKeyManager
from corvus_client._runloop import SyncRunloop
from corvus_client._sync.sshkey import SyncSshKeyManager


def manager() -> tuple[AsyncSshKeyManager, Mock, Mock]:
    vm = Mock(injectSshKeys=AsyncMock())
    mgr = Mock(get=AsyncMock(return_value=SimpleNamespace(vm=vm)))
    daemon = Mock(vms=AsyncMock(return_value=SimpleNamespace(mgr=mgr)))
    return AsyncSshKeyManager(daemon), mgr, vm


@pytest.mark.parametrize(
    ("ref", "by_name", "expected"),
    [
        (42, False, {"id": 42}),
        ("42", False, {"id": 42}),
        ("web", False, {"name": "web"}),
        ("42", True, {"name": "42"}),
    ],
)
@pytest.mark.parametrize("reset", [False, True])
def test_async_inject(
    ref: int | str, by_name: bool, expected: dict[str, object], reset: bool
) -> None:
    keys, mgr, vm = manager()
    asyncio.run(keys.inject(ref, "alice", reset=reset, by_name=by_name))
    mgr.get.assert_awaited_once()
    assert mgr.get.await_args.kwargs["ref"].to_dict() == expected
    vm.injectSshKeys.assert_awaited_once_with(username="alice", reset=reset)


def test_sync_inject_defaults() -> None:
    keys, mgr, vm = manager()
    runloop = SyncRunloop()
    try:
        SyncSshKeyManager(keys, runloop).inject("web", "root")
    finally:
        runloop.close()
    mgr.get.assert_awaited_once()
    assert mgr.get.await_args.kwargs["ref"].to_dict() == {"name": "web"}
    vm.injectSshKeys.assert_awaited_once_with(username="root", reset=False)


def test_async_inject_propagates_failure() -> None:
    keys, _, vm = manager()
    vm.injectSshKeys.side_effect = RuntimeError("guest unavailable")
    with pytest.raises(RuntimeError, match="guest unavailable"):
        asyncio.run(keys.inject("web", "root"))
