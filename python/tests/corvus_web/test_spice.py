"""SPICE session creation resolves wildcard bind hosts to the VM node."""

from __future__ import annotations

import asyncio
from types import SimpleNamespace
from typing import cast
from unittest.mock import AsyncMock, Mock

import pytest
from corvus_client import AsyncClient
from corvus_client.types import NamedRef, ViewGrant

from corvus_web.routes import spice


@pytest.mark.parametrize("grant_host", ["0.0.0.0", "::", ""])
def test_create_spice_session_uses_vm_node_for_wildcard_host(grant_host: str) -> None:
    client = Mock(spec=AsyncClient)
    vm = Mock()
    vm.show = AsyncMock(
        return_value=SimpleNamespace(node=NamedRef(id=12, name="node-b"))
    )
    vm.view_grant = AsyncMock(
        return_value=ViewGrant(
            host=grant_host, port=5901, password="secret", ttl_seconds=120
        )
    )
    node = Mock()
    node.show = AsyncMock(return_value=SimpleNamespace(host="192.0.2.12"))
    client.vms.get = AsyncMock(return_value=vm)
    client.nodes.get = AsyncMock(return_value=node)

    try:
        result = asyncio.run(spice.create_spice_session(7, cast(AsyncClient, client)))
        session_id = cast(str, result["session_id"])
        session = spice._SESSIONS[session_id]

        assert session.host == "192.0.2.12"
        assert session.port == 5901
        assert result == {
            "session_id": session_id,
            "password": "secret",
            "ttl_seconds": 120,
        }
        client.nodes.get.assert_awaited_once_with(12)
        vm.view_grant.assert_awaited_once_with()
    finally:
        spice._SESSIONS.clear()


def test_create_spice_session_preserves_explicit_grant_host() -> None:
    client = Mock(spec=AsyncClient)
    vm = Mock()
    vm.show = AsyncMock(
        return_value=SimpleNamespace(node=NamedRef(id=12, name="node-b"))
    )
    vm.view_grant = AsyncMock(
        return_value=ViewGrant(
            host="spice.example.test", port=5901, password="secret", ttl_seconds=120
        )
    )
    node = Mock()
    node.show = AsyncMock(return_value=SimpleNamespace(host="192.0.2.12"))
    client.vms.get = AsyncMock(return_value=vm)
    client.nodes.get = AsyncMock(return_value=node)

    try:
        result = asyncio.run(spice.create_spice_session(7, cast(AsyncClient, client)))

        session_id = cast(str, result["session_id"])
        assert spice._SESSIONS[session_id].host == "spice.example.test"
    finally:
        spice._SESSIONS.clear()
