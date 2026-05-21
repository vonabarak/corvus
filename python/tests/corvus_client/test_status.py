"""Smoke test for the async client's bootstrap + status flow."""

from __future__ import annotations

import asyncio

import capnp
import pytest

from corvus_client._async.client import AsyncClient


def test_status_returns_version_and_uptime(daemon_socket):
    """Connect to a fresh daemon and call status() once."""

    async def run():
        async with capnp.kj_loop():
            async with AsyncClient(unix_socket=str(daemon_socket)) as c:
                info = await c.status()
                await c.ping()
                assert info.version, "version should be a non-empty string"
                assert info.uptime_seconds >= 0
                # protocol_version is a small int; not validating exact value.
                assert info.protocol_version > 0
                return info

    info = asyncio.run(run())
    assert isinstance(info.version, str)
