"""Gateway session ownership, retry policy, and request isolation."""

from __future__ import annotations

import asyncio
from collections.abc import Callable
from typing import cast
from unittest.mock import AsyncMock, Mock

import pytest
from corvus_client import AsyncClient
from corvus_client.exceptions import ConnectError
from fastapi import FastAPI, Request, WebSocket

from corvus_web import connection
from corvus_web.app import create_app
from corvus_web.config import CorvusWebConfig
from corvus_web.deps import get_client
from corvus_web.routes.vms import AudioDeviceBody, add_audio_device
from corvus_web.websocket import run_daemon_websocket


def _client() -> Mock:
    client = Mock(spec=AsyncClient)
    client.__aenter__ = AsyncMock(return_value=client)
    client.ping = AsyncMock()
    client.close = AsyncMock()
    return client


async def _until(condition: Callable[[], bool]) -> None:
    async def loop() -> None:
        while not condition():
            await asyncio.sleep(0)

    await asyncio.wait_for(loop(), timeout=2)


def test_backoff_cleanup_and_fresh_session(monkeypatch: pytest.MonkeyPatch) -> None:
    async def scenario() -> None:
        original_sleep = asyncio.sleep
        delays: list[float] = []

        async def sleep(delay: float) -> None:
            if delay:
                delays.append(delay)
            await original_sleep(0)

        monkeypatch.setattr(asyncio, "sleep", sleep)
        failures = [_client() for _ in range(7)]
        for client in failures:
            client.ping.side_effect = ConnectError("offline")
        first, second = _client(), _client()
        first_lost, second_lost = asyncio.Event(), asyncio.Event()
        first.wait_disconnected = AsyncMock(side_effect=first_lost.wait)
        second.wait_disconnected = AsyncMock(side_effect=second_lost.wait)
        factory = Mock(side_effect=[*failures, first, second])
        manager = connection.DaemonConnection(factory)
        with pytest.raises(ConnectError):
            manager.get_session()
        manager.start()
        try:
            await _until(lambda: manager.session is not None)
            old_session = manager.get_session()
            assert old_session.client is first
            assert delays == [1, 2, 4, 8, 16, 30, 30]
            for client in failures:
                client.close.assert_awaited_once()
            first_lost.set()
            await _until(
                lambda: manager.session is not None and manager.session.client is second
            )
            assert old_session.disconnected.is_set()
            assert delays[-1] == 1
            assert manager.get_session() is not old_session
            first.close.assert_awaited_once()
            assert factory.call_count == 9
        finally:
            await manager.close()
        second.close.assert_awaited_once()
        assert manager.session is None

    asyncio.run(scenario())


@pytest.mark.parametrize("stage", ["connect", "backoff", "connected"])
def test_shutdown_cleans_up_in_every_stage(
    stage: str, monkeypatch: pytest.MonkeyPatch
) -> None:
    async def scenario() -> None:
        started = asyncio.Event()
        blocked = asyncio.Event()
        client = _client()

        async def block(*_args: object) -> None:
            started.set()
            await blocked.wait()

        if stage == "connect":
            client.__aenter__.side_effect = block
        elif stage == "backoff":
            client.ping.side_effect = ConnectError("offline")
            monkeypatch.setattr(asyncio, "sleep", block)
        else:
            client.wait_disconnected = AsyncMock(side_effect=block)
        manager = connection.DaemonConnection(lambda: cast(AsyncClient, client))
        manager.start()
        await asyncio.wait_for(started.wait(), timeout=2)
        await manager.close()
        client.close.assert_awaited_once()
        assert manager.session is None

    asyncio.run(scenario())


def test_connection_attempt_is_bounded(monkeypatch: pytest.MonkeyPatch) -> None:
    async def scenario() -> None:
        client = _client()
        blocked = asyncio.Event()
        client.ping.side_effect = blocked.wait
        monkeypatch.setattr(connection, "CONNECT_TIMEOUT_SECONDS", 0.01)
        manager = connection.DaemonConnection(lambda: cast(AsyncClient, client))
        manager.start()
        try:
            await _until(lambda: client.close.await_count == 1)
            assert manager.session is None
        finally:
            await manager.close()

    asyncio.run(scenario())


def test_lifespan_does_not_wait_for_daemon(monkeypatch: pytest.MonkeyPatch) -> None:
    async def scenario() -> None:
        client = _client()
        client.__aenter__.side_effect = ConnectError("offline")
        monkeypatch.setattr("corvus_web.app.AsyncClient", lambda **_kwargs: client)
        app = create_app(CorvusWebConfig(daemon_unix_socket="/missing.sock"))
        async with app.router.lifespan_context(app):
            with pytest.raises(ConnectError):
                get_client(Request({"type": "http", "app": app}))
            await _until(lambda: client.close.await_count == 1)

    asyncio.run(scenario())


def test_mutating_request_is_not_replayed() -> None:
    async def scenario() -> None:
        client = _client()
        vm = Mock()
        vm.add_audio_device = AsyncMock(side_effect=ConnectError("lost after send"))
        client.vms.get = AsyncMock(return_value=vm)
        with pytest.raises(ConnectError):
            await add_audio_device(
                1,
                AudioDeviceBody(backend="pulse", model=None),
                cast(AsyncClient, client),
            )
        vm.add_audio_device.assert_awaited_once()
        client.vms.get.assert_awaited_once_with(1)

    asyncio.run(scenario())


@pytest.mark.parametrize("available", [False, True])
def test_websocket_closes_when_daemon_unavailable(available: bool) -> None:
    async def scenario() -> None:
        client = _client()
        manager = connection.DaemonConnection(lambda: cast(AsyncClient, client))
        session = connection.DaemonSession(cast(AsyncClient, client))
        if available:
            manager.session = session
        app = FastAPI()
        app.state.connection = manager
        ws = Mock(spec=WebSocket)
        ws.app = app
        ws.accept = AsyncMock()
        ws.close = AsyncMock()
        entered, cleaned = asyncio.Event(), asyncio.Event()

        async def idle(_client: AsyncClient) -> None:
            entered.set()
            try:
                await asyncio.Event().wait()
            finally:
                cleaned.set()

        task = asyncio.create_task(run_daemon_websocket(cast(WebSocket, ws), idle))
        if available:
            await asyncio.wait_for(entered.wait(), timeout=2)
            session.disconnected.set()
        await asyncio.wait_for(task, timeout=2)
        assert any(call.kwargs.get("code") == 1013 for call in ws.close.call_args_list)
        assert cleaned.is_set() == available

    asyncio.run(scenario())
