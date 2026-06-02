"""Bridge mechanics tests (mocked ``AsyncClient``).

The bridge is exercised end-to-end — worker thread, ``capnp.kj_loop``,
``asyncio.run_coroutine_threadsafe``, Qt signal marshalling — with
``AsyncClient`` swapped for a stub. The stub respects the async
context-manager protocol and the small surface the bridge calls
(``status()`` in Phase 1) so the lifespan exercises the real
``AsyncExitStack`` + ``kj_loop`` interaction.

Real-daemon coverage lands once the corvus_client conftest's
``daemon_socket`` fixture is promoted to a shared helper — see Phase 2
of doc/plans/corvus-desktop.
"""

from __future__ import annotations

from collections.abc import Callable
from dataclasses import dataclass
from typing import Any, ClassVar

import pytest
from corvus_client.exceptions import ConnectError
from corvus_desktop import client_bridge
from corvus_desktop.cli import DesktopConfig
from PySide6.QtCore import QEventLoop, QTimer
from PySide6.QtTest import QSignalSpy

# --------------------------------------------------------------- test config


def _config() -> DesktopConfig:
    return DesktopConfig(
        daemon_unix_socket="/tmp/does-not-matter.sock",
        daemon_host=None,
        daemon_port=9876,
        daemon_tls=None,
        daemon_cert_dir=None,
        log_level="debug",
    )


# --------------------------------------------------------------- helpers


def _pump_until(predicate: Callable[[], bool], timeout_ms: int = 5000) -> bool:
    """Run a transient Qt event loop until ``predicate()`` is truthy
    or the timeout fires. The signal-spy mechanism relies on the loop
    draining queued cross-thread emissions, so we need a real event
    loop running while we wait — not a busy spin."""
    loop = QEventLoop()
    timer = QTimer()
    timer.setInterval(20)
    deadline = QTimer()
    deadline.setSingleShot(True)
    deadline.setInterval(timeout_ms)

    def _tick() -> None:
        if predicate():
            loop.quit()

    timer.timeout.connect(_tick)
    deadline.timeout.connect(loop.quit)
    timer.start()
    deadline.start()
    loop.exec()
    timer.stop()
    deadline.stop()
    return predicate()


# --------------------------------------------------------------- stubs


@dataclass(frozen=True)
class _FakeStatus:
    """Stand-in for ``corvus_client.types.StatusInfo``."""

    version: str
    protocol_version: int
    uptime_seconds: int
    connections: int


class _FakeTaskSubscription:
    """Stub returned by ``_FakeTaskManager.subscribe``."""

    def __init__(self) -> None:
        self.closed = False

    async def close(self) -> None:
        self.closed = True


class _FakeTaskManager:
    """Stand-in for ``client.tasks`` — enough surface for bridge tests."""

    def __init__(self) -> None:
        self.list_calls: list[dict[str, Any]] = []
        self.list_return: list[Any] = []
        self.subscriptions: dict[int, _FakeTaskSubscription] = {}
        self.subscribe_callbacks: dict[int, Any] = {}

    async def list(self, **kwargs: Any) -> list[Any]:
        self.list_calls.append(kwargs)
        return list(self.list_return)

    async def subscribe(self, task_id: int, on_event: Any) -> _FakeTaskSubscription:
        sub = _FakeTaskSubscription()
        self.subscriptions[task_id] = sub
        self.subscribe_callbacks[task_id] = on_event
        return sub


class _FakeAsyncClient:
    """Stub ``AsyncClient`` for bridge tests.

    Honours the async-context-manager protocol so the bridge's real
    ``AsyncExitStack`` + ``kj_loop`` lifespan runs unmodified. Records
    every ``__init__``/``status`` call so tests can assert on them.
    """

    instances: ClassVar[list[_FakeAsyncClient]] = []

    def __init__(self, **kwargs: Any) -> None:
        self.init_kwargs = kwargs
        self.status_calls = 0
        self.tasks = _FakeTaskManager()
        type(self).instances.append(self)

    async def __aenter__(self) -> _FakeAsyncClient:
        return self

    async def __aexit__(self, *exc: object) -> None:
        return None

    async def status(self) -> _FakeStatus:
        self.status_calls += 1
        return _FakeStatus(
            version="0.test",
            protocol_version=1,
            uptime_seconds=42,
            connections=1,
        )


class _FailingAsyncClient:
    """Stub that fails its TLS handshake / connect — exercises
    ``connection_failed`` emission path."""

    def __init__(self, **kwargs: Any) -> None:
        pass

    async def __aenter__(self) -> _FailingAsyncClient:
        raise ConnectError("synthetic: daemon unreachable")

    async def __aexit__(self, *exc: object) -> None:
        return None


# --------------------------------------------------------------- fixtures


@pytest.fixture(autouse=True)
def _reset_fake_instances() -> None:
    _FakeAsyncClient.instances = []


@pytest.fixture
def fake_client(monkeypatch: pytest.MonkeyPatch) -> type[_FakeAsyncClient]:
    monkeypatch.setattr(client_bridge, "AsyncClient", _FakeAsyncClient)
    return _FakeAsyncClient


@pytest.fixture
def failing_client(monkeypatch: pytest.MonkeyPatch) -> type[_FailingAsyncClient]:
    monkeypatch.setattr(client_bridge, "AsyncClient", _FailingAsyncClient)
    return _FailingAsyncClient


# --------------------------------------------------------------- tests


def test_bridge_connects_and_returns_status(
    qapp: Any, fake_client: type[_FakeAsyncClient]
) -> None:
    """Happy path: start → connected → request_status → status_ready."""
    bridge = client_bridge.CorvusBridge(_config())
    connected_spy = QSignalSpy(bridge.connected)
    status_spy = QSignalSpy(bridge.status_ready)
    failed_spy = QSignalSpy(bridge.connection_failed)

    bridge.start()
    try:
        assert _pump_until(lambda: connected_spy.count() > 0), (
            "bridge never emitted `connected`"
        )
        assert failed_spy.count() == 0, "unexpected connection_failed"
        assert len(fake_client.instances) == 1
        kwargs = fake_client.instances[0].init_kwargs
        assert kwargs["unix_socket"] == "/tmp/does-not-matter.sock"

        bridge.request_status()
        assert _pump_until(lambda: status_spy.count() > 0), (
            "bridge never emitted `status_ready`"
        )
        payload = status_spy.at(0)[0]
        assert payload.version == "0.test"
        assert payload.uptime_seconds == 42
        assert fake_client.instances[0].status_calls == 1
    finally:
        bridge.shutdown(timeout=5.0)
        assert bridge._thread is None or not bridge._thread.is_alive()


def test_bridge_emits_connection_failed(
    qapp: Any, failing_client: type[_FailingAsyncClient]
) -> None:
    """``__aenter__`` raising ``CorvusError`` → ``connection_failed``."""
    bridge = client_bridge.CorvusBridge(_config())
    connected_spy = QSignalSpy(bridge.connected)
    failed_spy = QSignalSpy(bridge.connection_failed)

    bridge.start()
    try:
        assert _pump_until(lambda: failed_spy.count() > 0), (
            "bridge never emitted `connection_failed`"
        )
        assert connected_spy.count() == 0, "unexpected connected after failure"
        msg = failed_spy.at(0)[0]
        assert "synthetic" in msg
    finally:
        bridge.shutdown(timeout=5.0)


def test_request_before_connect_is_safe(
    qapp: Any, fake_client: type[_FakeAsyncClient]
) -> None:
    """A slot called before ``connected`` fires drops the request
    rather than raising. The GUI shouldn't crash if the user clicks
    a button between QApplication.exec and the bridge handshake."""
    bridge = client_bridge.CorvusBridge(_config())
    failed_spy = QSignalSpy(bridge.operation_failed)
    # NB: bridge.start() not called — loop is not running.
    bridge.request_status()
    # operation_failed should fire synchronously from request_status
    # because there's no worker loop yet. No need to pump.
    assert failed_spy.count() == 1
    entry = failed_spy.at(0)
    op, msg = entry[0], entry[1]
    assert op == "bridge"
    assert "not connected" in msg


def test_request_task_list_returns_results(
    qapp: Any, fake_client: type[_FakeAsyncClient]
) -> None:
    """``request_task_list`` → ``task_list_ready`` carries the daemon's reply."""
    bridge = client_bridge.CorvusBridge(_config())
    connected_spy = QSignalSpy(bridge.connected)
    list_spy = QSignalSpy(bridge.task_list_ready)
    bridge.start()
    try:
        assert _pump_until(lambda: connected_spy.count() > 0)
        # Seed the fake's return value before requesting.
        fake_client.instances[0].tasks.list_return = [{"id": 1}, {"id": 2}]
        bridge.request_task_list(limit=10, subsystem="vm", result=None)
        assert _pump_until(lambda: list_spy.count() > 0)
        payload = list_spy.at(0)[0]
        assert payload == [{"id": 1}, {"id": 2}]
        kwargs = fake_client.instances[0].tasks.list_calls[0]
        assert kwargs == {"limit": 10, "subsystem": "vm", "result": None}
    finally:
        bridge.shutdown(timeout=5.0)


def test_subscribe_task_pipes_events_to_signal(
    qapp: Any, fake_client: type[_FakeAsyncClient]
) -> None:
    """Events delivered to the subscription callback should fan out as
    ``task_event`` signals tagged with the task id."""
    bridge = client_bridge.CorvusBridge(_config())
    connected_spy = QSignalSpy(bridge.connected)
    events_spy = QSignalSpy(bridge.task_event)
    bridge.start()
    try:
        assert _pump_until(lambda: connected_spy.count() > 0)
        client = fake_client.instances[0]

        bridge.subscribe_task(42)
        assert _pump_until(lambda: 42 in client.tasks.subscriptions)

        cb = client.tasks.subscribe_callbacks[42]
        loop = bridge._loop
        assert loop is not None
        import asyncio as _asyncio

        # The callback is an async function on the worker loop; drive
        # it via run_coroutine_threadsafe so it runs in the right
        # context (where the signal emission is wired).
        fut = _asyncio.run_coroutine_threadsafe(cb({"sentinel": "started"}), loop)
        fut.result(timeout=5)

        assert _pump_until(lambda: events_spy.count() > 0)
        task_id, payload = events_spy.at(0)[0], events_spy.at(0)[1]
        assert task_id == 42
        assert payload == {"sentinel": "started"}

        bridge.unsubscribe_task(42)
        assert _pump_until(lambda: client.tasks.subscriptions[42].closed)
    finally:
        bridge.shutdown(timeout=5.0)


def test_pause_resume_subscriptions_round_trip(
    qapp: Any, fake_client: type[_FakeAsyncClient]
) -> None:
    """Pause drops every live task sub; resume re-establishes them."""
    bridge = client_bridge.CorvusBridge(_config())
    connected_spy = QSignalSpy(bridge.connected)
    bridge.start()
    try:
        assert _pump_until(lambda: connected_spy.count() > 0)
        client = fake_client.instances[0]
        bridge.subscribe_task(11)
        bridge.subscribe_task(22)
        assert _pump_until(lambda: len(client.tasks.subscriptions) == 2)
        bridge.pause_subscriptions()
        # After pause, the original subs are closed.
        assert _pump_until(
            lambda: all(s.closed for s in client.tasks.subscriptions.values())
        )
        # Resume re-subscribes; the fake manager replaces each entry.
        bridge.resume_subscriptions()
        # We can't just check len — resume replays the original ids
        # and the fake keeps overwriting, so check the
        # subscribe_callbacks dict was repopulated.
        original_ids = {11, 22}

        def restored() -> bool:
            return all(
                tid in client.tasks.subscriptions
                and not client.tasks.subscriptions[tid].closed
                for tid in original_ids
            )

        assert _pump_until(restored)
    finally:
        bridge.shutdown(timeout=5.0)


def test_shutdown_is_idempotent(qapp: Any, fake_client: type[_FakeAsyncClient]) -> None:
    """``shutdown()`` may be called twice (e.g. once on aboutToQuit
    and once in a finally)."""
    bridge = client_bridge.CorvusBridge(_config())
    spy = QSignalSpy(bridge.connected)
    bridge.start()
    assert _pump_until(lambda: spy.count() > 0)
    bridge.shutdown(timeout=5.0)
    bridge.shutdown(timeout=5.0)  # second call must not raise
