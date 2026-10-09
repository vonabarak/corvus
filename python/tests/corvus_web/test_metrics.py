"""Metrics availability and sample freshness without a running daemon."""

from __future__ import annotations

import asyncio
import time
from collections.abc import Callable
from types import SimpleNamespace
from typing import cast
from unittest.mock import AsyncMock, Mock

import pytest
from corvus_client import AsyncClient
from corvus_client.exceptions import ConnectError, VmNotFound
from corvus_client.types import DriveIo, NetIo, VmStats
from fastapi import FastAPI, Request

from corvus_web.connection import DaemonConnection, DaemonSession
from corvus_web.routes import metrics


def _make_vm_stats(
    sampled_at_nanos: int = 1_000_000_000_000,
    drives: list[DriveIo] | None = None,
    nets: list[NetIo] | None = None,
) -> VmStats:
    """Thin helper to avoid typing every VmStats field."""
    return VmStats(
        sampled_at_nanos=sampled_at_nanos,
        interval_millis=1000,
        cpu_jiffies_total=100,
        clk_tck=100,
        host_rss_bytes=64 * 1024 * 1024,
        balloon_actual_bytes=32 * 1024 * 1024,
        balloon_max_bytes=64 * 1024 * 1024,
        drives=drives or [],
        nets=nets or [],
    )


def _vm_up_lines(body: bytes) -> list[bytes]:
    """Return data lines (not HELP/TYPE) for corvus_vm_up."""
    return [line for line in body.split(b"\n") if line.startswith(b"corvus_vm_up{")]


def _node_load1_lines(body: bytes) -> list[bytes]:
    """Return data lines (not HELP/TYPE) for corvus_node_load1."""
    return [
        line for line in body.split(b"\n") if line.startswith(b"corvus_node_load1{")
    ]


def _request(cache: metrics._MetricsCache) -> Request:
    app = FastAPI()
    app.state.metrics_cache = cache
    return Request({"type": "http", "app": app})


@pytest.mark.parametrize("age,status", [(None, 503), (0, 200), (60, 200), (61, 503)])
def test_metrics_availability(
    age: float | None, status: int, monkeypatch: pytest.MonkeyPatch
) -> None:
    """Exercise the handler, including the grace boundary and empty cluster."""
    monkeypatch.setattr(metrics, "monotonic", lambda: 100.0)
    cache = metrics._MetricsCache()
    if age is not None:
        cache.last_successful_refresh = 100 - age
    response = asyncio.run(metrics.metrics(_request(cache)))
    assert response.status_code == status
    if status == 503:
        assert b"metrics cache" in bytes(response.body)


def test_app_caches_are_independent() -> None:
    warm = metrics._MetricsCache()
    warm.last_successful_refresh = time.monotonic()
    assert asyncio.run(metrics.metrics(_request(warm))).status_code == 200
    assert (
        asyncio.run(metrics.metrics(_request(metrics._MetricsCache()))).status_code
        == 503
    )


def _poll_client() -> Mock:
    client = Mock(spec=AsyncClient)
    node = SimpleNamespace(
        id=1,
        name="test-node",
        cpu_count=2,
        ram_total=100,
        ram_free=50,
        storage_bytes_total=200,
        storage_bytes_free=100,
        load_avg1=0.5,
    )
    client.nodes.list = AsyncMock(return_value=[node])
    client.vms.list = AsyncMock(
        return_value=[SimpleNamespace(id=1, name="test-vm", node=node)]
    )
    vm = Mock()
    vm.show = AsyncMock(return_value=SimpleNamespace(stats=_make_vm_stats()))
    client.vms.get = AsyncMock(return_value=vm)
    return client


@pytest.mark.parametrize("failure", ["nodes", "vms", "show"])
def test_failed_poll_preserves_cache_and_recovers(
    failure: str, monkeypatch: pytest.MonkeyPatch
) -> None:
    async def scenario() -> None:
        clock = [100.0]
        monkeypatch.setattr(metrics, "monotonic", lambda: clock[0])
        client = _poll_client()
        manager = DaemonConnection(lambda: cast(AsyncClient, client))
        manager.session = DaemonSession(cast(AsyncClient, client))
        app = FastAPI()
        app.state.connection = manager
        poller = await metrics.start_metrics_poller(app)

        async def until(condition: Callable[[], bool]) -> None:
            async def loop() -> None:
                while not condition():
                    await asyncio.sleep(0)

            await asyncio.wait_for(loop(), timeout=2)

        try:
            cache = metrics.get_cache(app)
            await until(lambda: cache.last_successful_refresh is not None)
            original_node, original_vm = cache.nodes[1], cache.vms[1]
            failing = {
                "nodes": client.nodes.list,
                "vms": client.vms.list,
                "show": client.vms.get.return_value.show,
            }[failure]
            attempted = asyncio.Event()

            async def fail() -> None:
                attempted.set()
                raise ConnectError("disconnected")

            failing.side_effect = fail
            clock[0] = 130
            manager.connected.set()
            await asyncio.wait_for(attempted.wait(), timeout=2)
            await asyncio.sleep(0)
            assert cache.last_successful_refresh == 100
            assert cache.nodes[1] is original_node
            assert cache.vms[1] is original_vm
            request = Request({"type": "http", "app": app})
            response = await metrics.metrics(request)
            assert response.status_code == 200
            assert _vm_up_lines(bytes(response.body))
            clock[0] = 161
            assert (await metrics.metrics(request)).status_code == 503
            failing.side_effect = None
            manager.connected.set()
            await until(lambda: cache.last_successful_refresh == 161)
            assert (await metrics.metrics(request)).status_code == 200
        finally:
            await metrics.shutdown_metrics_poller(poller)

    asyncio.run(scenario())


def test_old_connection_cannot_publish_a_completed_poll() -> None:
    async def scenario() -> None:
        client = _poll_client()
        entered, release = asyncio.Event(), asyncio.Event()
        original_nodes = client.nodes.list.return_value

        async def nodes() -> object:
            entered.set()
            await release.wait()
            return original_nodes

        client.nodes.list.side_effect = nodes
        manager = DaemonConnection(lambda: cast(AsyncClient, client))
        manager.session = DaemonSession(cast(AsyncClient, client))
        app = FastAPI()
        app.state.connection = manager
        poller = await metrics.start_metrics_poller(app)
        try:
            await asyncio.wait_for(entered.wait(), timeout=2)
            manager.session.disconnected.set()
            manager.session = None
            release.set()
            # Wait for the staged poll to finish, then let the poller publish.
            while not client.vms.get.return_value.show.await_count:
                await asyncio.sleep(0)
            await asyncio.sleep(0)
            cache = metrics.get_cache(app)
            assert cache.last_successful_refresh is None
            assert cache.nodes == {}
            assert cache.vms == {}
        finally:
            await metrics.shutdown_metrics_poller(poller)

    asyncio.run(scenario())


@pytest.mark.parametrize("stats", [None, _make_vm_stats(sampled_at_nanos=0)])
def test_missing_stats_do_not_make_an_empty_cluster_unhealthy(
    stats: VmStats | None,
) -> None:
    async def scenario() -> None:
        client = _poll_client()
        client.vms.get.return_value.show.return_value.stats = stats
        cache = await metrics._refresh_once(cast(AsyncClient, client))
        assert cache.vms == {}
        assert (await metrics.metrics(_request(cache))).status_code == 200
        client.vms.get.side_effect = VmNotFound("deleted during enumeration")
        cache = await metrics._refresh_once(cast(AsyncClient, client))
        assert cache.last_successful_refresh is not None

    asyncio.run(scenario())


class TestStaleEntryExclusion:
    """Entries older than STALE_SECONDS are excluded from /metrics output."""

    def setup_method(self) -> None:
        self.cache = metrics._MetricsCache()

    def test_fresh_vm_included(self) -> None:
        """VMs with captured_at within STALE_SECONDS appear in output."""
        now = time.monotonic()
        self.cache.vms[1] = metrics._CachedSample(
            stats=_make_vm_stats(sampled_at_nanos=int(now * 1e9)),
            vm_id=1,
            vm_name="test-vm",
            node_name="test-node",
            captured_at=now,
        )
        body = metrics._emit(self.cache)
        lines = _vm_up_lines(body)
        assert len(lines) == 1
        assert b"test-vm" in lines[0]

    def test_stale_vm_excluded(self) -> None:
        """VMs with captured_at older than STALE_SECONDS are excluded."""
        now = time.monotonic()
        old_time = now - metrics.STALE_SECONDS - 1  # 1 second stale
        self.cache.vms[1] = metrics._CachedSample(
            stats=_make_vm_stats(sampled_at_nanos=int(old_time * 1e9)),
            vm_id=1,
            vm_name="stale-vm",
            node_name="test-node",
            captured_at=old_time,
        )
        body = metrics._emit(self.cache)
        lines = _vm_up_lines(body)
        assert lines == [], f"expected no VM data lines, got: {lines}"

    def test_fresh_node_included(self) -> None:
        """Nodes with captured_at within STALE_SECONDS appear in output."""
        now = time.monotonic()
        self.cache.nodes[1] = metrics._CachedNode(
            node_id=1,
            name="test-node",
            cpu_count=4,
            ram_total=8589934592,
            ram_free=4294967296,
            storage_bytes_total=100 * 1024 * 1024 * 1024,
            storage_bytes_free=50 * 1024 * 1024 * 1024,
            load_avg1=0.5,
            captured_at=now,
        )
        body = metrics._emit(self.cache)
        lines = _node_load1_lines(body)
        assert len(lines) == 1
        assert b"test-node" in lines[0]

    def test_stale_node_excluded(self) -> None:
        """Nodes with captured_at older than STALE_SECONDS are excluded."""
        now = time.monotonic()
        old_time = now - metrics.STALE_SECONDS - 1
        self.cache.nodes[1] = metrics._CachedNode(
            node_id=1,
            name="stale-node",
            cpu_count=4,
            ram_total=8589934592,
            ram_free=4294967296,
            storage_bytes_total=100 * 1024 * 1024 * 1024,
            storage_bytes_free=50 * 1024 * 1024 * 1024,
            load_avg1=0.5,
            captured_at=old_time,
        )
        body = metrics._emit(self.cache)
        lines = _node_load1_lines(body)
        assert lines == [], f"expected no node data lines, got: {lines}"

    def test_mixed_fresh_and_stale(self) -> None:
        """Only fresh entries appear; stale ones are silently filtered."""
        now = time.monotonic()
        stale_time = now - metrics.STALE_SECONDS - 1
        # Add a stale VM and a fresh VM.
        self.cache.vms[1] = metrics._CachedSample(
            stats=_make_vm_stats(sampled_at_nanos=int(stale_time * 1e9)),
            vm_id=1,
            vm_name="stale-vm",
            node_name="test-node",
            captured_at=stale_time,
        )
        self.cache.vms[2] = metrics._CachedSample(
            stats=_make_vm_stats(sampled_at_nanos=int(now * 1e9)),
            vm_id=2,
            vm_name="fresh-vm",
            node_name="test-node",
            captured_at=now,
        )
        body = metrics._emit(self.cache)
        lines = _vm_up_lines(body)
        assert len(lines) == 1
        assert b"fresh-vm" in lines[0]
        assert b"stale-vm" not in lines[0]
