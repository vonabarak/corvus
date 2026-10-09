"""Prometheus exposition endpoint.

corvus-web is the single HTTP-facing component for a cluster, so
Prometheus scrapes one target and gets every VM on every node,
distinguished by labels.

This module:

  * Maintains an in-memory cache of the latest ``VmStats`` per VM
    and latest ``NodeStats`` per node (driven by a background poll
    against the daemon's ``vm.show`` and ``node.show`` RPCs at the
    agent's natural cadence of 10 s).
  * Exposes ``GET /metrics`` that walks the cache and emits the
    Prometheus text format. Cumulative counters end in ``_total``
    so PromQL's ``rate()`` works directly; instantaneous values are
    gauges.

Stale entries (no successful refresh in ``STALE_SECONDS``) are
excluded from the output — Prometheus picks up "VM is gone" via
``absent()`` queries.
"""

from __future__ import annotations

import asyncio
import logging
from collections.abc import Iterator
from contextlib import suppress
from dataclasses import dataclass
from time import monotonic
from typing import TYPE_CHECKING, cast

from corvus_client.exceptions import VmNotFound
from corvus_client.types import VmStats
from fastapi import APIRouter, FastAPI, Request, Response
from prometheus_client import (
    CONTENT_TYPE_LATEST,
    CollectorRegistry,
    Counter,
    Gauge,
    generate_latest,
)

if TYPE_CHECKING:
    from corvus_client import AsyncClient

    from ..connection import DaemonConnection

logger = logging.getLogger(__name__)

router = APIRouter()

# How often the background task refreshes the cache. Matches the
# agent's StatusPoller cadence so we don't sample faster than the
# agent emits.
POLL_INTERVAL_SECONDS = 10.0

# Samples older than this are excluded. When the entire poll has failed
# for this long, /metrics returns 503 instead of a misleading empty 200.
STALE_SECONDS = 60.0


@dataclass
class _CachedSample:
    stats: VmStats
    vm_id: int
    vm_name: str
    node_name: str
    captured_at: float


@dataclass
class _CachedNode:
    node_id: int
    name: str
    cpu_count: int | None
    ram_total: int | None
    ram_free: int | None
    storage_bytes_total: int | None
    storage_bytes_free: int | None
    load_avg1: float | None
    captured_at: float


class _MetricsCache:
    """Per-application snapshot the /metrics handler reads from."""

    def __init__(self) -> None:
        self.vms: dict[int, _CachedSample] = {}
        self.nodes: dict[int, _CachedNode] = {}
        self.last_successful_refresh: float | None = None


def get_cache(app: FastAPI) -> _MetricsCache:
    return cast("_MetricsCache", app.state.metrics_cache)


async def start_metrics_poller(app: FastAPI) -> asyncio.Task[None]:
    """Poll independently of HTTP requests and wake promptly on reconnection."""
    app.state.metrics_cache = _MetricsCache()
    connection = cast("DaemonConnection", app.state.connection)
    cache = get_cache(app)

    async def loop() -> None:
        while True:
            connection.connected.clear()
            session = connection.session
            if session is not None:
                try:
                    snapshot = await _refresh_once(session.client)
                    # A completed poll from an old session cannot make a new
                    # connection appear healthy or overwrite its samples.
                    if (
                        connection.session is session
                        and not session.disconnected.is_set()
                    ):
                        cache.nodes.update(snapshot.nodes)
                        cache.vms.update(snapshot.vms)
                        cache.last_successful_refresh = snapshot.last_successful_refresh
                except Exception as exc:
                    logger.warning("metrics poller iteration failed: %s", exc)
            with suppress(asyncio.TimeoutError):
                await asyncio.wait_for(
                    connection.connected.wait(), POLL_INTERVAL_SECONDS
                )

    return asyncio.create_task(loop(), name="corvus-web-metrics-poller")


async def _refresh_once(client: AsyncClient) -> _MetricsCache:
    """Stage a complete poll; exceptions leave the published cache untouched."""
    snapshot = _MetricsCache()
    nodes = await client.nodes.list()
    for n in nodes:
        snapshot.nodes[n.id] = _CachedNode(
            node_id=n.id,
            name=n.name,
            cpu_count=n.cpu_count,
            ram_total=n.ram_total,
            ram_free=n.ram_free,
            storage_bytes_total=n.storage_bytes_total,
            storage_bytes_free=n.storage_bytes_free,
            load_avg1=n.load_avg1,
            captured_at=0,
        )

    vms = await client.vms.list()
    for v in vms:
        try:
            details = await (await client.vms.get(v.id)).show()
        except VmNotFound:
            # Deletion between list and show is a normal enumeration race.
            continue
        if details.stats is None or details.stats.sampled_at_nanos == 0:
            continue
        snapshot.vms[v.id] = _CachedSample(
            stats=details.stats,
            vm_id=v.id,
            vm_name=v.name,
            node_name=v.node.name,
            captured_at=0,
        )

    now = monotonic()
    for node in snapshot.nodes.values():
        node.captured_at = now
    for sample in snapshot.vms.values():
        sample.captured_at = now
    snapshot.last_successful_refresh = now
    return snapshot


@router.get("/metrics", response_class=Response)
async def metrics(request: Request) -> Response:
    """Serve recent cached samples even during a brief daemon outage."""
    cache = get_cache(request.app)
    if cache.last_successful_refresh is None:
        return Response(
            "# metrics cache not yet warmed up\n",
            media_type=CONTENT_TYPE_LATEST,
            status_code=503,
        )
    if monotonic() - cache.last_successful_refresh > STALE_SECONDS:
        return Response(
            "# metrics cache stale: no successful daemon refresh for 60 seconds\n",
            media_type=CONTENT_TYPE_LATEST,
            status_code=503,
        )
    return Response(_emit(cache), media_type=CONTENT_TYPE_LATEST)


def _emit(cache: _MetricsCache) -> bytes:
    """Build a fresh registry, populate it from `cache`, and let
    prometheus_client serialise the text. A fresh registry per
    scrape keeps GC simple — Counter/Gauge objects are tied to the
    snapshot and discarded with the response."""

    reg = CollectorRegistry()
    vm_labels = ("vm_id", "vm_name", "node")
    node_labels = ("node",)

    vm_up = Gauge(
        "corvus_vm_up", "VM has a recent stats sample", vm_labels, registry=reg
    )
    cpu_seconds_total = Counter(
        "corvus_vm_cpu_seconds_total",
        "Cumulative vCPU thread time",
        vm_labels,
        registry=reg,
    )
    rss = Gauge(
        "corvus_vm_memory_rss_bytes",
        "Host RSS for the QEMU process",
        vm_labels,
        registry=reg,
    )
    balloon_actual = Gauge(
        "corvus_vm_memory_balloon_actual_bytes",
        "Current memory the guest sees (0 if no balloon device)",
        vm_labels,
        registry=reg,
    )
    balloon_max = Gauge(
        "corvus_vm_memory_balloon_max_bytes",
        "VM's configured RAM ceiling (0 if no balloon device)",
        vm_labels,
        registry=reg,
    )
    drive_labels = (*vm_labels, "drive")
    disk_read_bytes_total = Counter(
        "corvus_vm_disk_read_bytes_total",
        "Per-drive cumulative bytes read since QEMU launch",
        drive_labels,
        registry=reg,
    )
    disk_write_bytes_total = Counter(
        "corvus_vm_disk_write_bytes_total",
        "Per-drive cumulative bytes written since QEMU launch",
        drive_labels,
        registry=reg,
    )
    disk_read_ops_total = Counter(
        "corvus_vm_disk_read_ops_total",
        "Per-drive cumulative read operations",
        drive_labels,
        registry=reg,
    )
    disk_write_ops_total = Counter(
        "corvus_vm_disk_write_ops_total",
        "Per-drive cumulative write operations",
        drive_labels,
        registry=reg,
    )
    tap_labels = (*vm_labels, "tap")
    net_rx_bytes_total = Counter(
        "corvus_vm_net_rx_bytes_total",
        "Per-TAP cumulative rx bytes",
        tap_labels,
        registry=reg,
    )
    net_tx_bytes_total = Counter(
        "corvus_vm_net_tx_bytes_total",
        "Per-TAP cumulative tx bytes",
        tap_labels,
        registry=reg,
    )

    node_load1 = Gauge(
        "corvus_node_load1", "Node 1-minute load average", node_labels, registry=reg
    )
    node_ram_total = Gauge(
        "corvus_node_memory_total_bytes",
        "Node total RAM, in bytes",
        node_labels,
        registry=reg,
    )
    node_ram_free = Gauge(
        "corvus_node_memory_free_bytes",
        "Node free RAM, in bytes",
        node_labels,
        registry=reg,
    )
    node_storage_total = Gauge(
        "corvus_node_storage_total_bytes",
        "Node total storage (basePath fs), in bytes",
        node_labels,
        registry=reg,
    )
    node_storage_free = Gauge(
        "corvus_node_storage_free_bytes",
        "Node free storage (basePath fs), in bytes",
        node_labels,
        registry=reg,
    )

    now = monotonic()
    for sample in _fresh_vms(cache, now):
        labels = (str(sample.vm_id), sample.vm_name, sample.node_name)
        vm_up.labels(*labels).set(1)
        # Counter._value.set bypasses the monotonic enforcement;
        # the underlying values from the agent are themselves
        # monotonic (QEMU lifetime counters), so we just install
        # them directly each scrape.
        _set_counter(
            cpu_seconds_total.labels(*labels),
            _jiffies_to_seconds(sample.stats),
        )
        rss.labels(*labels).set(sample.stats.host_rss_bytes)
        balloon_actual.labels(*labels).set(sample.stats.balloon_actual_bytes)
        balloon_max.labels(*labels).set(sample.stats.balloon_max_bytes)
        for d in sample.stats.drives:
            dlabels = (*labels, d.name)
            _set_counter(disk_read_bytes_total.labels(*dlabels), d.read_bytes_total)
            _set_counter(disk_write_bytes_total.labels(*dlabels), d.write_bytes_total)
            _set_counter(disk_read_ops_total.labels(*dlabels), d.read_ops_total)
            _set_counter(disk_write_ops_total.labels(*dlabels), d.write_ops_total)
        for n in sample.stats.nets:
            nlabels = (*labels, n.tap_name)
            _set_counter(net_rx_bytes_total.labels(*nlabels), n.rx_bytes_total)
            _set_counter(net_tx_bytes_total.labels(*nlabels), n.tx_bytes_total)

    for node in _fresh_nodes(cache, now):
        nl = (node.name,)
        if node.load_avg1 is not None:
            node_load1.labels(*nl).set(node.load_avg1)
        if node.ram_total is not None:
            node_ram_total.labels(*nl).set(node.ram_total)
        if node.ram_free is not None:
            node_ram_free.labels(*nl).set(node.ram_free)
        if node.storage_bytes_total is not None:
            node_storage_total.labels(*nl).set(node.storage_bytes_total)
        if node.storage_bytes_free is not None:
            node_storage_free.labels(*nl).set(node.storage_bytes_free)

    return generate_latest(reg)


def _jiffies_to_seconds(stats: VmStats) -> float:
    """Convert cumulative jiffies → seconds using the agent's
    reported clk_tck. Returns 0 when clk_tck is missing (defensive;
    the agent always stamps a real value)."""
    if stats.clk_tck == 0:
        return 0.0
    return stats.cpu_jiffies_total / stats.clk_tck


def _set_counter(metric: Counter, value: float) -> None:
    """The agent emits monotonic counters; mirror them into the
    prometheus_client Counter without going through inc(). Using
    the private ``_value.set`` is the documented pattern for
    re-publishing externally-sourced cumulative counters (see
    https://prometheus.github.io/client_python/instrumenting/counter/
    — "Counters from external sources")."""
    # Counter children expose _value (a _ThreadSafeFloat).
    metric._value.set(value)


def _fresh_vms(cache: _MetricsCache, now: float) -> Iterator[_CachedSample]:
    for s in cache.vms.values():
        if now - s.captured_at <= STALE_SECONDS:
            yield s


def _fresh_nodes(cache: _MetricsCache, now: float) -> Iterator[_CachedNode]:
    for n in cache.nodes.values():
        if now - n.captured_at <= STALE_SECONDS:
            yield n


# ---------------------------------------------------------------------------
# Lifespan helpers (called from corvus_web/app.py)


async def shutdown_metrics_poller(task: asyncio.Task[None] | None) -> None:
    """Cancel the poller task started by `start_metrics_poller`.
    No-ops if `task is None` (poller never started)."""
    if task is None:
        return
    task.cancel()
    with suppress(asyncio.CancelledError, Exception):
        await task
