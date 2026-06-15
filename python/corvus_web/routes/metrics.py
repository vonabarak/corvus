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
import time
from collections.abc import Iterator
from contextlib import suppress
from dataclasses import dataclass
from typing import Annotated, Any

from corvus_client.exceptions import CorvusError
from corvus_client.types import VmStats
from fastapi import APIRouter, Depends, Response
from prometheus_client import (
    CONTENT_TYPE_LATEST,
    CollectorRegistry,
    Counter,
    Gauge,
    generate_latest,
)

from ..deps import get_client

logger = logging.getLogger(__name__)

router = APIRouter()

# How often the background task refreshes the cache. Matches the
# agent's StatusPoller cadence so we don't sample faster than the
# agent emits.
POLL_INTERVAL_SECONDS = 10.0

# Entries older than this are skipped on /metrics scrape. Two
# missed cycles → the VM disappeared from the agent's snapshot or
# corvus-web lost connectivity to the daemon. Prometheus's
# `absent()` then surfaces the gap.
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
    ram_mb_total: int | None
    ram_mb_free: int | None
    storage_bytes_total: int | None
    storage_bytes_free: int | None
    load_avg1: float | None
    captured_at: float


class _MetricsCache:
    """Per-process snapshot the /metrics handler reads from."""

    def __init__(self) -> None:
        self.vms: dict[int, _CachedSample] = {}
        self.nodes: dict[int, _CachedNode] = {}
        # Set when at least one poll cycle completed — lets the
        # endpoint return 503 during cold-start instead of an
        # empty (and misleading) /metrics response.
        self.warmed_up = False


# Module-level cache + task handle. The lifespan in app.py wires
# `start_metrics_poller(app)` and stashes the task on app.state
# for clean shutdown.
_cache = _MetricsCache()


def get_cache() -> _MetricsCache:
    return _cache


async def start_metrics_poller(app) -> asyncio.Task:  # type: ignore[no-untyped-def]
    """Spawn the background polling task. Returns the task so the
    lifespan can cancel it on shutdown."""

    async def loop() -> None:
        while True:
            try:
                await _refresh_once(app.state.client)
            except Exception as exc:  # broad: never let the poller die
                logger.warning("metrics poller iteration failed: %s", exc)
            await asyncio.sleep(POLL_INTERVAL_SECONDS)

    return asyncio.create_task(loop(), name="corvus-web-metrics-poller")


async def _refresh_once(client) -> None:  # type: ignore[no-untyped-def]
    now = time.time()
    # Refresh per-node observations first; if a node's agent has
    # disconnected the daemon returns the most recent observation
    # anyway, which is what Prometheus's `absent()` keys off after
    # STALE_SECONDS.
    try:
        nodes = await client.nodes.list()
    except CorvusError as exc:
        logger.warning("metrics poller: nodes.list() failed: %s", exc)
        nodes = []
    for n in nodes:
        _cache.nodes[n.id] = _CachedNode(
            node_id=n.id,
            name=n.name,
            cpu_count=n.cpu_count,
            ram_mb_total=n.ram_mb_total,
            ram_mb_free=n.ram_mb_free,
            storage_bytes_total=n.storage_bytes_total,
            storage_bytes_free=n.storage_bytes_free,
            load_avg1=n.load_avg1,
            captured_at=now,
        )

    try:
        vms = await client.vms.list()
    except CorvusError as exc:
        logger.warning("metrics poller: vms.list() failed: %s", exc)
        return

    # Fetch each VM's latest sample. vm.show() reads from the
    # daemon's in-memory cache (slice 3) — a quick TVar lookup, no
    # DB hit per call.
    for v in vms:
        try:
            details = await (await client.vms.get(v.id)).show()
        except CorvusError as exc:
            logger.debug("metrics poller: vm %d show() failed: %s", v.id, exc)
            continue
        if details.stats is None:
            continue
        if details.stats.sampled_at_nanos == 0:
            # Daemon has no real sample yet — skip rather than
            # emit zero-valued metrics that would muddy rate().
            continue
        _cache.vms[v.id] = _CachedSample(
            stats=details.stats,
            vm_id=v.id,
            vm_name=v.name,
            node_name=v.node.name,
            captured_at=now,
        )

    _cache.warmed_up = True


# ---------------------------------------------------------------------------
# /metrics endpoint


@router.get("/metrics", response_class=Response)
async def metrics(
    client: Annotated[Any, Depends(get_client)],
) -> Response:
    """Render the Prometheus exposition format from the in-memory
    cache. The dependency on ``get_client`` is unused at request
    time but ensures the endpoint inherits the same auth posture
    as the rest of the API surface."""
    del client  # auth dependency only
    if not _cache.warmed_up:
        return Response(
            "# metrics cache not yet warmed up\n",
            media_type=CONTENT_TYPE_LATEST,
            status_code=503,
        )
    text = _emit(_cache)
    return Response(text, media_type=CONTENT_TYPE_LATEST)


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

    now = time.time()
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
        if node.ram_mb_total is not None:
            node_ram_total.labels(*nl).set(node.ram_mb_total * 1024 * 1024)
        if node.ram_mb_free is not None:
            node_ram_free.labels(*nl).set(node.ram_mb_free * 1024 * 1024)
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


def _set_counter(metric: Any, value: float) -> None:
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


async def shutdown_metrics_poller(task: asyncio.Task | None) -> None:
    """Cancel the poller task started by `start_metrics_poller`.
    No-ops if `task is None` (poller never started)."""
    if task is None:
        return
    task.cancel()
    with suppress(asyncio.CancelledError, Exception):
        await task
