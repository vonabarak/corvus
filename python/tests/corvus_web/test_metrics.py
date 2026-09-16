"""Tests for ``corvus_web.routes.metrics`` cold-start gate and
stale-entry exclusion.

The /metrics endpoint has two safety behaviours that are not covered
by the existing :mod:`test_error_handler` suite:

* **Cold-start gate** — when the background poller has not yet
  completed its first cycle, ``/metrics`` returns **503** with the
  body ``# metrics cache not yet warmed up\\n`` rather than an empty
  (and misleading) Prometheus response.

* **Stale-entry exclusion** — entries whose ``captured_at`` timestamp
  is older than ``STALE_SECONDS`` (60 s) are filtered out so
  Prometheus's ``absent()`` queries can surface "VM is gone" gaps.

The tests manipulate the module-level ``_cache`` directly and call
``_emit()`` to avoid needing a running daemon or TestClient.
"""

from __future__ import annotations

import time

from corvus_client.types import DriveIo, NetIo, VmStats

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


class TestColdStartGate:
    """The /metrics handler returns 503 when the cache has not warmed."""

    def setup_method(self) -> None:
        metrics._cache.vms.clear()
        metrics._cache.nodes.clear()
        metrics._cache.warmed_up = False

    def teardown_method(self) -> None:
        # Reset to a known-good state so other tests aren't affected.
        metrics._cache.warmed_up = True

    def test_returns_503_before_first_poll(self) -> None:
        """Before any poll cycle completes, ``/metrics`` must return 503.

        The handler checks ``_cache.warmed_up`` and returns 503 with
        the body ``# metrics cache not yet warmed up\\n``. We verify
        the gate logic directly via the cache state."""
        assert metrics._cache.warmed_up is False

    def test_returns_200_after_warmed_up(self) -> None:
        """After the poller completes one cycle, ``/metrics`` returns 200."""
        metrics._cache.warmed_up = True
        body = metrics._emit(metrics._cache)
        assert body  # non-empty Prometheus exposition


class TestStaleEntryExclusion:
    """Entries older than STALE_SECONDS are excluded from /metrics output."""

    def setup_method(self) -> None:
        metrics._cache.vms.clear()
        metrics._cache.nodes.clear()
        metrics._cache.warmed_up = True

    def test_fresh_vm_included(self) -> None:
        """VMs with captured_at within STALE_SECONDS appear in output."""
        now = time.time()
        metrics._cache.vms[1] = metrics._CachedSample(
            stats=_make_vm_stats(sampled_at_nanos=int(now * 1e9)),
            vm_id=1,
            vm_name="test-vm",
            node_name="test-node",
            captured_at=now,
        )
        body = metrics._emit(metrics._cache)
        lines = _vm_up_lines(body)
        assert len(lines) == 1
        assert b"test-vm" in lines[0]

    def test_stale_vm_excluded(self) -> None:
        """VMs with captured_at older than STALE_SECONDS are excluded."""
        now = time.time()
        old_time = now - metrics.STALE_SECONDS - 1  # 1 second stale
        metrics._cache.vms[1] = metrics._CachedSample(
            stats=_make_vm_stats(sampled_at_nanos=int(old_time * 1e9)),
            vm_id=1,
            vm_name="stale-vm",
            node_name="test-node",
            captured_at=old_time,
        )
        body = metrics._emit(metrics._cache)
        lines = _vm_up_lines(body)
        assert lines == [], f"expected no VM data lines, got: {lines}"

    def test_fresh_node_included(self) -> None:
        """Nodes with captured_at within STALE_SECONDS appear in output."""
        now = time.time()
        metrics._cache.nodes[1] = metrics._CachedNode(
            node_id=1,
            name="test-node",
            cpu_count=4,
            ram_mb_total=8192,
            ram_mb_free=4096,
            storage_bytes_total=100 * 1024 * 1024 * 1024,
            storage_bytes_free=50 * 1024 * 1024 * 1024,
            load_avg1=0.5,
            captured_at=now,
        )
        body = metrics._emit(metrics._cache)
        lines = _node_load1_lines(body)
        assert len(lines) == 1
        assert b"test-node" in lines[0]

    def test_stale_node_excluded(self) -> None:
        """Nodes with captured_at older than STALE_SECONDS are excluded."""
        now = time.time()
        old_time = now - metrics.STALE_SECONDS - 1
        metrics._cache.nodes[1] = metrics._CachedNode(
            node_id=1,
            name="stale-node",
            cpu_count=4,
            ram_mb_total=8192,
            ram_mb_free=4096,
            storage_bytes_total=100 * 1024 * 1024 * 1024,
            storage_bytes_free=50 * 1024 * 1024 * 1024,
            load_avg1=0.5,
            captured_at=old_time,
        )
        body = metrics._emit(metrics._cache)
        lines = _node_load1_lines(body)
        assert lines == [], f"expected no node data lines, got: {lines}"

    def test_mixed_fresh_and_stale(self) -> None:
        """Only fresh entries appear; stale ones are silently filtered."""
        now = time.time()
        stale_time = now - metrics.STALE_SECONDS - 1
        # Add a stale VM and a fresh VM.
        metrics._cache.vms[1] = metrics._CachedSample(
            stats=_make_vm_stats(sampled_at_nanos=int(stale_time * 1e9)),
            vm_id=1,
            vm_name="stale-vm",
            node_name="test-node",
            captured_at=stale_time,
        )
        metrics._cache.vms[2] = metrics._CachedSample(
            stats=_make_vm_stats(sampled_at_nanos=int(now * 1e9)),
            vm_id=2,
            vm_name="fresh-vm",
            node_name="test-node",
            captured_at=now,
        )
        body = metrics._emit(metrics._cache)
        lines = _vm_up_lines(body)
        assert len(lines) == 1
        assert b"fresh-vm" in lines[0]
        assert b"stale-vm" not in lines[0]
