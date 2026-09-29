"""Sink-cap lifecycle coverage for the daemon's push-based streams.

The daemon's StatusPoller fans out per-VM events
(``GuestAgentStatus`` / ``VmStats``) to every subscribed sink on a
~10 s cadence. The ticker is load-bearing: a wedged subscriber back-
pressuring the chain caused the ws25 ticker-deadlock (fixed by
[4d2ddfd](https://github.com/vonabarak/corvus/commit/4d2ddfd) — each
``callSink`` push is now wrapped in a 5 s ``timeout``; a sink that
fails to ACK in that window is pruned). No existing test guards the
pruning contract.

What's covered:

* A subscription delivers at least one push within one poller tick
  + buffer (proves the wiring works end-to-end).
* Two independent subscribers to the same VM both receive every
  push (proves fan-out is per-sink, not per-VM).
* Dropping one subscriber doesn't starve the other (the daemon's
  per-VM subscriber list is correctly indexed by handle identity,
  not e.g. ``head : tail``).
* VM-statistics subscriptions decode samples, fan out to independent
  subscribers, and leave surviving sinks live when one is closed.

This focused coverage does not exercise Ctrl+Alt+Del behavior, web/CLI route
breadth, or task-stream terminal semantics.

What's NOT covered here:

* The slow-sink-times-out path that motivated the ws25 fix.
  Building a real slow sink through the sync client would block
  the runloop thread (the on_event callback runs on it), so a
  ``time.sleep(10)`` inside the sink would also block the test's
  own RPC calls. Pinning the 5 s ``dispatchTimeoutMicros``
  numerically would need a custom async sink server that doesn't
  share the harness's runloop — out of scope for this bucket.
  The "two subscribers, drop one" test above provides indirect
  coverage: if pruning were broken, the survivor would also stop
  receiving.
* ``TaskManager.subscribe`` to an already-finished task. The sync
  client doesn't expose ``task.subscribe``; we'd need to drive
  the async cap directly. Bucket 9's ``test_task_cancellation``
  exercises the subscribe path for in-progress tasks.
"""

from __future__ import annotations

import threading
import time
from collections.abc import Callable

from corvus_client.types import GuestAgentStatus, VmStats
from corvus_test_harness import SingleNodeCase, Vm


class TestGuestAgentSubscriptionLifecycle(SingleNodeCase):
    """One VM, three subscription scenarios. Shares the boot since
    the daemon-side state under test (subscriber list) is per-VM
    and indexed by handle identity, so the per-test subscribe/close
    pairs don't interfere across methods."""

    def _drain(
        self,
        events: list[GuestAgentStatus],
        *,
        target: int,
        timeout_sec: float,
        poll_sec: float = 0.5,
    ) -> None:
        """Wait until ``len(events) >= target`` or ``timeout_sec``
        elapses. The list is mutated by the subscription's
        on_event callback (which runs on the runloop thread); we
        poll the length here from the test thread."""
        deadline = time.monotonic() + timeout_sec
        while time.monotonic() < deadline:
            if len(events) >= target:
                return
            time.sleep(poll_sec)
        raise AssertionError(
            f"only received {len(events)}/{target} events within {timeout_sec}s"
        )

    def test_subscription_delivers_pushes(self) -> None:
        """The simplest contract: ``subscribe_guest_agent`` actually
        receives events on the agent's poll cadence. The
        ``StatusPoller`` interval is 10 s; we allow 30 s (one boot
        plus three windows) so a slow nested-KVM host doesn't
        flake."""
        with Vm(self) as vm:
            received: list[GuestAgentStatus] = []
            lock = threading.Lock()

            def on_event(ev: GuestAgentStatus) -> None:
                with lock:
                    received.append(ev)

            sub = vm.cap.subscribe_guest_agent(on_event)
            try:
                self._drain(received, target=1, timeout_sec=30.0)
                # Sample one to make sure the wire decode worked
                # (a bare "got called" check would pass even if the
                # event were an empty placeholder).
                with lock:
                    sample = received[-1]
                assert hasattr(sample, "vm_id"), f"unexpected event shape: {sample!r}"
            finally:
                sub.close()

    def test_two_subscribers_receive_independently(self) -> None:
        """Two subscribers to the same VM both see pushes — fan-out
        per sink, not "first one wins"."""
        with Vm(self) as vm:
            a_events: list[GuestAgentStatus] = []
            b_events: list[GuestAgentStatus] = []
            lock = threading.Lock()

            def make_handler(
                target: list[GuestAgentStatus],
            ) -> Callable[[GuestAgentStatus], None]:
                def on_event(ev: GuestAgentStatus) -> None:
                    with lock:
                        target.append(ev)

                return on_event

            sub_a = vm.cap.subscribe_guest_agent(make_handler(a_events))
            sub_b = vm.cap.subscribe_guest_agent(make_handler(b_events))
            try:
                self._drain(a_events, target=1, timeout_sec=30.0)
                self._drain(b_events, target=1, timeout_sec=30.0)
            finally:
                sub_a.close()
                sub_b.close()

    def test_dropping_one_subscriber_does_not_starve_the_other(self) -> None:
        """The daemon's per-VM subscriber list is keyed by handle
        identity. Closing one handle must prune ONLY that handle —
        the other survivor continues to receive events on schedule.

        Subscribe A and B, observe both receive ≥1 push, close A,
        observe B continues to receive new pushes. If pruning were
        broken (e.g. closing one truncated the list), B would
        also stop."""
        with Vm(self) as vm:
            a_events: list[GuestAgentStatus] = []
            b_events: list[GuestAgentStatus] = []
            lock = threading.Lock()

            sub_a = vm.cap.subscribe_guest_agent(
                lambda ev: a_events.append(ev) if not lock.locked() else None
            )
            sub_b = vm.cap.subscribe_guest_agent(
                lambda ev: b_events.append(ev) if not lock.locked() else None
            )
            try:
                self._drain(a_events, target=1, timeout_sec=30.0)
                self._drain(b_events, target=1, timeout_sec=30.0)

                # Close A; record B's event count at this instant.
                sub_a.close()
                b_baseline = len(b_events)

                # Wait one full poll cycle + buffer. B must
                # receive at least one more event.
                self._drain(
                    b_events,
                    target=b_baseline + 1,
                    timeout_sec=20.0,
                )
            finally:
                # sub_a already closed above; this is idempotent.
                sub_a.close()
                sub_b.close()

    def test_close_does_not_raise(self) -> None:
        """``close()`` runs without error and is idempotent.

        Stronger contract — "the daemon stops pushing to a closed
        sink within N seconds" — would be the natural assertion,
        but pycapnp's Python-side cap GC (and the fact that the
        daemon's subscriber list isn't keyed by Handle identity,
        only by the Sink server cap) means a closed subscription
        keeps receiving events until the underlying RPC connection
        drops. Documented in
        ``python/corvus_client/_async/streams.py:208`` — the
        promise is "drop the cap and the daemon prunes on next
        push failure", but ``close()`` only nulls Python refs and
        doesn't reliably trigger a cap release in pycapnp 2.x.

        The ``test_dropping_one_subscriber_does_not_starve_the_other``
        case above provides the regression coverage that matters:
        if pruning misbehaved in a way that affected OTHER
        subscribers (the ws25 ticker-deadlock), this would catch
        it. The strict per-sink eventual stop is left as future
        work pending a deterministic cap-release path on the
        Python side."""
        with Vm(self) as vm:
            events: list[GuestAgentStatus] = []
            lock = threading.Lock()

            def on_event(ev: GuestAgentStatus) -> None:
                with lock:
                    events.append(ev)

            sub = vm.cap.subscribe_guest_agent(on_event)
            self._drain(events, target=1, timeout_sec=30.0)
            sub.close()
            # Idempotent: second close shouldn't raise.
            sub.close()

    def test_stats_subscribers_fan_out_and_survive_close(self) -> None:
        """Live VM-statistics samples fan out, and closing one sink leaves
        the other receiving later decoded samples.

        A stats stream is scoped to the VM capability, so receiving a
        ``VmStats`` object from this subscription proves it belongs to the
        booted VM. We intentionally do not assert that the closed sink stops
        receiving: pycapnp 2.x does not deterministically release its cap on
        Python-side ``close()``.
        """
        with Vm(self) as vm:
            a_events: list[VmStats] = []
            b_events: list[VmStats] = []
            lock = threading.Lock()

            def make_handler(target: list[VmStats]) -> Callable[[VmStats], None]:
                def on_event(sample: VmStats) -> None:
                    with lock:
                        target.append(sample)

                return on_event

            def drain(
                target: list[VmStats], *, target_count: int, timeout_sec: float
            ) -> None:
                deadline = time.monotonic() + timeout_sec
                while time.monotonic() < deadline:
                    with lock:
                        if len(target) >= target_count:
                            return
                    time.sleep(0.5)
                with lock:
                    count = len(target)
                raise AssertionError(
                    f"only received {count}/{target_count} stats samples "
                    f"within {timeout_sec}s"
                )

            sub_a = vm.cap.subscribe_stats(make_handler(a_events))
            sub_b = vm.cap.subscribe_stats(make_handler(b_events))
            try:
                drain(a_events, target_count=1, timeout_sec=30.0)
                drain(b_events, target_count=1, timeout_sec=30.0)
                with lock:
                    samples = (a_events[-1], b_events[-1])
                    b_baseline = len(b_events)
                for sample in samples:
                    assert sample.sampled_at_nanos > 0
                    assert sample.interval_millis > 0

                sub_a.close()
                drain(b_events, target_count=b_baseline + 1, timeout_sec=20.0)
            finally:
                sub_a.close()
                sub_b.close()
