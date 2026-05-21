"""Background asyncio + pycapnp event loop for the sync wrapper.

pycapnp 2.x is async-only: every RPC call returns a `_RemotePromise`
that is `await`-able under `capnp.kj_loop()`. The sync `Client` runs an
asyncio loop on a dedicated thread, enters `kj_loop()` once, and ships
coroutines from the main thread for execution.

This module owns the background thread; the per-client instance lives
in `corvus_client._sync.client.Client`.

# Why automatic GC must not run on the main thread

pycapnp capabilities have C++ destructors that call kj's
`OutgoingMessage::send()` to deliver the Finish message back to the
server. `send()` requires the kj event loop to be live on the
*calling* thread. In this client the kj loop runs on a dedicated
background thread (the runloop), not the main test thread.

If CPython's automatic garbage collector fires on the main thread
while a pycapnp wrapper is reachable in a cycle, GC will finalise
that wrapper *on the main thread*. The C++ destructor finds no kj
loop alive here and aborts the process. The crash signature is
either:

  * a corrupted stack frame inside a `concurrent.futures.Future`
    allocation chain (`Future.__init__` → `Condition.__init__` →
    `_deque()`); the `_deque()` `PyObject_GC_New` is what
    triggered GC, or
  * `Garbage-collecting` in any user-code allocation site
    (e.g. pytest's own fixture machinery), with the bottom frame
    pointing at `_pytest/fixtures.py`.

Both have the same root cause: an automatic GC running on the wrong
thread for pycapnp's finalisers.

The fix is structural: **automatic GC is disabled for the entire
lifetime of every `SyncRunloop`**, and the runloop thread invokes
`gc.collect()` itself on every drop-drain tick (every 50 ms). This
forces every finaliser CPython runs to be on the runloop thread,
where kj is alive.

The disable is reference-counted across multiple `SyncRunloop`
instances in the same process (e.g. the `TwoVmsCase` fixture creates
two clients). Whoever first disables records the prior `gc.isenabled()`
state and restores it on the last close.

# `_RawFuture` for the cross-thread call path

`asyncio.run_coroutine_threadsafe(coro, loop)` allocates a
`concurrent.futures.Future` → `threading.Condition` → `RLock` +
`collections.deque` cascade on every call. Replacing it with a
hand-rolled `_RawFuture` (a `__slots__` object backed by a single
`_thread.allocate_lock()` mutex — not GC-tracked) eliminates that
allocation pressure and removes the most obvious crash trigger.

# Cap-drop queue

`LoopBoundResource.__del__` calls `schedule_drop`, which is just
`deque.append` (GIL-atomic in CPython, allocation-free on the common
path). The runloop thread drains the deque every
`_DRAIN_INTERVAL_SEC`, dropping caps where kj is alive.
"""

from __future__ import annotations

import _thread
import asyncio
import gc
import threading
from collections import deque
from typing import Any, Coroutine

import capnp


# How often the loop wakes up to drain the cap-drop deque. 50 ms is
# short enough that caps don't pile up under heavy churn (test suites
# create + drop hundreds of caps per second) and long enough to be
# negligible CPU overhead when idle.
_DRAIN_INTERVAL_SEC = 0.05

# How often the runloop runs `gc.collect(0)`. Generation-0 collection
# is microseconds on a clean heap and bounded by how many objects were
# allocated since the last sweep; we don't need to do it every drain
# tick. 250 ms keeps total GC overhead well under 1% of runloop CPU
# even under heavy RPC churn.
_GC_INTERVAL_SEC = 0.25

# How often to run a full collection (`gc.collect()` over all
# generations). Full collection is the only way to reclaim cycles
# across older generations, but it's the slowest. 5 seconds is rare
# enough that even a 50 ms full collect adds <1% overhead, and frequent
# enough to keep cyclic garbage from accumulating across a long test
# run.
_GC_FULL_INTERVAL_SEC = 5.0


# Process-global guard around `gc.disable()`. Multiple `SyncRunloop`
# instances in the same process (e.g. a TwoVmsCase with two `Client`s)
# must not race on enable/disable. The first claimer records the
# pre-existing GC state; the last releaser restores it.
_gc_claim_lock = threading.Lock()
_gc_claim_count = 0
_gc_was_enabled_before_claim = False


def _claim_gc_disable() -> None:
    global _gc_claim_count, _gc_was_enabled_before_claim
    with _gc_claim_lock:
        if _gc_claim_count == 0:
            _gc_was_enabled_before_claim = gc.isenabled()
            if _gc_was_enabled_before_claim:
                gc.disable()
        _gc_claim_count += 1


def _release_gc_disable() -> None:
    global _gc_claim_count
    with _gc_claim_lock:
        if _gc_claim_count <= 0:
            return
        _gc_claim_count -= 1
        if _gc_claim_count == 0 and _gc_was_enabled_before_claim:
            gc.enable()


class _RawFuture:
    """A minimal, allocation-light single-shot result barrier.

    Replaces `concurrent.futures.Future` on the sync-call path. The
    backing lock is a `_thread.allocate_lock()` — a raw C mutex with
    no `tp_traverse`, so the GC never visits it and allocating one
    does not increment the GC counter.

    Semantics: one writer (`set_result`/`set_exception` on the loop
    thread) and one reader (`result` on the main thread). The lock
    is acquired at construction and released by the writer; the
    reader blocks on `acquire`.
    """

    __slots__ = ("_lock", "_result", "_exception")

    def __init__(self) -> None:
        self._lock = _thread.allocate_lock()
        self._lock.acquire()
        self._result: Any = None
        self._exception: BaseException | None = None

    def set_result(self, value: Any) -> None:
        self._result = value
        self._lock.release()

    def set_exception(self, exc: BaseException) -> None:
        # Strip the cross-thread reference chain. The exception was
        # raised on the runloop thread; its `__traceback__` chains
        # through `Capnp.lib.capnp` C-bridge frames, and its
        # `__context__` typically holds the original `capnp.KjException`
        # captured by `@translate_errors`. All of those can transitively
        # hold pycapnp wrappers whose C++ destructors require kj_loop
        # alive on the calling thread.
        #
        # The main thread will eventually catch this exception
        # (`except Exception:`), exit the handler, and decref the
        # exception to zero — at which point its traceback and chained
        # exceptions dealloc on the **main** thread, abort()ing pycapnp.
        #
        # Releasing those references here, while still on the runloop
        # thread, lets them dealloc safely (kj_loop is alive on this
        # thread). The main thread receives a "bare" exception: same
        # type, same args, same `__dict__` — but no cross-thread refs.
        # Python re-installs a fresh `__traceback__` for the
        # main-thread frames at the next `raise`, so the user-visible
        # traceback still points at the call site.
        exc.__traceback__ = None
        exc.__context__ = None
        exc.__cause__ = None
        self._exception = exc
        self._lock.release()

    def result(self) -> Any:
        # Block until the writer releases.
        self._lock.acquire()
        # Lock is now ours; release immediately so subsequent reads
        # (if any — none today) don't deadlock.
        self._lock.release()
        if self._exception is not None:
            raise self._exception
        return self._result


class SyncRunloop:
    """A dedicated asyncio + kj_loop event loop on a background thread.

    Each `Client` constructs one. The loop runs until `close()` is called
    (which sets the stop event and joins the thread).
    """

    def __init__(self) -> None:
        # Disable automatic GC for the whole client lifetime; the
        # runloop thread will run `gc.collect()` itself periodically.
        # See module docstring for the rationale (pycapnp finalisers
        # must run on the kj-loop thread).
        _claim_gc_disable()
        self._gc_claimed = True

        self._loop = asyncio.new_event_loop()
        self._stop = asyncio.Event()
        self._ready = threading.Event()
        self._error: BaseException | None = None
        # Cap-drop queue. Appended from any thread (typically the main
        # thread during __del__ → GC), drained on the loop thread.
        # `deque` was chosen for two reasons: append/popleft are
        # GIL-atomic in CPython so no explicit lock is needed, and they
        # don't allocate on the common path (only when crossing the
        # internal 64-slot block boundary).
        self._drop_queue: deque[Any] = deque()
        self._thread = threading.Thread(
            target=self._serve, daemon=True, name="corvus-runloop"
        )
        self._thread.start()
        self._ready.wait()
        if self._error is not None:
            _release_gc_disable()
            self._gc_claimed = False
            raise self._error

    def _serve(self) -> None:
        try:
            asyncio.set_event_loop(self._loop)

            async def park():
                # Bind the stop event to *this* loop.
                self._stop = asyncio.Event()
                async with capnp.kj_loop():
                    # Kick off the cap-drop drainer + the GC drivers.
                    # The drainer fires every _DRAIN_INTERVAL_SEC.
                    # `_gc_gen0_tick` runs `gc.collect(0)` every
                    # _GC_INTERVAL_SEC. `_gc_full_tick` runs a full
                    # collection every _GC_FULL_INTERVAL_SEC. All three
                    # reschedule themselves until `_stop` flips.
                    self._loop.call_soon(self._drain_drops_tick)
                    self._loop.call_later(_GC_INTERVAL_SEC, self._gc_gen0_tick)
                    self._loop.call_later(_GC_FULL_INTERVAL_SEC, self._gc_full_tick)
                    self._ready.set()
                    await self._stop.wait()
                    # Final drain + full collection after stop: caps
                    # appended between the last tick and now must still
                    # see their destructor run with kj alive.
                    self._drain_drops_once()
                    gc.collect()

            self._loop.run_until_complete(park())
        except BaseException as e:  # noqa: BLE001 - we re-raise on the calling thread
            self._error = e
            self._ready.set()
        finally:
            try:
                self._loop.close()
            except Exception:
                pass

    def _drain_drops_once(self) -> None:
        """Pop every queued cap; each popleft's return value goes out
        of scope at the implicit `del` after the statement, running its
        destructor synchronously on this (loop) thread."""
        while True:
            try:
                self._drop_queue.popleft()
            except IndexError:
                return

    def _drain_drops_tick(self) -> None:
        """One drain pass, then reschedule. Runs on the loop thread.

        GC is driven separately by `_gc_gen0_tick` and
        `_gc_full_tick`; deque draining and GC have different optimal
        cadences (draining wants to be eager so caps don't pile up;
        GC wants to be lazy so it doesn't burn the runloop thread).
        """
        self._drain_drops_once()
        if not self._stop.is_set():
            self._loop.call_later(_DRAIN_INTERVAL_SEC, self._drain_drops_tick)

    def _gc_gen0_tick(self) -> None:
        """`gc.collect(0)` + reschedule. Runs on the loop thread.

        Generation-0 collection only scans recently-allocated objects,
        bounded by `gc.get_threshold()[0]` (default 700). On the runloop
        thread, where pycapnp's kj loop is alive, any finalisers
        CPython invokes during collection run safely.
        """
        gc.collect(0)
        if not self._stop.is_set():
            self._loop.call_later(_GC_INTERVAL_SEC, self._gc_gen0_tick)

    def _gc_full_tick(self) -> None:
        """A full `gc.collect()` + reschedule. Runs on the loop thread.

        Generation 0 picks up short-lived cycles quickly; older
        generations need an occasional full sweep to reclaim cycles
        that survived a few gen-0 collections. We do this rarely
        because a full collect over a long-lived heap can take tens
        of milliseconds.
        """
        gc.collect()
        if not self._stop.is_set():
            self._loop.call_later(_GC_FULL_INTERVAL_SEC, self._gc_full_tick)

    def schedule_drop(self, obj: Any) -> None:
        """Append `obj` to the cap-drop queue. Thread-safe; safe to
        call from `__del__` during GC. The object's destructor runs
        on the loop thread no later than `_DRAIN_INTERVAL_SEC` after
        this call (and typically much sooner if the loop is idle).

        Silently no-ops if the loop has already torn down — the cap
        will then be dropped on whatever thread the interpreter's
        final-GC happens on, which is the best we can do.
        """
        if not self._thread.is_alive():
            return
        # The whole point of the deque-based approach: this append is
        # the only Python-level allocation that __del__ performs, and
        # CPython's deque append is GIL-atomic + allocation-free on
        # the common path.
        self._drop_queue.append(obj)

    def run(self, coro: Coroutine[Any, Any, Any]) -> Any:
        """Schedule `coro` on the background loop; block until it returns.

        Avoids `asyncio.run_coroutine_threadsafe` entirely (which would
        allocate `concurrent.futures.Future` + `threading.Condition` +
        `RLock` + a GC-tracked deque on every call). The handoff goes
        through a `_RawFuture` whose only allocation is a single
        `_thread.allocate_lock()` mutex — not GC-tracked.
        """
        if not self._thread.is_alive():
            raise RuntimeError("SyncRunloop is closed")

        loop = self._loop
        fut = _RawFuture()

        def _on_done(task: asyncio.Future) -> None:
            # Runs on the loop thread.
            if task.cancelled():
                fut.set_exception(asyncio.CancelledError())
                return
            exc = task.exception()
            if exc is not None:
                fut.set_exception(exc)
            else:
                fut.set_result(task.result())

        def _schedule() -> None:
            # Runs on the loop thread.
            task = loop.create_task(coro)
            task.add_done_callback(_on_done)

        loop.call_soon_threadsafe(_schedule)
        return fut.result()

    def close(self) -> None:
        if not self._thread.is_alive():
            if self._gc_claimed:
                _release_gc_disable()
                self._gc_claimed = False
            return

        fut = _RawFuture()
        loop = self._loop

        def _signal() -> None:
            self._stop.set()
            fut.set_result(None)

        try:
            loop.call_soon_threadsafe(_signal)
        except RuntimeError:
            # Loop already closed.
            pass

        try:
            fut.result()
        except Exception:
            pass
        self._thread.join(timeout=5)
        if self._gc_claimed:
            _release_gc_disable()
            self._gc_claimed = False
