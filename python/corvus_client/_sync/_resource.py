"""Base class for sync wrappers holding a cap-bearing async wrapper.

pycapnp's `_DynamicCapabilityClient` (and the `_Response` it transitively
owns via its question/answer table entry) has a C++ destructor that
calls `OutgoingMessage::send()` to deliver the Finish message back to
the server. `send()` requires the kj event loop to be live on the
calling thread; in this client the loop runs on a dedicated background
thread, not the caller's thread.

If a sync wrapper's last reference is dropped on the main thread, the
underlying cap is dealloc'd there → kj aborts with
`expected loop != nullptr`. The fix is to ensure the cap's last
reference is dropped on the runloop thread.

`LoopBoundResource.__del__` does the transfer via
`SyncRunloop.schedule_drop`: a thread-safe `deque.append` that the
loop thread drains every ~50 ms. The deque-based handoff avoids
allocating any Python objects in `__del__` itself — historically, the
old `asyncio.run_coroutine_threadsafe` path constructed a coroutine, a
`concurrent.futures.Future`, and a `threading.Condition` *during GC*,
which occasionally tripped a SIGABRT in CPython's allocator interleaved
with pycapnp's own C-level finalizers.
"""
from __future__ import annotations


class LoopBoundResource:
    """Sync-wrapper mixin: release `_a` on the runloop thread at GC.

    Subclasses must populate two attributes at construction:
      * `_a`  — the AsyncXxx wrapper whose cap must be dropped on the
                runloop thread.
      * `_rl` — the owning `SyncRunloop`.
    """

    def __del__(self):
        # __del__ runs during garbage collection; keep it
        # allocation-minimal. Anything heavier than the dict-pop
        # below risks reentering pycapnp's C-level finalizers in a
        # half-collected state. See module docstring for the
        # history.
        try:
            d = self.__dict__
            a = d.pop("_a", None)
            rl = d.get("_rl", None)
        except Exception:
            return
        if a is None or rl is None:
            return
        # `schedule_drop` is a thin wrapper around `deque.append`:
        # GIL-atomic in CPython and almost never allocates on the
        # common path. The runloop drains the queue periodically on
        # its own thread, where pycapnp's kj loop is alive.
        try:
            rl.schedule_drop(a)
        except Exception:
            # The runloop may already be torn down (interpreter
            # shutdown). Nothing safe to do here; the cap will go
            # through the interpreter's final-GC, which is racy but
            # also unavoidable at this point.
            pass
