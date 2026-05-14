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

`LoopBoundResource.__del__` does the transfer: pops the inner
`_a` (AsyncXxx) attribute, ships it into a coroutine on the runloop,
drops every other ref, and blocks until the loop's Task completes —
which is when the coro frame (and the final ref to `_a`) is torn down
on the loop thread, running the cap destructor with kj alive.
"""
from __future__ import annotations

import asyncio


class LoopBoundResource:
    """Sync-wrapper mixin: release `_a` on the runloop thread at GC."""

    def __del__(self):
        try:
            d = self.__dict__
            a = d.pop("_a", None)
            rl = d.get("_rl", None)
        except Exception:
            return
        if a is None or rl is None:
            return
        thread = getattr(rl, "_thread", None)
        loop = getattr(rl, "_loop", None)
        if thread is None or loop is None or not thread.is_alive():
            # Runloop already torn down; nothing safe to do here.
            return

        async def _drop(_x):
            pass

        try:
            coro = _drop(a)
        except Exception:
            return
        # Drop our local ref so the coro frame is the sole holder.
        a = None
        try:
            fut = asyncio.run_coroutine_threadsafe(coro, loop)
        except Exception:
            return
        # Drop our coro ref; the loop's Task is now the only holder.
        coro = None
        try:
            fut.result(timeout=5)
        except Exception:
            pass
