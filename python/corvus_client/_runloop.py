"""Background asyncio + pycapnp event loop for the sync wrapper.

pycapnp 2.x is async-only: every RPC call returns a `_RemotePromise`
that is `await`-able under `capnp.kj_loop()`. The sync `Client` runs an
asyncio loop on a dedicated thread, enters `kj_loop()` once, and uses
`asyncio.run_coroutine_threadsafe` to ship coroutines from the main
thread for execution.

This module owns the background thread; the per-client instance lives
in `corvus_client._sync.client.Client`.
"""
from __future__ import annotations

import asyncio
import threading
from typing import Any, Coroutine

import capnp


class SyncRunloop:
    """A dedicated asyncio + kj_loop event loop on a background thread.

    Each `Client` constructs one. The loop runs until `close()` is called
    (which sets the stop event and joins the thread).
    """

    def __init__(self) -> None:
        self._loop = asyncio.new_event_loop()
        self._stop = asyncio.Event()
        self._ready = threading.Event()
        self._error: BaseException | None = None
        self._thread = threading.Thread(target=self._serve, daemon=True, name="corvus-runloop")
        self._thread.start()
        self._ready.wait()
        if self._error is not None:
            raise self._error

    def _serve(self) -> None:
        try:
            asyncio.set_event_loop(self._loop)

            async def park():
                # Bind the stop event to *this* loop.
                self._stop = asyncio.Event()
                async with capnp.kj_loop():
                    self._ready.set()
                    await self._stop.wait()

            self._loop.run_until_complete(park())
        except BaseException as e:  # noqa: BLE001 - we re-raise on the calling thread
            self._error = e
            self._ready.set()
        finally:
            try:
                self._loop.close()
            except Exception:
                pass

    def run(self, coro: Coroutine[Any, Any, Any]) -> Any:
        """Schedule `coro` on the background loop; block until it returns."""
        if not self._thread.is_alive():
            raise RuntimeError("SyncRunloop is closed")
        fut = asyncio.run_coroutine_threadsafe(coro, self._loop)
        return fut.result()

    def close(self) -> None:
        if not self._thread.is_alive():
            return

        async def _signal_stop():
            self._stop.set()

        try:
            asyncio.run_coroutine_threadsafe(_signal_stop(), self._loop).result(timeout=5)
        except Exception:
            pass
        self._thread.join(timeout=5)
