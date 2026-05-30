"""Server impls for the schema's sink capabilities.

Each sink class extends the corresponding `*.Server` superclass that
pycapnp synthesises from the schema. Methods called by the daemon push
events onto an `asyncio.Queue`, which the caller drains via async
generators or async callbacks.

A sentinel `_END` value indicates "stream finished — no more events."
"""

from __future__ import annotations

import asyncio
from collections.abc import AsyncIterator, Awaitable, Callable
from typing import Any

from .. import _schema
from . import _convert as conv

_END = object()  # sentinel pushed onto queues when the daemon ends a stream


# ---------------------------------------------------------------------------
# Byte sinks (serial console, HMP monitor)
# ---------------------------------------------------------------------------


class _ByteSinkServer(_schema.streams.ByteSink.Server):
    """Receive bytes from the daemon and forward them to a queue."""

    def __init__(self, queue: asyncio.Queue[Any]):
        self._q = queue

    async def write(self, chunk, _context):
        await self._q.put(("write", bytes(chunk)))

    async def end(self, _context):
        await self._q.put(("end", None))


# ---------------------------------------------------------------------------
# Build event sink
# ---------------------------------------------------------------------------


class _BuildEventSinkServer(_schema.streams.BuildEventSink.Server):
    def __init__(self, queue: asyncio.Queue[Any]):
        self._q = queue

    async def push(self, event, _context):
        await self._q.put(("event", conv.build_event(event)))

    async def end(self, _context):
        await self._q.put(("end", None))


# ---------------------------------------------------------------------------
# Guest-agent status sink
# ---------------------------------------------------------------------------


class _GuestAgentStatusSinkServer(_schema.streams.GuestAgentStatusSink.Server):
    def __init__(self, callback: Callable[[Any], Awaitable[None]]):
        self._cb = callback

    async def push(self, status, _context):
        await self._cb(conv.guest_agent_status(status))


# ---------------------------------------------------------------------------
# VM stats sink (live resource-consumption updates)


class _VmStatsSinkServer(_schema.vm.VmStatsSink.Server):
    def __init__(self, callback: Callable[[Any], Awaitable[None]]):
        self._cb = callback

    async def onStats(self, stats, _context):
        await self._cb(conv.vm_stats(stats))


# ---------------------------------------------------------------------------
# Task progress sink
# ---------------------------------------------------------------------------


class _TaskProgressSinkServer(_schema.streams.TaskProgressSink.Server):
    def __init__(self, callback: Callable[[Any], Awaitable[None]]):
        self._cb = callback

    async def push(self, event, _context):
        await self._cb(conv.task_progress_event(event))


# ---------------------------------------------------------------------------
# High-level streaming helpers
# ---------------------------------------------------------------------------


async def stream_build_events(daemon, yaml: str) -> AsyncIterator[Any]:
    """Yield BuildEvent dataclasses, then a final `('task_id', N)` tuple.

    Usage:
        async for item in stream_build_events(c.daemon, yaml_text):
            if isinstance(item, tuple):
                kind, payload = item
                # kind == 'task_id', payload == int
            else:
                # BuildLogLine / BuildStepStart / ... event dataclasses
                ...
    """
    queue: asyncio.Queue[Any] = asyncio.Queue()
    sink = _BuildEventSinkServer(queue)
    promise = daemon.build(yaml=yaml, sink=sink)
    # Drain events from the queue. The daemon signals end-of-stream
    # by calling sink.end(); we then await the original build() promise
    # for the task id and yield it as the last item.
    while True:
        kind, payload = await queue.get()
        if kind == "event":
            yield payload
            continue
        if kind == "end":
            break
    resp = await promise
    yield ("task_id", resp.taskId)


class GuestAgentSubscription:
    """Live subscription returned by `AsyncVm.subscribe_guest_agent`.

    Drop the subscription (let it go out of scope, or call `close()`)
    to unsubscribe — the daemon prunes its subscriber list when the
    handle cap is dropped on the client side.
    """

    def __init__(self, handle, sink):
        self._handle = handle
        self._sink = sink  # keep alive

    async def close(self) -> None:
        self._handle = None
        self._sink = None


class TaskProgressSubscription(GuestAgentSubscription):
    """Same lifecycle as `GuestAgentSubscription`; alias for clarity."""


async def subscribe_guest_agent(
    vm_cap, on_event: Callable[[Any], Awaitable[None]]
) -> GuestAgentSubscription:
    sink = _GuestAgentStatusSinkServer(on_event)
    resp = await vm_cap.subscribeGuestAgent(sink=sink)
    return GuestAgentSubscription(resp.handle, sink)


class VmStatsSubscription(GuestAgentSubscription):
    """Same lifecycle as `GuestAgentSubscription`; alias for clarity.

    The daemon pushes one `VmStats` per agent poll cycle (~10 s)
    until the handle is dropped (`close()` or out-of-scope)."""


async def subscribe_stats(
    vm_cap, on_event: Callable[[Any], Awaitable[None]]
) -> VmStatsSubscription:
    sink = _VmStatsSinkServer(on_event)
    resp = await vm_cap.subscribeStats(sink=sink)
    return VmStatsSubscription(resp.handle, sink)


async def subscribe_task_progress(
    task_mgr_cap,
    task_id: int,
    on_event: Callable[[Any], Awaitable[None]],
) -> TaskProgressSubscription:
    sink = _TaskProgressSinkServer(on_event)
    resp = await task_mgr_cap.subscribe(taskId=task_id, sink=sink)
    return TaskProgressSubscription(resp.handle, sink)


# ---------------------------------------------------------------------------
# Bidirectional byte streams (serial console, HMP monitor)
# ---------------------------------------------------------------------------


class ByteStream:
    """Bidirectional console session.

    Daemon→client bytes arrive via the inbound queue; call `read()` to
    drain one chunk (returns `None` when the daemon closes the stream).
    Client→daemon bytes go through `write(chunk)`; call `close()` to
    signal end-of-input.
    """

    def __init__(self, input_cap, inbound_queue: asyncio.Queue[Any]):
        self._input = input_cap
        self._q = inbound_queue
        self._closed_in = False

    async def read(self) -> bytes | None:
        kind, payload = await self._q.get()
        if kind == "end":
            return None
        return payload

    async def write(self, chunk: bytes) -> None:
        if self._closed_in:
            raise RuntimeError("ByteStream input already closed")
        # `chunk` is the schema field; using kwargs-style call here
        # doesn't collide with pycapnp's `_send(name=..., word_count=...)`.
        await self._input.write(chunk=chunk)

    async def close(self) -> None:
        if not self._closed_in:
            self._closed_in = True
            try:
                await self._input.end()
            except Exception:
                pass


async def open_byte_stream(method, **kwargs) -> ByteStream:
    """Open a bidirectional byte-pipe via the given cap method.

    `method` is e.g. `vm_cap.serialConsole` or `vm_cap.hmpMonitor`.
    The caller passes the sink and the daemon returns the input cap.
    """
    queue: asyncio.Queue[Any] = asyncio.Queue()
    sink = _ByteSinkServer(queue)
    resp = await method(sink=sink, **kwargs)
    return ByteStream(resp.input, queue)
