"""Shared per-test helpers.

`with_client` wraps a test coroutine in the kj_loop + AsyncClient
context boilerplate so individual tests stay focused on the calls
they're actually exercising.
"""

from __future__ import annotations

import asyncio
from collections.abc import Awaitable, Callable
from typing import TypeVar

import capnp
from corvus_client._async.client import AsyncClient

T = TypeVar("T")


def with_client(socket_path) -> Callable[[Callable[[AsyncClient], Awaitable[T]]], T]:
    """Return a runner: pass an async function `(client) -> result`."""

    def _run(fn: Callable[[AsyncClient], Awaitable[T]]) -> T:
        async def _main():
            async with capnp.kj_loop():
                async with AsyncClient(unix_socket=str(socket_path)) as c:
                    return await fn(c)

        return asyncio.run(_main())

    return _run
