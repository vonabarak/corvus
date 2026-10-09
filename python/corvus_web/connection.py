"""Own the gateway's daemon sessions without replaying RPC requests."""

from __future__ import annotations

import asyncio
import logging
from collections.abc import Callable
from contextlib import suppress
from dataclasses import dataclass, field

from corvus_client import AsyncClient
from corvus_client.exceptions import ConnectError

logger = logging.getLogger(__name__)
CONNECT_TIMEOUT_SECONDS = 5.0
MAX_RETRY_SECONDS = 30.0


@dataclass
class DaemonSession:
    """One generation of capabilities and its disconnect notification."""

    client: AsyncClient
    disconnected: asyncio.Event = field(default_factory=asyncio.Event)


class DaemonConnection:
    """A single supervisor publishes only fully established sessions.

    Consumers retain a session for the duration of their operation. Replacing
    it never retries that operation or transfers its resource capabilities.
    """

    def __init__(self, client_factory: Callable[[], AsyncClient]) -> None:
        self._client_factory = client_factory
        self.session: DaemonSession | None = None
        self.connected = asyncio.Event()
        self._task: asyncio.Task[None] | None = None

    def get_session(self) -> DaemonSession:
        session = self.session
        if session is None or session.disconnected.is_set():
            raise ConnectError("Corvus daemon is unavailable; reconnecting")
        return session

    def start(self) -> None:
        if self._task is not None:
            raise RuntimeError("Daemon connection supervisor already started")
        self._task = asyncio.create_task(self._run(), name="corvus-web-connection")

    async def close(self) -> None:
        if self._task is not None:
            self._task.cancel()
            with suppress(asyncio.CancelledError):
                await self._task
            self._task = None

    async def _run(self) -> None:
        delay = 1.0
        while True:
            client: AsyncClient | None = None
            try:
                client = self._client_factory()
                await asyncio.wait_for(
                    self._establish(client), timeout=CONNECT_TIMEOUT_SECONDS
                )
                self.session = DaemonSession(client)
                self.connected.set()
                delay = 1.0
                logger.info("Connected to Corvus daemon")
                await client.wait_disconnected()
                logger.warning("Corvus daemon disconnected; reconnecting")
            except Exception as exc:
                logger.warning(
                    "Corvus daemon connection failed: %s; retry in %gs", exc, delay
                )
            finally:
                if self.session is not None:
                    self.session.disconnected.set()
                    self.session = None
                if client is not None:
                    try:
                        await client.close()
                    except Exception as exc:
                        logger.warning("Closing daemon connection failed: %s", exc)
            await asyncio.sleep(delay)
            delay = min(delay * 2, MAX_RETRY_SECONDS)

    @staticmethod
    async def _establish(client: AsyncClient) -> None:
        await client.__aenter__()
        await client.ping()
