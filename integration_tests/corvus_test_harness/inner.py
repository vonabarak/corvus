"""Open a pycapnp `Client` against the inner Corvus daemon.

The inner daemon's TCP listener is exposed to the host via a
`VsockTcpRelay`. We poll `Client.status()` with backoff until the
inner daemon answers — first-boot times can be 30-90s depending on
guest-agent / systemd ordering.
"""
from __future__ import annotations

import time
from typing import Optional

from corvus_client import Client
from corvus_client.exceptions import ConnectError, CorvusError

from .transport import VsockTcpRelay


def open_client(
    relay: VsockTcpRelay,
    *,
    boot_timeout_sec: float = 180.0,
    poll_interval_sec: float = 1.0,
) -> Client:
    """Block until the inner daemon answers `status()`, then return its client.

    Raises `TimeoutError` if `boot_timeout_sec` elapses without a
    successful status call. The caller is responsible for closing the
    returned `Client`.
    """
    host, port = relay.endpoint
    deadline = time.monotonic() + boot_timeout_sec
    last_err: Optional[BaseException] = None
    while time.monotonic() < deadline:
        try:
            c = Client(host=host, port=port)
        except (ConnectError, CorvusError, OSError) as e:
            last_err = e
            time.sleep(poll_interval_sec)
            continue
        try:
            c.status()
        except (ConnectError, CorvusError, OSError) as e:
            last_err = e
            try:
                c.close()
            except Exception:
                pass
            time.sleep(poll_interval_sec)
            continue
        return c
    raise TimeoutError(
        f"inner Corvus daemon did not answer on {host}:{port} within "
        f"{boot_timeout_sec:.0f}s; last error: {last_err!r}"
    )
