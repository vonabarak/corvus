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
    ensure_self_node: bool = True,
    self_node_name: str = "self",
) -> Client:
    """Block until the inner daemon answers `status()`, then return its client.

    Raises `TimeoutError` if `boot_timeout_sec` elapses without a
    successful status call. The caller is responsible for closing
    the returned `Client`.

    If ``ensure_self_node`` is ``True`` (the default), also
    registers a ``Node`` row in the inner daemon pointing at
    ``127.0.0.1``. This is required by multi-node Phase 1: every
    VM / network / disk in the inner daemon's DB carries a node
    FK, and the scheduler's default placement needs at least one
    online node to pick from. Skips the register step if a node
    with ``self_node_name`` already exists (idempotent across
    test re-runs that share an inner daemon).
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
        if ensure_self_node:
            try:
                _register_self_node(c, self_node_name)
            except Exception as e:
                try:
                    c.close()
                except Exception:
                    pass
                raise RuntimeError(
                    f"inner daemon answered on {host}:{port} but "
                    f"registering self-node {self_node_name!r} failed: {e}"
                ) from e
        return c
    raise TimeoutError(
        f"inner Corvus daemon did not answer on {host}:{port} within "
        f"{boot_timeout_sec:.0f}s; last error: {last_err!r}"
    )


def _register_self_node(client: Client, name: str) -> None:
    """Idempotently register a self-pointing Node row.

    Inner daemons run with their nodeagent + netd on the same
    box, listening on the conventional 9878 / 9877 ports. The
    'host' value is '127.0.0.1' because the daemon is on that
    host too — there's no cross-host hop inside a test node.
    """
    # If a node with this name already exists, do nothing — this
    # makes the function safe to call multiple times.
    try:
        existing = client.nodes.get(name, by_name=True)
        # If we get here, the node already exists. Nothing to do.
        del existing
        return
    except CorvusError:
        pass
    client.nodes.create(name, "127.0.0.1")
