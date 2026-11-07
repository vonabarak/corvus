"""Open a pycapnp `Client` against the inner Corvus daemon.

The inner daemon's TCP listener is exposed to the host via a
`VsockTcpRelay`. We poll `Client.status()` with backoff until the
inner daemon answers — first-boot times can be 30-90s depending on
guest-agent / systemd ordering.
"""

from __future__ import annotations

import time
from pathlib import Path

from corvus_client import Client
from corvus_client._sync.node import SyncNode
from corvus_client.exceptions import ConnectError, CorvusError, NodeNotFound

from .transport import VsockTcpRelay


def open_client(
    relay: VsockTcpRelay,
    *,
    boot_timeout_sec: float = 180.0,
    poll_interval_sec: float = 1.0,
    ensure_self_node: bool = True,
    self_node_name: str = "self",
    self_node_host: str = "127.0.0.1",
    cert_dir: Path | None = None,
    tls: bool | None = None,
) -> Client:
    """Block until the inner daemon answers `status()`, then return its client.

    Raises `TimeoutError` if `boot_timeout_sec` elapses without a
    successful status call. The caller is responsible for closing
    the returned `Client`.

    ``cert_dir`` / ``tls`` are passed straight through to the
    :class:`Client` constructor. The harness defaults
    (:meth:`TestNode.client`) pass ``tls=True`` plus a per-node
    ``cert_dir`` minted by :meth:`Topology.deploy_certs` — the
    inner daemon's systemd unit refuses to start without a cert
    trio at ``/etc/corvus/``. The bare arg defaults here stay
    ``tls=None`` ("auto") so direct callers fall through to the
    pycapnp default; that's fine for callers that bring their
    own cert config.

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
    last_err: BaseException | None = None
    while time.monotonic() < deadline:
        try:
            c = Client(host=host, port=port, cert_dir=cert_dir, tls=tls)
        except (ConnectError, CorvusError, NodeNotFound, OSError) as e:
            last_err = e
            time.sleep(poll_interval_sec)
            continue
        try:
            c.status()
        except (ConnectError, CorvusError, NodeNotFound, OSError) as e:
            last_err = e
            try:
                c.close()
            except Exception:
                pass
            time.sleep(poll_interval_sec)
            continue
        if ensure_self_node:
            try:
                _register_self_node(c, self_node_name, self_node_host)
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


def _register_self_node(client: Client, name: str, host: str) -> None:
    """Register this node and wait for its supervisor-owned agent session."""
    try:
        node = client.nodes.get(name, by_name=True)
    except NodeNotFound:
        node = client.nodes.create(name, host)
    _wait_for_self_node_ready(node)


def _wait_for_self_node_ready(
    node: SyncNode,
    *,
    timeout_sec: float = 30.0,
    poll_interval_sec: float = 0.1,
) -> None:
    """Wait for a read-only round trip through the daemon's existing session."""
    deadline = time.monotonic() + timeout_sec
    last_message = "No probe completed"
    while time.monotonic() < deadline:
        result = node.ping()
        last_message = result.message
        if result.reachable:
            return
        time.sleep(poll_interval_sec)
    raise RuntimeError(
        "inner daemon's nodeagent never became reachable through "
        f"the supervisor: {last_message}"
    )
