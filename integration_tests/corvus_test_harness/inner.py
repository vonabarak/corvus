"""Open a pycapnp `Client` against the inner Corvus daemon.

The inner daemon's TCP listener is exposed to the host via a
`VsockTcpRelay`. We poll `Client.status()` with backoff until the
inner daemon answers — first-boot times can be 30-90s depending on
guest-agent / systemd ordering.
"""
from __future__ import annotations

import time
from pathlib import Path
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
    cert_dir: Optional[Path] = None,
    tls: Optional[bool] = None,
) -> Client:
    """Block until the inner daemon answers `status()`, then return its client.

    Raises `TimeoutError` if `boot_timeout_sec` elapses without a
    successful status call. The caller is responsible for closing
    the returned `Client`.

    ``cert_dir`` / ``tls`` are passed straight through to the
    :class:`Client` constructor. ``tls=None`` (the default) means
    "auto" — pycapnp's TCP transport will turn on TLS. For the
    integration suite right now the inner daemons run with
    ``--no-tls`` baked into their systemd units (see
    ``yaml/corvus-test-node/systemd/``), so test callers pass
    ``tls=False`` until the per-VM cert-deploy lands. Once it
    does, callers will pass an explicit ``cert_dir`` pointing at
    a temp dir holding the trio the harness PKI minted.

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
            c = Client(host=host, port=port, cert_dir=cert_dir, tls=tls)
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
    """Idempotently register a self-pointing Node row and wait for
    its per-node supervisor to dial the nodeagent.

    Inner daemons run with their nodeagent + netd on the same
    box, listening on the conventional 9878 / 9877 ports. The
    'host' value is '127.0.0.1' because the daemon is on that
    host too — there's no cross-host hop inside a test node.

    The daemon's 'handleNodeAdd' spawns the per-node reconnect
    supervisor as a background async; the supervisor's dial
    races with whichever test method runs next. Without the
    poll below, the first 'client.disks.create(…)' inside a
    test sees 'nodeagent for node 1 unavailable' before the
    dial completes. We probe a cheap agent-dependent op
    ('disks.create' on a sentinel name, immediately deleted)
    until it succeeds.
    """
    # If a node with this name already exists, skip the create
    # but still run the connectivity poll — across test re-runs
    # against a long-lived daemon the supervisor may already be
    # connected; the probe returns fast in that case.
    need_create = True
    try:
        existing = client.nodes.get(name, by_name=True)
        del existing
        need_create = False
    except CorvusError:
        pass
    if need_create:
        client.nodes.create(name, "127.0.0.1")
    _wait_for_self_node_ready(client)


def _wait_for_self_node_ready(
    client: Client,
    *,
    timeout_sec: float = 30.0,
    poll_interval_sec: float = 0.1,
) -> None:
    """Block until the inner daemon's per-node supervisor has
    finished its initial nodeagent dial.

    Probes a cheap, idempotent agent call. Any 'unavailable'
    error from the daemon means the supervisor's dial hasn't
    landed yet — keep polling. Any other error surfaces
    immediately (it's a real problem).
    """
    deadline = time.monotonic() + timeout_sec
    last_err: Optional[BaseException] = None
    while time.monotonic() < deadline:
        try:
            d = client.disks.create("__harness_probe__", size_mb=1)
            d.delete()
            return
        except CorvusError as e:
            last_err = e
            if "unavailable" not in str(e).lower():
                raise
            time.sleep(poll_interval_sec)
    raise RuntimeError(
        f"inner daemon's nodeagent never became reachable through "
        f"the supervisor: {last_err!r}"
    )
