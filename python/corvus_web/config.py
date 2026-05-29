"""Runtime configuration for the corvus-web gateway."""

from __future__ import annotations

import os
from dataclasses import dataclass, field
from pathlib import Path


def _default_unix_socket() -> str:
    """Mirror the daemon's default: ``$XDG_RUNTIME_DIR/corvus/corvus.sock``,
    with the same ``/tmp/corvus/corvus.sock`` fallback the Haskell code
    uses when XDG_RUNTIME_DIR is unset (see src/Corvus/Types.hs)."""
    xdg = os.environ.get("XDG_RUNTIME_DIR")
    base = xdg if xdg else "/tmp"
    return str(Path(base) / "corvus" / "corvus.sock")


def _default_frontend_dir() -> Path:
    """The vite build output dir, shipped inside the wheel as
    ``corvus_web/static/``. When the SPA hasn't been built the directory
    exists but is empty — the SPA route handler degrades to a 404 with
    a helpful message in that case."""
    return Path(__file__).parent / "static"


@dataclass(frozen=True)
class CorvusWebConfig:
    """Resolved configuration for one ``corvus-web`` process.

    Built from CLI flags in :mod:`corvus_web.cli`; passed to
    :func:`corvus_web.app.create_app`. Frozen because the FastAPI
    lifespan reads it concurrently and a mid-run mutation would be
    a bug."""

    # HTTP server bind. Defaults to localhost-only per the v1 design —
    # there is no auth in front of the gateway yet, so binding to a
    # routable address would silently expose the daemon. Operators
    # tunnel via SSH.
    #
    # 8080 is the conventional "user-facing HTTP UI" port and avoids
    # the corvus family of RPC ports (9876 corvusd, 9877 corvus-netd,
    # 9878 corvus-nodeagent), which all carry Cap'n Proto traffic.
    bind_host: str = "127.0.0.1"
    bind_port: int = 8080

    # Exactly one of these must be set. Mirrors the AsyncClient kwargs.
    daemon_unix_socket: str | None = None
    daemon_host: str | None = None
    daemon_port: int = 9876

    # TLS knobs for TCP connections to the daemon. ``tls=None`` (the
    # default) means "auto": TLS on for TCP, off for Unix. ``cert_dir``
    # lets the operator point at a non-default cert bundle.
    daemon_tls: bool | None = None
    daemon_cert_dir: Path | None = None

    # Where vite's build output lives. The wheel ships static/ inside
    # the package; operators running from a source checkout can point
    # this at frontend/dist/ instead.
    frontend_dir: Path = field(default_factory=_default_frontend_dir)

    # Logging is uvicorn's responsibility (we just pass this through),
    # but it's part of the config so the CLI surfaces one --log-level
    # flag for both uvicorn and our own logger.
    log_level: str = "info"
