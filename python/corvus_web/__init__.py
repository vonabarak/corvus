"""corvus-web — HTTP/WebSocket gateway in front of the corvus daemon.

Wraps the Cap'n Proto RPC surface (``corvus_client.AsyncClient``) in
a FastAPI app and serves a React SPA built from ``/frontend``.

Public entry point: ``corvus_web.cli:main`` (also exposed as the
``corvus-web`` console script).
"""

from __future__ import annotations

__version__ = "0.1.0"
