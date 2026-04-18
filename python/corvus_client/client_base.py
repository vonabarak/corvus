"""Base class for the Corvus client.

Holds the persistent-connection lifecycle (__init__, __enter__, __exit__,
close, _call) plus the envelope/JSON plumbing. The per-RPC methods live
in the auto-generated module (_generated.py), which defines a Client
class extending this one.
"""
from __future__ import annotations

import json
import os
from typing import Any, Optional

from . import _corvus
from .exceptions import from_err, from_response_tag


def _default_unix_socket() -> str:
    runtime = os.environ.get("XDG_RUNTIME_DIR", "/tmp")
    return os.path.join(runtime, "corvus", "corvus.sock")


class BaseClient:
    """Persistent Corvus client. Concrete RPC methods are mixed in by the
    auto-generated Client subclass (see _generated.py)."""

    def __init__(
        self,
        unix_socket: Optional[str] = None,
        host: Optional[str] = None,
        port: int = 9876,
    ) -> None:
        if unix_socket and host:
            raise ValueError("specify exactly one of unix_socket or host")
        if host is None and unix_socket is None:
            unix_socket = _default_unix_socket()
        transport: dict = (
            {"unix": unix_socket} if unix_socket is not None
            else {"tcp": [host, port]}
        )
        transport_bytes = json.dumps(transport, separators=(",", ":")).encode("utf-8")
        result = _corvus.open(transport_bytes)
        if isinstance(result, (bytes, bytearray)):
            err = json.loads(result)["err"]
            raise from_err(err["kind"], err.get("details"))
        self._capsule = result
        self._closed = False

    def close(self) -> None:
        if not self._closed:
            _corvus.close(self._capsule)
            self._closed = True

    def __enter__(self) -> "BaseClient":
        return self

    def __exit__(self, exc_type, exc, tb) -> None:
        self.close()

    def __del__(self) -> None:
        try:
            self.close()
        except Exception:
            pass

    def _call(self, op: str, **args: Any) -> Any:
        if self._closed:
            raise RuntimeError("Client is closed")
        clean = {k: v for k, v in args.items() if v is not None}
        request_obj = {"op": op, **clean}
        request_bytes = json.dumps(request_obj, separators=(",", ":")).encode("utf-8")
        response_bytes = _corvus.call_open(self._capsule, request_bytes)
        reply = json.loads(response_bytes)
        if "ok" in reply:
            payload = reply["ok"]
            # The success channel includes variants that semantically
            # represent errors (vm_not_found, invalid_transition, etc.).
            # Translate the tag to an exception at one common point.
            if isinstance(payload, dict):
                tag = payload.get("tag")
                if tag:
                    exc = from_response_tag(tag, payload)
                    if exc is not None:
                        raise exc
            return payload
        err = reply["err"]
        raise from_err(err["kind"], err.get("details"))
