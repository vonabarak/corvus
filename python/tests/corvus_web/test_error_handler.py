"""Centralised typed-exception handler in :mod:`corvus_web.app`.

Without this handler, any route that doesn't explicitly catch a
``corvus_client`` typed exception turns into a silent 500 with an
empty body — the WebUI then has no message to render. Tests assert
each subclass of :class:`~corvus_client.exceptions.CorvusError` lands
on the expected HTTP status, and that the JSON body carries the
daemon's verbatim message under ``detail``.

The mapping function :func:`corvus_web.app._corvus_error_status` is
pure (typed exception → HTTP status), so the tests call it directly
instead of standing up a FastAPI TestClient — keeps the suite free of
an ``httpx`` runtime dependency in the project venv.
"""

from __future__ import annotations

import json
from collections.abc import Callable

import pytest
from corvus_client.exceptions import (
    ConnectError,
    CorvusError,
    DiskInUse,
    GuestAgentError,
    InvalidTransition,
    NetworkNotFound,
    ServerError,
    VmNotFound,
)
from fastapi.responses import JSONResponse

from corvus_web.app import _corvus_error_status


def _render(exc: CorvusError) -> tuple[int, dict[str, object]]:
    """Apply the handler the same way ``create_app`` wires it: take
    the status from ``_corvus_error_status`` and the body from
    ``JSONResponse(content={"detail": str(exc)})``. Returns the
    decoded ``(status, body)`` pair so assertions read naturally."""
    status = _corvus_error_status(exc)
    response = JSONResponse(status_code=status, content={"detail": str(exc)})
    return response.status_code, json.loads(response.body)


@pytest.mark.parametrize(
    "factory,expected_status,detail_substring",
    [
        # FSM-rejection — the original bug.
        (
            lambda: InvalidTransition("stopped", "Network 'corvus' is not running"),
            409,
            "Network 'corvus' is not running",
        ),
        # Not-found.
        (lambda: VmNotFound("VM 'web-1' not found"), 404, "VM 'web-1' not found"),
        (lambda: NetworkNotFound("Network 'br0' not found"), 404, "br0"),
        # State-conflict.
        (lambda: DiskInUse("Disk in use by VM 'web-1'"), 409, "Disk in use"),
        # Downstream unavailability.
        (lambda: ConnectError("daemon socket missing"), 502, "daemon socket"),
        (lambda: GuestAgentError("agent not reachable"), 503, "agent"),
        # Generic fallthrough — daemon refused for an unmodelled reason.
        (
            lambda: ServerError("something the translator didn't recognise"),
            400,
            "translator",
        ),
    ],
)
def test_handler_maps_exceptions_to_status(
    factory: Callable[[], CorvusError],
    expected_status: int,
    detail_substring: str,
) -> None:
    status, body = _render(factory())
    assert status == expected_status
    assert "detail" in body
    assert detail_substring in body["detail"]  # type: ignore[operator]


def test_invalid_transition_detail_carries_full_message() -> None:
    """The handler must emit BOTH halves of the InvalidTransition
    (current status + reason) so the operator sees what state the VM
    is in and what's blocking the request. The composed message is
    constructed by :class:`InvalidTransition.__init__`; we assert the
    handler doesn't truncate or rewrite it."""
    exc = InvalidTransition("stopped", "Network 'corvus' is not running")
    status, body = _render(exc)
    assert status == 409
    assert (
        body["detail"]
        == "cannot transition from stopped: Network 'corvus' is not running"
    )
