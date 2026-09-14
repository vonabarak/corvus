"""Corvus client exception hierarchy and KjException translation.

The Corvus daemon emits Cap'n Proto exceptions via `Capnp.Rpc.throwFailed
"<message>"`. pycapnp surfaces these as `capnp.KjException` with a
`.description` field. The daemon renders each error in the canonical
form "<code> :: <message>" (see `src/Corvus/Wire/Error.hs` and the
`ErrorCode` enum in `schema/enums.capnp`); this module maps the code
via a dict lookup to a typed Python exception. Anything unrecognized
degrades to `ServerError` so a new daemon-side error never crashes the
client.
"""

from __future__ import annotations

import re
from collections.abc import Awaitable, Callable
from typing import Any, TypeVar

import capnp

# ---------------------------------------------------------------------------
# Hierarchy
# ---------------------------------------------------------------------------


class CorvusError(Exception):
    """Base class. Always caught in a catch-all block."""

    def __init__(self, message: str, details: Any = None) -> None:
        super().__init__(message)
        self.details = details


class ConnectError(CorvusError):
    """Could not reach the daemon (socket missing, refused, timed out)."""


class ProtocolError(CorvusError):
    """Wire format or version mismatch with the daemon."""


class ServerError(CorvusError):
    """The daemon returned a generic error (`internal_error` code)."""


class BadEnvelope(CorvusError):
    """The envelope bytes were malformed — client bug, not a daemon issue."""


class VmNotFound(CorvusError):
    """No VM matches the supplied ref."""


class VmRunning(CorvusError):
    """A VM running-state condition blocks the operation.

    Base of a small family: the daemon's `vm_not_running` and
    `vm_headless` codes map to the subclasses below, so catching this
    covers either.
    """


class VmNotRunning(VmRunning):
    """The VM is not running, but the operation requires it to be."""


class VmHeadless(VmRunning):
    """The VM has no SPICE display, so no console is available."""


class InvalidTransition(CorvusError):
    """The requested state change isn't allowed from the VM's current status."""

    def __init__(self, status: str, reason: str) -> None:
        super().__init__(f"cannot transition from {status}: {reason}")
        self.status = status
        self.reason = reason


class AmbiguousRef(CorvusError):
    """A name ref matched multiple entities across nodes; use the numeric id."""


class TaskNotFound(CorvusError):
    """No task with the given id."""


class DiskNotFound(CorvusError):
    """No disk image matches the ref."""


class DiskInUse(CorvusError):
    """Disk is still attached to running VMs."""


class DiskHasOverlays(CorvusError):
    """Disk is the backing image of one or more overlays."""


class DriveNotFound(CorvusError):
    """VM has no drive matching the ref."""


class VmMustBeStopped(CorvusError):
    """Operation requires the VM to be stopped first."""


class FormatNotSupported(CorvusError):
    """Disk format doesn't support the requested operation (e.g. snapshots on raw)."""


class NetworkNotFound(CorvusError):
    """No virtual network matches the ref."""


class NetworkInUse(CorvusError):
    """Network is referenced by interfaces or running VMs."""


class NetworkAlreadyRunning(CorvusError):
    """Network is already running."""


class NetworkNotRunning(CorvusError):
    """Network must be running for this operation."""


class NetworkError(CorvusError):
    """Generic network subsystem error."""


class NetIfNotFound(CorvusError):
    """VM has no network interface with that id."""


class SnapshotNotFound(CorvusError):
    """Disk has no snapshot with that ref."""


class SshKeyNotFound(CorvusError):
    """No SSH key matches the ref."""


class SshKeyInUse(CorvusError):
    """SSH key is attached to running VMs."""


class SharedDirNotFound(CorvusError):
    """VM has no shared directory with that ref."""


class TemplateNotFound(CorvusError):
    """No template matches the ref."""


class NodeNotFound(CorvusError):
    """No node matches the ref."""


class NodeInUse(CorvusError):
    """Node still has VMs, networks, or disk placements attached."""


class GuestAgentNotEnabled(CorvusError):
    """VM doesn't have the QEMU guest agent enabled."""


class GuestAgentError(CorvusError):
    """Guest agent communication failed."""


# ---------------------------------------------------------------------------
# Code → exception mapping
# ---------------------------------------------------------------------------

# The closed set of daemon wire error codes — the snake_case tokens of
# the `ErrorCode` enum in `schema/enums.capnp` (the single source of
# truth) — mapped to typed exceptions. The daemon renders errors as
# "<code> :: <message>"; the code picks the class, the message stays
# the human-readable body.
_CODE_MAP: dict[str, type[CorvusError]] = {
    "vm_not_found": VmNotFound,
    "disk_not_found": DiskNotFound,
    "snapshot_not_found": SnapshotNotFound,
    "drive_not_found": DriveNotFound,
    "network_not_found": NetworkNotFound,
    "netif_not_found": NetIfNotFound,
    "ssh_key_not_found": SshKeyNotFound,
    "shared_dir_not_found": SharedDirNotFound,
    "template_not_found": TemplateNotFound,
    "task_not_found": TaskNotFound,
    "node_not_found": NodeNotFound,
    "disk_in_use": DiskInUse,
    "disk_has_overlays": DiskHasOverlays,
    "vm_must_be_stopped": VmMustBeStopped,
    "vm_not_running": VmNotRunning,
    "vm_headless": VmHeadless,
    "network_in_use": NetworkInUse,
    "network_already_running": NetworkAlreadyRunning,
    "network_not_running": NetworkNotRunning,
    "ssh_key_in_use": SshKeyInUse,
    "node_in_use": NodeInUse,
    "invalid_transition": InvalidTransition,
    "format_not_supported": FormatNotSupported,
    "guest_agent_not_enabled": GuestAgentNotEnabled,
    "guest_agent_error": GuestAgentError,
    "ambiguous_ref": AmbiguousRef,
    "internal_error": ServerError,
    "protocol_error": ProtocolError,
}

# The wire delimiter, matching 'renderWireError' in
# src/Corvus/Wire/Error.hs.
_DELIMITER = " :: "


def _split_wire_error(body: str) -> tuple[str, str] | None:
    """Split a daemon error message into (code, message).

    Codes are matched longest-first against the head of the body, so a
    message that itself contains the delimiter ("code :: a :: b") still
    round-trips. Returns None for a legacy (code-less) message or an
    unknown code.
    """
    for code in sorted(_CODE_MAP, key=len, reverse=True):
        if body.startswith(code + _DELIMITER):
            return code, body[len(code) + len(_DELIMITER) :]
    return None


# Match `(remote):0: failed: remote exception: <message>` envelopes that
# pycapnp wraps around the daemon's throwFailed strings.
_REMOTE_EXC_RE = re.compile(r"remote exception:\s*(.*)$", re.DOTALL)

# FSM transition rejection. The daemon's `statusOrThrow` in
# `src/Corvus/Rpc/Vm.hs` formats the `invalid_transition` message part as
# `"invalid transition from <status>: <reason>"`, where <status> is the
# lower-case enumToText for VmStatus. The code tells the translator it's
# an invalid_transition; this regex extracts the (status, reason) pair
# from the message part because `InvalidTransition.__init__` takes
# (status, reason), which doesn't fit the (body, details) constructor
# shape used by the other codes.
_INVALID_TRANSITION_RE = re.compile(
    r"^invalid transition from (\w+):\s*(.*)$", re.DOTALL
)


def _bare_message(description: str) -> str:
    """Strip pycapnp's envelope so we see just the daemon message."""
    m = _REMOTE_EXC_RE.search(description)
    return m.group(1).strip() if m else description.strip()


def translate_kj_exception(exc: capnp.KjException) -> CorvusError:
    """Map a `capnp.KjException` to a typed Python exception."""
    description = getattr(exc, "description", None) or str(exc)
    body = _bare_message(description)
    split = _split_wire_error(body)
    if split is not None:
        code, message = split
        if code == "invalid_transition":
            m = _INVALID_TRANSITION_RE.match(message)
            if m:
                return InvalidTransition(m.group(1), m.group(2).strip())
            return ServerError(message, details=description)
        return _CODE_MAP[code](message, details=description)
    # Legacy daemon (no code prefix) or a code this client doesn't know:
    # degrade to the generic error instead of crashing.
    return ServerError(body, details=description)


# ---------------------------------------------------------------------------
# Decorators that wrap pycapnp calls and translate errors
# ---------------------------------------------------------------------------

T = TypeVar("T")


def translate_async(
    fn: Callable[..., Awaitable[T]],
) -> Callable[..., Awaitable[T]]:
    """Async-method decorator: translate `KjException` → typed exception.

    Also invalidates a cached manager cap (`self._mgr`) on failure.
    Empirically, a cap-method on a cached pycapnp manager cap leaves
    the cap wedged after the call raises (e.g. `vms.get(nonexistent)
    → VmNotFound`): subsequent calls on the same cached cap deadlock.
    Clearing the cache forces the next access to re-fetch a fresh cap
    via the daemon. The same does NOT apply to resource caps
    (`self._cap` on `AsyncVm` etc.) — those are tied to a specific
    entity and silently re-fetching would change identity.
    """

    async def wrapped(*args, **kwargs):
        try:
            return await fn(*args, **kwargs)
        except capnp.KjException as e:
            if args and hasattr(args[0], "_mgr"):
                args[0]._mgr = None
            raise translate_kj_exception(e) from None

    wrapped.__wrapped__ = fn  # type: ignore[attr-defined]
    wrapped.__name__ = getattr(fn, "__name__", "wrapped")
    return wrapped


def translate_errors(cls):
    """Class decorator: wrap every async public method to translate errors.

    Applied to each Async* class so callers see typed
    `CorvusError` subclasses instead of raw `capnp.KjException`.
    """
    import inspect

    for name, attr in list(vars(cls).items()):
        if name.startswith("_"):
            continue
        if inspect.iscoroutinefunction(attr):
            setattr(cls, name, translate_async(attr))
    return cls
