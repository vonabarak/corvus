"""Corvus client exception hierarchy and KjException translation.

The Corvus daemon emits Cap'n Proto exceptions via `Capnp.Rpc.throwFailed
"<message>"`. pycapnp surfaces these as `capnp.KjException` with a
`.description` field. We pattern-match the message against a small list
of canonical strings and raise typed Python exceptions; anything we
don't recognize becomes a plain `CorvusError` so a new daemon-side
message doesn't crash the client.

This is a temporary translation layer. If/when the daemon adopts
structured exception codes on the wire (see `src/Corvus/Wire/Errors.hs`),
this module shrinks to a dict lookup.
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
    """The daemon returned a generic RespError."""


class BadEnvelope(CorvusError):
    """The envelope bytes were malformed — client bug, not a daemon issue."""


class VmNotFound(CorvusError):
    """No VM matches the supplied ref."""


class VmRunning(CorvusError):
    """Operation requires the VM to be stopped, but it's running."""


class InvalidTransition(CorvusError):
    """The requested state change isn't allowed from the VM's current status."""

    def __init__(self, status: str, reason: str) -> None:
        super().__init__(f"cannot transition from {status}: {reason}")
        self.status = status
        self.reason = reason


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
# Message → exception mapping
# ---------------------------------------------------------------------------

# Regex-match table: case-sensitive, ordered most-specific first.
#
# The daemon emits either a static message ("Disk not found") or a
# decorated form from Corvus.Handlers.Resolve ("Disk '<name>' not found",
# "Disk #42 not found"). The patterns below match both shapes.
_MESSAGE_TABLE = (
    # In-use / state errors first (specific phrases).
    (re.compile(r"^Disk has overlays$"), DiskHasOverlays),
    (re.compile(r"^Disk in use\b"), DiskInUse),
    (re.compile(r"^Drive not found"), DriveNotFound),
    (re.compile(r"^Guest agent not enabled"), GuestAgentNotEnabled),
    (re.compile(r"^Net-if not found"), NetIfNotFound),
    (re.compile(r"^Network already running"), NetworkAlreadyRunning),
    (re.compile(r"^Network in use\b"), NetworkInUse),
    (re.compile(r"^Network not running"), NetworkNotRunning),
    (re.compile(r"^SSH key in use\b"), SshKeyInUse),
    (re.compile(r"^Serial console buffer not available"), GuestAgentError),
    (re.compile(r"^HMP monitor buffer not available"), GuestAgentError),
    (re.compile(r"^VM has no SPICE display"), VmRunning),
    (re.compile(r"^VM is not running"), VmRunning),
    (re.compile(r"^VM is not headless"), VmRunning),
    (re.compile(r"^VM not running"), VmRunning),
    (re.compile(r"^VM must be stopped"), VmMustBeStopped),
    # Not-found patterns. Match either the static message or the
    # Resolve-helper decorated form ("Type '<name>' not found",
    # "Type #<id> not found").
    (re.compile(r"^VM\b.*\bnot found"), VmNotFound),
    (re.compile(r"^Disk\b.*\bnot found"), DiskNotFound),
    (re.compile(r"^Snapshot\b.*\bnot found"), SnapshotNotFound),
    (re.compile(r"^Network\b.*\bnot found"), NetworkNotFound),
    (re.compile(r"^SSH key\b.*\bnot found"), SshKeyNotFound),
    (re.compile(r"^Template\b.*\bnot found"), TemplateNotFound),
    (re.compile(r"^Task\b.*\bnot found"), TaskNotFound),
    (re.compile(r"^Node\b.*\bnot found"), NodeNotFound),
    (re.compile(r"^Node\b.*\bis still referenced"), NodeInUse),
    (re.compile(r"^Shared directory\b.*\bnot found"), SharedDirNotFound),
)

# Match `(remote):0: failed: remote exception: <message>` envelopes that
# pycapnp wraps around the daemon's throwFailed strings.
_REMOTE_EXC_RE = re.compile(r"remote exception:\s*(.*)$", re.DOTALL)


def _bare_message(description: str) -> str:
    """Strip pycapnp's envelope so substring matches see just the daemon msg."""
    m = _REMOTE_EXC_RE.search(description)
    return m.group(1).strip() if m else description.strip()


def translate_kj_exception(exc: capnp.KjException) -> CorvusError:
    """Map a `capnp.KjException` to a typed Python exception."""
    description = getattr(exc, "description", None) or str(exc)
    body = _bare_message(description)
    for pattern, cls in _MESSAGE_TABLE:
        if pattern.search(body):
            return cls(body, details=description)
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
