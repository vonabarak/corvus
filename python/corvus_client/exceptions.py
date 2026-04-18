"""Corvus client exception hierarchy.

Known "err" kinds and Response discriminator tags from the native bridge
map one-to-one to classes here. Unknown kinds fall back to CorvusError
so a new daemon-side error kind doesn't crash the client.
"""
from __future__ import annotations

from typing import Any


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


class GuestAgentNotEnabled(CorvusError):
    """VM doesn't have the QEMU guest agent enabled."""


class GuestAgentError(CorvusError):
    """Guest agent communication failed."""


_KIND_TO_CLASS = {
    "connect": ConnectError,
    "protocol": ProtocolError,
    "server": ServerError,
    "bad_envelope": BadEnvelope,
    "vm_not_found": VmNotFound,
    "vm_running": VmRunning,
    "vm_must_be_stopped": VmMustBeStopped,
    "task_not_found": TaskNotFound,
    "disk_not_found": DiskNotFound,
    "disk_in_use": DiskInUse,
    "disk_has_overlays": DiskHasOverlays,
    "drive_not_found": DriveNotFound,
    "format_not_supported": FormatNotSupported,
    "network_not_found": NetworkNotFound,
    "network_in_use": NetworkInUse,
    "network_already_running": NetworkAlreadyRunning,
    "network_not_running": NetworkNotRunning,
    "network_error": NetworkError,
    "netif_not_found": NetIfNotFound,
    "snapshot_not_found": SnapshotNotFound,
    "ssh_key_not_found": SshKeyNotFound,
    "ssh_key_in_use": SshKeyInUse,
    "shared_dir_not_found": SharedDirNotFound,
    "template_not_found": TemplateNotFound,
    "guest_agent_not_enabled": GuestAgentNotEnabled,
    "guest_agent_error": GuestAgentError,
}


def from_err(kind: str, details: Any) -> CorvusError:
    """Translate a structured {"kind":..., "details":...} dict to an exception."""
    if kind == "invalid_transition":
        return InvalidTransition(
            status=details.get("status", "unknown") if isinstance(details, dict) else "unknown",
            reason=details.get("reason", "") if isinstance(details, dict) else "",
        )
    cls = _KIND_TO_CLASS.get(kind, CorvusError)
    message = _extract_message(kind, details)
    return cls(message, details)


def _extract_message(kind: str, details: Any) -> str:
    if isinstance(details, dict):
        msg = details.get("message")
        if isinstance(msg, str):
            return msg
    return kind


# ---------------------------------------------------------------------------
# Response-tag → exception mapping
#
# Several Response variants carry error-shaped information in the success
# channel (the daemon returns them as normal responses, not as RespError).
# We translate those into the right Python exception at the Client layer.
# ---------------------------------------------------------------------------

_TAG_TO_CLASS = {
    "vm_not_found": VmNotFound,
    "vm_running": VmRunning,
    "vm_must_be_stopped": VmMustBeStopped,
    "disk_not_found": DiskNotFound,
    "drive_not_found": DriveNotFound,
    "snapshot_not_found": SnapshotNotFound,
    "shared_dir_not_found": SharedDirNotFound,
    "net_if_not_found": NetIfNotFound,
    "ssh_key_not_found": SshKeyNotFound,
    "template_not_found": TemplateNotFound,
    "network_not_found": NetworkNotFound,
    "network_in_use": NetworkInUse,
    "network_already_running": NetworkAlreadyRunning,
    "network_not_running": NetworkNotRunning,
    "task_not_found": TaskNotFound,
    "guest_agent_not_enabled": GuestAgentNotEnabled,
}


def from_response_tag(tag: str, payload: dict) -> CorvusError | None:
    """Map a Response discriminator tag to an exception, or None if the tag
    is a non-error success response."""
    if tag == "error":
        return ServerError(payload.get("message", "server error"), payload)
    if tag == "network_error":
        return NetworkError(payload.get("message", "network error"), payload)
    if tag == "guest_agent_error":
        return GuestAgentError(payload.get("message", "guest agent error"), payload)
    if tag == "format_not_supported":
        return FormatNotSupported(payload.get("message", "format not supported"), payload)
    if tag == "invalid_transition":
        return InvalidTransition(
            status=payload.get("status", "unknown"),
            reason=payload.get("reason", ""),
        )
    if tag == "disk_in_use":
        vms = payload.get("attached_vms", [])
        return DiskInUse(f"disk attached to {len(vms)} VM(s)", vms)
    if tag == "disk_has_overlays":
        overlays = payload.get("overlays", [])
        return DiskHasOverlays(f"disk has {len(overlays)} overlay(s)", overlays)
    if tag == "ssh_key_in_use":
        vms = payload.get("used_by_vms", [])
        return SshKeyInUse(f"key used by {len(vms)} VM(s)", vms)
    cls = _TAG_TO_CLASS.get(tag)
    if cls is None:
        return None
    return cls(tag)
