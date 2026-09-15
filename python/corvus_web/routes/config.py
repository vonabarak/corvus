"""Configuration endpoint: enum values and valid transitions.

Serves the enum constants and lifecycle transition table that the
frontend needs to render forms and action buttons in sync with the
daemon.  The daemon is authoritative; this endpoint is a cache for
the SPA so it never has to hard-code values that drift.
"""

from __future__ import annotations

from typing import Any

from fastapi import APIRouter

router = APIRouter(tags=["config"])


# ── Enum values (must match schema/enums.capnp) ──────────────────

_DRIVE_INTERFACES: list[str] = [
    "",
    "virtio",
    "scsi",
    "ide",
    "sata",
    "floppy",
]
_DRIVE_MEDIA: list[str] = [
    "",
    "disk",
    "cdrom",
    "floppy",
]
_CACHE_TYPES: list[str] = [
    "",
    "none",
    "writethrough",
    "writeback",
    "directsync",
    "unsafe",
]

# ── Lifecycle transitions ─────────────────────────────────────────
# Mirrors `validateTransition` in src/Corvus/Model/VmState.hs.
# The frontend calls /api/config to get this table so it never
# shows a disabled button that would actually succeed.
_TRANSITIONS: dict[str, list[str]] = {
    "stopped": ["start"],
    "starting": ["stop", "reset"],
    "running": ["stop", "pause", "reset", "save", "send-ctrl-alt-del"],
    "stopping": ["reset"],
    "paused": ["start", "reset", "save"],
    "saved": ["start", "reset"],
    "error": ["reset"],
}


@router.get("")
async def get_config() -> dict[str, Any]:
    """Return configuration data the frontend needs to stay in sync
    with the daemon: valid enum values and the lifecycle transition
    table."""
    return {
        "drive_interfaces": _DRIVE_INTERFACES,
        "drive_media": _DRIVE_MEDIA,
        "cache_types": _CACHE_TYPES,
        "transitions": _TRANSITIONS,
    }
