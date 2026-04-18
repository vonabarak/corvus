"""Corvus client — Haskell-backed native extension."""
from __future__ import annotations

import atexit

from . import _corvus  # noqa: F401
# Client is auto-generated from the Haskell Request type. Running
# `make python-codegen` refreshes _generated.py after any protocol edit.
from ._generated import Client
from .exceptions import (
    BadEnvelope,
    ConnectError,
    CorvusError,
    DiskHasOverlays,
    DiskInUse,
    DiskNotFound,
    DriveNotFound,
    FormatNotSupported,
    GuestAgentError,
    GuestAgentNotEnabled,
    InvalidTransition,
    NetIfNotFound,
    NetworkAlreadyRunning,
    NetworkError,
    NetworkInUse,
    NetworkNotFound,
    NetworkNotRunning,
    ProtocolError,
    ServerError,
    SharedDirNotFound,
    SnapshotNotFound,
    SshKeyInUse,
    SshKeyNotFound,
    TaskNotFound,
    TemplateNotFound,
    VmMustBeStopped,
    VmNotFound,
    VmRunning,
)

atexit.register(_corvus._shutdown)

__all__ = [
    "Client",
    "CorvusError",
    "ConnectError",
    "ProtocolError",
    "ServerError",
    "BadEnvelope",
    "VmNotFound",
    "VmRunning",
    "VmMustBeStopped",
    "InvalidTransition",
    "TaskNotFound",
    "DiskNotFound",
    "DiskInUse",
    "DiskHasOverlays",
    "DriveNotFound",
    "FormatNotSupported",
    "NetworkNotFound",
    "NetworkInUse",
    "NetworkAlreadyRunning",
    "NetworkNotRunning",
    "NetworkError",
    "NetIfNotFound",
    "SnapshotNotFound",
    "SshKeyNotFound",
    "SshKeyInUse",
    "SharedDirNotFound",
    "TemplateNotFound",
    "GuestAgentNotEnabled",
    "GuestAgentError",
]
__version__ = "0.1.0"
