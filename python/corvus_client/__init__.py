"""Corvus client — pure-Python pycapnp implementation.

The package layout:
  - corvus_client.Client       — sync API (wraps the async core on a background loop)
  - corvus_client.AsyncClient  — async API (asyncio + pycapnp.kj_loop)
  - corvus_client.entity_ref   — helper to build EntityRef union values
  - corvus_client.exceptions   — typed exception hierarchy
"""
from __future__ import annotations

from ._async.client import AsyncClient
from ._entityref import entity_ref
from ._sync.client import Client
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
    NodeInUse,
    NodeNotFound,
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

__all__ = [
    "AsyncClient",
    "Client",
    "entity_ref",
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
    "NodeNotFound",
    "NodeInUse",
    "NetIfNotFound",
    "SnapshotNotFound",
    "SshKeyNotFound",
    "SshKeyInUse",
    "SharedDirNotFound",
    "TemplateNotFound",
    "GuestAgentNotEnabled",
    "GuestAgentError",
]
__version__ = "0.2.0"
