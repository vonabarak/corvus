"""Test harness for Corvus integration tests.

Public entry points are the pytest fixtures defined in
`tests-integration/conftest.py`; this package holds the building
blocks they're composed from:

- `outer.Crv`            — drives the outer-Corvus daemon via the `crv` CLI
- `images.ImageReady`    — ensures the integration-test image is applied
- `host_binary.HostBinary` — validates the freshly compiled inner binary
- `transport.VsockTcpRelay` — bridges host TCP ↔ guest VSOCK
- `inner.client`         — opens a pycapnp Client against the inner daemon
- `topology.Topology`    — declarative multi-VM scenarios for tests
- `base_images`          — register pre-baked disks with the inner daemon
- `ssh.GuestShell`       — SSH/exec into a VM for low-level work
- `version`              — outer-version + nested-KVM sanity checks
"""

from . import base_images
from .host_binary import HostBinary
from .images import ImageReady
from .inner import open_client
from .outer import Crv
from .topology import Topology, TestVm
from .transport import VsockTcpRelay
from .version import (
    NestedKvmStatus,
    check_nested_kvm,
    check_outer_version,
)

__all__ = [
    "Crv",
    "HostBinary",
    "ImageReady",
    "NestedKvmStatus",
    "TestVm",
    "Topology",
    "VsockTcpRelay",
    "base_images",
    "check_nested_kvm",
    "check_outer_version",
    "open_client",
]
