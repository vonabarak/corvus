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
- `cases.{Single,Two,Three}VmCase` — class-based test bases with
                           class-scoped VMs
- `ssh.GuestShell`       — single-leg host→outer SSH over VSOCK
- `ssh.InnerGuestShell`  — host→outer→inner SSH tunnel for doubly-nested VMs
- `version`              — outer-version + nested-KVM sanity checks
"""

from . import base_images
from .cases import (
    IntegrationTestCase,
    SingleVmCase,
    ThreeVmsCase,
    TwoVmsCase,
)
from .host_binary import HostBinary
from .images import ImageReady
from .inner import open_client
from .inner_vm import InnerVm, InnerVmSsh, InnerVmUefi
from .outer import Crv
from .spice import SpiceLinkInfo, probe_spice_link
from .ssh import HOST_ALPINE_KEY_PATH, GuestShell, InnerGuestShell, SshResult
from .topology import Topology, TestVm
from .transport import VsockTcpRelay
from .version import (
    NestedKvmStatus,
    check_nested_kvm,
    check_outer_version,
)

__all__ = [
    "Crv",
    "GuestShell",
    "HOST_ALPINE_KEY_PATH",
    "HostBinary",
    "ImageReady",
    "InnerGuestShell",
    "InnerVm",
    "InnerVmSsh",
    "InnerVmUefi",
    "IntegrationTestCase",
    "NestedKvmStatus",
    "SingleVmCase",
    "SpiceLinkInfo",
    "SshResult",
    "TestVm",
    "ThreeVmsCase",
    "Topology",
    "TwoVmsCase",
    "VsockTcpRelay",
    "base_images",
    "check_nested_kvm",
    "check_outer_version",
    "open_client",
    "probe_spice_link",
]
