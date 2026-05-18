"""Test harness for Corvus integration tests.

Public entry points are the pytest fixtures defined in
`integration_tests/conftest.py`; this package holds the building
blocks they're composed from:

- `outer.Crv`            — drives the outer Corvus daemon via the `crv` CLI
- `images.ImageReady`    — ensures the integration-test image is applied
- `host_binary.HostBinary` — validates the freshly compiled inner binary
- `transport.VsockTcpRelay` — bridges host TCP ↔ node VSOCK
- `inner.client`         — opens a pycapnp Client against a node's inner daemon
- `topology.Topology`    — declarative multi-node scenarios for tests
- `base_images`          — register pre-baked disks with a node's inner daemon
- `cases.{Single,Two,Three}NodeCase` — class-based test bases with
                           class-scoped nodes
- `ssh.NodeShell`        — single-leg host→node SSH over VSOCK
- `ssh.VmShell`          — host→node→vm SSH tunnel for vms
- `vm.{Vm,VmSsh,VmUefi}` — context managers for the vm lifecycle
- `netd_client.NetdClient` — sync wrapper around the `corvus-netd` agent
- `version`              — outer-daemon-version + nested-KVM sanity checks
"""

from . import base_images
from .cases import (
    IntegrationTestCase,
    SingleNodeCase,
    ThreeNodesCase,
    TwoNodesCase,
)
from .host_binary import HostBinary
from .images import ImageReady
from .inner import open_client
from .netd_client import NetdClient
from .outer import Crv
from .spice import SpiceLinkInfo, probe_spice_link
from .ssh import HOST_ALPINE_KEY_PATH, NodeShell, SshResult, VmShell
from .topology import TestNode, Topology
from .transport import VsockTcpRelay
from .version import (
    NestedKvmStatus,
    check_nested_kvm,
    check_outer_version,
)
from .vm import Vm, VmCloudInit, VmSsh, VmUefi, VmWindows

__all__ = [
    "Crv",
    "HOST_ALPINE_KEY_PATH",
    "HostBinary",
    "ImageReady",
    "IntegrationTestCase",
    "NestedKvmStatus",
    "NetdClient",
    "NodeShell",
    "SingleNodeCase",
    "SpiceLinkInfo",
    "SshResult",
    "TestNode",
    "ThreeNodesCase",
    "Topology",
    "TwoNodesCase",
    "Vm",
    "VmCloudInit",
    "VmShell",
    "VmSsh",
    "VmUefi",
    "VmWindows",
    "VsockTcpRelay",
    "base_images",
    "check_nested_kvm",
    "check_outer_version",
    "open_client",
    "probe_spice_link",
]
