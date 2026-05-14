"""Multi-node scenarios — two inner Corvus daemons cooperating or
contending over shared state. Impossible under the single-process
outer deployment.

Both nodes are created via `topology.add()`; each gets its own pycapnp
client. The two inner daemons see independent databases (Postgres
lives inside each VM) but can be wired to a shared network for
cross-node connectivity tests.
"""
from __future__ import annotations

import pytest

pytestmark = pytest.mark.slow


def test_two_independent_inner_daemons(topology):
    """Two inner daemons start, both answer status() independently."""
    a = topology.add("alpha")
    b = topology.add("beta")
    client_a = a.client()
    client_b = b.client()
    info_a = client_a.status()
    info_b = client_b.status()
    # Same source tree → same version reported by both.
    assert info_a.version == info_b.version
    # Independent uptimes; we just assert both are non-negative.
    assert info_a.uptime_seconds >= 0
    assert info_b.uptime_seconds >= 0


@pytest.mark.skip(reason="TODO: design once shared-network across VMs is wired up")
def test_two_nodes_on_shared_network(topology):
    raise NotImplementedError
