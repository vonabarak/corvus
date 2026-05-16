"""Multi-node scenarios — two inner Corvus daemons cooperating or
contending over shared state. Impossible under the single-process
outer deployment.

The `TwoNodesCase` base brings up both VMs class-scoped; each method
shares the same pair. The two inner daemons see independent databases
(Postgres lives inside each VM) but can be wired to a shared network
for cross-node connectivity tests.
"""
from __future__ import annotations

import pytest

from corvus_test_harness import TwoNodesCase


pytestmark = pytest.mark.slow


class TestTwoIndependentDaemons(TwoNodesCase):
    def test_status_on_both(self):
        """Two inner daemons start, both answer status() independently."""
        info_a = self.client_alpha.status()
        info_b = self.client_beta.status()
        # Same source tree → same version reported by both.
        assert info_a.version == info_b.version
        # Independent uptimes; we just assert both are non-negative.
        assert info_a.uptime_seconds >= 0
        assert info_b.uptime_seconds >= 0


@pytest.mark.skip(reason="TODO: design once shared-network across VMs is wired up")
class TestTwoNodesOnSharedNetwork(TwoNodesCase):
    def test_shared_network(self):
        raise NotImplementedError
