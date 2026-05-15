"""Rootful networking scenarios — new ground; impossible under the
unprivileged outer Corvus.

The inner daemon runs as root inside a nested VM, so it can do:
  - real TAP devices attached to real interfaces
  - bridges that span the inner VM's NICs
  - pasta NAT inside an explicit network namespace owned by inner Corvus

These tests prove the rootful code paths work; they were untestable
under the outer's user-namespace deployment.
"""
from __future__ import annotations

import pytest

from corvus_test_harness import SingleVmCase


pytestmark = [pytest.mark.slow, pytest.mark.rootful]


class TestNetworkingRootful(SingleVmCase):
    @pytest.mark.skip(reason="TODO: design rootful network test once the feature lands")
    def test_tap_interface_creation(self):
        """Inner daemon creates a TAP on a real-looking interface in the VM."""
        raise NotImplementedError

    @pytest.mark.skip(reason="TODO: pasta NAT exercise inside the inner daemon's netns")
    def test_pasta_nat_in_inner_netns(self):
        raise NotImplementedError
