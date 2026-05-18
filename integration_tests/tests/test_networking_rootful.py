"""Rootful networking scenarios driven by the privileged corvus-netd
agent on the test node.

Bridges and TAPs live in the host root netns (libvirt-style); the
daemon owns intent and the agent owns kernel state. These tests prove
that the inner daemon's request → agent → kernel pipeline works for
features that were untestable under the prior unprivileged user-ns
deployment.
"""
from __future__ import annotations

import pytest

from corvus_test_harness import SingleNodeCase


pytestmark = [pytest.mark.slow, pytest.mark.rootful]


class TestNetworkingRootful(SingleNodeCase):
    @pytest.mark.skip(reason="TODO: design rootful network test once the feature lands")
    def test_tap_interface_creation(self):
        """Inner daemon creates a TAP on a real-looking interface in the VM."""
        raise NotImplementedError
