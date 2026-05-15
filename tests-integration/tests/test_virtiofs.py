"""Virtiofs shared directories against the inner daemon.

Mirrors VirtiofsIntegrationSpec:
  - add shared-dir, mount inside the doubly-nested VM, read + write
  - missing-path error surfaced cleanly
  - remove + verify unmount inside the guest
"""
from __future__ import annotations

import pytest

from corvus_test_harness import SingleVmCase


pytestmark = pytest.mark.slow


class TestVirtiofs(SingleVmCase):
    @pytest.mark.skip(reason="TODO: port VirtiofsIntegrationSpec; requires inner OS image")
    def test_shared_dir_read_write(self):
        raise NotImplementedError
