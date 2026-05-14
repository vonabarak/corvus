"""Virtiofs shared directories against the inner daemon.

Mirrors VirtiofsIntegrationSpec:
  - add shared-dir, mount inside the doubly-nested VM, read + write
  - missing-path error surfaced cleanly
  - remove + verify unmount inside the guest
"""
from __future__ import annotations

import pytest

pytestmark = pytest.mark.slow


@pytest.mark.skip(reason="TODO: port VirtiofsIntegrationSpec; requires inner OS image")
def test_shared_dir_read_write(single_client):
    raise NotImplementedError
