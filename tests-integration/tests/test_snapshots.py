"""Snapshot operations against the inner daemon.

Mirrors SnapshotIntegrationSpec:
  - create / rollback / merge / list
  - duplicate-name rejection
  - running-VM rejection
  - format-not-supported (raw disks)
"""
from __future__ import annotations

import pytest

pytestmark = pytest.mark.slow


@pytest.mark.skip(reason="TODO: port SnapshotIntegrationSpec coverage")
def test_snapshot_create_rollback_merge(single_client):
    raise NotImplementedError
