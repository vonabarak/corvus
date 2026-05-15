"""Snapshot operations against the inner daemon.

Mirrors SnapshotIntegrationSpec:
  - create / rollback / merge / list
  - duplicate-name rejection
  - running-VM rejection
  - format-not-supported (raw disks)
"""
from __future__ import annotations

import pytest

from corvus_test_harness import SingleVmCase


pytestmark = pytest.mark.slow


class TestSnapshots(SingleVmCase):
    @pytest.mark.skip(reason="TODO: port SnapshotIntegrationSpec coverage")
    def test_snapshot_create_rollback_merge(self):
        raise NotImplementedError
