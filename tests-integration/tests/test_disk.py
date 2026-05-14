"""Disk subsystem against the inner daemon.

Mirrors the deleted DiskIntegrationSpec scope:
  - create / register / clone
  - overlay (createOverlay) + rebase
  - import (local path) + import-url
  - hot attach/detach (rw + ro variants)
  - refresh after out-of-band qemu-img write
  - resize

Tests run against the inner daemon (root inside the test VM), so disk
files live under the VM's `/var/lib/corvus` (or the inner equivalent),
not the host. Each test cleans up the inner-side disks it creates.
"""
from __future__ import annotations

import pytest

pytestmark = pytest.mark.slow


@pytest.mark.skip(reason="TODO: port DiskIntegrationSpec coverage from doc/integration-tests-pre-capnp.md")
def test_disk_create_and_delete(single_client):
    raise NotImplementedError
