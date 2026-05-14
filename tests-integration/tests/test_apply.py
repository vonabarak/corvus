"""End-to-end YAML apply against the inner daemon.

Mirrors ApplyIntegrationSpec:
  - declarative disks + networks + VMs + ssh keys
  - skipExisting idempotency
  - typed errors when referenced entities are missing

The fixture passes the YAML to the INNER daemon (via the pycapnp
`apply` cap method), not via `crv` against the outer.
"""
from __future__ import annotations

import pytest

pytestmark = pytest.mark.slow


@pytest.mark.skip(reason="TODO: port ApplyIntegrationSpec")
def test_apply_creates_disks_and_networks(single_client):
    raise NotImplementedError
