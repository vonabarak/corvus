"""Build pipeline against the inner daemon.

Mirrors BuildIntegrationSpec:
  - bake-VM lifecycle (provisioner ordering, artifact registration)
  - cleanup on failure / cleanup: onSuccess
  - streaming BuildEvents reach the caller

The inner daemon needs a doubly-nested KVM to run a real bake VM —
slow but the only honest reproduction. Marked `slow`; the `runcmd`
preamble of the inner template should be the cheapest possible
provisioner.
"""
from __future__ import annotations

import pytest

pytestmark = pytest.mark.slow


@pytest.mark.skip(reason="TODO: port BuildIntegrationSpec; requires doubly-nested KVM")
def test_build_minimal_pipeline_streams_events(single_client):
    raise NotImplementedError
