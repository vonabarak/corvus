"""Cloud-init scenarios against the inner daemon.

Mirrors CloudInitIntegrationSpec:
  - SSH-key deployment across Alpine / Ubuntu / Debian / Gentoo /
    AlmaLinux base images (each requires its own template inside
    the inner Corvus — these are heavy tests).
  - Custom user-data injection
  - CRUD on the cloud-init config table
  - network-config propagation

Importing OS base images into the inner daemon takes minutes; these
tests are gated behind `@pytest.mark.slow` and should be opted into
explicitly (`pytest -m slow`).
"""
from __future__ import annotations

import pytest

from corvus_test_harness import SingleNodeCase


pytestmark = pytest.mark.slow


class TestCloudInit(SingleNodeCase):
    @pytest.mark.skip(reason="TODO: port CloudInitIntegrationSpec; requires node-side OS images")
    def test_cloud_init_ssh_key_deployment_alpine(self):
        raise NotImplementedError
