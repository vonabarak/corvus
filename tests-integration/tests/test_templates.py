"""Template CRUD + instantiate against the inner daemon.

Mirrors TemplateIntegrationSpec:
  - create from YAML
  - instantiate → VM
  - cloud-init config propagation
  - delete with no dependents
"""
from __future__ import annotations

import pytest

pytestmark = pytest.mark.slow


@pytest.mark.skip(reason="TODO: port TemplateIntegrationSpec")
def test_template_create_and_instantiate(single_client):
    raise NotImplementedError
