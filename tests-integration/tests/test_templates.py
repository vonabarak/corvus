"""Template CRUD + instantiate against the inner daemon.

Mirrors TemplateIntegrationSpec:
  - create from YAML
  - instantiate → VM
  - cloud-init config propagation
  - delete with no dependents
"""
from __future__ import annotations

import pytest

from corvus_test_harness import SingleVmCase


pytestmark = pytest.mark.slow


class TestTemplates(SingleVmCase):
    @pytest.mark.skip(reason="TODO: port TemplateIntegrationSpec")
    def test_template_create_and_instantiate(self):
        raise NotImplementedError
