"""Guest-agent exec against the inner daemon.

Mirrors the deleted Spec for `Vm.guestExec`:
  - command exec returns exit code, stdout, stderr
  - non-zero exits propagate
  - guest-agent-not-enabled error
"""
from __future__ import annotations

import pytest

pytestmark = pytest.mark.slow


@pytest.mark.skip(reason="TODO: port guest-exec coverage; needs inner OS image with QGA")
def test_guest_exec_echo(single_client):
    raise NotImplementedError
