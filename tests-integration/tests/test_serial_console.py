"""Serial-console replay + capture against the inner daemon.

Mirrors SerialConsoleIntegrationSpec:
  - replay on reconnect (ring-buffer history is preserved)
  - capture while disconnected
  - headless guard (refuses console on graphical VMs)
  - stopped-VM guard
  - reboot persistence (buffer survives guest reboot)

Uses the streaming `ByteSink` cap on the pycapnp client.
"""
from __future__ import annotations

import pytest

from corvus_test_harness import SingleNodeCase


pytestmark = pytest.mark.slow


class TestSerialConsole(SingleNodeCase):
    @pytest.mark.skip(reason="TODO: port SerialConsoleIntegrationSpec")
    def test_serial_console_replay(self):
        raise NotImplementedError
