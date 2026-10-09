"""Real daemon outages must not require restarting the HTTP gateway.

These tests keep the gateway process and node agent running while stopping
the daemon. They check cold startup, cache expiry, idle stream closure, and
recovery of real VM samples over the harness's mTLS TCP relay.
"""

from __future__ import annotations

import time
from collections.abc import Callable
from urllib.error import HTTPError

import pytest
from corvus_test_harness import SingleNodeCase, VmSsh, WebGateway
from websockets.exceptions import ConnectionClosed
from websockets.sync.client import connect as ws_connect


def _status(web: WebGateway, path: str) -> int:
    try:
        with web.get_response(path) as response:
            return response.status
    except HTTPError as exc:
        exc.close()
        return exc.code


def _wait(condition: Callable[[], bool], *, timeout: float = 90) -> None:
    deadline = time.monotonic() + timeout
    while time.monotonic() < deadline:
        if condition():
            return
        time.sleep(0.5)
    raise AssertionError(f"gateway did not reach expected state within {timeout}s")


@pytest.mark.slow
@pytest.mark.timeout(300)
class TestWebReconnect(SingleNodeCase):
    def _drop_client(self) -> None:
        # Harness capabilities belong to the old daemon connection too.
        if self.node._client is not None:
            self.node._client.close()
            self.node._client = None

    def test_startup_without_daemon(self) -> None:
        """HTTP listens while offline, then REST and metrics recover in place."""
        self._drop_client()
        try:
            self.node.run("sudo systemctl stop corvus.service")
            with WebGateway(self.node, wait_for_daemon=False) as web:
                pid = web.pid
                assert _status(web, "/api/status") == 502
                assert _status(web, "/metrics") == 503
                self.node.run("sudo systemctl start corvus.service")
                _wait(lambda: _status(web, "/api/status") == 200)
                _wait(lambda: _status(web, "/metrics") == 200)
                assert web.pid == pid
                _wait(lambda: "corvus_node_memory_total_bytes{" in web.get("/metrics"))
        finally:
            self.node.run("sudo systemctl start corvus.service")

    def test_outage_expiry_and_repeated_recovery(self) -> None:
        """A live VM survives an outage; stale scrapes fail and fresh data returns.

        Checking actual metric data lines catches the original failure, where
        HTTP 200 contained only HELP/TYPE declarations. The gateway PID and
        guest marker prove recovery did not rely on restarting either process.
        A second daemon restart catches reuse of stale manager capabilities.
        """
        with VmSsh(self) as vm, WebGateway(self.node) as web:
            vm_id = vm.cap.show().id
            vm.run("printf persistent > /tmp/web-reconnect-marker")
            pid = web.pid

            def has_sample() -> bool:
                if _status(web, "/metrics") != 200:
                    return False
                return any(
                    line.startswith("corvus_vm_up{") and f'vm_id="{vm_id}"' in line
                    for line in web.get("/metrics").splitlines()
                )

            _wait(has_sample)
            agent_pid = self.node.run(
                "systemctl show -p MainPID --value corvus-nodeagent.service"
            ).stdout
            try:
                with ws_connect(
                    web.ws_url(f"/api/vms/{vm_id}/stats/ws"), open_timeout=10
                ) as ws:
                    ws.recv(timeout=30)
                    self._drop_client()
                    self.node.run("sudo systemctl stop corvus.service")
                    _wait(lambda: _status(web, "/api/status") == 502, timeout=15)
                    assert _status(web, "/metrics") == 200
                    deadline = time.monotonic() + 15
                    while True:
                        try:
                            ws.recv(timeout=max(0.1, deadline - time.monotonic()))
                        except ConnectionClosed as exc:
                            assert exc.rcvd is not None and exc.rcvd.code == 1013
                            break
                        assert time.monotonic() < deadline
                assert (
                    vm.run("cat /tmp/web-reconnect-marker").stdout.strip()
                    == "persistent"
                )
                _wait(lambda: _status(web, "/metrics") == 503, timeout=75)
                assert web.pid == pid
            finally:
                self.node.run("sudo systemctl start corvus.service")
                _wait(lambda: _status(web, "/api/status") == 200)
                vm.client = self.client
                vm.cap = self.client.vms.get(vm_id)

            _wait(has_sample)
            assert (
                vm.run("cat /tmp/web-reconnect-marker").stdout.strip() == "persistent"
            )
            self._drop_client()
            try:
                self.node.run("sudo systemctl restart corvus.service")
                _wait(lambda: _status(web, "/api/status") == 200)
                with ws_connect(
                    web.ws_url(f"/api/vms/{vm_id}/stats/ws"), open_timeout=10
                ) as ws:
                    ws.recv(timeout=30)
                _wait(has_sample)
            finally:
                self.node.run("sudo systemctl start corvus.service")
                _wait(lambda: _status(web, "/api/status") == 200)
                vm.client = self.client
                vm.cap = self.client.vms.get(vm_id)
            assert web.pid == pid
            assert (
                self.node.run(
                    "systemctl show -p MainPID --value corvus-nodeagent.service"
                ).stdout
                == agent_pid
            )
