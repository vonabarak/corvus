"""A dropped nodeagent status sink must recover without restarting services."""

from __future__ import annotations

import shlex
import time
from collections.abc import Callable

import pytest
from corvus_test_harness import SingleNodeCase, SqliteDatabase, Vm


@pytest.mark.timeout(600)
class TestStatusSubscriptionRecovery(SingleNodeCase):
    def _wait(
        self, predicate: Callable[[], bool], message: str, timeout: float
    ) -> None:
        deadline = time.monotonic() + timeout
        while time.monotonic() < deadline:
            if predicate():
                return
            time.sleep(0.5)
        raise AssertionError(message)

    def _service_identity(self, service: str) -> str:
        return (
            self.node.run(f"systemctl show {service} -p MainPID -p InvocationID")
            .stdout.decode()
            .strip()
        )

    def test_status_recovers_after_ack_timeout(self) -> None:
        with Vm(self) as vm, SqliteDatabase(self.node) as database:
            daemon_identity = self._service_identity("corvus.service")
            agent_identity = self._service_identity("corvus-nodeagent.service")
            daemon_pid = int(
                self.node.run("systemctl show corvus.service -p MainPID --value")
                .stdout.decode()
                .strip()
            )
            assert daemon_pid > 1

            def healthcheck() -> str | None:
                value = database.query(
                    "SELECT node_agent_healthcheck FROM node WHERE name = ?",
                    (self.node.short_name,),
                )[0][0]
                assert value is None or isinstance(value, str)
                return value

            baseline = healthcheck()
            self._wait(
                lambda: healthcheck() is not None and healthcheck() != baseline,
                "Node healthchecks did not advance before fault injection",
                30,
            )
            baseline = healthcheck()
            cursor_output = self.node.run(
                "sudo journalctl -u corvus-nodeagent.service -n 0 --show-cursor --no-pager"
            ).stdout.decode()
            cursor = cursor_output.split("-- cursor: ", 1)[1].strip()
            journal = (
                "sudo journalctl -u corvus-nodeagent.service --no-pager "
                f"--after-cursor={shlex.quote(cursor)}"
            )
            try:
                self.node.run(f"sudo kill -STOP {daemon_pid}")
                self._wait(
                    lambda: (
                        "dropping VmStatusSink: no ACK within 5s"
                        in self.node.run(journal).stdout.decode()
                    ),
                    "Nodeagent did not drop the unacknowledged status sink",
                    30,
                )
            finally:
                self.node.run(f"sudo kill -CONT {daemon_pid}")

            # The timed-out call may finish after CONT. A single new healthcheck
            # is insufficient: require the supervisor to recreate the subscription.
            self._wait(
                lambda: (
                    "subscribed to VM status push"
                    in self.node.run(
                        "sudo journalctl -u corvus.service --no-pager "
                        f"--after-cursor={shlex.quote(cursor)}"
                    ).stdout.decode()
                ),
                "Daemon did not restore its status subscription",
                90,
            )
            self._wait(
                lambda: (
                    healthcheck() is not None
                    and healthcheck() != baseline
                    and vm.cap.show().status == "running"
                ),
                "Status snapshots did not resume after the ACK timeout",
                30,
            )
            for _ in range(2):
                vm.cap.reset()
                vm.cap.start(wait=False)
                self._wait(
                    lambda: vm.cap.show().status == "running",
                    "Guest booted but did not reach running after subscription recovery",
                    90,
                )
                rows = database.query(
                    "SELECT healthcheck FROM vm WHERE id = ?", (vm.cap.show().id,)
                )
                assert rows[0][0] is not None

            assert self._service_identity("corvus.service") == daemon_identity
            assert self._service_identity("corvus-nodeagent.service") == agent_identity
