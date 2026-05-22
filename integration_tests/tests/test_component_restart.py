"""The daemon, nodeagent, and netd must survive each other's restarts.

Three system services run on a single node: ``corvus.service``
(daemon), ``corvus-nodeagent.service`` (per-host VM agent), and
``corvus-netd.service`` (privileged network agent). They talk to
each other over TCP loopback with mTLS — but the listeners come
up in essentially arbitrary order across reboots and during
operator-driven restarts. The system has to tolerate that:

* The daemon must not die when an agent it talks to is bounced.
* Functional state — the daemon's listener, ``node list``,
  ``status`` — must remain reachable while the agents flap.
* When the bounced agent comes back, the daemon must reconnect
  to it without manual intervention.

The bug this test was written to catch: bouncing the nodeagent
while the daemon is up left the daemon process in a state where
the next client connection got
``CapnpConnectFailed "... does not exist (No such file or directory)"``
— the daemon had also torn its own listener down even though
only the agent was supposed to be restarting.

Strategy:

* Standard ``SingleNodeCase`` harness: daemon + nodeagent + netd
  already deployed as system services on a single VM.
* For each unit (and a couple of compound variants), restart it
  via ``systemctl restart``, then re-open a fresh pycapnp
  ``Client`` and verify ``status()`` answers. The fresh open is
  load-bearing — the failure mode is "daemon's listening socket
  is gone", which surfaces at ``connect()`` time, not on a
  retained socket.
"""

from __future__ import annotations

import time

import pytest
from corvus_test_harness import SingleNodeCase

pytestmark = [pytest.mark.slow, pytest.mark.timeout(300)]

# Production unit names — see component_deploy.DAEMON_UNIT etc.
DAEMON_UNIT = "corvus.service"
NODE_AGENT_UNIT = "corvus-nodeagent.service"
NETD_UNIT = "corvus-netd.service"

# After a restart, give systemd a moment to bring the unit back
# before we make assertions against it. This is NOT the daemon's
# reconnect budget — the reconnect loop has its own 5s retry
# cadence, and we exercise it via repeated probes below rather
# than a single fixed sleep.
SYSTEMD_SETTLE_SEC = 1.0

# Total budget for the daemon to re-dial a bounced agent and
# return a healthy ``status()`` reply. Comfortably above the
# 5s reconnect-loop backoff plus one healthcheck cycle.
RECONNECT_BUDGET_SEC = 30.0


class TestComponentRestart(SingleNodeCase):
    """Each component restarts independently; the others survive."""

    # ----------------------------------------------------------------
    # Helpers

    def _restart(self, unit: str) -> None:
        """``systemctl restart`` the unit and wait for systemd to
        report it active again. Raises if it doesn't recover —
        the bug we hunt is downstream of this, so an unhealthy
        restart is a setup failure, not a real defect."""

        self.node.run(f"sudo systemctl restart {unit}", check=True)
        # Brief settle: ``systemctl restart`` returns once systemd
        # has re-queued the unit, not once ``ExecStart`` succeeded.
        time.sleep(SYSTEMD_SETTLE_SEC)
        deadline = time.monotonic() + 15.0
        while time.monotonic() < deadline:
            r = self.node.run(
                f"systemctl is-active {unit}",
                check=False,
            )
            if r.stdout.decode().strip() == "active":
                return
            time.sleep(0.5)
        raise AssertionError(
            f"{unit} did not return to 'active' within 15s of "
            f"'systemctl restart {unit}'"
        )

    def _main_pid(self, unit: str) -> str:
        """Return the systemd-reported MainPID for *unit* as a
        string (empty if the unit isn't running). Empty string
        is treated as "not running" by the comparison below; a
        PID change between two snapshots therefore catches both
        "the unit was bounced" and "the unit died and stayed
        down" (the latter still surfaces a different value than
        the original PID)."""

        return (
            self.node.run(f"systemctl show -p MainPID --value {unit}")
            .stdout.decode()
            .strip()
        )

    def _drop_cached_client(self) -> None:
        """Force a fresh dial on next ``self.client`` access.

        The harness lazily memoises one pycapnp ``Client`` per
        ``TestNode``. After a restart that takes the daemon's
        TCP listener down (the bug we're hunting) OR after a
        clean daemon restart (the listener comes back but the
        old TCP socket is stale), reusing the cached client is
        meaningless. Dropping it forces a fresh ``connect()``
        which is exactly where the daemon-listener-gone failure
        surfaces.
        """
        if self.node._client is not None:
            try:
                self.node._client.close()
            except Exception:
                pass
            self.node._client = None

    def _fresh_status_must_work(self, label: str) -> None:
        """Open a fresh client and call ``status()``; poll until
        the daemon's reconnect-and-redial loop has settled or
        the budget expires.

        Why polling: the daemon's per-node supervisor reconnects
        on a 5s backoff. Asserting on the first call after a
        restart can race that backoff even when the daemon is
        perfectly healthy. The bug we actually care about is
        "daemon's TCP listener went away" — that does NOT
        recover with time, so a polling assertion cleanly
        separates the two failure modes:

        * connection refused / no route → fail immediately (the
          daemon process is gone; nothing to wait for).
        * RPC error after handshake → keep polling; only fail
          once the reconnect budget elapses.
        """

        start = time.monotonic()
        last_err: Exception | None = None
        while time.monotonic() - start < RECONNECT_BUDGET_SEC:
            self._drop_cached_client()
            try:
                info = self.client.status()
                assert info.protocol_version > 0
                assert info.uptime_seconds >= 0
                return
            except (ConnectionRefusedError, FileNotFoundError) as e:
                # The daemon's listener is gone — exact bug.
                # Connection refused = listener not bound;
                # FileNotFoundError = Unix-socket path missing.
                raise AssertionError(
                    f"{label}: daemon listener gone after restart "
                    f"(daemon process likely died): {e!r}"
                ) from e
            except Exception as e:
                last_err = e
                time.sleep(0.5)
        raise AssertionError(
            f"{label}: client never recovered within "
            f"{RECONNECT_BUDGET_SEC}s. Last error: {last_err!r}"
        )

    # ----------------------------------------------------------------
    # Baseline

    def test_baseline_all_healthy(self):
        """Before any restart, everything talks. Anchors the rest."""

        self._fresh_status_must_work("baseline")

    # ----------------------------------------------------------------
    # Single-component restarts

    def test_restart_nodeagent_daemon_survives(self):
        """Bouncing nodeagent must not take down the daemon.

        This is the original bug: prior to the fix, restarting
        the nodeagent left the daemon process in a bad state and
        the next client dial got "does not exist" against the
        daemon's listening socket.

        We assert the daemon's PID didn't change — a passing
        ``status()`` call alone wouldn't distinguish "daemon
        survived" from "daemon crashed and systemd restarted it
        within the polling budget".
        """

        daemon_pid_before = self._main_pid(DAEMON_UNIT)
        self._restart(NODE_AGENT_UNIT)
        self._fresh_status_must_work("after nodeagent restart")
        daemon_pid_after = self._main_pid(DAEMON_UNIT)
        assert daemon_pid_before == daemon_pid_after, (
            f"daemon process was bounced as a side effect of nodeagent "
            f"restart (PID {daemon_pid_before} → {daemon_pid_after})"
        )

    def test_restart_netd_daemon_survives(self):
        """Bouncing netd must not take down the daemon. Same
        symmetry as the nodeagent case; the daemon's per-node
        supervisor holds independent dials to each agent. PID
        check has the same rationale as the nodeagent case."""

        daemon_pid_before = self._main_pid(DAEMON_UNIT)
        self._restart(NETD_UNIT)
        self._fresh_status_must_work("after netd restart")
        daemon_pid_after = self._main_pid(DAEMON_UNIT)
        assert daemon_pid_before == daemon_pid_after, (
            f"daemon process was bounced as a side effect of netd "
            f"restart (PID {daemon_pid_before} → {daemon_pid_after})"
        )

    def test_restart_daemon_agents_survive(self):
        """Bouncing the daemon: the agents must remain up, and the
        daemon must come back and re-dial them.

        Verifies the reverse direction: the agents don't depend
        on a live daemon (they're stateless listeners), so a
        daemon bounce is purely a daemon-side recovery test.
        """

        # Snapshot agent PIDs before restart so we can assert they
        # weren't bounced by a daemon-side BindsTo / cascade.
        pid_before_node = self._main_pid(NODE_AGENT_UNIT)
        pid_before_netd = self._main_pid(NETD_UNIT)

        self._restart(DAEMON_UNIT)

        pid_after_node = self._main_pid(NODE_AGENT_UNIT)
        pid_after_netd = self._main_pid(NETD_UNIT)

        assert pid_before_node == pid_after_node, (
            f"nodeagent was restarted as a side effect of daemon "
            f"restart (PID changed from {pid_before_node} to "
            f"{pid_after_node})"
        )
        assert pid_before_netd == pid_after_netd, (
            f"netd was restarted as a side effect of daemon "
            f"restart (PID changed from {pid_before_netd} to "
            f"{pid_after_netd})"
        )

        self._fresh_status_must_work("after daemon restart")

    # ----------------------------------------------------------------
    # Compound restarts — repeated bounces, alternating order.

    def test_alternating_restarts(self):
        """A handful of bounces in alternating order. Catches
        accumulating leaks (descriptors, threads, stale
        supervisor registrations) that a single restart wouldn't
        expose.

        The daemon PID is captured before the agent flapping
        starts and re-checked between rounds — any agent restart
        that silently crashes the daemon would fail here, even
        when the polling ``status()`` masks it by hitting the
        daemon's systemd auto-restart window.
        """

        daemon_pid = self._main_pid(DAEMON_UNIT)
        for round_index in range(3):
            self._restart(NODE_AGENT_UNIT)
            self._fresh_status_must_work(f"round {round_index} after nodeagent restart")
            assert self._main_pid(DAEMON_UNIT) == daemon_pid, (
                f"round {round_index}: daemon PID changed after "
                f"nodeagent restart (expected {daemon_pid})"
            )
            self._restart(NETD_UNIT)
            self._fresh_status_must_work(f"round {round_index} after netd restart")
            assert self._main_pid(DAEMON_UNIT) == daemon_pid, (
                f"round {round_index}: daemon PID changed after "
                f"netd restart (expected {daemon_pid})"
            )

        self._restart(DAEMON_UNIT)
        self._fresh_status_must_work("after final daemon restart")
