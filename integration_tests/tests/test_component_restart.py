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
* The public daemon ``shutdown()`` RPC must stop only the daemon;
  live agent-owned VM and network state must reconcile when an
  operator starts it again.

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

This focused coverage does not exercise Ctrl+Alt+Del behavior, web/CLI route
breadth, or task-stream terminal semantics.
"""

from __future__ import annotations

import secrets
import time

import pytest
from corvus_test_harness import SingleNodeCase, Vm, VmSsh

pytestmark = pytest.mark.timeout(300)

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

    def _start(self, unit: str) -> None:
        """Start a deliberately stopped unit and wait for systemd to report
        it active. Kept separate from ``_restart`` because the graceful
        shutdown test must prove the unit was actually inactive first."""

        self.node.run(f"sudo systemctl start {unit}", check=True)
        deadline = time.monotonic() + 15.0
        while time.monotonic() < deadline:
            result = self.node.run(f"systemctl is-active {unit}", check=False)
            if result.stdout.decode().strip() == "active":
                return
            time.sleep(0.5)
        raise AssertionError(f"{unit} did not become active within 15s of start")

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

    def test_restart_nodeagent_recovers_running_vm(self):
        """A running VM is re-started after nodeagent loses its QEMU state.

        Nodeagent cleanup deliberately terminates its QEMU children on a
        service restart.  The daemon must therefore notice the reconnected
        agent reports the VM as unknown, reapply its desired state, and leave
        a non-ephemeral attached data disk intact.
        """

        marker = f"corvus-nodeagent-restart-{secrets.token_hex(6)}"
        data_disk = f"{marker}-data"
        self.client.disks.create(data_disk, size_mb=8, format="raw")
        try:
            with Vm(self) as vm:
                vm_id = vm.cap.show().id
                vm.cap.attach_disk(data_disk, interface="virtio")
                written = vm.cap.guest_exec(
                    f"/bin/sh -c 'until test -b /dev/vdb; do sleep 1; done; "
                    f"printf %s {marker} | dd of=/dev/vdb bs=1 conv=notrunc "
                    "status=none; sync'"
                )
                assert written.exit_code == 0, written

                self._restart(NODE_AGENT_UNIT)
                self._fresh_status_must_work("after nodeagent restart with running VM")

                # _fresh_status_must_work replaces the harness's cached Client;
                # refresh the context manager's capability before using it again
                # (including its eventual cleanup).
                vm.client = self.client
                vm.cap = self.client.vms.get(vm_id)

                deadline = time.monotonic() + RECONNECT_BUDGET_SEC
                last_result = None
                while time.monotonic() < deadline:
                    info = vm.cap.show()
                    if info.status == "running":
                        last_result = vm.cap.guest_exec(
                            f"dd if=/dev/vdb bs=1 count={len(marker)} 2>/dev/null"
                        )
                        if last_result.exit_code == 0 and last_result.stdout == marker:
                            break
                    time.sleep(0.5)
                else:
                    raise AssertionError(
                        "VM did not recover after nodeagent restart; "
                        f"last guest command result: {last_result!r}"
                    )
        finally:
            try:
                self.client.disks.get(data_disk, by_name=True).delete()
            except Exception:
                pass

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

    def test_shutdown_rpc_preserves_agent_owned_running_state(self):
        """A public graceful shutdown stops only the daemon and restores its
        view of a live managed VM/network after an explicit start.

        The marker is read over the existing direct guest SSH transport while
        the daemon is down. That distinguishes daemon reconciliation from a
        VM that merely happened to be restarted during recovery.
        """
        network_name = f"shutdown-net-{secrets.token_hex(3)}"
        marker = f"corvus-shutdown-{secrets.token_hex(6)}"
        network = self.client.networks.create(
            network_name,
            subnet="10.98.0.0/24",
            dhcp=True,
            nat=False,
        )
        try:
            network.start()

            class _ManagedVm(VmSsh):
                def _net_ifs(self):
                    return [{"type": "managed", "network_ref": network_name}]

            with _ManagedVm(self, name=f"shutdown-vm-{secrets.token_hex(3)}") as vm:
                vm_id = vm.cap.show().id
                allocated_ip = vm.cap.list_net_ifs()[0].ip_address
                assert allocated_ip is not None
                vm.run(f"printf %s {marker} > /tmp/corvus-shutdown-marker")

                nodeagent_pid = self._main_pid(NODE_AGENT_UNIT)
                netd_pid = self._main_pid(NETD_UNIT)
                self.client.shutdown()
                self._drop_cached_client()

                # The RPC acknowledgement is sent before the daemon's final
                # listener teardown reaches systemd. Poll rather than racing
                # that hand-off; an on-failure restart still leaves the unit
                # active and fails this assertion within the bounded budget.
                deadline = time.monotonic() + 15.0
                daemon_state = ""
                while time.monotonic() < deadline:
                    daemon_state = (
                        self.node.run(f"systemctl is-active {DAEMON_UNIT}", check=False)
                        .stdout.decode()
                        .strip()
                    )
                    if daemon_state == "inactive":
                        break
                    time.sleep(0.5)
                else:
                    raise AssertionError(
                        f"shutdown RPC left {DAEMON_UNIT} {daemon_state!r}"
                    )
                assert self._main_pid(NODE_AGENT_UNIT) == nodeagent_pid
                assert self._main_pid(NETD_UNIT) == netd_pid
                marker_result = vm.run("cat /tmp/corvus-shutdown-marker")
                assert marker_result.stdout.strip() == marker

                self._start(DAEMON_UNIT)
                self._fresh_status_must_work("after public daemon shutdown")

                # The old capability belongs to the pre-shutdown client. Keep
                # the context manager's cleanup path on the fresh connection.
                vm.client = self.client
                vm.cap = self.client.vms.get(vm_id)
                deadline = time.monotonic() + RECONNECT_BUDGET_SEC
                while time.monotonic() < deadline:
                    info = vm.cap.show()
                    if info.status == "running":
                        break
                    time.sleep(0.5)
                else:
                    raise AssertionError(
                        "VM was not reconciled as running after daemon start"
                    )

                assert (
                    vm.run("cat /tmp/corvus-shutdown-marker").stdout.strip() == marker
                )
                nics = vm.cap.list_net_ifs()
                assert len(nics) == 1
                assert nics[0].ip_address == allocated_ip
                assert self.client.networks.get(network_name).show().running
        finally:
            try:
                fresh_network = self.client.networks.get(network_name)
                fresh_network.stop(force=True)
                fresh_network.delete()
            except Exception:
                pass

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
