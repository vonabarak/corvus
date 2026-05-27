"""Multi-node scenarios.

`TestTwoIndependentDaemons` covers the 'two VMs ⇒ two
independent inner daemons (each with its own CA)' case. Both
clients answer `status()` independently and validate against
their respective CA.

`TestMultiNodeDispatch` covers the multi-node feature proper —
ONE daemon driving TWO nodeagents (alpha runs the daemon + its
own agents, beta runs agents only). Both nodes are signed by
the same CA so alpha's daemon can verify beta's
`corvus-node:beta` cert. ALPHA's daemon registers BETA as a
second node, reached over the host's VDE switch by BETA's
DHCP-leased IP. A VM created on the 'self' node spawns qemu on
alpha; a VM created on 'remote-beta' spawns qemu on beta. The
test asserts each qemu lands on its expected node AND is absent
from the other — that's the property the per-node routing in
the daemon is supposed to guarantee.
"""

from __future__ import annotations

import time

import pytest
from corvus_client import ServerError
from corvus_test_harness import OneDaemonTwoNodesCase, TwoDaemonsCase


def _qemu_count(node, vm_name: str) -> int:
    """Count qemu-system processes on `node` whose argv mentions
    `vm_name`.

    Corvus launches QEMU with `-name <vmName>,process=corvus-vm-<id>`
    (see `src/Corvus/Node/Command.hs`), so the VM's name is always
    in the command line. The `^qemu-system` anchor keeps the shell
    that wraps this very pgrep call from matching its own argv —
    SSH executes the remote command via `$SHELL -c`, and that
    shell's argv contains the regex literal which would otherwise
    trip a false positive.
    """
    r = node.run(
        f"pgrep -af '^qemu-system.*-name {vm_name},' || true",
        check=False,
        timeout_sec=10.0,
    )
    out = r.stdout.decode("utf-8", errors="replace").strip()
    return sum(1 for line in out.splitlines() if line.strip())


def _poll_until(cond, *, timeout_sec: float, msg: str, poll_sec: float = 0.5) -> None:
    deadline = time.monotonic() + timeout_sec
    while time.monotonic() < deadline:
        if cond():
            return
        time.sleep(poll_sec)
    raise AssertionError(f"{msg} (waited {timeout_sec}s)")


def _retry_start(vm, *, attempts: int = 30, sleep_sec: float = 1.0) -> None:
    """`vm.start()` tolerant of 'nodeagent unavailable' for a freshly
    registered remote node.

    `nodes.create` returns as soon as the row is persisted; the
    per-node supervisor's first dial races with our follow-up
    `vm.start`. Until the dial lands the start fails with
    'nodeagent for node N unavailable'. Re-raise anything else.
    """
    last: ServerError | None = None
    for _ in range(attempts):
        try:
            vm.start(wait=False)
            return
        except ServerError as e:
            last = e
            if "unavailable" not in str(e).lower():
                raise
            time.sleep(sleep_sec)
    raise AssertionError(f"vm.start failed after {attempts} attempts: {last}")


class TestTwoIndependentDaemons(TwoDaemonsCase):
    def test_status_on_both(self):
        """Two inner daemons start, both answer status() independently."""
        info_a = self.client_alpha.status()
        info_b = self.client_beta.status()
        # Same source tree → same version reported by both.
        assert info_a.version == info_b.version
        # Independent uptimes; we just assert both are non-negative.
        assert info_a.uptime_seconds >= 0
        assert info_b.uptime_seconds >= 0


class TestMultiNodeDispatch(OneDaemonTwoNodesCase):
    """One daemon (alpha's), two nodeagents (alpha + beta).

    Both test-node VMs are up from the class fixture; we drive
    ONLY alpha's daemon, and register beta's nodeagent as a
    second node in it. A VM started on 'self' must spawn qemu
    on alpha; a VM started on 'remote-beta' must spawn qemu on
    beta.
    """

    def test_vm_lands_on_chosen_node(self):
        # The harness's deploy step already captured beta's outer
        # IP into the cert SAN; reuse that value here to register
        # beta with alpha's daemon.
        beta_ip = self.node_beta.outer_ip

        client = self.client_alpha
        # The harness's `open_client` registered the local node
        # under the test-node's short_name (so the daemon's
        # per-node supervisor's mTLS dial finds the agent's
        # `corvus-node:<short_name>` cert). For alpha that means
        # the self-node row is named "alpha".
        self_name = self.node_alpha.short_name
        # Register beta's nodeagent as a second node. The name
        # MUST match beta's nodeagent cert CN suffix
        # (`corvus-node:beta`), so we register it as plain "beta"
        # rather than the older "remote-beta" alias.
        beta_name = self.node_beta.short_name
        remote = client.nodes.create(
            beta_name,
            beta_ip,
            node_agent_port=9878,
            net_agent_port=9877,
            description="alpha→beta cross-node test",
        )
        try:
            # ── Phase 1: VM on alpha (the self-node) ──────────
            alpha_vm = client.vms.create(
                "mn-alpha-vm",
                cpu_count=1,
                ram_mb=128,
                node=self_name,
                headless=True,
                guest_agent=False,
                cloud_init=False,
            )
            try:
                _retry_start(alpha_vm)
                _poll_until(
                    lambda: _qemu_count(self.node_alpha, "mn-alpha-vm") == 1,
                    timeout_sec=15.0,
                    msg="qemu for 'mn-alpha-vm' did not spawn on alpha",
                )
                assert _qemu_count(self.node_beta, "mn-alpha-vm") == 0, (
                    "qemu for 'mn-alpha-vm' leaked onto beta"
                )
                # reset() is the fast SIGTERM/SIGKILL path — the
                # graceful stop would block 300 s on ACPI without
                # a guest OS to ack the powerdown.
                alpha_vm.reset()
                _poll_until(
                    lambda: _qemu_count(self.node_alpha, "mn-alpha-vm") == 0,
                    timeout_sec=15.0,
                    msg="qemu for 'mn-alpha-vm' did not exit after reset",
                )
            finally:
                try:
                    alpha_vm.delete()
                except Exception:
                    pass

            # ── Phase 2: VM on beta (the remote node) ─────────
            beta_vm = client.vms.create(
                "mn-beta-vm",
                cpu_count=1,
                ram_mb=128,
                node=beta_name,
                headless=True,
                guest_agent=False,
                cloud_init=False,
            )
            try:
                _retry_start(beta_vm)
                _poll_until(
                    lambda: _qemu_count(self.node_beta, "mn-beta-vm") == 1,
                    timeout_sec=15.0,
                    msg="qemu for 'mn-beta-vm' did not spawn on beta",
                )
                assert _qemu_count(self.node_alpha, "mn-beta-vm") == 0, (
                    "qemu for 'mn-beta-vm' leaked onto alpha"
                )
                beta_vm.reset()
                _poll_until(
                    lambda: _qemu_count(self.node_beta, "mn-beta-vm") == 0,
                    timeout_sec=15.0,
                    msg="qemu for 'mn-beta-vm' did not exit after reset",
                )
            finally:
                try:
                    beta_vm.delete()
                except Exception:
                    pass
        finally:
            try:
                remote.delete()
            except Exception:
                pass


def _wait_for_node_field(client, node_name: str, predicate, *, timeout_sec: float):
    """Poll ``client.nodes.get(node_name).show()`` until ``predicate(details)``
    returns True. Returns the last-fetched details on success; raises
    ``AssertionError`` with the last snapshot on timeout."""
    deadline = time.monotonic() + timeout_sec
    last = None
    while time.monotonic() < deadline:
        try:
            last = client.nodes.get(node_name).show()
        except ServerError:
            time.sleep(0.5)
            continue
        if predicate(last):
            return last
        time.sleep(0.5)
    raise AssertionError(
        f"timed out after {timeout_sec}s waiting on node {node_name!r}; "
        f"last details: admin_state={getattr(last, 'admin_state', '?')!r} "
        f"netd_connected={getattr(last, 'netd_connected', '?')!r} "
        f"last_node_agent_push_at={getattr(last, 'last_node_agent_push_at', '?')!r}"
    )


class TestNetdReachability(OneDaemonTwoNodesCase):
    """Regression for the "stale netd cap" bug.

    Before [the supervisor's liveness probe](../../src/Corvus/NodeSupervisor.hs),
    a successful netd dial parked in ``blockUntilShutdown`` and
    never noticed when the netd process died —
    ``ssAgents[node].ncNetAgent`` stayed ``Just nac`` and
    ``crv node list`` kept reporting the dead agent as ``online``.
    The fix adds a periodic call in the wait loop so socket loss
    is detected and the cap is cleared.

    The poll window is generous (120 s) because under doubly-
    nested KVM + TLS, the kernel's FIN-from-peer propagation
    through OpenSSL's read-buffer up to Cap'n Proto's call
    handler can take noticeably longer than on bare-metal —
    we're verifying the FIX, not the cadence.
    """

    @pytest.fixture(scope="class", autouse=True)
    def _register_beta(self):
        """Register beta with alpha and wait until alpha's supervisor
        successfully dials beta's netd. Teardown drops the node
        row so each test class leaves a clean daemon state."""
        client = self.client_alpha
        beta_name = self.node_beta.short_name
        beta_ip = self.node_beta.outer_ip
        # Use list-and-find rather than .get(by_name=True) — get
        # raises NodeNotFound (typed CorvusError subclass) on a
        # fresh cluster, which is the common case for this test
        # class. Matches the registration idiom in
        # test_vm_migration.py / test_multi_node_disks.py.
        existing = next(
            (n for n in client.nodes.list() if n.name == beta_name),
            None,
        )
        if existing is None:
            remote = client.nodes.create(
                beta_name,
                beta_ip,
                node_agent_port=9878,
                net_agent_port=9877,
                description="netd-reachability test",
            )
        else:
            remote = client.nodes.get(beta_name, by_name=True)
        _wait_for_node_field(
            client,
            beta_name,
            lambda d: d.netd_connected is True,
            timeout_sec=30.0,
        )
        try:
            yield
        finally:
            # Best-effort: leave beta's netd running so other
            # test classes that boot under the same harness
            # session see it healthy.
            try:
                self.node_beta.run(
                    "sudo systemctl start corvus-netd",
                    check=False,
                    timeout_sec=10.0,
                )
            except Exception:
                pass
            try:
                remote.delete()
            except Exception:
                pass

    def test_netd_stop_flips_connected_to_false(self):
        """Stopping ``corvus-netd`` on beta makes alpha's daemon
        report ``netd_connected = False`` for beta within ~120 s.

        Without the supervisor's liveness ping the daemon kept
        the stale cap forever and this assertion would never
        flip — the FIX is what's under test."""
        client = self.client_alpha
        beta_name = self.node_beta.short_name

        # Pre-condition (re-checked in the fixture too): netd is
        # currently observed as connected.
        details = client.nodes.get(beta_name).show()
        assert details.netd_connected is True, details

        # Stop netd. Combine the stop with an immediate ``pgrep``
        # so a flaky stop ("returned rc=0 but netd is still
        # alive") surfaces here rather than as a confusing
        # timeout 120 s later.
        stop_cp = self.node_beta.run(
            "sudo systemctl stop corvus-netd "
            "&& sleep 1 "
            "&& (pgrep -af corvus-netd && echo NETD_ALIVE || echo NETD_STOPPED)",
            timeout_sec=45.0,
        )
        stop_out = stop_cp.stdout.decode(errors="replace")
        assert "NETD_STOPPED" in stop_out, (
            f"systemctl stop corvus-netd did not kill the process: {stop_out!r}"
        )
        try:
            after = _wait_for_node_field(
                client,
                beta_name,
                lambda d: d.netd_connected is False,
                timeout_sec=120.0,
            )
            assert after.netd_connected is False, after
            assert after.netd_disabled is False, (
                f"netd_disabled flipped unexpectedly: {after}"
            )
        finally:
            # Restore netd so the fixture's cleanup (and other
            # tests in this class, if any) see a healthy node.
            self.node_beta.run(
                "sudo systemctl start corvus-netd",
                check=False,
                timeout_sec=15.0,
            )
            _wait_for_node_field(
                client,
                beta_name,
                lambda d: d.netd_connected is True,
                timeout_sec=60.0,
            )


class TestAdminStateStickiness(OneDaemonTwoNodesCase):
    """Regression guard for the "admin state silently reverts to
    draining" bug.

    The user-visible bug fires when the agent's git-short-hash
    differs from the daemon's; the daemon then re-fires
    ``refuseMismatchedAgent`` on every ~10 s push, undoing the
    operator's manual ``--admin-state online``. In an
    integration-test build the agent and daemon ship from the
    same source so versions always match — but the same
    stickiness property must still hold for the operator-facing
    contract. This test asserts that an explicit flip to
    ``online`` survives multiple agent pushes (and is therefore
    a regression guard against ANY future code path that
    silently reverts a manual admin-state edit).

    The precise one-shot-per-(node, agent_version) semantics are
    covered by the unit spec at
    [test/Corvus/VmStatusSinkSpec.hs](../../test/Corvus/VmStatusSinkSpec.hs)
    — the integration test cannot trigger a version mismatch
    without separately built binaries.
    """

    @pytest.fixture(scope="class", autouse=True)
    def _register_beta(self):
        client = self.client_alpha
        beta_name = self.node_beta.short_name
        beta_ip = self.node_beta.outer_ip
        existing = next(
            (n for n in client.nodes.list() if n.name == beta_name),
            None,
        )
        if existing is None:
            remote = client.nodes.create(
                beta_name,
                beta_ip,
                node_agent_port=9878,
                net_agent_port=9877,
                description="admin-state stickiness test",
            )
        else:
            remote = client.nodes.get(beta_name, by_name=True)
        _wait_for_node_field(
            client,
            beta_name,
            lambda d: d.last_node_agent_push_at is not None,
            timeout_sec=30.0,
        )
        try:
            yield
        finally:
            # Leave the node online + registered for any later
            # test that piggybacks on the same class topology.
            try:
                client.nodes.get(beta_name).edit(admin_state="online")
            except Exception:
                pass
            try:
                remote.delete()
            except Exception:
                pass

    def test_admin_state_online_is_sticky_across_pushes(self):
        """Flip beta to draining, then back to online; assert
        ``admin_state`` stays ``online`` across ~30 s of agent
        pushes (3+ push cycles at the default 10 s cadence)."""
        client = self.client_alpha
        beta_name = self.node_beta.short_name

        node = client.nodes.get(beta_name)
        node.drain()
        assert node.show().admin_state == "draining"

        node.edit(admin_state="online")
        assert node.show().admin_state == "online"

        # Wait across multiple push cycles. The agent pushes
        # NodeStats every ~10 s; 35 s covers three cycles plus
        # slack for cold start. Without Fix 1 a version mismatch
        # would silently flip back to draining here; without the
        # broader stickiness guarantee, ANY future regression
        # introducing an automatic revert would also surface.
        deadline = time.monotonic() + 35.0
        last_state = "online"
        while time.monotonic() < deadline:
            details = client.nodes.get(beta_name).show()
            last_state = details.admin_state
            assert last_state == "online", (
                f"admin_state reverted from 'online' to {last_state!r} "
                f"unexpectedly; details={details!r}"
            )
            time.sleep(2.0)
        assert last_state == "online"
