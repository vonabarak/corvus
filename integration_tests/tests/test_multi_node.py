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

pytestmark = pytest.mark.slow


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


@pytest.mark.skip(reason="TODO: design once shared-network across VMs is wired up")
class TestTwoNodesOnSharedNetwork(OneDaemonTwoNodesCase):
    def test_shared_network(self):
        raise NotImplementedError
