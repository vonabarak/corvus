"""Multi-node scenarios.

`TestTwoIndependentDaemons` covers the boring 'two VMs ⇒ two
independent inner daemons' case the `TwoNodesCase` fixture
already gives us: both daemons answer `status()` independently.

`TestMultiNodeDispatch` covers the multi-node feature proper —
ONE daemon driving TWO nodeagents. Both test-node VMs come up
the normal way (`TwoNodesCase`); we then have ALPHA's daemon
register BETA's nodeagent as a second node ('remote-beta',
reached over the host's VDE switch by BETA's DHCP-leased IP).
A VM created on the 'self' node spawns qemu on alpha; a VM
created on 'remote-beta' spawns qemu on beta. The test asserts
each qemu lands on its expected node AND is absent from the
other — that's the property the per-node routing in the daemon
is supposed to guarantee.
"""
from __future__ import annotations

import time

import pytest

from corvus_client import ServerError
from corvus_test_harness import TwoNodesCase


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


def _node_outer_ip(node) -> str:
    """Return `node`'s DHCP-leased IP on the host's VDE switch.

    Each test-node has two IPv4 global-scope addresses inside the
    guest: an outer NIC reached via the host's VDE switch and a
    virtual `vde0` on 192.168.92.1/24 that the test-node uses for
    its OWN nested children. We want the outer one — that's how
    alpha's daemon can reach beta's nodeagent.
    """
    r = node.run("ip -4 -o addr show scope global")
    out = r.stdout.decode("utf-8", errors="replace")
    for line in out.splitlines():
        # Each line looks like:
        #   "2: enp0s4    inet 192.168.89.197/22 brd … scope global …"
        parts = line.split()
        if len(parts) < 4:
            continue
        ip = parts[3].split("/", 1)[0]
        if ip.startswith("127.") or ip.startswith("192.168.92."):
            continue
        return ip
    raise RuntimeError(
        f"could not find an outer VDE IP on node {node.short_name!r}; "
        f"`ip -4 -o addr show` returned:\n{out}"
    )


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


class TestTwoIndependentDaemons(TwoNodesCase):
    def test_status_on_both(self):
        """Two inner daemons start, both answer status() independently."""
        info_a = self.client_alpha.status()
        info_b = self.client_beta.status()
        # Same source tree → same version reported by both.
        assert info_a.version == info_b.version
        # Independent uptimes; we just assert both are non-negative.
        assert info_a.uptime_seconds >= 0
        assert info_b.uptime_seconds >= 0


class TestMultiNodeDispatch(TwoNodesCase):
    """One daemon (alpha's), two nodeagents (alpha + beta).

    Both test-node VMs are up from the class fixture; we drive
    ONLY alpha's daemon, and register beta's nodeagent as a
    second node in it. A VM started on 'self' must spawn qemu
    on alpha; a VM started on 'remote-beta' must spawn qemu on
    beta.
    """

    def test_vm_lands_on_chosen_node(self):
        beta_ip = _node_outer_ip(self.node_beta)

        client = self.client_alpha
        # The 'self' node (→ 127.0.0.1) is already registered by
        # the harness's open_client(). Register beta's nodeagent
        # as a second node.
        remote = client.nodes.create(
            "remote-beta",
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
                node="self",
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
                assert (
                    _qemu_count(self.node_beta, "mn-alpha-vm") == 0
                ), "qemu for 'mn-alpha-vm' leaked onto beta"
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
                node="remote-beta",
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
                assert (
                    _qemu_count(self.node_alpha, "mn-beta-vm") == 0
                ), "qemu for 'mn-beta-vm' leaked onto alpha"
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
class TestTwoNodesOnSharedNetwork(TwoNodesCase):
    def test_shared_network(self):
        raise NotImplementedError
