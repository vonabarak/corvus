"""End-to-end integration tests for managed virtual networks.

Drives the full daemon → corvus-netd → kernel pipeline:

  * `client.networks.create(...)` / `.start()` ask the inner daemon
    to apply a NetworkSpec via the steady-state corvus-netd agent.
  * `vm.add_net_if(type="managed", network_ref=...)` asks the
    daemon to apply a TapSpec when the VM starts, so QEMU opens
    the named TAP and joins the bridge.
  * Assertions exercise both kernel-side state (visible from the
    node) and in-guest reachability (via VSOCK SSH).

Each test class gets its own dedicated node, so its corvus-netd
instance is private to the class.
"""
from __future__ import annotations

import contextlib
import re
import time
from typing import Callable, Iterator

import pytest

from corvus_test_harness import SingleNodeCase, VmSsh


pytestmark = [pytest.mark.slow]


# ---------------------------------------------------------------------------
# Helpers


def _wait_for_vm_iface_ip(
    vm,
    *,
    network_id: int,
    timeout_sec: float = 30.0,
    poll_sec: float = 2.0,
) -> str:
    """Wait until the QGA poller reports a guest IP on the managed NIC.

    Returns the bare IPv4 address (no CIDR suffix).
    """
    deadline = time.monotonic() + timeout_sec
    while time.monotonic() < deadline:
        for nif in vm.cap.list_net_ifs():
            if nif.network_id != network_id:
                continue
            ips = nif.guest_ip_addresses
            if not ips:
                continue
            # The field is a comma-separated list of "ip/prefix"; just
            # take the first IPv4 entry.
            for item in ips.split(","):
                item = item.strip()
                if not item or ":" in item:  # skip IPv6
                    continue
                if "/" in item:
                    return item.split("/", 1)[0]
                return item
        time.sleep(poll_sec)
    raise AssertionError(
        f"guest_ip_addresses never appeared on the managed NIC "
        f"(network_id={network_id}) within {timeout_sec:.0f}s"
    )


def _bridge_name(network_id: int) -> str:
    """Render the agent's bridge ifname for a given DB network id.

    Mirrors `Corvus.NetAgentClient.Spec.corvusBridgeName` (base-36).
    """
    if network_id < 0:
        raise ValueError(f"negative network id: {network_id}")
    if network_id == 0:
        return "corvus-br-0"
    chars = "0123456789abcdefghijklmnopqrstuvwxyz"
    digits: list[str] = []
    n = network_id
    while n:
        digits.append(chars[n % 36])
        n //= 36
    return "corvus-br-" + "".join(reversed(digits))


def _bridge_ifindex(node, bridge: str) -> int:
    r = node.run(f"cat /sys/class/net/{bridge}/ifindex", check=False)
    if r.returncode != 0:
        raise AssertionError(
            f"bridge {bridge} not present: "
            f"{r.stderr.decode(errors='replace')}"
        )
    return int(r.stdout.strip())


@contextlib.contextmanager
def _network(
    client_fn: Callable[[], "Client"],
    name: str,
    subnet: str,
    *,
    dhcp: bool = False,
    nat: bool = False,
):
    """Create, start, and (on exit) stop + delete a network.

    Takes a *callable* returning a Client rather than a Client itself
    so a test body that restarts the daemon mid-flight (and thus
    invalidates whatever Client we entered with) still gets a fresh
    Client for the stop+delete cleanup. Without this re-resolution,
    cleanup tries to use a stale pycapnp cap whose runloop has been
    closed, which leaks "coroutine was never awaited" warnings.
    """
    nw = client_fn().networks.create(name, subnet, dhcp=dhcp, nat=nat)
    try:
        nw.start()
        yield nw
    finally:
        # Re-resolve the network by name on a fresh client; the
        # original `nw` cap may be stale (daemon may have been
        # restarted). If the network was already torn down by the
        # body, `.get` raises and we skip the rest.
        try:
            fresh = client_fn().networks.get(name)
        except Exception:
            fresh = None
        if fresh is not None:
            try:
                fresh.stop(force=True)
            except Exception:
                pass
            try:
                fresh.delete()
            except Exception:
                pass


class _ManagedVm(VmSsh):
    """Alpine VM with a single managed NIC. SSH via VSOCK (no user NIC).

    Subclasses set `network_name` (the network the managed NIC joins).
    """

    network_name: str = ""

    def _net_ifs(self):
        if not self.network_name:
            raise RuntimeError("_ManagedVm.network_name must be set")
        return [{"type": "managed", "network_ref": self.network_name}]


# Inside the alpine VM, the managed NIC is eth0 (it's the only NIC).
GUEST_NIC = "eth0"

# A reasonable external target to exercise NAT. Cloudflare's 1.1.1.1 is
# globally pingable and rarely filters ICMP.
EXTERNAL_TARGET = "1.1.1.1"


def _node_has_outbound_internet(node) -> bool:
    """Probe whether the test-node itself can reach 'EXTERNAL_TARGET'.

    The NAT-chain tests rely on the test-node forwarding VM traffic
    out through its own default route to the host's internet. If the
    test-node has no outbound (e.g. the host's VDE switch isn't
    NAT'd to a real uplink), every 'ping {EXTERNAL_TARGET}' from
    inside a managed-VM is going to time out — *not* because the
    daemon's per-network NAT is broken but because there's nowhere
    for the masqueraded packets to go.

    Cached per-node so we only pay the 2 s ping once per class.
    Returns 'True' on a successful ping, 'False' otherwise (the
    test should be skipped).
    """
    cache_key = "_corvus_outbound_ok"
    cached = getattr(node, cache_key, None)
    if cached is not None:
        return cached
    # 'TestNode.run' returns 'subprocess.CompletedProcess' (whose
    # exit status is 'returncode'), not the inner VM's 'SshResult'
    # ('exit_code'). Don't confuse the two.
    r = node.run(
        f"ping -c 1 -W 2 {EXTERNAL_TARGET}",
        check=False,
    )
    ok = r.returncode == 0
    setattr(node, cache_key, ok)
    return ok


# ---------------------------------------------------------------------------
# Tests


class TestManagedNetworking(SingleNodeCase):
    """End-to-end managed-network tests against the inner daemon
    + corvus-netd.service running on the same node."""

    NODES = ("net",)

    # -- 1. Isolated network, manual IP --------------------------------------

    def test_isolated_network_manual_ip(self):
        """Network without DHCP/NAT: VM has no auto IP; manual config
        gives reachability with the node and isolates from the outside."""
        node = self.node
        client = self.client

        with _network(lambda: self.client, "iso-net", "10.50.0.0/24") as nw:
            bridge = _bridge_name(nw.show().id)
            # The agent set the bridge IP to the network's gateway.
            addr = node.run(f"ip -o -4 addr show dev {bridge}")
            assert b"10.50.0.1/24" in addr.stdout

            class _Vm(_ManagedVm):
                network_name = "iso-net"

            with _Vm(self) as vm:
                # No DHCP → no IPv4 on the managed NIC.
                r = vm.run(f"ip -4 -o addr show dev {GUEST_NIC}")
                # `ip -o` returns one line per address; with no v4
                # addresses configured, output is empty.
                assert r.stdout.strip() == "", (
                    f"managed NIC has unexpected IPv4: {r.stdout!r}"
                )

                # Bring the link up and assign a static IP.
                vm.run(
                    f"doas ip link set {GUEST_NIC} up && "
                    f"doas ipaddr add 10.50.0.10/24 dev {GUEST_NIC}"
                )

                # VM → bridge (the node's host netns).
                vm.run(f"ping -c 2 -W 2 -I {GUEST_NIC} 10.50.0.1")

                # Node → VM.
                node.run("ping -c 2 -W 2 10.50.0.10")

                # No NAT, no default route on the managed NIC → cannot
                # reach the outside world via this NIC.
                r = vm.run(
                    f"ping -c 1 -W 2 -I {GUEST_NIC} {EXTERNAL_TARGET}",
                    check=False,
                )
                assert r.exit_code != 0, (
                    f"unexpected internet reachability without NAT: {r}"
                )

    # -- 2. DHCP + NAT, two VMs ----------------------------------------------

    def test_dhcp_nat_inter_vm(self):
        """Network with DHCP+NAT: two VMs both get IPs, can ping each
        other, the bridge, and the outside world."""
        node = self.node
        client = self.client

        if not _node_has_outbound_internet(node):
            pytest.skip(
                f"test-node {node.name!r} can't reach {EXTERNAL_TARGET} on "
                "its own — the NAT-chain test requires the host's VDE "
                "switch to be NAT'd to a real uplink, otherwise the "
                "masqueraded VM packets have nowhere to go. Skipping."
            )

        with _network(lambda: self.client, "dhcp-net", "10.51.0.0/24", dhcp=True, nat=True) as nw:
            net_id = nw.show().id
            bridge = _bridge_name(net_id)
            # Sanity: bridge + masquerade rule are live.
            assert b"10.51.0.1/24" in node.run(
                f"ip -o -4 addr show dev {bridge}"
            ).stdout
            nft = node.run("sudo nft list table inet corvus_fw")
            assert b"masquerade" in nft.stdout

            class _Vm(_ManagedVm):
                network_name = "dhcp-net"

            with _Vm(self, name="dhcp-vm1") as vm1, \
                 _Vm(self, name="dhcp-vm2") as vm2:
                # Alpine doesn't auto-DHCP secondary NICs (and our
                # managed NIC is the only NIC). The harness's baked
                # alpine image leaves it down. Bring it up + dhcp.
                for vm in (vm1, vm2):
                    vm.run(
                        f"doas ip link set {GUEST_NIC} up && "
                        f"doas udhcpc -i {GUEST_NIC} -n -q -t 5 -T 2"
                    )

                ip1 = _wait_for_vm_iface_ip(vm1, network_id=net_id)
                ip2 = _wait_for_vm_iface_ip(vm2, network_id=net_id)
                assert ip1.startswith("10.51.0."), ip1
                assert ip2.startswith("10.51.0."), ip2
                assert ip1 != ip2

                # vm1 ↔ vm2
                vm1.run(f"ping -c 2 -W 2 {ip2}")
                vm2.run(f"ping -c 2 -W 2 {ip1}")

                # Each VM → bridge (its gateway).
                vm1.run("ping -c 2 -W 2 10.51.0.1")
                vm2.run("ping -c 2 -W 2 10.51.0.1")

                # Each VM → outside world via NAT.
                vm1.run(f"ping -c 2 -W 4 {EXTERNAL_TARGET}")
                vm2.run(f"ping -c 2 -W 4 {EXTERNAL_TARGET}")

    # -- 3. Cross-network isolation ------------------------------------------

    def test_cross_network_isolation(self):
        """Two distinct networks: VM on net-a can't reach VM on net-b
        (agent's `iifname "corvus-br*" oifname "corvus-br*" drop`)."""
        client = self.client

        with _network(lambda: self.client, "iso-a", "10.60.0.0/24") as a, \
             _network(lambda: self.client, "iso-b", "10.61.0.0/24") as b:

            class _VmA(_ManagedVm):
                network_name = "iso-a"

            class _VmB(_ManagedVm):
                network_name = "iso-b"

            with _VmA(self, name="iso-vm-a") as vm_a, \
                 _VmB(self, name="iso-vm-b") as vm_b:
                vm_a.run(
                    f"doas ip link set {GUEST_NIC} up && "
                    f"doas ipaddr add 10.60.0.10/24 dev {GUEST_NIC}"
                )
                vm_b.run(
                    f"doas ip link set {GUEST_NIC} up && "
                    f"doas ipaddr add 10.61.0.10/24 dev {GUEST_NIC}"
                )
                # Each VM can ping its own gateway…
                vm_a.run("ping -c 1 -W 2 10.60.0.1")
                vm_b.run("ping -c 1 -W 2 10.61.0.1")
                # …but cannot reach the other network's VM. The
                # routing-wise reason is "no default route"; the
                # firewall-wise reason (visible to anyone reading
                # corvus_fw) is the cross-bridge drop rule.
                r = vm_a.run(
                    "ping -c 1 -W 2 10.61.0.10", check=False
                )
                assert r.exit_code != 0
                r = vm_b.run(
                    "ping -c 1 -W 2 10.60.0.10", check=False
                )
                assert r.exit_code != 0

    # -- 4. Daemon re-apply on reconnect -------------------------------------

    def test_daemon_reapply_on_reconnect(self):
        """Restart the daemon. The agent owns kernel state, so the
        bridge survives. After reconnect, the daemon walks the DB and
        re-applies idempotently — the bridge's ifindex is unchanged,
        no new bridge is created, and the daemon's view of the
        network's `running` flag stays True.

        (Note: GracefulShutdown stops running VMs as part of daemon
        shutdown — VM-side connectivity recovery would require QEMU
        TAP-reopen support, which is out of scope.)
        """
        node = self.node
        client = self.client

        with _network(lambda: self.client, "rea-net", "10.52.0.0/24", dhcp=True, nat=True) as nw:
            net_id = nw.show().id
            bridge = _bridge_name(net_id)
            ifindex_before = _bridge_ifindex(node, bridge)

            # Snapshot dnsmasq pid via /proc — survives daemon
            # restart only if the agent kept it alive.
            r = node.run(
                f"pgrep -f 'dnsmasq.*--interface={bridge}'", check=False
            )
            dnsmasq_pid_before = (
                r.stdout.strip().splitlines()[0] if r.returncode == 0 else b""
            )

            node.run("sudo systemctl restart corvus-test.service")
            for _ in range(60):
                r = node.run("ss -ltn | grep -q ':9876 '", check=False)
                if r.returncode == 0:
                    break
                time.sleep(0.5)
            else:
                pytest.fail("daemon's TCP listener never returned")

            # Drop the stale client (the restart killed its TCP
            # connection through the VSOCK relay); next access to
            # `self.client` opens a fresh one.
            try:
                node._client.close()
            except Exception:
                pass
            node._client = None

            # Give the daemon's reconnect-and-reapply loop a moment
            # to talk to the agent and re-apply the running networks.
            time.sleep(2.0)

            # Agent kept its kernel state: bridge ifindex unchanged.
            ifindex_after = _bridge_ifindex(node, bridge)
            assert ifindex_after == ifindex_before, (
                f"bridge recreated across daemon restart "
                f"({ifindex_before} → {ifindex_after})"
            )

            # dnsmasq is still the same process (kept alive by agent).
            r = node.run(
                f"pgrep -f 'dnsmasq.*--interface={bridge}'", check=False
            )
            dnsmasq_pid_after = (
                r.stdout.strip().splitlines()[0] if r.returncode == 0 else b""
            )
            assert dnsmasq_pid_after == dnsmasq_pid_before, (
                f"dnsmasq pid changed across daemon restart "
                f"({dnsmasq_pid_before!r} → {dnsmasq_pid_after!r})"
            )

            # Daemon's DB view still shows the network running.
            # `self.client` reopens the cap to the post-restart
            # daemon. The reapplyRunningNetworks path on the daemon's
            # reconnect would have re-issued applyNetwork for any
            # running=true row; if it hadn't, the row would be
            # running=false after a startup reset.
            info = self.client.networks.get("rea-net").show()
            assert info.running is True

    # -- 5. Netd restart wipes-and-reconciles --------------------------------

    def test_netd_restart_wipes_and_reconciles(self):
        """Restart corvus-netd. Its startup cleanup wipes corvus-*
        kernel state; the daemon's reconnect loop re-applies. The
        bridge reappears with a fresh ifindex (proving the wipe ran
        AND the daemon re-applied).

        (Note: a running VM's QEMU process holds a fd to the OLD
        TAP, which is destroyed by the wipe. VM-side recovery
        needs QEMU TAP-reopen support, out of scope for this PR.)
        """
        node = self.node
        client = self.client

        with _network(lambda: self.client, "wip-net", "10.53.0.0/24", dhcp=True, nat=True) as nw:
            net_id = nw.show().id
            bridge = _bridge_name(net_id)
            ifindex_before = _bridge_ifindex(node, bridge)

            node.run("sudo systemctl restart corvus-netd.service")

            # Wait for the daemon to notice and re-apply. The bridge
            # will reappear with a fresh ifindex (proves the wipe ran
            # AND the daemon re-applied).
            deadline = time.monotonic() + 30.0
            ifindex_after = None
            while time.monotonic() < deadline:
                r = node.run(
                    f"cat /sys/class/net/{bridge}/ifindex 2>/dev/null",
                    check=False,
                )
                if r.returncode == 0:
                    ifindex_after = int(r.stdout.strip())
                    if ifindex_after != ifindex_before:
                        break
                time.sleep(0.5)
            assert ifindex_after is not None and ifindex_after != ifindex_before, (
                f"bridge never came back with a new ifindex "
                f"after netd restart (before={ifindex_before}, "
                f"after={ifindex_after})"
            )

            # The daemon's DB still says the network is running, and
            # listNetworks on the (now re-applied) agent agrees.
            info = client.networks.get("wip-net").show()
            assert info.running is True

    # -- 8. Hard error when agent is down ------------------------------------

    def test_netd_unavailable_hard_errors(self):
        """With corvus-netd stopped, network operations fail fast
        with 'netd unavailable' — no fallback, no hang."""
        node = self.node
        client = self.client

        # Best-effort: there shouldn't be any networks running yet for
        # this test (every other test in this class wraps networks in
        # `_network`, which deletes on exit). Sanity check anyway.
        for nw in client.networks.list():
            pass  # purely existence-check; we don't act on stale rows.

        node.run("sudo systemctl stop corvus-netd.service")
        try:
            t0 = time.monotonic()
            with pytest.raises(Exception) as exc_info:
                client.networks.create(
                    "noagent", "10.54.0.0/24", dhcp=False, nat=False
                ).start()
            elapsed = time.monotonic() - t0
            # Hard contract: hard error, no hang.
            assert elapsed < 10.0, (
                f"network start hung for {elapsed:.1f}s with no agent"
            )
            msg = str(exc_info.value)
            # Either "netd unavailable" (ssNetAgent cleared by the
            # reconnect-and-hold async) or a Cap'n Proto disconnect
            # error (the daemon's cached client just saw the agent's
            # listener go away). Both prove the daemon didn't fall
            # back to a legacy path.
            assert (
                re.search(r"netd unavailable", msg, re.IGNORECASE)
                or re.search(r"disconnect", msg, re.IGNORECASE)
            ), (
                f"unexpected error (no 'netd unavailable' or "
                f"disconnect substring): {msg!r}"
            )
        finally:
            node.run("sudo systemctl start corvus-netd.service")
            # Wait for the listener.
            for _ in range(50):
                r = node.run("ss -ltn | grep -q ':9877 '", check=False)
                if r.returncode == 0:
                    break
                time.sleep(0.1)

            # Drop the half-created DB row from the failed start.
            try:
                client.networks.get("noagent").delete()
            except Exception:
                pass

        # Retry once the daemon's connect-and-hold async has
        # reconnected. The async retries with backoff (~1-2 s);
        # poll until a fresh `applyNetwork` succeeds via create+start.
        deadline = time.monotonic() + 30.0
        last_exc = None
        while time.monotonic() < deadline:
            try:
                nw = client.networks.create(
                    "after", "10.54.0.0/24", dhcp=False, nat=False
                )
                nw.start()
                break
            except Exception as e:
                last_exc = e
                # Tear down the half-created row so create() works
                # next iteration.
                try:
                    client.networks.get("after").delete()
                except Exception:
                    pass
                time.sleep(1.0)
        else:
            pytest.fail(
                f"daemon never re-applied after netd restart: {last_exc}"
            )
        # Clean up.
        try:
            nw.stop(force=True)
        except Exception:
            pass
        try:
            nw.delete()
        except Exception:
            pass
