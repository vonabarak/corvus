"""Smoke tests for `corvus-netd`, against the node's steady-state agent.

The test-node image ships `corvus-netd.service` (root, port 9877). The
inner corvus daemon dials this same instance for every managed-network
operation. These tests drive the declarative agent surface directly via
the sync `NetdClient` wrapper, tunnelled through an SSH `-L`
port-forward to the node's `127.0.0.1:9877`.

Each test class gets its own dedicated node, so this class has the
agent to itself; no isolation tricks needed.

Scope:
  * applyNetwork is idempotent + reconciles deltas
  * deleteNetwork tears down kernel state
  * applyTap rejects unknown bridge name
  * applyTap creates a persistent TAP with TUNSETOWNER
  * setIpForwarding flips /proc
  * shutdown cleanup removes every `corvus-*` resource on the node
"""

from __future__ import annotations

import shutil
import socket
import subprocess
import time
from collections.abc import Iterator

import capnp
import pytest
from corvus_test_harness import NetdClient, SingleNodeCase
from corvus_test_harness.cases import state_for
from corvus_test_harness.ssh import HOST_ALPINE_KEY_PATH

NETD_SERVICE = "corvus-netd.service"
NETD_NODE_PORT = 9877


# ---------------------------------------------------------------------------
# Host-side TCP port-forward


def _pick_free_port() -> int:
    with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as s:
        s.bind(("127.0.0.1", 0))
        return s.getsockname()[1]


def _open_port_forward(cid: int, host_port: int) -> subprocess.Popen:
    socat = shutil.which("socat")
    ssh = shutil.which("ssh")
    if not socat or not ssh:
        raise RuntimeError("`ssh` and `socat` must both be on PATH")
    argv = [
        ssh,
        "-N",
        "-o",
        "StrictHostKeyChecking=no",
        "-o",
        "UserKnownHostsFile=/dev/null",
        "-o",
        "BatchMode=yes",
        "-o",
        "ExitOnForwardFailure=yes",
        "-o",
        f"ProxyCommand={socat} - VSOCK-CONNECT:{cid}:22",
        "-i",
        str(HOST_ALPINE_KEY_PATH),
        "-L",
        f"127.0.0.1:{host_port}:127.0.0.1:{NETD_NODE_PORT}",
        f"corvus@vsock-{cid}",
    ]
    proc = subprocess.Popen(argv, stdout=subprocess.DEVNULL, stderr=subprocess.PIPE)
    deadline = time.monotonic() + 10.0
    while time.monotonic() < deadline:
        if proc.poll() is not None:
            err = b""
            if proc.stderr is not None:
                err = proc.stderr.read()
            raise RuntimeError(
                f"ssh port forward exited early: {err.decode(errors='replace')}"
            )
        try:
            with socket.create_connection(("127.0.0.1", host_port), timeout=0.2):
                return proc
        except OSError:
            time.sleep(0.1)
    proc.terminate()
    raise RuntimeError(f"ssh port forward never ready on 127.0.0.1:{host_port}")


def _close_port_forward(proc: subprocess.Popen) -> None:
    if proc.poll() is not None:
        return
    proc.terminate()
    try:
        proc.wait(timeout=2.0)
    except subprocess.TimeoutExpired:
        proc.kill()
        proc.wait(timeout=2.0)


# ---------------------------------------------------------------------------
# Spec-building helpers


def _network_spec(
    name: str,
    cidr: str = "",
    mtu: int = 1500,
    nat_enabled: bool = False,
    nat_uplink: str = "",
    dhcp_enabled: bool = False,
    dhcp_start: str = "",
    dhcp_end: str = "",
    dhcp_lease: str = "12h",
    dhcp_domain: str = "",
):
    return {
        "name": name,
        "cidr": cidr,
        "mtu": mtu,
        "nat": {"enabled": nat_enabled, "uplinkIf": nat_uplink},
        "dhcp": {
            "enabled": dhcp_enabled,
            "rangeStart": dhcp_start,
            "rangeEnd": dhcp_end,
            "leaseTime": dhcp_lease,
            "domain": dhcp_domain,
            "extraArgs": [],
        },
    }


# ---------------------------------------------------------------------------
# Test class


class TestNetdDeclarative(SingleNodeCase):
    """Declarative agent surface, against the node's steady-state
    corvus-netd.service."""

    NODES = ("netd",)

    @pytest.fixture(scope="class")
    def netd_endpoint(self, request) -> Iterator[tuple[str, int]]:
        state = state_for(request.cls)
        if state.topology is None:
            pytest.skip("class topology not initialised; upstream fixture failed")
        node = state.topology.nodes[0]
        host_port = _pick_free_port()
        forward = _open_port_forward(node.cid, host_port)
        try:
            yield ("127.0.0.1", host_port)
        finally:
            _close_port_forward(forward)

    @pytest.fixture
    def agent(self, netd_endpoint, tmp_path_factory) -> Iterator[NetdClient]:
        """One fresh agent connection + session per test method.

        netd's CN-prefix check requires the peer to present a
        ``corvus-daemon:…`` cert (the daemon is its only
        legitimate caller in production), so the harness mints a
        host-side corvus-daemon trio off the class's CA for this
        dial.
        """

        host, port = netd_endpoint
        rl = self.node.client()._rl
        cert_dir = self.topology.ca.mint_host_cert_dir(
            role="corvus-daemon",
            name=f"netd-test-{port}",
            root=tmp_path_factory.mktemp("netd-smoke-cert"),
        )
        with NetdClient.connect(host, port, rl, cert_dir=cert_dir) as nc:
            yield nc

    # -- Liveness / version --------------------------------------------------

    def test_ping(self, agent):
        agent.ping()

    def test_version_advertises_declarative_capabilities(self, agent):
        info = agent.version()
        assert info.semver.startswith("0.")
        assert set(info.capabilities) == {
            "network",
            "tap",
            "ip-forwarding",
            "events",
        }

    # -- Networks ------------------------------------------------------------

    def test_apply_network_creates_bridge_in_kernel(self, agent):
        """applyNetwork puts a real bridge in the node's host netns."""
        name = "corvus-br-a"
        node = self.node

        agent.apply_network(_network_spec(name, cidr="10.191.0.1/24"))

        link = node.run(f"ip -d link show {name}", check=False)
        assert link.returncode == 0, (
            f"bridge {name} not found: {link.stderr.decode(errors='replace')}"
        )
        assert b"bridge " in link.stdout
        addr = node.run(f"ip -o -4 addr show dev {name}")
        assert b"10.191.0.1/24" in addr.stdout
        agent.delete_network(name)

    def test_apply_network_is_idempotent(self, agent):
        """Two applyNetwork calls with the same spec → bridge MAC unchanged."""
        name = "corvus-br-b"
        node = self.node
        spec = _network_spec(name, cidr="10.190.0.1/24")

        agent.apply_network(spec)
        mac1 = node.run(f"cat /sys/class/net/{name}/address").stdout.strip()
        agent.apply_network(spec)
        mac2 = node.run(f"cat /sys/class/net/{name}/address").stdout.strip()

        assert mac1 == mac2, (
            f"bridge recreated on idempotent apply: {mac1!r} → {mac2!r}"
        )
        agent.delete_network(name)

    def test_apply_network_reconciles_cidr_change(self, agent):
        """Updating CIDR with applyNetwork swaps the IP in place."""
        name = "corvus-br-c"
        node = self.node

        agent.apply_network(_network_spec(name, cidr="10.189.0.1/24"))
        agent.apply_network(_network_spec(name, cidr="10.189.0.2/24"))

        addr = node.run(f"ip -o -4 addr show dev {name}")
        assert b"10.189.0.2/24" in addr.stdout
        assert b"10.189.0.1/24" not in addr.stdout
        agent.delete_network(name)

    def test_apply_network_nat_installs_masquerade(self, agent):
        """applyNetwork with nat.enabled adds a masquerade rule."""
        name = "corvus-br-n"
        subnet = "10.188.0.0/24"
        node = self.node

        agent.apply_network(_network_spec(name, cidr="10.188.0.1/24", nat_enabled=True))

        table = node.run("sudo nft list table inet corvus_fw")
        assert b"masquerade" in table.stdout
        # nft renders 10.188.0.1/24 as the implied network 10.188.0.0/24.
        assert subnet.encode() in table.stdout or b"10.188.0.1" in table.stdout
        agent.delete_network(name)

    def test_apply_network_dhcp_spawns_dnsmasq(self, agent):
        """applyNetwork with dhcp.enabled spawns dnsmasq."""
        name = "corvus-br-d"
        node = self.node

        agent.apply_network(
            _network_spec(
                name,
                cidr="10.187.0.1/24",
                dhcp_enabled=True,
                dhcp_start="10.187.0.100",
                dhcp_end="10.187.0.200",
            )
        )

        proc = node.run(f"pgrep -af 'dnsmasq.*--interface={name}'", check=False)
        assert proc.returncode == 0, "dnsmasq not found for the bridge"
        agent.delete_network(name)

    def test_delete_network_tears_down_everything(self, agent):
        """deleteNetwork removes bridge, NAT rule, and dnsmasq."""
        name = "corvus-br-x"
        node = self.node

        agent.apply_network(
            _network_spec(
                name,
                cidr="10.186.0.1/24",
                nat_enabled=True,
                dhcp_enabled=True,
                dhcp_start="10.186.0.100",
                dhcp_end="10.186.0.200",
            )
        )
        agent.delete_network(name)

        link = node.run(f"ip link show {name}", check=False)
        assert link.returncode != 0
        dn = node.run(f"pgrep -f 'dnsmasq.*--interface={name}'", check=False)
        assert dn.returncode != 0

    # -- TAPs ----------------------------------------------------------------

    def test_apply_tap_requires_known_bridge(self, agent):
        """applyTap against an unknown bridge fails fast."""
        with pytest.raises(capnp.KjException):
            agent.apply_tap(
                {
                    "name": "corvus-tap-z",
                    "bridge": "corvus-br-nope",
                    "uid": 1000,
                    "gid": 1000,
                }
            )

    def test_apply_tap_creates_persistent_tap(self, agent):
        """applyTap creates a persistent TAP with the requested owner uid."""
        bridge = "corvus-br-tp"
        tap = "corvus-tap-tp"
        node = self.node

        agent.apply_network(_network_spec(bridge, cidr="10.185.0.1/24"))
        agent.apply_tap({"name": tap, "bridge": bridge, "uid": 1000, "gid": 1000})

        link = node.run(f"ip -d link show {tap}")
        assert b"tun " in link.stdout
        assert b"tap " in link.stdout
        assert f"master {bridge}".encode() in link.stdout
        owner = node.run(f"cat /sys/class/net/{tap}/owner")
        assert owner.stdout.strip() == b"1000"

        agent.delete_tap(tap)
        agent.delete_network(bridge)

    # -- Kernel knobs --------------------------------------------------------

    def test_set_ip_forwarding_toggles_proc(self, agent):
        node = self.node
        before = node.run("cat /proc/sys/net/ipv4/ip_forward").stdout.strip()

        try:
            agent.set_ip_forwarding(enabled=True, family="v4")
            assert node.run("cat /proc/sys/net/ipv4/ip_forward").stdout.strip() == b"1"

            agent.set_ip_forwarding(enabled=False, family="v4")
            assert node.run("cat /proc/sys/net/ipv4/ip_forward").stdout.strip() == b"0"
        finally:
            node.run(
                f"echo {before.decode()} | sudo tee /proc/sys/net/ipv4/ip_forward >/dev/null",
                check=False,
            )

    # -- Cleanup-on-shutdown -------------------------------------------------

    def test_shutdown_cleanup_removes_everything(self, agent):
        """`systemctl stop` runs cleanup; corvus-* resources go away.

        Stops the node's own corvus-netd.service in-place (the inner
        daemon notices, but no other test depends on it). Restarts
        at the end so the rest of the class keeps working.
        """
        name = "corvus-br-zz"
        node = self.node

        agent.apply_network(
            _network_spec(
                name,
                cidr="10.184.0.1/24",
                nat_enabled=True,
                dhcp_enabled=True,
                dhcp_start="10.184.0.100",
                dhcp_end="10.184.0.200",
            )
        )

        try:
            # Stop the steady-state agent. The Main.hs SIGTERM
            # handler runs cleanupCorvusKernelState before exit.
            node.run(f"sudo systemctl stop {NETD_SERVICE}", check=False)
            time.sleep(0.5)

            link = node.run(f"ip link show {name}", check=False)
            assert link.returncode != 0, "bridge survived shutdown"
            nft = node.run(
                "sudo nft list table inet corvus_fw 2>/dev/null",
                check=False,
            )
            assert nft.returncode != 0, "corvus_fw table survived shutdown"
            dn = node.run(f"pgrep -f 'dnsmasq.*--interface={name}'", check=False)
            assert dn.returncode != 0, "dnsmasq survived shutdown"
        finally:
            # Restart so the rest of the class (and the inner daemon)
            # has a healthy endpoint again. The port-forward stays up
            # across this restart because the SSH tunnel is
            # connection-less from the agent's view.
            node.run(f"sudo systemctl start {NETD_SERVICE}", check=False)
            for _ in range(50):
                ss = node.run(f"ss -ltn | grep -q ':{NETD_NODE_PORT} '", check=False)
                if ss.returncode == 0:
                    break
                time.sleep(0.1)
