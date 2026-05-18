"""Smoke tests for `corvus-netd`, against the node's steady-state agent.

The test-node image ships `corvus-netd.service` (root, port 9877). The
inner corvus daemon dials this same instance for every managed-network
operation. The smoke tests here exercise the declarative agent surface
directly via pycapnp, tunnelled through an SSH `-L` port-forward to the
node's `127.0.0.1:9877`.

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
from typing import Iterator

import capnp
import pytest

from corvus_client._schema import netagent as NETAGENT_SCHEMA
from corvus_test_harness import SingleNodeCase
from corvus_test_harness.cases import state_for
from corvus_test_harness.ssh import HOST_ALPINE_KEY_PATH


# `corvus_client._schema` already loads `netagent.capnp` at import time, so
# we reuse that instance — pycapnp's process-global schema registry rejects
# a second load of the same schema.

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
        "-o", "StrictHostKeyChecking=no",
        "-o", "UserKnownHostsFile=/dev/null",
        "-o", "BatchMode=yes",
        "-o", "ExitOnForwardFailure=yes",
        "-o", f"ProxyCommand={socat} - VSOCK-CONNECT:{cid}:22",
        "-i", str(HOST_ALPINE_KEY_PATH),
        "-L", f"127.0.0.1:{host_port}:127.0.0.1:{NETD_NODE_PORT}",
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
# pycapnp plumbing — reuse the harness's existing kj_loop


async def _with_client(host: str, port: int, body):
    stream = await capnp.AsyncIoStream.create_connection(host=host, port=port)
    client = capnp.TwoPartyClient(stream)
    agent = client.bootstrap().cast_as(NETAGENT_SCHEMA.NetAgent)
    return await body(agent)


def _run_on_node_loop(node, coro_factory):
    rl = node.client()._rl

    async def wrapper():
        return await coro_factory()

    return rl.run(wrapper())


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
    corvus-netd.service.

    Each test method opens its own session (the agent allows multiple
    concurrent sessions) so failures don't cascade.
    """

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

    # -- Liveness / version --------------------------------------------------

    def test_ping(self, netd_endpoint):
        host, port = netd_endpoint

        async def body(agent):
            await agent.ping()

        _run_on_node_loop(self.node, lambda: _with_client(host, port, body))

    def test_version_advertises_declarative_capabilities(self, netd_endpoint):
        host, port = netd_endpoint

        async def body(agent):
            return (await agent.version()).info

        info = _run_on_node_loop(self.node, lambda: _with_client(host, port, body))
        assert info.semver.startswith("0.")
        # The agent advertises the declarative surface.
        assert set(info.capabilities) == {
            "network", "tap", "ip-forwarding", "events",
        }

    # -- Networks ------------------------------------------------------------

    def test_apply_network_creates_bridge_in_kernel(self, netd_endpoint):
        """applyNetwork puts a real bridge in the node's host netns."""
        host, port = netd_endpoint
        name = "corvus-br-a"
        node = self.node

        async def body(agent):
            sess = (await agent.session(owner="test-uid-1000")).session
            spec = _network_spec(name, cidr="10.191.0.1/24")
            await sess.applyNetwork(spec=spec)
            import asyncio
            link = await asyncio.to_thread(
                node.run, f"ip -d link show {name}", check=False
            )
            assert link.returncode == 0, (
                f"bridge {name} not found: "
                f"{link.stderr.decode(errors='replace')}"
            )
            assert b"bridge " in link.stdout
            addr = await asyncio.to_thread(
                node.run, f"ip -o -4 addr show dev {name}"
            )
            assert b"10.191.0.1/24" in addr.stdout
            # Cleanup via the agent (no privileged shell-out needed).
            await sess.deleteNetwork(name)

        _run_on_node_loop(node, lambda: _with_client(host, port, body))

    def test_apply_network_is_idempotent(self, netd_endpoint):
        """Two applyNetwork calls with the same spec → bridge MAC unchanged."""
        host, port = netd_endpoint
        name = "corvus-br-b"
        node = self.node

        async def body(agent):
            sess = (await agent.session(owner="test-uid-1000")).session
            spec = _network_spec(name, cidr="10.190.0.1/24")
            await sess.applyNetwork(spec=spec)
            import asyncio
            mac1 = (
                await asyncio.to_thread(
                    node.run,
                    f"cat /sys/class/net/{name}/address",
                )
            ).stdout.strip()
            await sess.applyNetwork(spec=spec)
            mac2 = (
                await asyncio.to_thread(
                    node.run,
                    f"cat /sys/class/net/{name}/address",
                )
            ).stdout.strip()
            assert mac1 == mac2, (
                f"bridge recreated on idempotent apply: {mac1!r} → {mac2!r}"
            )
            await sess.deleteNetwork(name)

        _run_on_node_loop(node, lambda: _with_client(host, port, body))

    def test_apply_network_reconciles_cidr_change(self, netd_endpoint):
        """Updating CIDR with applyNetwork swaps the IP in place."""
        host, port = netd_endpoint
        name = "corvus-br-c"
        node = self.node

        async def body(agent):
            sess = (await agent.session(owner="test-uid-1000")).session
            await sess.applyNetwork(
                spec=_network_spec(name, cidr="10.189.0.1/24")
            )
            await sess.applyNetwork(
                spec=_network_spec(name, cidr="10.189.0.2/24")
            )
            import asyncio
            addr = await asyncio.to_thread(
                node.run, f"ip -o -4 addr show dev {name}"
            )
            assert b"10.189.0.2/24" in addr.stdout
            assert b"10.189.0.1/24" not in addr.stdout
            await sess.deleteNetwork(name)

        _run_on_node_loop(node, lambda: _with_client(host, port, body))

    def test_apply_network_nat_installs_masquerade(self, netd_endpoint):
        """applyNetwork with nat.enabled adds a masquerade rule."""
        host, port = netd_endpoint
        name = "corvus-br-n"
        subnet = "10.188.0.0/24"
        node = self.node

        async def body(agent):
            sess = (await agent.session(owner="test-uid-1000")).session
            await sess.applyNetwork(
                spec=_network_spec(
                    name,
                    cidr="10.188.0.1/24",
                    nat_enabled=True,
                )
            )
            import asyncio
            table = await asyncio.to_thread(
                node.run, "sudo nft list table inet corvus_fw"
            )
            assert b"masquerade" in table.stdout
            # The masquerade rule's saddr is the bridge CIDR. The
            # agent passes the bridge's CIDR (10.188.0.1/24); nft
            # renders that as the implied network 10.188.0.0/24.
            assert subnet.encode() in table.stdout or b"10.188.0.1" in table.stdout
            await sess.deleteNetwork(name)

        _run_on_node_loop(node, lambda: _with_client(host, port, body))

    def test_apply_network_dhcp_spawns_dnsmasq(self, netd_endpoint):
        """applyNetwork with dhcp.enabled spawns dnsmasq."""
        host, port = netd_endpoint
        name = "corvus-br-d"
        node = self.node

        async def body(agent):
            sess = (await agent.session(owner="test-uid-1000")).session
            await sess.applyNetwork(
                spec=_network_spec(
                    name,
                    cidr="10.187.0.1/24",
                    dhcp_enabled=True,
                    dhcp_start="10.187.0.100",
                    dhcp_end="10.187.0.200",
                )
            )
            import asyncio
            proc = await asyncio.to_thread(
                node.run,
                f"pgrep -af 'dnsmasq.*--interface={name}'",
                check=False,
            )
            assert proc.returncode == 0, "dnsmasq not found for the bridge"
            await sess.deleteNetwork(name)

        _run_on_node_loop(node, lambda: _with_client(host, port, body))

    def test_delete_network_tears_down_everything(self, netd_endpoint):
        """deleteNetwork removes bridge, NAT rule, and dnsmasq."""
        host, port = netd_endpoint
        name = "corvus-br-x"
        node = self.node

        async def body(agent):
            sess = (await agent.session(owner="test-uid-1000")).session
            await sess.applyNetwork(
                spec=_network_spec(
                    name,
                    cidr="10.186.0.1/24",
                    nat_enabled=True,
                    dhcp_enabled=True,
                    dhcp_start="10.186.0.100",
                    dhcp_end="10.186.0.200",
                )
            )
            # Positional arg: pycapnp's stub clashes if you pass
            # `name=` as kwarg (`name` is internal to the call descriptor).
            await sess.deleteNetwork(name)
            import asyncio
            link = await asyncio.to_thread(
                node.run, f"ip link show {name}", check=False
            )
            assert link.returncode != 0
            dn = await asyncio.to_thread(
                node.run,
                f"pgrep -f 'dnsmasq.*--interface={name}'",
                check=False,
            )
            assert dn.returncode != 0

        _run_on_node_loop(node, lambda: _with_client(host, port, body))

    # -- TAPs ----------------------------------------------------------------

    def test_apply_tap_requires_known_bridge(self, netd_endpoint):
        """applyTap against an unknown bridge fails fast."""
        host, port = netd_endpoint

        async def body(agent):
            sess = (await agent.session(owner="test-uid-1000")).session
            with pytest.raises(capnp.KjException):
                await sess.applyTap(
                    spec={
                        "name": "corvus-tap-z",
                        "bridge": "corvus-br-nope",
                        "uid": 1000,
                        "gid": 1000,
                    }
                )

        _run_on_node_loop(self.node, lambda: _with_client(host, port, body))

    def test_apply_tap_creates_persistent_tap(self, netd_endpoint):
        """applyTap creates a persistent TAP with the requested owner uid."""
        host, port = netd_endpoint
        bridge = "corvus-br-tp"
        tap = "corvus-tap-tp"
        node = self.node

        async def body(agent):
            sess = (await agent.session(owner="test-uid-1000")).session
            await sess.applyNetwork(spec=_network_spec(bridge, cidr="10.185.0.1/24"))
            await sess.applyTap(
                spec={
                    "name": tap,
                    "bridge": bridge,
                    "uid": 1000,
                    "gid": 1000,
                }
            )
            import asyncio
            link = await asyncio.to_thread(
                node.run, f"ip -d link show {tap}"
            )
            assert b"tun " in link.stdout
            assert b"tap " in link.stdout
            assert f"master {bridge}".encode() in link.stdout
            owner = await asyncio.to_thread(
                node.run, f"cat /sys/class/net/{tap}/owner"
            )
            assert owner.stdout.strip() == b"1000"
            await sess.deleteTap(tap)
            await sess.deleteNetwork(bridge)

        _run_on_node_loop(node, lambda: _with_client(host, port, body))

    # -- Kernel knobs --------------------------------------------------------

    def test_set_ip_forwarding_toggles_proc(self, netd_endpoint):
        host, port = netd_endpoint
        node = self.node

        before = node.run("cat /proc/sys/net/ipv4/ip_forward").stdout.strip()

        try:
            async def enable(agent):
                sess = (await agent.session(owner="test-uid-1000")).session
                await sess.setIpForwarding(enabled=True, family="v4")

            _run_on_node_loop(node, lambda: _with_client(host, port, enable))
            assert (
                node.run("cat /proc/sys/net/ipv4/ip_forward").stdout.strip()
                == b"1"
            )

            async def disable(agent):
                sess = (await agent.session(owner="test-uid-1000")).session
                await sess.setIpForwarding(enabled=False, family="v4")

            _run_on_node_loop(node, lambda: _with_client(host, port, disable))
            assert (
                node.run("cat /proc/sys/net/ipv4/ip_forward").stdout.strip()
                == b"0"
            )
        finally:
            node.run(
                f"echo {before.decode()} | sudo tee /proc/sys/net/ipv4/ip_forward >/dev/null",
                check=False,
            )

    # -- Cleanup-on-shutdown -------------------------------------------------

    def test_shutdown_cleanup_removes_everything(self, netd_endpoint):
        """`systemctl stop` runs cleanup; corvus-* resources go away.

        This test stops the node's own corvus-netd.service in-place
        (the inner daemon notices, but no other test depends on it).
        Restart at the end so the rest of the class keeps working.
        """
        host, port = netd_endpoint
        name = "corvus-br-zz"
        node = self.node

        async def body(agent):
            sess = (await agent.session(owner="test-uid-1000")).session
            await sess.applyNetwork(
                spec=_network_spec(
                    name,
                    cidr="10.184.0.1/24",
                    nat_enabled=True,
                    dhcp_enabled=True,
                    dhcp_start="10.184.0.100",
                    dhcp_end="10.184.0.200",
                )
            )

        try:
            _run_on_node_loop(node, lambda: _with_client(host, port, body))
            # Stop the steady-state agent. The Main.hs SIGTERM
            # handler runs cleanupCorvusKernelState before exit.
            node.run(f"sudo systemctl stop {NETD_SERVICE}", check=False)
            # Brief settle.
            time.sleep(0.5)
            link = node.run(f"ip link show {name}", check=False)
            assert link.returncode != 0, "bridge survived shutdown"
            nft = node.run(
                "sudo nft list table inet corvus_fw 2>/dev/null",
                check=False,
            )
            assert nft.returncode != 0, "corvus_fw table survived shutdown"
            dn = node.run(
                f"pgrep -f 'dnsmasq.*--interface={name}'", check=False
            )
            assert dn.returncode != 0, "dnsmasq survived shutdown"
        finally:
            # Restart so the rest of the class (and the inner daemon)
            # has a healthy endpoint again. The port-forward stays up
            # across this restart because the SSH tunnel is
            # connection-less from the agent's view.
            node.run(f"sudo systemctl start {NETD_SERVICE}", check=False)
            # Wait for the listener to come back.
            for _ in range(50):
                ss = node.run(
                    f"ss -ltn | grep -q ':{NETD_NODE_PORT} '", check=False
                )
                if ss.returncode == 0:
                    break
                time.sleep(0.1)
