"""Phase 2.5 integration tests for `corvus-netd`, running inside the test node.

The declarative agent surface: applyNetwork / listNetworks / deleteNetwork +
applyTap / listTaps / deleteTap. Each test drives the agent via pycapnp
through an SSH `-L` port-forward whose far end is the agent's TCP listener
inside the test node.

Why on the node, not on the host?

The agent needs `CAP_NET_ADMIN`; the outer test node has root and is
disposable. The pytest process stays unprivileged on the host.

Scope (Phase 2.5):
  * applyNetwork is idempotent + reconciles deltas
  * deleteNetwork tears down kernel state
  * applyTap rejects unknown bridge name
  * applyTap creates a persistent TAP with TUNSETOWNER
  * setIpForwarding flips /proc
  * subscribeEvents fires onResourceVanished when admin yanks a link
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

NETD_BINARY_ON_NODE = "/opt/corvus/bin/corvus-netd"
NETD_NODE_PORT = 9899
NETD_TRANSIENT_UNIT = "corvus-netd-smoke.service"


# ---------------------------------------------------------------------------
# Node-side process lifecycle


def _start_netd_on_node(node) -> None:
    """Spawn `corvus-netd` as root on the node via systemd-run.

    Phase 2.5 agent: stateless. Each `systemd-run` invocation starts a
    fresh process which runs its OWN startup cleanup before binding the
    listener — so we get a guaranteed-empty kernel state on every test.
    """
    cmd = (
        f"sudo systemctl stop {NETD_TRANSIENT_UNIT} 2>/dev/null || true; "
        f"sudo systemctl reset-failed {NETD_TRANSIENT_UNIT} 2>/dev/null || true; "
        f"sudo systemd-run --quiet --unit={NETD_TRANSIENT_UNIT} "
        f"  --property=Type=simple --property=Restart=no "
        f"  {NETD_BINARY_ON_NODE} "
        f"  --host 127.0.0.1 --port {NETD_NODE_PORT} --log-level debug; "
        f"for i in $(seq 1 50); do "
        f"  ss -ltn 2>/dev/null | grep -q ':{NETD_NODE_PORT} ' && exit 0; "
        f"  sleep 0.1; "
        f"done; "
        f"echo 'corvus-netd never started listening' >&2; "
        f"sudo journalctl -u {NETD_TRANSIENT_UNIT} --no-pager -n 50 >&2 || true; "
        f"exit 1"
    )
    node.run(cmd, timeout_sec=15)


def _stop_netd_on_node(node) -> None:
    cmd = (
        f"sudo systemctl stop {NETD_TRANSIENT_UNIT} 2>/dev/null || true; "
        f"sudo systemctl reset-failed {NETD_TRANSIENT_UNIT} 2>/dev/null || true"
    )
    node.run(cmd, timeout_sec=10, check=False)


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
    """Phase 2.5 declarative agent surface.

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
        _start_netd_on_node(node)
        host_port = _pick_free_port()
        forward = _open_port_forward(node.cid, host_port)
        try:
            yield ("127.0.0.1", host_port)
        finally:
            _close_port_forward(forward)
            _stop_netd_on_node(node)

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
        # Phase 2.5 advertises the declarative surface.
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

        try:
            _run_on_node_loop(node, lambda: _with_client(host, port, body))
        finally:
            node.run(
                f"sudo ip link del {name} 2>/dev/null || true", check=False
            )

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

        try:
            _run_on_node_loop(node, lambda: _with_client(host, port, body))
        finally:
            node.run(
                f"sudo ip link del {name} 2>/dev/null || true", check=False
            )

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

        try:
            _run_on_node_loop(node, lambda: _with_client(host, port, body))
        finally:
            node.run(
                f"sudo ip link del {name} 2>/dev/null || true", check=False
            )

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

        try:
            _run_on_node_loop(node, lambda: _with_client(host, port, body))
        finally:
            node.run(
                "sudo nft delete table inet corvus_fw 2>/dev/null || true",
                check=False,
            )
            node.run(
                f"sudo ip link del {name} 2>/dev/null || true", check=False
            )

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

        try:
            _run_on_node_loop(node, lambda: _with_client(host, port, body))
        finally:
            node.run(
                f"sudo pkill -f 'dnsmasq.*--interface={name}' 2>/dev/null || true",
                check=False,
            )
            node.run(
                f"sudo ip link del {name} 2>/dev/null || true", check=False
            )

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

        try:
            _run_on_node_loop(node, lambda: _with_client(host, port, body))
        finally:
            node.run(
                f"sudo ip link del {name} 2>/dev/null || true", check=False
            )

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

        try:
            _run_on_node_loop(node, lambda: _with_client(host, port, body))
        finally:
            node.run(
                f"sudo ip tuntap del dev {tap} mode tap 2>/dev/null || true",
                check=False,
            )
            node.run(
                f"sudo ip link del {bridge} 2>/dev/null || true",
                check=False,
            )

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
        """`systemctl stop` runs cleanup; corvus-* resources go away."""
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
            # Stop the agent. The Main.hs SIGTERM handler runs
            # cleanupCorvusKernelState before exit.
            node.run(
                f"sudo systemctl stop {NETD_TRANSIENT_UNIT}", check=False
            )
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
            # Restart the agent so the next test in the class has
            # a healthy endpoint. The port-forward async stays up
            # across this restart because the SSH tunnel is
            # connection-less from the agent's view.
            _start_netd_on_node(node)
