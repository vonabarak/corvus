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
  * DHCP + domain turns on the per-network DNS zone (port 53 on the
    bridge, authoritative, REFUSED off-zone, per-bridge lease file)
  * `hostDns` writes / removes a systemd-resolved drop-in on the node
"""

from __future__ import annotations

import shlex
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
    dhcp_host_dns: bool = True,
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
            "hostDns": dhcp_host_dns,
        },
    }


# ---------------------------------------------------------------------------
# DNS-query helper (inline, no `dig` dependency)
#
# The test node doesn't ship bind-tools, so we shell out a small
# Python snippet that builds a DNS UDP query by hand and prints just
# the response's RCODE. Inputs: target IP, qname. Output stdout:
# "NOERROR" / "NXDOMAIN" / "REFUSED" / "SERVFAIL" / "TIMEOUT" / "RCODE_<n>".

_DNS_QUERY_PY = r"""
import socket, struct, sys

def encode_qname(name):
    parts = name.split('.')
    return b''.join(bytes([len(p)]) + p.encode() for p in parts) + b'\x00'

target, qname = sys.argv[1], sys.argv[2]
header = struct.pack('!HHHHHH', 0x1234, 0x0100, 1, 0, 0, 0)  # RD=1
packet = header + encode_qname(qname) + struct.pack('!HH', 1, 1)  # A IN
s = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
s.settimeout(2.0)
try:
    s.sendto(packet, (target, 53))
    resp, _ = s.recvfrom(4096)
except socket.timeout:
    print('TIMEOUT')
    sys.exit(0)
rcode = struct.unpack('!H', resp[2:4])[0] & 0xf
print({0: 'NOERROR', 1: 'FORMERR', 2: 'SERVFAIL', 3: 'NXDOMAIN',
       4: 'NOTIMP', 5: 'REFUSED'}.get(rcode, f'RCODE_{rcode}'))
"""


def _dns_rcode(node, target_ip: str, qname: str) -> str:
    """Send a DNS A query to ``target_ip:53`` from ``node`` and
    return the response RCODE as a string."""
    cmd = f"python3 -c {shlex.quote(_DNS_QUERY_PY)} {shlex.quote(target_ip)} {shlex.quote(qname)}"
    return node.run(cmd).stdout.decode().strip()


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

    # -- DNS-on-bridge -------------------------------------------------------

    @staticmethod
    def _resolved_running(node) -> bool:
        """systemd-resolved present + active on the test node.

        The agent's HostDns module no-ops when /run/systemd/resolve/
        is missing, so the drop-in tests are conditional.
        """
        return node.run("test -d /run/systemd/resolve", check=False).returncode == 0

    def test_apply_network_no_domain_keeps_dns_off(self, agent):
        """Default behaviour: DHCP on, domain empty → port 0 (DNS off).

        Operators upgrading without setting a domain see the same
        DHCP-only dnsmasq they had before; the DNS side is opt-in
        per network.
        """
        name = "corvus-br-d0"
        node = self.node

        agent.apply_network(
            _network_spec(
                name,
                cidr="10.179.0.1/24",
                dhcp_enabled=True,
                dhcp_start="10.179.0.100",
                dhcp_end="10.179.0.200",
                dhcp_domain="",
            )
        )
        try:
            proc = node.run(f"pgrep -af 'dnsmasq.*--interface={name}'")
            argv = proc.stdout.decode()
            assert "--port=0" in argv, argv
            assert "--port=53" not in argv, argv
            assert "--domain=" not in argv, argv
            # Per-bridge leasefile is unconditional (the multi-network
            # collision fix that comes with the DNS change).
            assert (
                f"--dhcp-leasefile=/var/lib/corvus/netd/leases/{name}.leases" in argv
            ), argv
        finally:
            agent.delete_network(name)

    def test_apply_network_domain_turns_on_dns_zone(self, agent):
        """DHCP + domain → dnsmasq answers <hostname>.<domain> from leases.

        Verifies the full DNS-server contract:
          * argv carries --port=53 / --domain= / --local= / --dhcp-fqdn / --expand-hosts
          * UDP 53 is bound on the bridge IP
          * the zone is authoritative — unknown name in zone → NXDOMAIN
          * off-zone queries return REFUSED (not NOERROR; not forwarded)
          * per-bridge lease file exists at the documented path
        """
        name = "corvus-br-dns"
        bridge_ip = "10.178.0.1"
        domain = "corvuszone"
        node = self.node

        agent.apply_network(
            _network_spec(
                name,
                cidr=f"{bridge_ip}/24",
                dhcp_enabled=True,
                dhcp_start="10.178.0.100",
                dhcp_end="10.178.0.200",
                dhcp_domain=domain,
            )
        )
        try:
            proc = node.run(f"pgrep -af 'dnsmasq.*--interface={name}'")
            argv = proc.stdout.decode()
            assert "--port=53" in argv, argv
            assert f"--domain={domain}" in argv, argv
            assert f"--local=/{domain}/" in argv, argv
            assert "--dhcp-fqdn" in argv, argv
            assert "--expand-hosts" in argv, argv
            assert (
                f"--dhcp-leasefile=/var/lib/corvus/netd/leases/{name}.leases" in argv
            ), argv

            # dnsmasq is bound on the bridge IP, not a wildcard. We
            # check UDP (the protocol most resolvers use first); the
            # TCP listener follows the same bind because
            # --bind-interfaces / --listen-address are unconditional.
            ss = node.run("sudo ss -lnup")
            assert f"{bridge_ip}:53".encode() in ss.stdout, (
                f"no dnsmasq UDP listener on {bridge_ip}:53: {ss.stdout!r}"
            )

            # Per-bridge lease file lives where we documented.
            ls = node.run(f"sudo test -f /var/lib/corvus/netd/leases/{name}.leases")
            assert ls.returncode == 0

            # Authoritative-empty zone: no leases yet, so any name in
            # the zone NXDOMAINs (not SERVFAILs; not forwarded).
            assert _dns_rcode(node, bridge_ip, f"nosuch.{domain}") == "NXDOMAIN"

            # Off-zone queries are REFUSED — the "not an open
            # resolver" guarantee that lets a stub use the bridge as
            # one of multiple servers.
            assert _dns_rcode(node, bridge_ip, "example.com") == "REFUSED"
        finally:
            agent.delete_network(name)

    def test_per_bridge_lease_files_are_isolated(self, agent):
        """Two concurrent networks → two distinct lease files.

        Pre-fix this was the latent multi-network bug: both dnsmasqs
        wrote to the same compile-time default and clobbered each
        other's state.
        """
        a_name, b_name = "corvus-br-la", "corvus-br-lb"
        node = self.node

        agent.apply_network(
            _network_spec(
                a_name,
                cidr="10.177.0.1/24",
                dhcp_enabled=True,
                dhcp_start="10.177.0.100",
                dhcp_end="10.177.0.200",
            )
        )
        agent.apply_network(
            _network_spec(
                b_name,
                cidr="10.176.0.1/24",
                dhcp_enabled=True,
                dhcp_start="10.176.0.100",
                dhcp_end="10.176.0.200",
            )
        )
        try:
            for nm in (a_name, b_name):
                proc = node.run(f"pgrep -af 'dnsmasq.*--interface={nm}'")
                argv = proc.stdout.decode()
                assert (
                    f"--dhcp-leasefile=/var/lib/corvus/netd/leases/{nm}.leases" in argv
                ), argv
                ls = node.run(f"sudo test -f /var/lib/corvus/netd/leases/{nm}.leases")
                assert ls.returncode == 0, f"lease file for {nm} missing"
        finally:
            agent.delete_network(a_name)
            agent.delete_network(b_name)

    # -- Host-side systemd-resolved drop-in ----------------------------------

    def test_host_dns_binds_resolved_per_link(self, agent):
        """`hostDns=true` configures per-link systemd-resolved state.

        The agent sets the bridge's per-link DNS server, routing
        domain, and DNSSEC opt-out via ``resolvectl``. The per-link
        scoping is what lets host queries for ``*.<suffix>`` reach
        dnsmasq without disturbing global DNS or DNSSEC policy.
        """
        if not self._resolved_running(self.node):
            pytest.skip("systemd-resolved not running on test node")
        name = "corvus-br-hd"
        bridge_ip = "10.175.0.1"
        domain = "corvushd"
        node = self.node

        agent.apply_network(
            _network_spec(
                name,
                cidr=f"{bridge_ip}/24",
                dhcp_enabled=True,
                dhcp_start="10.175.0.100",
                dhcp_end="10.175.0.200",
                dhcp_domain=domain,
                dhcp_host_dns=True,
            )
        )
        try:
            # Per-link DNS server reported by resolvectl.
            dns = node.run(f"resolvectl dns {name}")
            assert bridge_ip.encode() in dns.stdout, dns.stdout

            # Per-link routing domain (the `~` prefix marks it
            # routing-only — only consulted for matching queries).
            dom = node.run(f"resolvectl domain {name}")
            assert f"~{domain}".encode() in dom.stdout, dom.stdout

            # Per-link DNSSEC opt-out. Without this, resolved tries
            # to validate dnsmasq's unsigned answers against a public
            # DNSSEC chain that doesn't exist for the private zone
            # and the lookup `failed-auxiliary`s.
            sec = node.run(f"resolvectl dnssec {name}")
            assert b"no" in sec.stdout.lower(), sec.stdout
        finally:
            agent.delete_network(name)
            # deleteNetwork reverts the per-link state as part of teardown.
            dns_after = node.run(f"resolvectl dns {name}", check=False)
            # `resolvectl dns` on a now-vanished interface fails; if
            # the link still exists (e.g. the test bridge was renamed),
            # the output should at least not still carry our bridge IP.
            if dns_after.returncode == 0:
                assert bridge_ip.encode() not in dns_after.stdout

    def test_host_dns_opt_out_skips_routing(self, agent):
        """`hostDns=false` keeps dnsmasq's DNS side on but skips the per-link config.

        Operators who run their own host-side resolver (or just don't
        want the agent touching resolved state) reach for ``--no-host-dns``.
        """
        if not self._resolved_running(self.node):
            pytest.skip("systemd-resolved not running on test node")
        name = "corvus-br-nhd"
        bridge_ip = "10.174.0.1"
        domain = "corvusoff"
        node = self.node

        agent.apply_network(
            _network_spec(
                name,
                cidr=f"{bridge_ip}/24",
                dhcp_enabled=True,
                dhcp_start="10.174.0.100",
                dhcp_end="10.174.0.200",
                dhcp_domain=domain,
                dhcp_host_dns=False,
            )
        )
        try:
            # No per-link DNS bound — resolvectl reports a default /
            # empty state, definitely not the bridge IP.
            dns = node.run(f"resolvectl dns {name}", check=False)
            assert dns.returncode != 0 or bridge_ip.encode() not in dns.stdout, (
                dns.stdout
            )
            # dnsmasq still answers DNS on the bridge — only the host
            # forwarding is suppressed.
            proc = node.run(f"pgrep -af 'dnsmasq.*--interface={name}'")
            assert "--port=53" in proc.stdout.decode()
        finally:
            agent.delete_network(name)

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
