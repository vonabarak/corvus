"""End-to-end test for the `vde` NIC type.

Most integration tests run with the new harness default
(`user`-mode NIC for inner VMs). VDE is deliberately kept out of
the default path because it underperforms `user`/`bridge`/`managed`
and several distros ship a QEMU build without VDE support. This
file is the single focused check that VDE itself still works
end-to-end for users who do enable it.

The test-node image bakes a `vde-switch.service` that listens on
``/run/vde2/switch.ctl`` and creates a host-side ``vde0`` link
with ``192.168.92.1/24``, plus a dnsmasq on ``vde0`` handing out
``192.168.92.10-200`` leases and an nftables masquerade rule that
NATs ``192.168.92.0/24`` outbound through the test-node's outer
NIC. This test boots two inner VMs onto that switch and walks
through L2 → L3 → DNS reachability.
"""

from __future__ import annotations

import secrets
import time

from corvus_test_harness import SingleNodeCase, VmSsh
from corvus_test_harness.runner import NodeShellRunner
from corvus_test_harness.ssh import HOST_ALPINE_KEY_PATH, NodeShell

# Alpine test image puts the sole NIC on eth0.
GUEST_NIC = "eth0"

# Test-node's vde0 gateway IP (baked into corvus-test-node.yml).
NODE_VDE_GATEWAY = "192.168.92.1"

# Pingable global anycast target; the test-node has outbound NAT
# to the host, so VMs reach it via the test-node masquerade.
INTERNET_TARGET = "1.1.1.1"

# A DNS name the test-node's resolver can definitely resolve.
DNS_TARGET = "example.com"


def _wait_for_lease(vm, *, timeout_sec: float = 30.0, poll_sec: float = 1.0) -> str:
    """Poll inside the guest until eth0 has a 192.168.92.x lease."""
    deadline = time.monotonic() + timeout_sec
    last = ""
    while time.monotonic() < deadline:
        r = vm.run(f"ip -4 -o addr show dev {GUEST_NIC}", check=False)
        last = r.stdout
        for line in last.splitlines():
            parts = line.split()
            # "2: eth0    inet 192.168.92.42/24 brd … scope global …"
            if len(parts) < 4 or parts[2] != "inet":
                continue
            ip = parts[3].split("/", 1)[0]
            if ip.startswith("192.168.92."):
                return ip
        time.sleep(poll_sec)
    raise AssertionError(
        f"{GUEST_NIC} never got a 192.168.92.x lease within "
        f"{timeout_sec:.0f}s; last `ip -4 -o addr show` returned:\n{last}"
    )


def _ping_with_retry(
    vm,
    target: str,
    *,
    attempts: int = 3,
    per_attempt_count: int = 2,
    per_attempt_wait_sec: int = 6,
) -> None:
    """Run ping inside a VM, retrying on transient failure.

    The first VM→outside packet has to provision conntrack +
    nftables state on the test-node; under heavy parallel load
    that cold path can exceed the per-attempt timeout. Subsequent
    attempts land on a hot conntrack entry and succeed in
    milliseconds. Modelled on
    test_networking_managed.py:_ping_with_retry.
    """
    last_err: BaseException | None = None
    for _ in range(attempts):
        try:
            vm.run(f"ping -c {per_attempt_count} -W {per_attempt_wait_sec} {target}")
            return
        except RuntimeError as e:
            last_err = e
    assert last_err is not None
    raise last_err


class _VdeVm(VmSsh):
    """Inner VM with a single VDE NIC attached to the test-node's
    on-host vde_switch UNIX socket. SSH still rides over VSOCK."""

    def _net_ifs(self):
        return [
            {
                "type": "vde",
                "host_device": "/run/vde2/switch.ctl",
            }
        ]


class TestVdeNetworking(SingleNodeCase):
    """VDE NIC + the test-node's baked vde-switch / dnsmasq /
    nftables stack — end-to-end."""

    NODES = ("vde",)

    def test_two_vms_full_connectivity(self):
        suffix = secrets.token_hex(3)
        with (
            _VdeVm(self, name=f"vde-a-{suffix}") as vm_a,
            _VdeVm(self, name=f"vde-b-{suffix}") as vm_b,
        ):
            # 1. Bring eth0 up and DHCP a lease from the test-node's
            #    dnsmasq on vde0. Alpine's default config leaves
            #    secondary NICs down.
            for vm in (vm_a, vm_b):
                vm.run(
                    f"doas ip link set {GUEST_NIC} up && "
                    f"doas udhcpc -i {GUEST_NIC} -n -q -t 5 -T 2"
                )

            ip_a = _wait_for_lease(vm_a)
            ip_b = _wait_for_lease(vm_b)
            assert ip_a != ip_b, (
                f"both VMs got the same VDE lease {ip_a}; dnsmasq "
                "isn't tracking client identity"
            )

            # 2. VM ↔ VM over the shared vde0 L2 broadcast domain.
            vm_a.run(f"ping -c 2 -W 2 {ip_b}")
            vm_b.run(f"ping -c 2 -W 2 {ip_a}")

            # 3. VM → test-node's vde0 gateway.
            for vm in (vm_a, vm_b):
                vm.run(f"ping -c 2 -W 2 {NODE_VDE_GATEWAY}")

            # 4. VM → outside world (exercises the test-node's
            #    nftables masquerade out through its outer NIC).
            for vm in (vm_a, vm_b):
                _ping_with_retry(vm, INTERNET_TARGET)

            # 5. DNS — dnsmasq on vde0 forwards to the test-node's
            #    own resolver, which inherits the host's upstream
            #    DNS via the managed outer network.
            for vm in (vm_a, vm_b):
                vm.run(f"nslookup {DNS_TARGET}")

    def test_node_list_shows_netd_disabled(self):
        """After stopping the netd agent and marking the inner node
        as netd-disabled, ``crv node list`` (run on the test-node
        against its own daemon) reports the node's NETD column as
        ``disabled``.

        VDE is the natural test bed for this flag: it's the only
        NIC type besides ``user`` that is still allowed when netd
        is gone, so an operator who runs a node deliberately
        without netd is exactly the operator who would care about
        ``crv node list`` reflecting that decision.
        """
        node = self.nodes[0]

        # 1. Stop the privileged agent so the daemon can no longer
        #    hold a live netd cap (the supervisor's reconnect loop
        #    will start retrying and failing, which is fine — the
        #    netd-disabled flip below cancels it). The test-node
        #    image is Gentoo with passwordless sudo for the wheel
        #    group; the user this SSH session lands in is in wheel.
        node.run("sudo systemctl stop corvus-netd")

        # 2. Mark the inner self-node as netd-disabled via the
        #    inner daemon's RPC. The daemon cancels the per-node
        #    supervisor on the flag flip, so the netd reconnect
        #    loop stops too — no log spam after this point.
        client = node.client()
        self_node = client.nodes.get(node.short_name, by_name=True)
        self_node.edit(netd_disabled=True)

        # 3. The harness deploys daemon / nodeagent / netd cert
        #    trios into /etc/corvus/ on the test-node but does
        #    NOT push a `corvus-client` trio there — only the
        #    pycapnp harness on the host has client material.
        #    Mint one for the in-VM crv by copying the host-side
        #    trio into the corvus user's XDG config dir (the
        #    daemon's TLS resolver checks `$XDG_CONFIG_HOME/corvus`
        #    before `/etc/corvus`, and the corvus user can write
        #    there without sudo). Without this step, the inner
        #    crv refuses to dial — the daemon enforces mTLS on
        #    every TCP listener.
        cert_dir = node._client_cert_dir
        assert cert_dir is not None, (
            "harness should have populated node._client_cert_dir during "
            "deploy_certs(); did the class fixture run?"
        )
        runner = NodeShellRunner(
            NodeShell(cid=node.cid, user="corvus", key_path=HOST_ALPINE_KEY_PATH),
            label=f"vsock:{node.short_name}",
        )
        # `~/.config/corvus` is the corvus user's home — no sudo
        # needed for the directory; the files themselves are
        # installed through sudo because that's what copy_bytes
        # does, which fixes the owner to corvus:corvus regardless
        # of the directory's owner.
        remote_xdg = "/home/corvus/.config/corvus"
        runner.mkdir_p(remote_xdg, mode=0o755, sudo=False)
        for filename, mode in (
            ("ca.crt", 0o644),
            ("corvus-client.crt", 0o644),
            ("corvus-client.key", 0o600),
        ):
            runner.copy_bytes(
                (cert_dir / filename).read_bytes(),
                f"{remote_xdg}/{filename}",
                mode=mode,
            )

        # 4. The user-visible signal: `crv node list` on the
        #    test-node, default text output. The NETD column for
        #    this node must say ``disabled``. Use the
        #    freshly-built inner crv from /opt/corvus/bin so we
        #    test the binary that matches the daemon. Default
        #    host/port (127.0.0.1:9876) targets the system unit.
        result = node.run("/opt/corvus/bin/crv node list", user="corvus")
        stdout = result.stdout.decode("utf-8", errors="replace")
        node_lines = [ln for ln in stdout.splitlines() if node.short_name in ln]
        assert node_lines, (
            f"no `crv node list` row mentions node {node.short_name!r}.\n"
            f"Full output:\n{stdout}"
        )
        assert any("disabled" in ln for ln in node_lines), (
            f"`crv node list` row for {node.short_name!r} does not "
            f"show 'disabled' in the NETD column.\n"
            f"Matching lines:\n" + "\n".join(node_lines)
        )
