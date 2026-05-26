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
