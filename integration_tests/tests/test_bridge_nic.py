"""End-to-end integration test for the `bridge` NIC type.

`type=bridge` attaches a VM to a Linux bridge that the user
created and manages themselves with OS tooling — `ip link add
type bridge`, NetworkManager, systemd-networkd, etc. Corvus does
not create or destroy the bridge.

For each bridge NIC the daemon asks `corvus-netd` to:
  1. Create a per-VM TAP (`corvus-tap-<base36(ifaceId)>`).
  2. Enslave it to the host bridge named in `--host-device`.
  3. Set TUNSETOWNER on the TAP so the unprivileged nodeagent's
     QEMU process can re-open it by ifname.

QEMU then opens the TAP with `-netdev tap,ifname=…` — the same
shape used for `managed` NICs; the `qemu-bridge-helper` path is
intentionally not used.

This test exercises:
  * Kernel state — a TAP appears, slaved to the user bridge.
  * End-to-end L3 — host can ping the guest over the bridge.
  * Cleanup — the TAP is gone after vmStop; the bridge survives.
"""

from __future__ import annotations

import re
import secrets
import time

from corvus_test_harness import SingleNodeCase, VmSsh

# Alpine test image puts its sole NIC on eth0.
GUEST_NIC = "eth0"


def _tap_slaves(node, bridge: str) -> list[str]:
    """Return the ifnames of every corvus-tap-* enslaved to `bridge`.

    Uses `bridge link show master <bridge>` rather than `bridge link
    show <bridge>` so we only see ports, not the bridge itself.
    Output lines look like
        3: corvus-tap-1: <BROADCAST,...> ... master <bridge> ...
    so we pull the ifname out with a regex that stops at the
    trailing colon and any `@if<N>` peer suffix.
    """
    r = node.run(f"bridge link show master {bridge}", check=False)
    if r.returncode != 0:
        return []
    out = r.stdout.decode(errors="replace")
    return re.findall(r"\bcorvus-tap-[A-Za-z0-9]+", out)


def _wait_for_no_tap_on_bridge(node, bridge: str, *, timeout_sec: float = 15.0) -> None:
    """Poll until no `corvus-tap-*` is enslaved to `bridge`.

    The TAP is reaped asynchronously by the daemon's post-QEMU-exit
    supervisor thread (`releaseManagedTaps`), so a brief poll is
    necessary after the VM stops.
    """
    deadline = time.monotonic() + timeout_sec
    while time.monotonic() < deadline:
        if not _tap_slaves(node, bridge):
            return
        time.sleep(0.5)
    leftover = _tap_slaves(node, bridge)
    raise AssertionError(
        f"corvus-tap-* still enslaved to {bridge} after {timeout_sec:.0f}s: {leftover}"
    )


class TestBridgeNic(SingleNodeCase):
    """`type=bridge` end-to-end against a user-managed host bridge."""

    NODES = ("bridge-net",)

    def test_vm_attaches_to_preexisting_host_bridge(self):
        """Create a bridge by hand, attach a VM with type=bridge,
        verify the kernel topology and L3 reachability, then verify
        the TAP is reaped on VM stop while the bridge survives."""
        node = self.node
        # Short suffix keeps the bridge name well under IFNAMSIZ (15).
        suffix = secrets.token_hex(3)
        bridge = f"brtest-{suffix}"
        host_addr = "10.99.0.1/24"
        guest_addr = "10.99.0.2/24"
        guest_ip = guest_addr.split("/", 1)[0]
        host_ip = host_addr.split("/", 1)[0]

        # Precondition: a user-managed bridge (Corvus does NOT
        # create it). Cleaned up in `finally` below.
        node.run(f"sudo ip link add {bridge} type bridge")
        try:
            node.run(f"sudo ip link set {bridge} up")
            node.run(f"sudo ip addr add {host_addr} dev {bridge}")

            class _BridgeVm(VmSsh):
                bridge_name = bridge

                def _net_ifs(self):
                    return [
                        {
                            "type": "bridge",
                            "host_device": self.bridge_name,
                        }
                    ]

            with _BridgeVm(self, name=f"brvm-{suffix}") as vm:
                # 1. Kernel state: exactly one corvus-tap-* is
                #    enslaved to the bridge.
                slaves = _tap_slaves(node, bridge)
                assert len(slaves) == 1, (
                    f"expected one corvus-tap-* on {bridge}, got {slaves}"
                )
                tap = slaves[0]

                # 2. The TAP is up.
                up = node.run(f"ip -o link show {tap}").stdout.decode()
                assert "state UP" in up or "LOWER_UP" in up, up

                # 3. QGA reports the NIC up in-guest.
                vm.run(f"doas ip link set {GUEST_NIC} up")
                # Some images don't auto-add an IP for bridge NICs;
                # assign one statically so we can ping it.
                vm.run(f"doas ip addr add {guest_addr} dev {GUEST_NIC}")

                # 4. End-to-end L3: host (bridge IP) → guest, and
                #    guest → host. The bridge has the host IP so we
                #    don't need a route trick.
                node.run(f"ping -c 2 -W 2 {guest_ip}")
                vm.run(f"ping -c 2 -W 2 -I {GUEST_NIC} {host_ip}")

            # 5. After teardown, the TAP is reaped but the user's
            #    bridge survives.
            _wait_for_no_tap_on_bridge(node, bridge)
            bridge_still_there = node.run(f"ip -o link show {bridge}", check=False)
            assert bridge_still_there.returncode == 0, (
                f"user-managed bridge {bridge} should have survived VM teardown"
            )
        finally:
            node.run(f"sudo ip link delete {bridge}", check=False)

    def test_add_netif_rejects_bridge_without_host_device(self):
        """The daemon refuses to record a bridge NIC with no
        host-device — the bridge name is required at add time."""
        # `vms.create` returns a VM cap; `add_net_if` raises on
        # daemon-side errors (the harness translates RespError into
        # a Python exception with the error string).
        vm = self.client.vms.create(
            f"brrej-{secrets.token_hex(3)}",
            cpu_count=1,
            ram_mb=128,
            headless=True,
            guest_agent=False,
        )
        try:
            try:
                vm.add_net_if(type="bridge", host_device="")
            except Exception as e:
                msg = str(e).lower()
                assert "bridge interface requires" in msg, (
                    f"unexpected error message: {e}"
                )
            else:
                raise AssertionError(
                    "add_net_if(type=bridge, host_device='') should have failed"
                )
        finally:
            vm.delete()
