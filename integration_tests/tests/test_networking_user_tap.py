"""End-to-end coverage for the ``user`` (SLIRP) and raw ``tap``
network-interface types.

[doc/networking.md](../../doc/networking.md) documents five NIC
types: ``user``, ``managed``, ``tap``, ``bridge``, ``macvtap``,
``vde``. The bridge/managed/vde/vxlan tests already exist
(:mod:`test_bridge_nic`, :mod:`test_networking_managed`,
:mod:`test_vde_networking`, :mod:`test_vxlan`). The two left
uncovered before this file:

* **``user`` (SLIRP, the default).** Operators land here by default
  when they ``--type user`` (or omit ``--type`` entirely). The
  fact that *every* other test in the suite implicitly uses
  ``type=user`` for outbound NICs (see harness ``Vm._net_ifs``)
  means the path is exercised constantly — but nothing has ever
  asserted its actual contract: canonical 10.0.2.x guest IP,
  10.0.2.2 gateway routing back to the QEMU host, no inbound
  reachability from outside, and the ``hostfwd=`` host-device
  trick to expose a guest port.
* **Raw ``tap``** (distinct from ``bridge``: corvus-netd is not
  involved — the operator pre-creates the TAP). Same skeleton as
  the bridge test, but no host bridge in the picture.

What's NOT covered here:

* ``macvtap``. In the harness the test node has a single outer
  NIC over which the harness drives every Cap'n Proto call;
  attaching a macvtap to it would briefly steal MAC ownership on
  the parent and break the host→node SSH/relay. A macvtap test
  needs a dedicated extra NIC inside the test image (or a
  separately-allocated parent device) — out of scope for this
  bucket.
* Validation rejections for malformed ``user``/``tap`` configs.
  The handler at ``src/Corvus/Handlers/NetIf.hs:60-65`` only
  validates ``host_device`` for the ``bridge`` type (already
  covered by :mod:`test_bridge_nic`). The other types pass
  hostDevice through to QEMU verbatim; an empty value just
  produces a QEMU command-line error at start. That's QEMU's
  contract, not Corvus's.
"""

from __future__ import annotations

import secrets
import time

from corvus_test_harness import SingleNodeCase, Vm, VmSsh

# Alpine test image puts its sole NIC on eth0.
GUEST_NIC = "eth0"

# QEMU SLIRP defaults (qemu-doc.html → "Using the user mode network
# stack"). The daemon doesn't override the subnet, so these match
# the guest's view 1:1.
SLIRP_GUEST_IP = "10.0.2.15"
SLIRP_GATEWAY = "10.0.2.2"
SLIRP_NAMESERVER = "10.0.2.3"
SLIRP_SUBNET_PREFIX = "10.0.2."


def _poll_until(cond, *, timeout_sec: float, msg: str, poll_sec: float = 0.5) -> None:
    deadline = time.monotonic() + timeout_sec
    while time.monotonic() < deadline:
        if cond():
            return
        time.sleep(poll_sec)
    raise AssertionError(f"{msg} (waited {timeout_sec}s)")


class TestUserModeSlirp(SingleNodeCase):
    """``type=user`` (SLIRP) semantics + hostfwd trick."""

    def test_default_user_nic_gets_canonical_slirp_address(self):
        """A user-mode NIC lands the guest at 10.0.2.15 with
        gateway 10.0.2.2 — QEMU's documented defaults. Asserts via
        the daemon's QGA poller (which we already trust to surface
        the guest interface list) and a guest-side ``ip route``
        check for the gateway.
        """
        with Vm(self) as vm:
            # Wait for the QGA poller to push the guest's IP back to
            # the daemon. Mirrors test_vm_lifecycle.py's pattern;
            # 30 s is comfortably above the poller's 10 s cadence.
            def _has_guest_ip() -> bool:
                for nif in vm.cap.list_net_ifs():
                    if nif.guest_ip_addresses and "/" in nif.guest_ip_addresses:
                        return True
                return False

            _poll_until(
                _has_guest_ip,
                timeout_sec=30.0,
                msg="QGA poller never reported guest_ip_addresses",
            )
            ips = [
                nif.guest_ip_addresses
                for nif in vm.cap.list_net_ifs()
                if nif.guest_ip_addresses
            ]
            # SLIRP always assigns 10.0.2.15 to the first DHCP
            # client; the poller's CIDR form is "10.0.2.15/24".
            assert any(SLIRP_GUEST_IP in ip for ip in ips), (
                f"expected SLIRP IP {SLIRP_GUEST_IP} in guest_ip_addresses; got {ips!r}"
            )

            # Guest-side: default route is via 10.0.2.2.
            r = vm.cap.guest_exec("/bin/sh -c 'ip route show default'")
            assert r.exit_code == 0, r
            assert SLIRP_GATEWAY in r.stdout, (
                f"default route does not go through {SLIRP_GATEWAY}: {r.stdout!r}"
            )

            # /etc/resolv.conf should carry the SLIRP nameserver
            # too — the DHCP options QEMU advertises include it.
            r = vm.cap.guest_exec("/bin/cat /etc/resolv.conf")
            assert r.exit_code == 0
            assert SLIRP_NAMESERVER in r.stdout, (
                f"resolv.conf missing {SLIRP_NAMESERVER}: {r.stdout!r}"
            )

    def test_slirp_isolates_guest_from_node(self):
        """Inbound from the test node to the guest's SLIRP IP must
        NOT connect. QEMU's user-mode networking is documented as
        having no host route to the guest's RFC1918 SLIRP subnet,
        and operators rely on that — exposing a guest port to the
        outside is the explicit job of ``hostfwd=…`` (covered by
        ``test_user_mode_hostfwd_exposes_guest_port_on_node``).

        We don't assert outbound reachability here: the guest's
        actual ability to reach an arbitrary host:port depends on
        the test node's network stack and routing, which is harness
        environment, not Corvus contract. ``test_default_user_nic_*``
        above is enough to prove the NAT wiring is in place.
        """
        node = self.node
        with Vm(self) as vm:
            # The Alpine test image runs sshd on 22, so if SLIRP
            # forwarded inbound by accident we'd see a successful
            # connect. nc -z -w 2 returns 0 on connect, non-zero
            # on timeout/refused.
            del vm  # explicit: the VM only needs to be alive
            probe = node.run(
                f"nc -z -w 2 {SLIRP_GUEST_IP} 22",
                check=False,
            )
            assert probe.returncode != 0, (
                f"node reached {SLIRP_GUEST_IP}:22 — SLIRP isolation is "
                f"supposed to block this. stdout={probe.stdout!r}, "
                f"stderr={probe.stderr!r}"
            )

    def test_user_mode_hostfwd_exposes_guest_port_on_node(self):
        """``host_device="hostfwd=tcp:127.0.0.1:<port>-:22"`` makes
        the guest's sshd reachable on the node's loopback. From
        the node, opening a TCP connection to ``127.0.0.1:<port>``
        and reading the first line of input returns SSH's
        ``SSH-2.0-…`` banner. After the VM stops, the port goes
        cold.

        VmSsh's default already uses a user-mode NIC; we override
        ``_net_ifs`` to add the hostfwd. VmSsh's VSOCK shell remains
        the primary access path — we're only asserting that the
        hostfwd is honoured on the side."""

        host_port = 40000 + secrets.randbelow(20000)

        class _HostFwdVm(VmSsh):
            def _net_ifs(self):
                return [
                    {
                        "type": "user",
                        "host_device": f"hostfwd=tcp:127.0.0.1:{host_port}-:22",
                    }
                ]

        node = self.node
        with _HostFwdVm(self) as vm:
            # Sanity: QEMU's argv carries the hostfwd we asked for.
            # If a future change to Qemu/Command.hs dropped the
            # SLIRP host_device suffix, this would surface here
            # with a clear diagnostic instead of a port-not-listening
            # timeout downstream.
            vm_id = vm.cap.show().id
            argv = node.run(
                f"pgrep -af 'corvus-vm-{vm_id}' || true",
                check=False,
            ).stdout.decode(errors="replace")
            assert f"hostfwd=tcp:127.0.0.1:{host_port}-:22" in argv, (
                f"hostfwd not in QEMU argv for VM {vm_id}; argv:\n{argv}"
            )

            # Confirm QEMU is bound to the hostfwd port at all.
            listening = node.run(
                f"ss -Hltn 'sport = :{host_port}'", check=False
            ).stdout.decode(errors="replace")
            assert str(host_port) in listening, (
                f"no listener on 127.0.0.1:{host_port} per ss; "
                f"ss output:\n{listening!r}"
            )

            # The hostfwd listener binds whenever QEMU starts user-
            # mode netdev. Poll the node-side TCP port for SSH banner
            # — SSH takes a few seconds to come up after kernel.
            # busybox nc on alpine doesn't accept -w with the
            # connect-and-read shape; we use bash's /dev/tcp and a
            # small `read` budget instead, which works on the
            # Gentoo test-node shell.
            def _banner_ok() -> bool:
                r = node.run(
                    f"bash -c 'exec 3<>/dev/tcp/127.0.0.1/{host_port} && "
                    f"timeout 3 head -1 <&3'",
                    check=False,
                    timeout_sec=10.0,
                )
                return b"SSH-2.0-" in r.stdout

            _poll_until(
                _banner_ok,
                timeout_sec=60.0,
                msg=f"never saw SSH banner on 127.0.0.1:{host_port}",
            )
            del vm  # explicit: VM tears down at with-block exit

        # After teardown the QEMU listener is gone; the node-side
        # port should refuse connections.
        cold = node.run(
            f"timeout 2 nc -z -w 2 127.0.0.1 {host_port}",
            check=False,
        )
        assert cold.returncode != 0, (
            f"127.0.0.1:{host_port} still accepting after VM stop — "
            f"the hostfwd should have died with QEMU"
        )


class TestRawTapNic(SingleNodeCase):
    """``type=tap`` against an operator-pre-created host TAP.
    Mirrors :mod:`test_bridge_nic` but with no bridge in the
    picture — QEMU opens the TAP by ifname, the daemon does not
    touch netd.

    The TAP must be created with ``user`` (TUNSETOWNER) set to
    whichever user the nodeagent runs QEMU as; otherwise QEMU
    can't open the file descriptor when it spawns. The test image
    runs corvus-nodeagent as ``corvus``."""

    def test_vm_attaches_to_preexisting_host_tap(self):
        node = self.node
        suffix = secrets.token_hex(3)
        tap = f"taptest-{suffix}"
        host_addr = "10.98.0.1/24"
        guest_addr = "10.98.0.2/24"
        guest_ip = guest_addr.split("/", 1)[0]
        host_ip = host_addr.split("/", 1)[0]

        # Pre-create the TAP. `user corvus` so QEMU (running as
        # corvus) can open the TAP fd. `pi off / vnet_hdr on` is
        # what `qemu-bridge-helper` would normally set; for a raw
        # tap QEMU sets these on its own.
        node.run(f"sudo ip tuntap add {tap} mode tap user corvus")
        try:
            node.run(f"sudo ip link set {tap} up")
            node.run(f"sudo ip addr add {host_addr} dev {tap}")

            class _RawTapVm(VmSsh):
                tap_name = tap

                def _net_ifs(self):
                    return [
                        {
                            "type": "tap",
                            "host_device": self.tap_name,
                        }
                    ]

            with _RawTapVm(self, name=f"taprawvm-{suffix}") as vm:
                # The guest NIC may or may not have come up with
                # DHCP — the host TAP has no DHCP server. Assign
                # a static IP manually.
                vm.run(f"doas ip link set {GUEST_NIC} up")
                vm.run(f"doas ip addr add {guest_addr} dev {GUEST_NIC}")

                # End-to-end L3 over the raw TAP.
                node.run(f"ping -c 2 -W 2 {guest_ip}")
                vm.run(f"ping -c 2 -W 2 -I {GUEST_NIC} {host_ip}")

                # Smoke check: the TAP doesn't carry a bridge
                # uplink in this scenario (distinct from bridge
                # NIC type). `bridge link show` lists nothing
                # related to taptest-*.
                bridges = node.run(
                    f"bridge link show | grep {tap} || true",
                    check=False,
                )
                assert not bridges.stdout.strip(), (
                    f"raw TAP {tap} unexpectedly enslaved to a bridge: "
                    f"{bridges.stdout!r}"
                )

            # Operator-owned TAP survives VM teardown — Corvus
            # doesn't touch user-managed TAPs.
            still_there = node.run(f"ip -o link show {tap}", check=False)
            assert still_there.returncode == 0, (
                f"user-managed TAP {tap} should have survived VM teardown"
            )
        finally:
            # Always remove the TAP — leaving it would taint
            # subsequent tests that re-use the same node fixture
            # if the name collides.
            node.run(f"sudo ip link delete {tap}", check=False)
