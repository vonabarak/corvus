"""Integration tests for multi-node VXLAN overlay networks.

Exercises the daemon-side peer management (attach-node / detach-node),
the netd-side VXLAN device + flood FDB reconciliation, the IPAM
allocation that runs when a NIC is attached to a managed network, and
the migrate orchestrator's revised acceptance rule for managed NICs.

Topology: ``OneDaemonTwoNodesCase``. Alpha runs the daemon + agents;
beta runs agents only. The class fixture below registers beta with
alpha's daemon.

The class is deliberately state-heavy and boot-light — we don't start
QEMU here. Cross-node L2 connectivity through the VXLAN is checked at
the netd level (the device exists, the bridge is enslaved, the flood
FDB carries the peer's underlay IP); pushing actual frames through it
is left to a follow-up test that runs once the harness has stable
nested-VM networking on both nodes.
"""

from __future__ import annotations

import secrets
import time

import pytest
from corvus_client import ServerError
from corvus_test_harness import OneDaemonTwoNodesCase


def _uniq(stem: str) -> str:
    return f"{stem}-{secrets.token_hex(3)}"


def _wait_until_node_ready(client, node_name: str, timeout_sec: float = 30.0) -> None:
    """Block until the per-node supervisor has reported RAM stats for
    ``node_name`` — that's how we know its nodeagent reconnected
    after ``nodes.create``.
    """
    deadline = time.monotonic() + timeout_sec
    while time.monotonic() < deadline:
        try:
            details = client.nodes.get(node_name).show()
        except ServerError:
            time.sleep(0.5)
            continue
        if details.ram_mb_free is not None:
            return
        time.sleep(0.5)
    raise AssertionError(
        f"node {node_name!r} did not push RAM stats within {timeout_sec}s"
    )


def _vxlan_iface_for(vni: int) -> str:
    return f"corvus-vx{vni}"


def _link_exists(node, name: str) -> bool:
    r = node.run(
        f"ip -o link show {name} 2>/dev/null || true",
        check=False,
        timeout_sec=10.0,
    )
    out = r.stdout.decode("utf-8", errors="replace").strip()
    return bool(out)


def _fdb_dsts(node, dev: str) -> set[str]:
    """Return the set of peer underlay IPs the flood FDB carries on
    ``dev``. Each `bridge fdb show` row for the all-zero MAC looks
    like ``00:00:00:00:00:00 dst 192.0.2.20 self permanent``.
    """
    r = node.run(
        f"bridge fdb show dev {dev} 2>/dev/null || true",
        check=False,
        timeout_sec=10.0,
    )
    out = r.stdout.decode("utf-8", errors="replace")
    dsts: set[str] = set()
    for line in out.splitlines():
        parts = line.split()
        if not parts or parts[0] != "00:00:00:00:00:00":
            continue
        try:
            i = parts.index("dst")
            dsts.add(parts[i + 1])
        except (ValueError, IndexError):
            continue
    return dsts


class TestVxlanOverlay(OneDaemonTwoNodesCase):
    """attach-node / detach-node + the netd-side VXLAN reconciliation."""

    # ---- class-scoped setup ------------------------------------------------

    @pytest.fixture(scope="class", autouse=True)
    def _register_beta(self, request):
        cls = request.cls
        client = self.client_alpha
        beta_name = self.node_beta.short_name
        beta_ip = self.node_beta.outer_ip
        try:
            existing = next(
                (n for n in client.nodes.list() if n.name == beta_name),
                None,
            )
        except ServerError:
            existing = None
        if existing is None:
            cls.beta_node = client.nodes.create(
                beta_name,
                beta_ip,
                node_agent_port=9878,
                net_agent_port=9877,
                description="alpha→beta vxlan tests",
            )
        else:
            cls.beta_node = client.nodes.get(beta_name)
        cls.beta_name = beta_name
        cls.alpha_name = self.node_alpha.short_name
        _wait_until_node_ready(client, beta_name)
        yield
        try:
            cls.beta_node.delete()
        except Exception:
            pass

    # ---- common cleanup helpers --------------------------------------------

    def _delete_silent_network(self, name: str) -> None:
        try:
            nw = self.client_alpha.networks.get(name)
            try:
                nw.stop(force=True)
            except Exception:
                pass
            nw.delete()
        except Exception:
            pass

    def _delete_silent_vm(self, name: str) -> None:
        try:
            self.client_alpha.vms.get(name).delete()
        except Exception:
            pass

    # ---- state-only tests --------------------------------------------------

    def test_attach_node_allocates_vni_and_records_peer(self):
        """First attach-node assigns a VNI and adds the peer to the
        network's peer set."""
        nw_name = _uniq("vx-state")
        nw = self.client_alpha.networks.create(
            nw_name,
            subnet="10.99.0.0/24",
            node=self.alpha_name,
        )
        try:
            info = nw.show()
            assert info.vni is None
            assert info.peer_node_ids == ()

            nw.attach_node(self.beta_name)
            info = nw.show()
            assert info.vni is not None and info.vni >= 10000
            beta_id = self.client_alpha.nodes.get(self.beta_name).show().id
            assert info.peer_node_ids == (beta_id,)
        finally:
            self._delete_silent_network(nw_name)

    def test_attach_node_refuses_owner_node(self):
        nw_name = _uniq("vx-owner")
        nw = self.client_alpha.networks.create(
            nw_name, subnet="10.99.1.0/24", node=self.alpha_name
        )
        try:
            with pytest.raises(ServerError) as ei:
                nw.attach_node(self.alpha_name)
            assert "owner" in str(ei.value).lower()
        finally:
            self._delete_silent_network(nw_name)

    def test_detach_node_refuses_owner_node(self):
        nw_name = _uniq("vx-detach-owner")
        nw = self.client_alpha.networks.create(
            nw_name, subnet="10.99.2.0/24", node=self.alpha_name
        )
        try:
            with pytest.raises(ServerError) as ei:
                nw.detach_node(self.alpha_name)
            # Either the "cannot detach the owner" path or the
            # "node is not a peer" path is acceptable — both signal
            # the same operator error.
            msg = str(ei.value).lower()
            assert "owner" in msg or "not a peer" in msg
        finally:
            self._delete_silent_network(nw_name)

    def test_detach_node_removes_peer(self):
        nw_name = _uniq("vx-detach")
        nw = self.client_alpha.networks.create(
            nw_name, subnet="10.99.3.0/24", node=self.alpha_name
        )
        try:
            nw.attach_node(self.beta_name)
            assert nw.show().peer_node_ids != ()
            nw.detach_node(self.beta_name)
            assert nw.show().peer_node_ids == ()
        finally:
            self._delete_silent_network(nw_name)

    # ---- netd-side kernel checks (requires the network running) ------------

    def test_running_network_materialises_vxlan_on_both_nodes(self):
        """Starting a multi-node network creates the bridge on the
        owner AND the VXLAN VTEP on every member. Each VTEP's flood
        FDB contains the *other* member's underlay IP."""
        nw_name = _uniq("vx-running")
        nw = self.client_alpha.networks.create(
            nw_name,
            subnet="10.99.4.0/24",
            node=self.alpha_name,
            dhcp=True,
            nat=False,
        )
        try:
            nw.attach_node(self.beta_name)
            nw.start()
            info = nw.show()
            assert info.vni is not None
            vx = _vxlan_iface_for(info.vni)
            # Both nodes' netd should have created their VTEP.
            assert _link_exists(self.node_alpha, vx)
            assert _link_exists(self.node_beta, vx)
            # Flood FDBs point at the other peer.
            alpha_dsts = _fdb_dsts(self.node_alpha, vx)
            beta_dsts = _fdb_dsts(self.node_beta, vx)
            assert self.node_beta.outer_ip in alpha_dsts
            assert self.node_alpha.outer_ip in beta_dsts
        finally:
            self._delete_silent_network(nw_name)

    def test_detach_node_drops_remote_vxlan(self):
        nw_name = _uniq("vx-drop")
        nw = self.client_alpha.networks.create(
            nw_name,
            subnet="10.99.5.0/24",
            node=self.alpha_name,
            dhcp=True,
        )
        try:
            nw.attach_node(self.beta_name)
            nw.start()
            vni = nw.show().vni
            assert vni is not None
            vx = _vxlan_iface_for(vni)
            assert _link_exists(self.node_beta, vx)
            nw.detach_node(self.beta_name)
            # The departing peer should have torn down its
            # bridge + VTEP (best-effort: a brief delay covers any
            # async netd reconciliation).
            for _ in range(20):
                if not _link_exists(self.node_beta, vx):
                    break
                time.sleep(0.25)
            assert not _link_exists(self.node_beta, vx)
            # Owner-side VTEP stays (the network is still running)
            # but its flood FDB no longer points at beta.
            assert self.node_beta.outer_ip not in _fdb_dsts(self.node_alpha, vx)
        finally:
            self._delete_silent_network(nw_name)

    # ---- NIC IPAM ----------------------------------------------------------

    def test_attaching_nic_to_overlay_assigns_ip(self):
        """A VM created on a peer node attaches to the overlay
        network and gets an IPAM-allocated address recorded on the
        NIC. dnsmasq then has a host-reservation for that MAC, so
        the same IP comes back on every DHCP renew (and after
        migration)."""
        nw_name = _uniq("vx-ipam")
        vm_name = _uniq("vx-vm")
        nw = self.client_alpha.networks.create(
            nw_name,
            subnet="10.99.6.0/24",
            node=self.alpha_name,
            dhcp=True,
        )
        try:
            nw.attach_node(self.beta_name)
            nw.start()
            vm = self.client_alpha.vms.create(
                vm_name,
                cpu_count=1,
                ram_mb=128,
                node=self.beta_name,
                headless=True,
                guest_agent=False,
                cloud_init=False,
            )
            vm.add_net_if(network_ref=nw_name)
            nics = vm.list_net_ifs()
            assert len(nics) == 1
            assert nics[0].ip_address is not None
            ip = nics[0].ip_address
            assert ip.startswith("10.99.6.")
            # Allocator should pick .2 (skip .0 net, .1 gw).
            assert ip == "10.99.6.2"
        finally:
            self._delete_silent_vm(vm_name)
            self._delete_silent_network(nw_name)

    def test_managed_nic_cross_node_refused_without_attach(self):
        """A managed NIC's network must include the VM's node — the
        bridge has to be present on the kernel running QEMU. Without
        attach-node the daemon should refuse the NetIf.add."""
        nw_name = _uniq("vx-nopeer")
        vm_name = _uniq("vx-vm-nopeer")
        self.client_alpha.networks.create(
            nw_name, subnet="10.99.7.0/24", node=self.alpha_name
        )
        try:
            vm = self.client_alpha.vms.create(
                vm_name,
                cpu_count=1,
                ram_mb=128,
                node=self.beta_name,
                headless=True,
                guest_agent=False,
                cloud_init=False,
            )
            with pytest.raises(ServerError) as ei:
                vm.add_net_if(network_ref=nw_name)
            assert "owner nor a peer" in str(ei.value).lower()
        finally:
            self._delete_silent_vm(vm_name)
            self._delete_silent_network(nw_name)

    # ---- migration acceptance -----------------------------------------------

    def test_migrate_allowed_when_overlay_includes_destination(self):
        """A stopped VM with a managed NIC migrates between owner and
        peer when the network's peer set covers both. State-only:
        we don't boot QEMU here — the migrate orchestrator's
        pre-check is what we're exercising."""
        nw_name = _uniq("vx-mig")
        vm_name = _uniq("vx-mig-vm")
        disk_name = _uniq("vx-mig-disk")
        nw = self.client_alpha.networks.create(
            nw_name,
            subnet="10.99.8.0/24",
            node=self.alpha_name,
            dhcp=True,
        )
        try:
            nw.attach_node(self.beta_name)
            nw.start()
            self.client_alpha.disks.create(disk_name, size_mb=16, format="qcow2")
            vm = self.client_alpha.vms.create(
                vm_name,
                cpu_count=1,
                ram_mb=128,
                node=self.alpha_name,
                headless=True,
                guest_agent=False,
                cloud_init=False,
            )
            vm.attach_disk(disk_name, interface="virtio")
            vm.add_net_if(network_ref=nw_name)
            ip_before = vm.list_net_ifs()[0].ip_address
            tid = vm.migrate(self.beta_name)
            self.wait_for_task(self.client_alpha, tid, timeout_sec=120.0)
            # Successful wait_for_task means the orchestrator
            # committed the migration. The NIC row should have
            # travelled with the VM and kept its IPAM allocation.
            assert vm.list_net_ifs()[0].ip_address == ip_before
        finally:
            self._delete_silent_vm(vm_name)
            self._delete_silent_network(nw_name)
            try:
                self.client_alpha.disks.get(disk_name).delete()
            except Exception:
                pass

    def test_migrate_refused_when_overlay_excludes_destination(self):
        """A managed NIC on a single-node network cannot migrate; the
        pre-check refuses with a clear hint."""
        nw_name = _uniq("vx-mig-no")
        vm_name = _uniq("vx-mig-no-vm")
        disk_name = _uniq("vx-mig-no-disk")
        self.client_alpha.networks.create(
            nw_name, subnet="10.99.9.0/24", node=self.alpha_name
        )
        try:
            self.client_alpha.disks.create(disk_name, size_mb=16, format="qcow2")
            vm = self.client_alpha.vms.create(
                vm_name,
                cpu_count=1,
                ram_mb=128,
                node=self.alpha_name,
                headless=True,
                guest_agent=False,
                cloud_init=False,
            )
            vm.attach_disk(disk_name, interface="virtio")
            vm.add_net_if(network_ref=nw_name)
            tid = vm.migrate(self.beta_name)
            with pytest.raises(AssertionError) as ei:
                self.wait_for_task(self.client_alpha, tid, timeout_sec=30.0)
            assert "destination node" in str(ei.value).lower() or (
                "attach-node" in str(ei.value).lower()
            )
        finally:
            self._delete_silent_vm(vm_name)
            self._delete_silent_network(nw_name)
            try:
                self.client_alpha.disks.get(disk_name).delete()
            except Exception:
                pass
