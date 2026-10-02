"""Integration tests for multi-node VXLAN overlay networks.

Exercises the daemon-side peer management (attach-node / detach-node),
the netd-side VXLAN device + flood FDB reconciliation, the IPAM
allocation that runs when a NIC is attached to a managed network, and
the migrate orchestrator's revised acceptance rule for managed NICs.

Topology: ``OneDaemonTwoNodesCase``. Alpha runs the daemon + agents;
beta runs agents only. The class fixture below registers beta with
alpha's daemon.

Most of the class is deliberately state-heavy and boot-light. Its slow
data-plane case also boots one Alpine guest on each node and sends
packets over the overlay, proving the VTEP/FDB control plane forwards
real guest frames in both directions.

This focused coverage does not exercise Ctrl+Alt+Del behavior, web/CLI route
breadth, or task-stream terminal semantics.
"""

from __future__ import annotations

import secrets
import time
from collections.abc import Iterator

import pytest
from corvus_client import Client, ServerError
from corvus_client._sync.vm import SyncVm
from corvus_test_harness import OneDaemonTwoNodesCase, TestNode, VmShell


def _uniq(stem: str) -> str:
    return f"{stem}-{secrets.token_hex(3)}"


def _wait_until_node_ready(
    client: Client, node_name: str, timeout_sec: float = 30.0
) -> None:
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


def _link_exists(node: TestNode, name: str) -> bool:
    r = node.run(
        f"ip -o link show {name} 2>/dev/null || true",
        check=False,
        timeout_sec=10.0,
    )
    out = r.stdout.decode("utf-8", errors="replace").strip()
    return bool(out)


def _fdb_dsts(node: TestNode, dev: str) -> set[str]:
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
    @classmethod
    def _register_beta(cls) -> Iterator[None]:
        case = cls()
        client = case.client_alpha
        beta_name = case.beta_name
        beta_ip = case.node_beta.outer_ip
        try:
            existing = next(
                (n for n in client.nodes.list() if n.name == beta_name),
                None,
            )
        except ServerError:
            existing = None
        if existing is None:
            beta_node = client.nodes.create(
                beta_name,
                beta_ip,
                node_agent_port=9878,
                net_agent_port=9877,
                description="alpha→beta vxlan tests",
            )
        else:
            beta_node = client.nodes.get(beta_name)
        _wait_until_node_ready(client, beta_name)
        yield
        try:
            beta_node.delete()
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

    def test_attach_node_allocates_vni_and_records_peer(self) -> None:
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

    def test_attach_node_refuses_owner_node(self) -> None:
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

    def test_detach_node_refuses_owner_node(self) -> None:
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

    def test_detach_node_removes_peer(self) -> None:
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

    def test_running_network_materialises_vxlan_on_both_nodes(self) -> None:
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

    def test_detach_node_drops_remote_vxlan(self) -> None:
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

    def test_attaching_nic_to_overlay_assigns_ip(self) -> None:
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

    def test_overlay_forwards_guest_packets_between_nodes(self) -> None:
        """Two DHCP guests on different nodes can ping across the VXLAN.

        The Alpine base is staged on both nodes before creating the
        overlays. ``create_overlay`` chooses one of the backing image's
        placements, so move beta's overlay explicitly; its backing image is
        already present there and the move transfers only the small overlay.
        VSOCK provides test control only: each guest has exactly one NIC, the
        managed overlay NIC under test.
        """
        nw_name = _uniq("vx-data")
        alpha_vm_name = _uniq("vx-alpha")
        beta_vm_name = _uniq("vx-beta")
        alpha_overlay = _uniq("vx-alpha-ovl")
        beta_overlay = _uniq("vx-beta-ovl")
        shells: list[VmShell] = []
        nw = self.client_alpha.networks.create(
            nw_name,
            subnet="10.99.10.0/24",
            node=self.alpha_name,
            dhcp=True,
            nat=False,
        )
        try:
            nw.attach_node(self.beta_name)
            nw.start()

            images = self.register_base_images()
            base_disk = images.get("alpine")
            if base_disk is None:
                pytest.skip(
                    "Alpine base image is unavailable; run `make image IMAGE=vm`"
                )
            self.stage_base_images_on(node_index=1)

            self.client_alpha.disks.create_overlay(
                alpha_overlay, base_disk, ephemeral=True
            )
            self.client_alpha.disks.create_overlay(
                beta_overlay, base_disk, ephemeral=True
            )
            move_task = self.client_alpha.disks.move(beta_overlay, self.beta_name)
            self.wait_for_task(self.client_alpha, move_task, timeout_sec=120.0)

            def boot(name: str, overlay: str, node_name: str) -> SyncVm:
                vm = self.client_alpha.vms.create(
                    name,
                    cpu_count=1,
                    ram_mb=512,
                    node=node_name,
                    headless=True,
                    guest_agent=True,
                    cloud_init=False,
                )
                vm.attach_disk(overlay, interface="virtio")
                vm.add_net_if(type="managed", network_ref=nw_name)
                vm.start(wait=True)
                return vm

            alpha_vm = boot(alpha_vm_name, alpha_overlay, self.alpha_name)
            beta_vm = boot(beta_vm_name, beta_overlay, self.beta_name)
            alpha_ip = alpha_vm.list_net_ifs()[0].ip_address
            beta_ip = beta_vm.list_net_ifs()[0].ip_address
            assert alpha_ip is not None and beta_ip is not None
            assert alpha_ip != beta_ip

            alpha_shell = self.vm_shell(alpha_vm, node_index=0)
            beta_shell = self.vm_shell(beta_vm, node_index=1)
            shells.extend((alpha_shell, beta_shell))
            alpha_shell.wait_ready(timeout_sec=90.0)
            beta_shell.wait_ready(timeout_sec=90.0)
            for shell in shells:
                shell.run(
                    "doas ip link set eth0 up && doas udhcpc -i eth0 -n -q -t 5 -T 2"
                )

            alpha_shell.run(f"ping -c 2 -W 3 {beta_ip}")
            beta_shell.run(f"ping -c 2 -W 3 {alpha_ip}")
        finally:
            for shell in shells:
                try:
                    shell.close()
                except Exception:
                    pass
            # Guests must be gone before tearing their managed bridge down.
            self._delete_silent_vm(alpha_vm_name)
            self._delete_silent_vm(beta_vm_name)
            self._delete_silent_network(nw_name)

    def test_managed_nic_cross_node_refused_without_attach(self) -> None:
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

    def test_migrate_allowed_when_overlay_includes_destination(self) -> None:
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

    def test_migrate_refused_when_overlay_excludes_destination(self) -> None:
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
