"""Recover a cluster after the loss of a host with live workloads."""

from __future__ import annotations

import time
from collections.abc import Callable

import pytest
from corvus_client import NodeInUse, ServerError
from corvus_test_harness import OneDaemonTwoNodesCase


def _wait_until(condition: Callable[[], bool], description: str) -> None:
    deadline = time.monotonic() + 40
    while time.monotonic() < deadline:
        if condition():
            return
        time.sleep(0.25)
    pytest.fail(description)


@pytest.mark.slow
class TestNodeForget(OneDaemonTwoNodesCase):
    def test_forget_fenced_host(self) -> None:
        client = self.client_alpha
        alpha = client.nodes.get(self.alpha_name)
        beta_ip = self.node_beta.outer_ip
        beta = client.nodes.create(self.beta_name, beta_ip)
        _wait_until(
            lambda: beta.ping().reachable and beta.show().netd_connected,
            "Beta agents did not connect",
        )
        lost = client.vms.create(
            "lost-vm", cpu_count=1, ram=134217728, node=self.beta_name, headless=True
        )
        survivor = client.vms.create(
            "surviving-vm", cpu_count=1, ram=134217728, node=self.alpha_name
        )
        replicated = client.disks.create("replicated", 8 << 20, node=self.beta_name)
        only_beta = client.disks.create("only-beta", 8 << 20, node=self.beta_name)
        client.disks.copy(replicated.show().id, self.alpha_name)
        owned = client.networks.create(
            "beta-owned", "10.93.0.0/24", node=self.beta_name
        )
        kept = client.networks.create(
            "alpha-owned", "10.94.0.0/24", node=self.alpha_name
        )
        owned.attach_node(self.alpha_name)
        kept.attach_node(self.beta_name)
        owned.start()
        kept.start()
        owned_id = owned.show().id
        vni = kept.show().vni
        assert vni is not None
        fdb_command = f"bridge fdb show dev corvus-vx{vni}"
        assert beta_ip.encode() in self.node_alpha.run(fdb_command, check=False).stdout
        # The owned network has a small ID in this fresh topology.
        assert 0 < owned_id < 10
        bridge = f"corvus-br-{owned_id}"
        assert self.node_alpha.run(f"ip link show {bridge}").returncode == 0
        nic = survivor.add_net_if(type="managed", network_ref=owned_id)
        lost.start(wait=True)
        self.wait_for_vm_status(lost, "running")

        before = client.tasks.list(subsystem="node")
        self.install_node_client_certs()
        cli = self.node_alpha.run("/opt/corvus/bin/crv node forget beta --dry-run")
        assert b"lost-vm" in cli.stdout
        preview = beta.forget_preview()
        assert [ref.name for ref in preview.vms] == ["lost-vm"]
        assert [ref.name for ref in preview.owned_networks] == ["beta-owned"]
        assert [ref.name for ref in preview.peer_networks] == ["alpha-owned"]
        assert [ref.name for ref in preview.zero_copy_images] == ["only-beta"]
        assert [ref.name for ref in preview.blocking_vms] == ["surviving-vm"]
        assert client.tasks.list(subsystem="node") == before
        with pytest.raises(ServerError, match="fenced"):
            beta.forget(confirmed_fenced=False)

        survivor.remove_net_if(nic)
        with pytest.raises(ServerError, match="connected"):
            beta.forget(confirmed_fenced=True)
        nic = survivor.add_net_if(type="managed", network_ref=owned_id)
        self.topology.fence_node(self.node_beta)
        _wait_until(
            lambda: not beta.ping().reachable and not beta.show().netd_connected,
            "Beta agent sessions remained connected after power loss",
        )
        with pytest.raises(NodeInUse):
            beta.delete()
        with pytest.raises(NodeInUse, match=r"beta-owned.*surviving-vm"):
            beta.forget(confirmed_fenced=True)
        assert beta.forget_preview() == preview
        survivor.remove_net_if(nic)

        self.node_alpha.run("sudo systemctl stop corvus-netd")
        try:
            _wait_until(
                lambda: not alpha.show().netd_connected, "Alpha netd stayed connected"
            )
            with pytest.raises(ServerError, match="beta-owned on peer alpha"):
                beta.forget(confirmed_fenced=True)
            assert [ref.name for ref in beta.forget_preview().vms] == ["lost-vm"]
            assert owned.show().id == owned_id
        finally:
            self.node_alpha.run("sudo systemctl start corvus-netd")
        _wait_until(lambda: alpha.show().netd_connected, "Alpha netd did not reconnect")

        result = beta.forget(confirmed_fenced=True)
        assert not result.warnings
        assert not kept.show().peer_node_ids
        assert (
            beta_ip.encode() not in self.node_alpha.run(fdb_command, check=False).stdout
        )
        assert (
            self.node_alpha.run(f"ip link show {bridge}", check=False).returncode != 0
        )
        assert {node.name for node in client.nodes.list()} == {self.alpha_name}
        assert {vm.name for vm in client.vms.list()} == {"surviving-vm"}
        assert {network.name for network in client.networks.list()} == {"alpha-owned"}
        assert not only_beta.show().placements
        assert [p.node.name for p in replicated.show().placements] == [self.alpha_name]
        survivor.attach_disk(replicated.show().id)
        assert any(
            task.command == "forget"
            for task in client.tasks.list(subsystem="node", result="success")
        )
