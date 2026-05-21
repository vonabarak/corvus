"""Node CRUD + multi-node placement scenarios.

The 'daemon_socket' fixture registers a self-pointing node row
('self', 127.0.0.1, agent_port) during fixture startup so the
daemon has somewhere to dispatch agent calls. Tests below
exercise additional registrations / lookups / lifecycle on top
of that.

The fixture's nodeagent answers from 127.0.0.1; second-node
registrations in these tests use unused ports — the rows
persist but their per-node supervisor never connects (which is
fine, the tests below don't actually try to drive workloads
through the second node).
"""

from __future__ import annotations

import pytest
from corvus_client import NodeInUse, NodeNotFound, ServerError

from ._helpers import with_client


def test_self_node_is_visible_after_fixture(daemon_socket):
    """The fixture's '_ensure_self_node' helper installs a row;
    list it back through the manager and verify the shape."""
    run = with_client(daemon_socket)

    async def go(c):
        nodes = await c.nodes.list()
        names = [n.name for n in nodes]
        assert "self" in names, names
        self_node = next(n for n in nodes if n.name == "self")
        assert self_node.host == "127.0.0.1"
        assert self_node.admin_state == "online"
        # Stats columns are populated by the agent's first
        # 'NodeStats' push (Phase 5). The fixture polls
        # 'disks.create' which forces a successful nodeagent
        # round-trip, but the stats poll is on a ~10 s tick
        # of its own so we don't assert on capacity values.

    run(go)


def test_node_show_returns_full_details(daemon_socket):
    run = with_client(daemon_socket)

    async def go(c):
        n = await c.nodes.get("self", by_name=True)
        details = await n.show()
        assert details.name == "self"
        assert details.host == "127.0.0.1"
        assert details.admin_state == "online"
        # 'base_path' defaults to /home/corvus/VMs unless the
        # AsyncNodeManager.create caller overrode it; the
        # fixture takes the default.
        assert details.base_path == "/home/corvus/VMs"

    run(go)


def test_node_create_then_delete_round_trip(daemon_socket):
    """A second node row coexists with 'self' and tears down
    cleanly when no resources reference it."""
    run = with_client(daemon_socket)

    async def go(c):
        n = await c.nodes.create(
            "beta",
            "10.0.0.99",
            node_agent_port=19878,
            net_agent_port=19877,
            description="second node for test",
        )
        details = await n.show()
        assert details.host == "10.0.0.99"
        assert details.node_agent_port == 19878
        assert details.description == "second node for test"

        listed = [x.name for x in await c.nodes.list()]
        assert "beta" in listed and "self" in listed

        await n.delete()
        with pytest.raises(NodeNotFound):
            await c.nodes.get("beta", by_name=True)

    run(go)


def test_node_edit_renames_and_updates_admin_state(daemon_socket):
    run = with_client(daemon_socket)

    async def go(c):
        n = await c.nodes.create(
            "edit-me", "10.0.0.50", node_agent_port=19000, net_agent_port=19001
        )
        await n.edit(description="annotated", admin_state="maintenance")
        details = await n.show()
        assert details.description == "annotated"
        assert details.admin_state == "maintenance"
        await n.edit(name="renamed")
        # The cap survives the rename — show still returns the
        # new name through the same cap.
        details2 = await n.show()
        assert details2.name == "renamed"
        # The old name no longer resolves.
        with pytest.raises(NodeNotFound):
            await c.nodes.get("edit-me", by_name=True)
        # The new name does.
        same = await c.nodes.get("renamed", by_name=True)
        await same.show()
        await n.delete()

    run(go)


def test_node_drain_marks_admin_state(daemon_socket):
    """`drain` is the shortcut for admin_state='draining'."""
    run = with_client(daemon_socket)

    async def go(c):
        n = await c.nodes.create(
            "drain-me", "10.0.0.51", node_agent_port=19002, net_agent_port=19003
        )
        await n.drain()
        details = await n.show()
        assert details.admin_state == "draining"
        await n.delete()

    run(go)


def test_node_delete_refuses_while_referenced(daemon_socket):
    """The DB-level refusal: a node with at least one VM / network /
    disk placement can't be deleted until the references are gone."""
    run = with_client(daemon_socket)

    async def go(c):
        n = await c.nodes.create(
            "refd",
            "10.0.0.52",
            node_agent_port=19004,
            net_agent_port=19005,
        )
        # Create a disk pinned to this new node. The scheduler
        # would otherwise pick the 'self' node since it's
        # alphabetically before 'refd' — but the fixture-spawned
        # nodeagent only answers for 'self', not 'refd', so
        # disks pinned to 'refd' fail at the agent dispatch.
        # Use a network instead — those don't need a live agent
        # to be persisted.
        net = await c.networks.create("refd-net", node="refd", subnet="10.66.1.0/24")
        with pytest.raises(NodeInUse):
            await n.delete()
        await net.delete()
        await n.delete()

    run(go)


def test_get_nonexistent_node_raises_node_not_found(daemon_socket):
    run = with_client(daemon_socket)

    async def go(c):
        with pytest.raises(NodeNotFound):
            await c.nodes.get("does-not-exist", by_name=True)

    run(go)


# ---------------------------------------------------------------------------
# Scheduler / placement
# ---------------------------------------------------------------------------


def test_vm_create_without_node_uses_scheduler(daemon_socket):
    """Omitting node= lets the scheduler pick. The fixture has only
    one online node with a reachable nodeagent ('self'), so the
    scheduler always picks that one — and the resulting VM row
    carries that node's id."""
    run = with_client(daemon_socket)

    async def go(c):
        nodes = await c.nodes.list()
        self_id = next(n.id for n in nodes if n.name == "self")

        vm = await c.vms.create("py-vm-sched", cpu_count=1, ram_mb=128)
        # 'crv vm show' / VmDetails don't surface the node FK
        # directly yet (Phase-3 follow-up), so check the row
        # made it into the list and clean up.
        listed = await c.vms.list()
        assert any(v.name == "py-vm-sched" for v in listed)
        await vm.delete()
        del self_id

    run(go)


def test_disk_create_without_node_uses_scheduler(daemon_socket):
    """The disk gets a single 'DiskImagePlacement' on the
    scheduler-picked node."""
    run = with_client(daemon_socket)

    async def go(c):
        disk = await c.disks.create("py-disk-sched", size_mb=1)
        try:
            info = await disk.show()
            assert len(info.placements) == 1
            placement = info.placements[0]
            assert placement.node_name == "self"
            # The file_path is the agent-resolved path on the
            # node's basePath. We don't assert on its exact
            # value — the daemon's basePath logic is exercised
            # by Haskell unit tests.
            assert placement.file_path
        finally:
            await disk.delete()

    run(go)


# NOTE: 'crv disk create' doesn't accept '--node' yet — Phase 3 wired
# DiskImageNode placement records but the wire-level params for disk
# create / register / import stayed lean (the daemon uses the
# scheduler's first-online-node fallback). A future commit will add
# '--node' to the disk-create surface; the test below covers the
# placement DTO shape from the scheduler path, and the
# cross-node-attach test below exercises the same-node invariant by
# pinning the *VM* (which the wire does support) to a node where the
# disk isn't.


def test_network_create_without_node_uses_scheduler(daemon_socket):
    run = with_client(daemon_socket)

    async def go(c):
        net = await c.networks.create("py-net-sched", subnet="10.66.2.0/24")
        try:
            # NetworkInfo doesn't yet surface the node id (Phase 4
            # added the FK but the DTO stayed lean). Round-tripping
            # 'show()' is enough to prove the create succeeded
            # through the scheduler path.
            info = await net.show()
            assert info.name == "py-net-sched"
            assert info.subnet == "10.66.2.0/24"
        finally:
            await net.delete()

    run(go)


def test_cross_node_disk_attach_is_refused(daemon_socket):
    """Same-node invariant: attaching a disk whose placement is on
    node A to a VM living on node B must be refused with a clear
    'not present on node …' error."""
    run = with_client(daemon_socket)

    async def go(c):
        # Persist a second node row (no live agent on its port —
        # we never actually drive anything through it, the test
        # just exercises the DB-level cross-node refusal).
        beta = await c.nodes.create(
            "beta",
            "10.0.0.98",
            node_agent_port=19006,
            net_agent_port=19007,
        )
        # Disk placement goes to the scheduler-picked node
        # (the 'self' node, the only one with a live agent —
        # the 'beta' row exists but its supervisor can't dial
        # its bogus port).
        disk = await c.disks.create("py-xnode-disk", size_mb=1)
        # VM on the 'beta' node — record-only, no start.
        vm = await c.vms.create(
            "py-xnode-vm",
            cpu_count=1,
            ram_mb=128,
            node="beta",
            headless=True,
        )
        info = await disk.show()
        with pytest.raises(ServerError) as excinfo:
            await vm.attach_disk(info.id)
        msg = str(excinfo.value)
        assert "not present on node" in msg, msg
        await vm.delete()
        await disk.delete()
        await beta.delete()

    run(go)
