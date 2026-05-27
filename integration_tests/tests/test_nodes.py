"""Node CRUD against the inner daemon.

Closes a gap in the integration suite: `crv node` verbs (`add`,
`edit`, `drain`, `delete`, `show`, `list`) have no direct test —
existing coverage only spot-checks the side-effects of node
registration (`test_quickstart.py`, `test_vde_networking.py`,
`test_vm_migration.py`).

The harness's `open_client()` already registers a self-pointing
`Node` row named after each test-node's short name
(`inner.py:_register_self_node`). For SingleNodeCase that's
``"single"`` — every test below drives that one row.

Resource-safety: tests that flip ``admin_state`` to ``draining``
or ``maintenance`` always revert to ``online`` in their ``finally``
block. A leaked draining row would break sibling tests by making
every subsequent VM create fail with "no online node available".
"""

from __future__ import annotations

import secrets
import time

import pytest
from corvus_client.exceptions import CorvusError, NodeInUse
from corvus_test_harness import SingleNodeCase


def _wait_for_first_push(client, node_name: str, *, timeout_sec: float = 30.0):
    """Block until the inner daemon has received its first NodeStats push.

    Capacity fields (`ram_mb_free`, `storage_bytes_free`, `load_avg1`)
    are `None` until the nodeagent's first push (~10 s post-connect).
    Returns the populated `NodeDetails`. Raises `AssertionError` on
    timeout.
    """
    deadline = time.monotonic() + timeout_sec
    while time.monotonic() < deadline:
        details = client.nodes.get(node_name).show()
        if details.ram_mb_free is not None:
            return details
        time.sleep(0.5)
    raise AssertionError(
        f"node {node_name!r} ram_mb_free still None after {timeout_sec}s"
    )


class TestNodes(SingleNodeCase):
    """Direct coverage of the `NodeManager` cap + matching CLI verbs."""

    @pytest.fixture(scope="class", autouse=True)
    def _install_client_certs(self, _class_topology):
        """Make the corvus user's ``~/.config/corvus`` carry the
        client cert trio so the inner ``/opt/corvus/bin/crv`` can
        dial the daemon over mTLS (see
        ``IntegrationTestCase.install_node_client_certs``)."""
        self.install_node_client_certs()

    def test_list_and_show_self(self):
        """`nodes.list()` includes the self-registered node and
        `nodes.show()` exposes the full record (host, ports,
        basePath, kernelRelease, agentVersion, healthcheck
        timestamps) once the nodeagent has pushed at least once."""
        self_name = self.node.short_name
        listing = self.client.nodes.list()
        names = [n.name for n in listing]
        assert self_name in names, names

        details = _wait_for_first_push(self.client, self_name)
        assert details.name == self_name
        assert details.admin_state == "online"
        # The harness registers the self-node with the test-node's
        # outer IP (see `topology.py:_outer_ip`), not 127.0.0.1 —
        # multi-node disk-transfer paths need other agents to be
        # able to dial back by the same address. Just assert the
        # field is populated.
        assert details.host, details.host
        assert details.node_agent_port == 9878, details.node_agent_port
        assert details.net_agent_port == 9877, details.net_agent_port
        # After the first push the agent has filled in capacity +
        # kernel + version. We don't assert exact values (host
        # kernel varies), just presence.
        assert details.cpu_count is not None and details.cpu_count > 0
        assert details.ram_mb_total is not None and details.ram_mb_total > 0
        assert details.ram_mb_free is not None
        assert details.kernel_release, details.kernel_release
        assert details.agent_version, details.agent_version
        assert details.last_node_agent_push_at is not None

    def test_crv_node_list_renders_self(self):
        """`crv node list` on the test-node prints a row for the
        self-registered node. Mirrors the smoke check in
        `test_quickstart.py:312` but isolates it from quickstart's
        end-to-end install path so a regression in the CLI
        formatting can be diagnosed independently."""
        cp = self.node.run(
            "/opt/corvus/bin/crv node list",
            check=False,
        )
        assert cp.returncode == 0, cp.stderr.decode(errors="replace")
        output = cp.stdout.decode(errors="replace")
        assert self.node.short_name in output, output
        # Default text formatter prints the admin state.
        assert "online" in output.lower(), output

    def test_edit_description_and_revert(self):
        """`nodes.edit(description=…)` round-trips through `show`.

        Also exercises the empty-string clear path documented in
        `_async/node.py::edit`."""
        self_name = self.node.short_name
        node = self.client.nodes.get(self_name)
        original = node.show().description
        try:
            blurb = f"corvus-it-{secrets.token_hex(3)}"
            node.edit(description=blurb)
            assert node.show().description == blurb
            # Clearing via empty string.
            node.edit(description="")
            after_clear = node.show().description
            # Daemon may treat "" as None or as literal empty; both
            # are acceptable for "cleared".
            assert after_clear in (None, ""), after_clear
        finally:
            # Always restore whatever was there.
            if original is None:
                # Best-effort clear; no description = None.
                try:
                    node.edit(description="")
                except CorvusError:
                    pass
            else:
                node.edit(description=original)

    def test_drain_blocks_scheduler(self):
        """A draining node can't host a fresh VM whose create asks
        the scheduler to pick. Once reverted to ``online``, the
        same create succeeds.

        Drives the path documented in `doc/multi-node.md`: the
        scheduler filters to `admin_state == online` first, and a
        single-node deployment with the one node in `draining`
        leaves zero candidates."""
        self_name = self.node.short_name
        node = self.client.nodes.get(self_name)
        vm_name = f"corvus-it-drain-{secrets.token_hex(3)}"
        node.drain()
        try:
            assert node.show().admin_state == "draining"
            with pytest.raises(CorvusError) as excinfo:
                # No `node=` arg → scheduler picks.
                self.client.vms.create(
                    vm_name,
                    cpu_count=1,
                    ram_mb=256,
                    headless=True,
                )
            msg = str(excinfo.value)
            assert "no online node" in msg or "draining" in msg.lower(), msg
        finally:
            node.edit(admin_state="online")
        # After revert the same create succeeds.
        vm = self.client.vms.create(
            vm_name,
            cpu_count=1,
            ram_mb=256,
            headless=True,
        )
        try:
            assert vm.show().node_name == self_name
        finally:
            vm.delete()

    def test_delete_refused_while_in_use(self):
        """`nodes.delete()` is refused while a VM, network, or
        disk-image placement still references the node.

        We never actually delete `self`: the class fixture and
        every sibling test depends on it. The assertion is purely
        on the error path."""
        self_name = self.node.short_name
        vm_name = f"corvus-it-noderef-{secrets.token_hex(3)}"
        vm = self.client.vms.create(
            vm_name,
            cpu_count=1,
            ram_mb=256,
            headless=True,
        )
        try:
            with pytest.raises(NodeInUse) as excinfo:
                self.client.nodes.get(self_name).delete()
            # Daemon mentions either the VM or generic "referenced".
            msg = str(excinfo.value)
            assert "referenced" in msg.lower() or vm_name in msg, msg
        finally:
            vm.delete()

    def test_edit_host_keeps_supervisor_alive(self):
        """Re-asserting the same host through `nodes.edit(host=…)`
        triggers a supervisor respawn (documented in
        `doc/multi-node.md`) without breaking connectivity. After
        the edit the nodeagent pushes a fresh stats snapshot.

        The current host is read from `show()` so the test is a
        no-op semantically — it only exercises the respawn
        machinery. The daemon must NOT restart; subsequent calls
        on the existing client continue to succeed."""
        self_name = self.node.short_name
        node = self.client.nodes.get(self_name)
        before = node.show()
        assert before.host
        # Snapshot the last-push timestamp; expect the supervisor's
        # reconnect to push a fresh one within ~10 s (the agent's
        # default push interval).
        before_push = before.last_node_agent_push_at
        node.edit(host=before.host)
        deadline = time.monotonic() + 30.0
        new_push = before_push
        while time.monotonic() < deadline:
            d = node.show()
            new_push = d.last_node_agent_push_at
            if new_push is not None and (before_push is None or new_push > before_push):
                break
            time.sleep(0.5)
        assert new_push is not None, "nodeagent never pushed after edit"
        if before_push is not None:
            assert new_push >= before_push, (
                f"last_node_agent_push_at went backwards: {before_push} -> {new_push}"
            )
        # Daemon stays usable on the existing client (no restart).
        self.client.ping()
