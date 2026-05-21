"""Integration tests for `disks.copy` / `disks.move` (multi-node).

These exercise the standalone disk-placement verbs introduced
alongside `vm.migrate`. Bytes flow agent-to-agent (the daemon
orchestrates but does not relay); see `doc/vm-migration.md` for
the full design.

Topology: `OneDaemonTwoNodesCase`. Alpha runs the daemon + agents;
beta runs agents only. The class fixture below registers beta with
alpha's daemon so both nodes are reachable from the same client.

Refusal matrix tested (see [planTransfer](/home/bobr/Work/corvus/src/Corvus/Handlers/Disk.hs)):

  - read-write attached: copy + move refused
  - read-only attached:  move refused, copy allowed
  - destination already has a placement: refused
  - overlay's backing chain missing on destination: refused
  - destination not online / equals source: refused
"""
from __future__ import annotations

import secrets
import time

import pytest

from corvus_client import ServerError
from corvus_test_harness import OneDaemonTwoNodesCase


pytestmark = pytest.mark.slow


def _uniq(stem: str) -> str:
    """6-hex-char suffix to keep concurrent runs / leak hunts trivial."""
    return f"{stem}-{secrets.token_hex(3)}"


class TestDiskCopyMove(OneDaemonTwoNodesCase):
    """Standalone `disks.copy` / `disks.move` against two nodes."""

    # ---- class-scoped setup ------------------------------------------------

    @pytest.fixture(scope="class", autouse=True)
    def _register_beta(self, request):
        """Register beta with alpha's daemon for every test in the class.

        Bookkeeping lives on the class object (rather than `self`)
        because pytest re-instantiates the test class per method. We
        also leak the registration on test failure (the per-class
        topology is leaked too — leave the daemon's view consistent
        with the kept VMs so a developer can inspect with `crv`).
        """
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
                description="alpha→beta disk-copy-move tests",
            )
        else:
            cls.beta_node = client.nodes.get(beta_name)
        cls.beta_name = beta_name
        cls.alpha_name = self.node_alpha.short_name
        # The daemon spawns the per-node supervisor when the row is
        # added; on first dial it can take a moment before agent
        # ops are ready.  Poll the agent's status push once instead
        # of blindly sleeping.
        _wait_until_node_ready(client, beta_name)
        yield
        # Best-effort cleanup. Leaks here aren't fatal — the class
        # topology gets torn down at the end of the class anyway.
        try:
            cls.beta_node.delete()
        except Exception:
            pass

    def _delete_silent(self, name: str) -> None:
        try:
            self.client_alpha.disks.get(name).delete()
        except Exception:
            pass

    def _placement_nodes(self, disk_name: str) -> set[str]:
        info = self.client_alpha.disks.get(disk_name).show()
        return {p.node_name for p in info.placements}

    def _placement_path_on(self, disk_name: str, node_name: str) -> str:
        info = self.client_alpha.disks.get(disk_name).show()
        for p in info.placements:
            if p.node_name == node_name:
                return p.file_path
        raise AssertionError(
            f"disk {disk_name!r} has no placement on node {node_name!r}; "
            f"placements: {[p.node_name for p in info.placements]!r}"
        )

    def _file_exists(self, node, path: str) -> bool:
        r = node.run(f"test -f {path!r}", check=False, timeout_sec=10.0)
        return r.returncode == 0

    def _md5_on(self, node, path: str) -> str:
        r = node.run(f"md5sum {path!r}", check=True, timeout_sec=15.0)
        out = r.stdout.decode("utf-8", errors="replace").strip()
        return out.split()[0]

    # ---- happy paths -------------------------------------------------------

    def test_copy_detached_disk(self):
        """Create a small disk on alpha; copy to beta. Both
        placements exist, both files present, bytes identical."""
        name = _uniq("copy")
        disk = self.client_alpha.disks.create(name, size_mb=16, format="qcow2")
        try:
            assert self._placement_nodes(name) == {self.alpha_name}
            tid = self.client_alpha.disks.copy(name, self.beta_name)
            self.wait_for_task(self.client_alpha, tid, timeout_sec=60.0)
            assert self._placement_nodes(name) == {self.alpha_name, self.beta_name}
            alpha_path = self._placement_path_on(name, self.alpha_name)
            beta_path = self._placement_path_on(name, self.beta_name)
            assert self._file_exists(self.node_alpha, alpha_path)
            assert self._file_exists(self.node_beta, beta_path)
            # Byte-equivalent (fresh qcow2: identical on both sides).
            assert self._md5_on(self.node_alpha, alpha_path) == self._md5_on(
                self.node_beta, beta_path
            )
        finally:
            self._delete_silent(name)

    def test_move_detached_disk(self):
        """Create on alpha; move to beta. One placement (beta);
        source file unlinked."""
        name = _uniq("move")
        self.client_alpha.disks.create(name, size_mb=16, format="qcow2")
        try:
            alpha_path = self._placement_path_on(name, self.alpha_name)
            tid = self.client_alpha.disks.move(name, self.beta_name)
            self.wait_for_task(self.client_alpha, tid, timeout_sec=60.0)
            assert self._placement_nodes(name) == {self.beta_name}
            beta_path = self._placement_path_on(name, self.beta_name)
            assert self._file_exists(self.node_beta, beta_path)
            assert not self._file_exists(self.node_alpha, alpha_path), (
                "source file still present after move"
            )
        finally:
            self._delete_silent(name)

    def test_copy_allowed_for_ro_attached(self):
        """`copy` of a r/o-attached disk is allowed (and yields two
        placements). `move` of the same disk is rejected by the next
        test; here we only assert the copy path."""
        disk_name = _uniq("copy-ro")
        vm_name = _uniq("copy-ro-vm")
        self.client_alpha.disks.create(disk_name, size_mb=16, format="qcow2")
        try:
            vm = self.client_alpha.vms.create(
                vm_name,
                cpu_count=1,
                ram_mb=128,
                node=self.alpha_name,
                headless=True,
                guest_agent=False,
                cloud_init=False,
            )
            try:
                vm.attach_disk(disk_name, interface="virtio", read_only=True)
                tid = self.client_alpha.disks.copy(disk_name, self.beta_name)
                self.wait_for_task(self.client_alpha, tid, timeout_sec=60.0)
                assert self._placement_nodes(disk_name) == {
                    self.alpha_name,
                    self.beta_name,
                }
            finally:
                vm.delete()
        finally:
            self._delete_silent(disk_name)

    # ---- refusal matrix ----------------------------------------------------

    def test_copy_refuses_rw_attached(self):
        """An r/w-attached disk can only move via `vm.migrate`."""
        disk_name = _uniq("rw-copy")
        vm_name = _uniq("rw-copy-vm")
        self.client_alpha.disks.create(disk_name, size_mb=16, format="qcow2")
        try:
            vm = self.client_alpha.vms.create(
                vm_name,
                cpu_count=1,
                ram_mb=128,
                node=self.alpha_name,
                headless=True,
                guest_agent=False,
                cloud_init=False,
            )
            try:
                vm.attach_disk(disk_name, interface="virtio")
                with pytest.raises(ServerError) as ei:
                    self.client_alpha.disks.copy(disk_name, self.beta_name)
                msg = str(ei.value)
                assert "read-write" in msg or "vm migrate" in msg
            finally:
                vm.delete()
        finally:
            self._delete_silent(disk_name)

    def test_move_refuses_rw_attached(self):
        disk_name = _uniq("rw-move")
        vm_name = _uniq("rw-move-vm")
        self.client_alpha.disks.create(disk_name, size_mb=16, format="qcow2")
        try:
            vm = self.client_alpha.vms.create(
                vm_name,
                cpu_count=1,
                ram_mb=128,
                node=self.alpha_name,
                headless=True,
                guest_agent=False,
                cloud_init=False,
            )
            try:
                vm.attach_disk(disk_name, interface="virtio")
                with pytest.raises(ServerError) as ei:
                    self.client_alpha.disks.move(disk_name, self.beta_name)
                assert "read-write" in str(ei.value) or "vm migrate" in str(ei.value)
            finally:
                vm.delete()
        finally:
            self._delete_silent(disk_name)

    def test_move_refuses_ro_attached(self):
        """Moving a r/o-attached disk is refused — only `copy` is
        allowed for the read-only case."""
        disk_name = _uniq("ro-move")
        vm_name = _uniq("ro-move-vm")
        self.client_alpha.disks.create(disk_name, size_mb=16, format="qcow2")
        try:
            vm = self.client_alpha.vms.create(
                vm_name,
                cpu_count=1,
                ram_mb=128,
                node=self.alpha_name,
                headless=True,
                guest_agent=False,
                cloud_init=False,
            )
            try:
                vm.attach_disk(disk_name, interface="virtio", read_only=True)
                with pytest.raises(ServerError) as ei:
                    self.client_alpha.disks.move(disk_name, self.beta_name)
                assert "can only be copied" in str(ei.value) or "read-only" in str(
                    ei.value
                )
            finally:
                vm.delete()
        finally:
            self._delete_silent(disk_name)

    def test_refuses_destination_already_has_placement(self):
        """A second copy to the same node refuses with a placement
        conflict message."""
        name = _uniq("dup")
        self.client_alpha.disks.create(name, size_mb=16, format="qcow2")
        try:
            tid = self.client_alpha.disks.copy(name, self.beta_name)
            self.wait_for_task(self.client_alpha, tid, timeout_sec=60.0)
            with pytest.raises(ServerError) as ei:
                self.client_alpha.disks.copy(name, self.beta_name)
            assert "already has a placement" in str(ei.value)
        finally:
            self._delete_silent(name)

    def test_refuses_overlay_missing_backing(self):
        """Copying an overlay whose backing image hasn't been staged
        on the destination is refused, with a clear hint message."""
        base_name = _uniq("ovl-base")
        overlay_name = _uniq("ovl-top")
        self.client_alpha.disks.create(base_name, size_mb=16, format="qcow2")
        try:
            self.client_alpha.disks.create_overlay(overlay_name, base_name)
            try:
                with pytest.raises(ServerError) as ei:
                    self.client_alpha.disks.copy(overlay_name, self.beta_name)
                assert "backing" in str(ei.value).lower()
            finally:
                self._delete_silent(overlay_name)
        finally:
            self._delete_silent(base_name)

    def test_overlay_works_after_staging_chain(self):
        """Staging the backing image first, then copying the overlay,
        succeeds — both placements exist on both nodes."""
        base_name = _uniq("chain-base")
        overlay_name = _uniq("chain-top")
        self.client_alpha.disks.create(base_name, size_mb=16, format="qcow2")
        try:
            self.client_alpha.disks.create_overlay(overlay_name, base_name)
            try:
                # Stage backing on beta first.
                tid = self.client_alpha.disks.copy(base_name, self.beta_name)
                self.wait_for_task(self.client_alpha, tid, timeout_sec=60.0)
                # Now the overlay can land.
                tid = self.client_alpha.disks.copy(overlay_name, self.beta_name)
                self.wait_for_task(self.client_alpha, tid, timeout_sec=60.0)
                assert self._placement_nodes(base_name) == {
                    self.alpha_name,
                    self.beta_name,
                }
                assert self._placement_nodes(overlay_name) == {
                    self.alpha_name,
                    self.beta_name,
                }
            finally:
                self._delete_silent(overlay_name)
        finally:
            self._delete_silent(base_name)

    def test_refuses_target_not_online(self):
        """Drain beta, then attempt copy — should refuse. Restore
        beta to `online` before leaving so subsequent tests work."""
        name = _uniq("draining")
        self.client_alpha.disks.create(name, size_mb=16, format="qcow2")
        beta_cap = self.client_alpha.nodes.get(self.beta_name)
        try:
            beta_cap.drain()
            try:
                with pytest.raises(ServerError) as ei:
                    self.client_alpha.disks.copy(name, self.beta_name)
                assert "not online" in str(ei.value)
            finally:
                beta_cap.edit(admin_state="online")
        finally:
            self._delete_silent(name)

    def test_refuses_target_equals_source(self):
        """Copying to the source node is a no-op the daemon refuses
        outright (the planner can't find a non-target placement)."""
        name = _uniq("self")
        self.client_alpha.disks.create(name, size_mb=16, format="qcow2")
        try:
            with pytest.raises(ServerError) as ei:
                self.client_alpha.disks.copy(name, self.alpha_name)
            # The daemon's exact wording is "no source placement
            # available (target is the only node hosting this disk)"
            # — match the substring rather than the full string so
            # future copy edits to that diagnostic don't break the
            # test.
            assert "target" in str(ei.value).lower() or "no source" in str(
                ei.value
            ).lower()
        finally:
            self._delete_silent(name)


def _wait_until_node_ready(client, node_name: str, *, timeout_sec: float = 30.0):
    """Poll until the daemon's per-node supervisor has dialled the
    agent at least once (`last_node_agent_push_at` is set).

    `nodes.create` returns as soon as the row is persisted; the
    supervisor's first dial is asynchronous and the first few
    follow-up RPCs race with it.
    """
    deadline = time.monotonic() + timeout_sec
    while time.monotonic() < deadline:
        try:
            details = client.nodes.get(node_name).show()
        except ServerError:
            time.sleep(0.5)
            continue
        if details.last_node_agent_push_at is not None:
            return
        time.sleep(0.5)
    # Don't fail hard — the first test's call will surface a clearer
    # error if the supervisor never came up.
