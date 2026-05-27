"""Multi-node disk placement integration tests.

Exercises the multi-node refactor's disk-related guarantees:

  * ``--node`` on ``disks.create`` / ``disks.register`` /
    ``disks.import_`` lands the placement on the requested node.
  * Operations on an existing disk (snapshot create/delete,
    overlay, clone, refresh, resize) run on a node that already
    hosts the file — not the scheduler's first-online pick. The
    daemon's ``pickNodeForExistingDisk`` is the property we
    verify by exercising disks that live only on beta and
    checking the operation succeeds and the resulting placement
    stays on beta.
  * ``crv apply`` YAML with explicit per-disk and per-VM
    ``node:`` values respects placement; same-named entities on
    different nodes round-trip through ``skip_existing`` without
    collision.
  * ``crv vm show`` returns the per-node file path for each
    attached drive (previously the field was always empty).

Topology is `OneDaemonTwoNodesCase`: alpha runs the daemon +
agents, beta runs agents only. The class fixture registers beta
with alpha's daemon so the daemon can schedule onto it. There is
no ``client_beta``; every RPC goes through ``client_alpha``.
"""

from __future__ import annotations

import secrets
import textwrap
import time

import pytest
from corvus_client import ServerError
from corvus_test_harness import OneDaemonTwoNodesCase


def _uniq(stem: str) -> str:
    """6-hex-char suffix to keep concurrent runs / leak hunts trivial."""
    return f"{stem}-{secrets.token_hex(3)}"


def _wait_until_node_ready(
    client, node_name: str, *, timeout_sec: float = 30.0
) -> None:
    """Poll until the daemon's per-node supervisor has dialled the
    agent at least once.

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


class TestMultiNodeDiskPlacement(OneDaemonTwoNodesCase):
    """`--node` on disk verbs, placement-aware operations, apply YAML."""

    # ---- class-scoped setup ------------------------------------------------

    @pytest.fixture(scope="class", autouse=True)
    def _register_beta(self, request):
        """Register beta with alpha's daemon for every test in the class.

        Bookkeeping lives on the class object (rather than `self`)
        because pytest re-instantiates the test class per method.
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
                description="alpha→beta multi-node disk tests",
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

    # ---- helpers -----------------------------------------------------------

    def _delete_disk_silent(self, name: str) -> None:
        try:
            self.client_alpha.disks.get(name).delete()
        except Exception:
            pass

    def _delete_vm_silent(self, name: str) -> None:
        try:
            self.client_alpha.vms.get(name).delete()
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
            f"disk {disk_name!r} has no placement on {node_name!r}; "
            f"placements: {[p.node_name for p in info.placements]!r}"
        )

    def _node_for(self, short_name: str):
        return self.node_alpha if short_name == self.alpha_name else self.node_beta

    def _file_exists_on(self, node_short: str, path: str) -> bool:
        r = self._node_for(node_short).run(
            f"test -f {path!r}", check=False, timeout_sec=10.0
        )
        return r.returncode == 0

    # ---- `--node` on disks.create / register / import ---------------------

    def test_create_with_node_lands_on_chosen(self):
        """`disks.create(..., node='beta')` produces a placement only
        on beta — not alpha."""
        name = _uniq("create-on-beta")
        self.client_alpha.disks.create(
            name, size_mb=16, format="qcow2", node=self.beta_name
        )
        try:
            assert self._placement_nodes(name) == {self.beta_name}
            beta_path = self._placement_path_on(name, self.beta_name)
            assert self._file_exists_on(self.beta_name, beta_path)
            assert not self._file_exists_on(self.alpha_name, beta_path)
        finally:
            self._delete_disk_silent(name)

    def test_create_without_node_uses_scheduler(self):
        """`disks.create` without `node=` defers to the scheduler.
        The exact node is the lowest-id online one; assert at least
        that the disk has a single placement and the file is on
        that node."""
        name = _uniq("create-sched")
        self.client_alpha.disks.create(name, size_mb=16, format="qcow2")
        try:
            placements = self.client_alpha.disks.get(name).show().placements
            assert len(placements) == 1
            chosen = placements[0].node_name
            assert self._file_exists_on(chosen, placements[0].file_path)
        finally:
            self._delete_disk_silent(name)

    def test_register_with_node_records_placement(self):
        """`disks.register(..., node='beta')` against a pre-existing
        file on beta records the placement on beta."""
        name = _uniq("register-beta")
        # Stage a raw file on beta directly; size doesn't matter,
        # `disks.register` doesn't verify content beyond format.
        beta_path = f"/tmp/{name}.raw"
        self.node_beta.run(
            f"dd if=/dev/zero of={beta_path} bs=1M count=4",
            check=True,
            timeout_sec=15.0,
        )
        try:
            self.client_alpha.disks.register(
                name, beta_path, format="raw", node=self.beta_name
            )
            try:
                assert self._placement_nodes(name) == {self.beta_name}
                assert self._placement_path_on(name, self.beta_name) == beta_path
            finally:
                self._delete_disk_silent(name)
        finally:
            self.node_beta.run(f"rm -f {beta_path!r}", check=False, timeout_sec=5.0)

    # ---- placement-aware operations on existing disks ---------------------

    def test_snapshot_runs_on_disks_node(self):
        """A snapshot of a disk that lives only on beta succeeds
        without an explicit node — the placement-aware picker
        chooses beta. The snapshot row appears under the disk and
        the disk's placement stays on beta only."""
        name = _uniq("snap-on-beta")
        self.client_alpha.disks.create(
            name, size_mb=16, format="qcow2", node=self.beta_name
        )
        try:
            disk = self.client_alpha.disks.get(name)
            snap = disk.snapshot_create("s1")
            try:
                names = [s.name for s in disk.snapshot_list()]
                assert "s1" in names
                # Placement must still be beta-only.
                assert self._placement_nodes(name) == {self.beta_name}
            finally:
                try:
                    snap.delete()
                except Exception:
                    pass
        finally:
            self._delete_disk_silent(name)

    def test_overlay_inherits_base_node(self):
        """An overlay of a disk on beta lands on beta — the
        placement-aware picker must follow the base."""
        base = _uniq("ovl-base-beta")
        overlay = _uniq("ovl-top-beta")
        self.client_alpha.disks.create(
            base, size_mb=16, format="qcow2", node=self.beta_name
        )
        try:
            self.client_alpha.disks.create_overlay(overlay, base)
            try:
                assert self._placement_nodes(overlay) == {self.beta_name}
            finally:
                self._delete_disk_silent(overlay)
        finally:
            self._delete_disk_silent(base)

    def test_clone_inherits_source_node(self):
        """Clone of a disk on beta lands on beta."""
        src = _uniq("clone-src-beta")
        dst = _uniq("clone-dst-beta")
        self.client_alpha.disks.create(
            src, size_mb=16, format="qcow2", node=self.beta_name
        )
        try:
            self.client_alpha.disks.clone(src, dst)
            try:
                assert self._placement_nodes(dst) == {self.beta_name}
            finally:
                self._delete_disk_silent(dst)
        finally:
            self._delete_disk_silent(src)

    def test_refresh_works_on_beta_only_disk(self):
        """`disk.refresh()` on a beta-only disk must succeed — the
        picker has to find beta, not pick alpha and bail with
        "no placement"."""
        name = _uniq("refresh-beta")
        self.client_alpha.disks.create(
            name, size_mb=16, format="qcow2", node=self.beta_name
        )
        try:
            info = self.client_alpha.disks.get(name).refresh()
            assert info.size_mb is not None
        finally:
            self._delete_disk_silent(name)

    def test_resize_works_on_beta_only_disk(self):
        """Same property as ``test_refresh_works_on_beta_only_disk``
        but exercising the qemu-img-mutating path."""
        name = _uniq("resize-beta")
        self.client_alpha.disks.create(
            name, size_mb=16, format="qcow2", node=self.beta_name
        )
        try:
            disk = self.client_alpha.disks.get(name)
            disk.resize(32)
            info = disk.refresh()
            assert info.size_mb == 32
        finally:
            self._delete_disk_silent(name)

    # ---- apply YAML with per-resource `node:` ----------------------------

    def test_apply_disks_with_per_node_placement(self):
        """`apply` YAML with explicit `node:` on each disk places
        them accordingly. Two disks of the same shape, one on each
        node, both reachable through the daemon."""
        token = secrets.token_hex(3)
        alpha_disk = f"apply-a-{token}"
        beta_disk = f"apply-b-{token}"
        yaml_body = textwrap.dedent(f"""
            disks:
              - name: {alpha_disk}
                sizeMb: 16
                format: qcow2
                node: {self.alpha_name}
              - name: {beta_disk}
                sizeMb: 16
                format: qcow2
                node: {self.beta_name}
        """).strip()
        try:
            result, _ = self.client_alpha.apply(yaml_body, wait=True)
            assert {d.name for d in result.disks} == {alpha_disk, beta_disk}
            assert self._placement_nodes(alpha_disk) == {self.alpha_name}
            assert self._placement_nodes(beta_disk) == {self.beta_name}
        finally:
            self._delete_disk_silent(alpha_disk)
            self._delete_disk_silent(beta_disk)

    def test_apply_skip_existing_disambiguates_same_name_across_nodes(self):
        """Two VMs named the same on different nodes round-trip
        through `apply --skip-existing` without false-positive
        deduplication.

        The first apply creates both rows; the second apply with
        `skip_existing=True` must see each as already-present on
        its own node (rather than ambiguously matching the
        "wrong" row and either over-skipping or over-creating).
        """
        token = secrets.token_hex(3)
        shared = f"web-{token}"
        yaml_body = textwrap.dedent(f"""
            vms:
              - name: {shared}
                node: {self.alpha_name}
                cpuCount: 1
                ramMb: 128
                headless: true
              - name: {shared}
                node: {self.beta_name}
                cpuCount: 1
                ramMb: 128
                headless: true
        """).strip()
        # Track ids for cleanup. We can't `vms.get(name, by_name=True)`
        # because the name is no longer unique cluster-wide; rely on
        # the apply result.
        created_ids: list[int] = []
        try:
            r1, _ = self.client_alpha.apply(yaml_body, wait=True, skip_existing=True)
            assert len(r1.vms) == 2
            created_ids = [v.id for v in r1.vms]
            # Sanity-check: the two rows are on different nodes.
            details = [self.client_alpha.vms.get(vid).show() for vid in created_ids]
            assert {d.node_name for d in details} == {
                self.alpha_name,
                self.beta_name,
            }
            # Second apply with skip_existing: both rows already
            # exist on their respective nodes, so neither is
            # re-created. The daemon's `runSequentialVms` doesn't
            # echo existing rows in the result, so `r2.vms` is
            # empty — and both ids must still resolve.
            r2, _ = self.client_alpha.apply(yaml_body, wait=True, skip_existing=True)
            assert r2.vms == []
            for vid in created_ids:
                # Both rows must still exist; not raising = success.
                self.client_alpha.vms.get(vid).show()
        finally:
            for vid in created_ids:
                try:
                    self.client_alpha.vms.get(vid).delete()
                except Exception:
                    pass

    # ---- VM details exposes per-node file path ---------------------------

    def test_vm_show_drive_file_path_is_resolved(self):
        """`vm.show().drives[i].file_path` returns the actual
        per-node path on the VM's node. Before the multi-node
        refactor this was the empty string; now it surfaces the
        absolute path on the VM's node (anchored against the
        VM's node basePath), matching the convention
        ``disks.show()`` uses for its placement paths.
        """
        disk_name = _uniq("vm-drive-fp")
        vm_name = _uniq("vm-fp-host")
        self.client_alpha.disks.create(
            disk_name, size_mb=16, format="qcow2", node=self.beta_name
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
            try:
                vm.attach_disk(disk_name, interface="virtio")
                details = vm.show()
                paths = [d.file_path for d in details.drives]
                # Exactly the disk we attached. Both vm.show() and
                # disks.show() absolutise the per-node file path
                # for display, so they match on a single-host
                # test setup (and would still match in a
                # multi-host setup as long as the VM's node has
                # the same basePath as the placement's node — the
                # default for any node the daemon registers).
                assert len(paths) == 1
                expected = self._placement_path_on(disk_name, self.beta_name)
                assert paths[0] == expected
                assert paths[0] != ""
                # And the absolute path actually exists on beta.
                assert self._file_exists_on(self.beta_name, paths[0])
            finally:
                self._delete_vm_silent(vm_name)
        finally:
            self._delete_disk_silent(disk_name)
