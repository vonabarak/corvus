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
        return {p.node.name for p in info.placements}

    def _placement_path_on(self, disk_name: str, node_name: str) -> str:
        info = self.client_alpha.disks.get(disk_name).show()
        for p in info.placements:
            if p.node.name == node_name:
                return p.file_path
        raise AssertionError(
            f"disk {disk_name!r} has no placement on node {node_name!r}; "
            f"placements: {[p.node.name for p in info.placements]!r}"
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
        placements exist, both files present, bytes identical.

        Storage-form invariant: with no ``to_path``, the daemon
        preserves the source's stored filePath verbatim — alpha's
        placement and beta's placement record the SAME relative
        string. Before the path-preservation fix this happened to
        be true by coincidence (default disks live at
        ``<name>.qcow2``); the assertion now makes it
        load-bearing so a regression flips the test."""
        name = _uniq("copy")
        self.client_alpha.disks.create(name, size_mb=16, format="qcow2")
        try:
            assert self._placement_nodes(name) == {self.alpha_name}
            tid = self.client_alpha.disks.copy(name, self.beta_name)
            self.wait_for_task(self.client_alpha, tid, timeout_sec=60.0)
            assert self._placement_nodes(name) == {self.alpha_name, self.beta_name}
            info = self.client_alpha.disks.get(name).show()
            stored_by_node = {p.node.name: p.file_path for p in info.placements}
            # The daemon stores filePath relative-to-basePath; for
            # a freshly created disk that means just the basename
            # — identical on both nodes.
            assert stored_by_node[self.alpha_name] == stored_by_node[self.beta_name]
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

    def _assert_transfer_fails(
        self,
        op,
        disk_name: str,
        to_node: str,
        *,
        message_must_match,
    ):
        """Run op(disk_name, to_node) (where op is disks.copy or
        disks.move) and assert the resulting task ends in
        ``error`` whose message contains any substring from
        ``message_must_match`` (case-insensitive).

        The daemon models copy/move as taskId-returning RPCs, so
        the synchronous return is always a numeric task id even
        when validation will reject the operation. The error
        materialises asynchronously in the task's ``message`` /
        ``result`` fields.
        """
        tid = op(disk_name, to_node)
        with pytest.raises(AssertionError) as ei:
            self.wait_for_task(self.client_alpha, tid, timeout_sec=30.0)
        lowered = str(ei.value).lower()
        assert any(p.lower() in lowered for p in message_must_match), (
            f"unexpected task error message: {ei.value!r} "
            f"(expected one of: {message_must_match!r})"
        )

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
                self._assert_transfer_fails(
                    self.client_alpha.disks.copy,
                    disk_name,
                    self.beta_name,
                    message_must_match=["read-write", "vm migrate"],
                )
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
                self._assert_transfer_fails(
                    self.client_alpha.disks.move,
                    disk_name,
                    self.beta_name,
                    message_must_match=["read-write", "vm migrate"],
                )
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
                self._assert_transfer_fails(
                    self.client_alpha.disks.move,
                    disk_name,
                    self.beta_name,
                    message_must_match=["can only be copied", "read-only"],
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
            self._assert_transfer_fails(
                self.client_alpha.disks.copy,
                name,
                self.beta_name,
                message_must_match=["already has a placement"],
            )
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
                self._assert_transfer_fails(
                    self.client_alpha.disks.copy,
                    overlay_name,
                    self.beta_name,
                    message_must_match=["backing"],
                )
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

    def test_copy_with_backing_chain_stages_all_ancestors(self):
        """``disks.copy(top, beta, with_backing_chain=True)`` walks
        the backing chain and copies every missing ancestor to beta
        before transferring the top overlay. Mirrors vm.migrate's
        chain-auto-copy contract — uses synthetic 16 MiB disks for
        determinism.

        Two-level chain (base ← mid ← top) so the test proves the
        recursion actually recurses; a single-parent overlay
        wouldn't distinguish the recursive walk from the existing
        one-step path.
        """
        base_name = _uniq("wbc-base")
        mid_name = _uniq("wbc-mid")
        top_name = _uniq("wbc-top")
        self.client_alpha.disks.create(base_name, size_mb=16, format="qcow2")
        try:
            self.client_alpha.disks.create_overlay(mid_name, base_name)
            try:
                self.client_alpha.disks.create_overlay(top_name, mid_name)
                try:
                    # Pre-condition: every level alpha-only.
                    assert self._placement_nodes(base_name) == {self.alpha_name}
                    assert self._placement_nodes(mid_name) == {self.alpha_name}
                    assert self._placement_nodes(top_name) == {self.alpha_name}
                    tid = self.client_alpha.disks.copy(
                        top_name, self.beta_name, with_backing_chain=True
                    )
                    self.wait_for_task(self.client_alpha, tid, timeout_sec=90.0)
                    # Every level landed on beta. Source stays.
                    assert self._placement_nodes(base_name) == {
                        self.alpha_name,
                        self.beta_name,
                    }
                    assert self._placement_nodes(mid_name) == {
                        self.alpha_name,
                        self.beta_name,
                    }
                    assert self._placement_nodes(top_name) == {
                        self.alpha_name,
                        self.beta_name,
                    }
                finally:
                    self._delete_silent(top_name)
            finally:
                self._delete_silent(mid_name)
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
                self._assert_transfer_fails(
                    self.client_alpha.disks.copy,
                    name,
                    self.beta_name,
                    message_must_match=["not online"],
                )
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
            self._assert_transfer_fails(
                self.client_alpha.disks.copy,
                name,
                self.alpha_name,
                # The daemon's exact wording is "no source
                # placement available (target is the only node
                # hosting this disk)" — match either keyword.
                message_must_match=["target", "no source"],
            )
        finally:
            self._delete_silent(name)

    # ----------------------------------------------------------------------
    # --to-path support + path-preservation + collision-guard tests.
    #
    # Each test's docstring tags the numbered issue from the
    # original change request (1-4) so a future regression is easy
    # to trace back to the requirement that caused the test.
    # ----------------------------------------------------------------------

    def _alpha_base(self) -> str:
        return self.client_alpha.nodes.get(self.alpha_name).show().base_path

    def _beta_base(self) -> str:
        return self.client_alpha.nodes.get(self.beta_name).show().base_path

    def _stage_qcow2(self, node, path: str, size_mb: int = 4) -> None:
        """Create a real qcow2 file at `path` on `node` (parent
        directory is created if missing)."""
        parent = path.rsplit("/", 1)[0] if "/" in path else "."
        node.run(f"mkdir -p {parent!r}", check=True, timeout_sec=10.0)
        node.run(
            f"qemu-img create -f qcow2 {path!r} {size_mb}M",
            check=True,
            timeout_sec=15.0,
        )

    def _expect_task_error(
        self,
        task_id: int,
        message_must_match,
        *,
        timeout_sec: float = 30.0,
    ) -> None:
        """Variant of `_assert_transfer_fails` that takes the
        already-returned `task_id` (so callers that pass kwargs
        like ``to_path`` to ``disks.copy`` can drive the failure
        check)."""
        with pytest.raises(AssertionError) as ei:
            self.wait_for_task(self.client_alpha, task_id, timeout_sec=timeout_sec)
        lowered = str(ei.value).lower()
        assert any(p.lower() in lowered for p in message_must_match), (
            f"unexpected task error message: {ei.value!r} "
            f"(expected one of: {message_must_match!r})"
        )

    # ---- Issue 1: --to-path accepted for copy + move ---------------------

    def test_copy_with_to_path_relative(self):
        """[Issue 1] `to_path="staging/x.qcow2"` lands the copy
        under `<beta.basePath>/staging/`.

        Note: ``disks.show()`` absolutises every placement's
        file_path for display (see
        ``Handlers/Disk.absolutizeDiskFilePath``), so the
        assertion compares against the resolved absolute path —
        the relative storage form is verified indirectly by
        checking the file lives at ``<beta.basePath>/staging/x.qcow2``.
        """
        name = _uniq("tp-rel")
        self.client_alpha.disks.create(name, size_mb=16, format="qcow2")
        try:
            tid = self.client_alpha.disks.copy(
                name, self.beta_name, to_path="staging/x.qcow2"
            )
            self.wait_for_task(self.client_alpha, tid, timeout_sec=60.0)
            beta_abs = f"{self._beta_base()}/staging/x.qcow2"
            assert self._placement_path_on(name, self.beta_name) == beta_abs
            assert self._file_exists(self.node_beta, beta_abs)
        finally:
            self._delete_silent(name)

    def test_copy_with_to_path_absolute(self):
        """[Issue 1] An absolute `to_path` is honoured verbatim
        and the placement records the absolute string (which
        `resolveDiskPath` will treat as already-absolute on the
        next operation)."""
        name = _uniq("tp-abs")
        abs_dest = f"/tmp/explicit-{name}.qcow2"
        self.client_alpha.disks.create(name, size_mb=16, format="qcow2")
        try:
            tid = self.client_alpha.disks.copy(name, self.beta_name, to_path=abs_dest)
            self.wait_for_task(self.client_alpha, tid, timeout_sec=60.0)
            assert self._placement_path_on(name, self.beta_name) == abs_dest
            assert self._file_exists(self.node_beta, abs_dest)
        finally:
            self.node_beta.run(f"rm -f {abs_dest!r}", check=False, timeout_sec=5.0)
            self._delete_silent(name)

    def test_move_with_to_path_relative(self):
        """[Issue 1] Move with `to_path`: source row + file gone,
        destination at the requested path."""
        name = _uniq("mv-tp")
        self.client_alpha.disks.create(name, size_mb=16, format="qcow2")
        try:
            # `disks.show()` returns the placement path already
            # absolutised, so it's safe to use directly with `test -f`.
            alpha_abs = self._placement_path_on(name, self.alpha_name)
            tid = self.client_alpha.disks.move(
                name, self.beta_name, to_path="moved/y.qcow2"
            )
            self.wait_for_task(self.client_alpha, tid, timeout_sec=60.0)
            assert self._placement_nodes(name) == {self.beta_name}
            beta_abs = f"{self._beta_base()}/moved/y.qcow2"
            assert self._placement_path_on(name, self.beta_name) == beta_abs
            assert self._file_exists(self.node_beta, beta_abs)
            assert not self._file_exists(self.node_alpha, alpha_abs), (
                "source file still present after move with --to-path"
            )
        finally:
            self._delete_silent(name)

    # ---- Issue 2: absolute source path mandates --to-path ----------------

    def test_copy_absolute_source_requires_to_path(self):
        """[Issue 2] A disk registered with an absolute path
        outside `basePath` cannot be copied without `to_path` —
        the daemon refuses with a clear message naming both
        'absolute' and '--to-path'."""
        name = _uniq("abs-need")
        abs_src = f"/tmp/abs-{name}.qcow2"
        self._stage_qcow2(self.node_alpha, abs_src)
        try:
            self.client_alpha.disks.register(
                name, abs_src, format="qcow2", node=self.alpha_name
            )
            try:
                # No `to_path`: the daemon must refuse via the
                # async task error mechanism.
                tid = self.client_alpha.disks.copy(name, self.beta_name)
                self._expect_task_error(
                    tid, message_must_match=["absolute", "--to-path"]
                )
                # Source placement intact.
                assert self._placement_nodes(name) == {self.alpha_name}
            finally:
                self._delete_silent(name)
        finally:
            self.node_alpha.run(f"rm -f {abs_src!r}", check=False, timeout_sec=5.0)

    def test_move_absolute_source_requires_to_path(self):
        """[Issue 2] Same property for move."""
        name = _uniq("mv-abs-need")
        abs_src = f"/tmp/abs-{name}.qcow2"
        self._stage_qcow2(self.node_alpha, abs_src)
        try:
            self.client_alpha.disks.register(
                name, abs_src, format="qcow2", node=self.alpha_name
            )
            try:
                tid = self.client_alpha.disks.move(name, self.beta_name)
                self._expect_task_error(
                    tid, message_must_match=["absolute", "--to-path"]
                )
                # Source row + file still present.
                assert self._placement_nodes(name) == {self.alpha_name}
                assert self._file_exists(self.node_alpha, abs_src)
            finally:
                self._delete_silent(name)
        finally:
            self.node_alpha.run(f"rm -f {abs_src!r}", check=False, timeout_sec=5.0)

    def test_copy_absolute_source_with_to_path_succeeds(self):
        """[Issue 2] An absolute source disk *can* be copied as
        long as the operator supplies `--to-path`. The result is a
        clean beta-side placement at the requested path."""
        name = _uniq("abs-ok")
        abs_src = f"/tmp/abs-ok-{name}.qcow2"
        self._stage_qcow2(self.node_alpha, abs_src)
        try:
            self.client_alpha.disks.register(
                name, abs_src, format="qcow2", node=self.alpha_name
            )
            try:
                tid = self.client_alpha.disks.copy(
                    name, self.beta_name, to_path="abs-relocated.qcow2"
                )
                self.wait_for_task(self.client_alpha, tid, timeout_sec=60.0)
                assert self._placement_nodes(name) == {
                    self.alpha_name,
                    self.beta_name,
                }
                beta_abs = f"{self._beta_base()}/abs-relocated.qcow2"
                # `disks.show()` returns the absolutised display
                # form; the relative storage form is verified
                # indirectly by checking the file lives at
                # ``<beta.basePath>/abs-relocated.qcow2``.
                assert self._placement_path_on(name, self.beta_name) == beta_abs
                assert self._file_exists(self.node_beta, beta_abs)
            finally:
                self._delete_silent(name)
        finally:
            self.node_alpha.run(f"rm -f {abs_src!r}", check=False, timeout_sec=5.0)

    # ---- Issue 3: relative path preserved across copy/move ----------------

    def test_copy_preserves_relative_subdir(self):
        """[Issue 3] A disk stored as `sub/foo.qcow2` on alpha
        ends up at `sub/foo.qcow2` (relative to beta's basePath)
        on beta — not flattened to `foo.qcow2`.

        ``disks.show()`` returns absolute paths; the file living
        at ``<beta.basePath>/sub/<name>.qcow2`` is what proves
        the relative storage form was preserved (a flattened
        copy would land at ``<beta.basePath>/<name>.qcow2``)."""
        name = _uniq("rel-pres")
        rel = f"sub/{name}.qcow2"
        alpha_abs = f"{self._alpha_base()}/{rel}"
        beta_abs = f"{self._beta_base()}/{rel}"
        self._stage_qcow2(self.node_alpha, alpha_abs)
        try:
            self.client_alpha.disks.register(
                name, alpha_abs, format="qcow2", node=self.alpha_name
            )
            try:
                # Sanity: alpha's display form is the absolutised
                # version of the relative storage (= alpha_abs).
                assert self._placement_path_on(name, self.alpha_name) == alpha_abs
                tid = self.client_alpha.disks.copy(name, self.beta_name)
                self.wait_for_task(self.client_alpha, tid, timeout_sec=60.0)
                # Beta resolves the SAME relative form against its
                # own basePath — load-bearing assertion: a flatten
                # bug would produce <beta_base>/<name>.qcow2 here.
                assert self._placement_path_on(name, self.beta_name) == beta_abs
                assert self._file_exists(self.node_beta, beta_abs)
            finally:
                self._delete_silent(name)
        finally:
            self.node_alpha.run(
                f"rm -rf {self._alpha_base()}/sub", check=False, timeout_sec=5.0
            )

    def test_move_preserves_relative_subdir(self):
        """[Issue 3] Same property as the copy version, for move."""
        name = _uniq("rel-pres-mv")
        rel = f"sub/{name}.qcow2"
        alpha_abs = f"{self._alpha_base()}/{rel}"
        beta_abs = f"{self._beta_base()}/{rel}"
        self._stage_qcow2(self.node_alpha, alpha_abs)
        try:
            self.client_alpha.disks.register(
                name, alpha_abs, format="qcow2", node=self.alpha_name
            )
            try:
                tid = self.client_alpha.disks.move(name, self.beta_name)
                self.wait_for_task(self.client_alpha, tid, timeout_sec=60.0)
                assert self._placement_nodes(name) == {self.beta_name}
                assert self._placement_path_on(name, self.beta_name) == beta_abs
                assert self._file_exists(self.node_beta, beta_abs)
                # Source file unlinked.
                assert not self._file_exists(self.node_alpha, alpha_abs)
            finally:
                self._delete_silent(name)
        finally:
            self.node_alpha.run(
                f"rm -rf {self._alpha_base()}/sub", check=False, timeout_sec=5.0
            )

    def test_copy_creates_missing_subdir_on_dest(self):
        """[Issue 3 / agent mkdir] When the destination
        subdirectory does not yet exist on beta, `importFromPeer`
        must `mkdir -p` it before opening the writer. Regression
        check for the agent-side `createDirectoryIfMissing` —
        without it, `openBinaryFile` fails with ENOENT."""
        name = _uniq("mkdir-p")
        rel = f"nested/deep/{name}.qcow2"
        alpha_abs = f"{self._alpha_base()}/{rel}"
        self._stage_qcow2(self.node_alpha, alpha_abs)
        try:
            self.client_alpha.disks.register(
                name, alpha_abs, format="qcow2", node=self.alpha_name
            )
            try:
                # Confirm beta has no nested/ dir at all to start.
                check_before = self.node_beta.run(
                    f"test -d {self._beta_base()}/nested",
                    check=False,
                    timeout_sec=5.0,
                )
                assert check_before.returncode != 0, (
                    "test fixture wasn't clean: beta already has nested/"
                )
                tid = self.client_alpha.disks.copy(name, self.beta_name)
                self.wait_for_task(self.client_alpha, tid, timeout_sec=60.0)
                # nested/deep/ must exist and the file must be inside.
                assert self._file_exists(self.node_beta, f"{self._beta_base()}/{rel}")
            finally:
                self._delete_silent(name)
        finally:
            self.node_alpha.run(
                f"rm -rf {self._alpha_base()}/nested",
                check=False,
                timeout_sec=5.0,
            )
            self.node_beta.run(
                f"rm -rf {self._beta_base()}/nested",
                check=False,
                timeout_sec=5.0,
            )

    # ---- Issue 4: collision guard fires cleanly --------------------------

    def test_copy_to_path_collision_refused(self):
        """[Issue 4] A second copy targeting the same `to_path`
        on the same node refuses cleanly with an "already in use"
        error — not via a leaked unique-constraint exception."""
        a = _uniq("col-a")
        b = _uniq("col-b")
        contended = "contended/x.qcow2"
        self.client_alpha.disks.create(a, size_mb=16, format="qcow2")
        self.client_alpha.disks.create(b, size_mb=16, format="qcow2")
        try:
            tid_a = self.client_alpha.disks.copy(a, self.beta_name, to_path=contended)
            self.wait_for_task(self.client_alpha, tid_a, timeout_sec=60.0)
            # The second copy must refuse with a clean error.
            tid_b = self.client_alpha.disks.copy(b, self.beta_name, to_path=contended)
            self._expect_task_error(
                tid_b,
                message_must_match=["already in use", "destination path"],
            )
            # Disk A's beta placement untouched.
            assert self.beta_name in self._placement_nodes(a)
            contended_abs = f"{self._beta_base()}/{contended}"
            assert self._placement_path_on(a, self.beta_name) == contended_abs
        finally:
            self.node_beta.run(
                f"rm -rf {self._beta_base()}/contended",
                check=False,
                timeout_sec=5.0,
            )
            self._delete_silent(a)
            self._delete_silent(b)

    def test_move_to_path_collision_refused(self):
        """[Issue 4] Same property for move."""
        a = _uniq("mv-col-a")
        b = _uniq("mv-col-b")
        contended = "contended-mv/x.qcow2"
        self.client_alpha.disks.create(a, size_mb=16, format="qcow2")
        self.client_alpha.disks.create(b, size_mb=16, format="qcow2")
        try:
            # First move places disk A at the contended path.
            tid_a = self.client_alpha.disks.move(a, self.beta_name, to_path=contended)
            self.wait_for_task(self.client_alpha, tid_a, timeout_sec=60.0)
            # Second move (a different source disk) must refuse.
            tid_b = self.client_alpha.disks.move(b, self.beta_name, to_path=contended)
            self._expect_task_error(
                tid_b,
                message_must_match=["already in use", "destination path"],
            )
            # Disk B's source placement remains intact (move rolled back).
            assert self.alpha_name in self._placement_nodes(b)
        finally:
            self.node_beta.run(
                f"rm -rf {self._beta_base()}/contended-mv",
                check=False,
                timeout_sec=5.0,
            )
            self._delete_silent(a)
            self._delete_silent(b)


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
