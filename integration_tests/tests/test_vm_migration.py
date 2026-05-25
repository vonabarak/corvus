"""Integration tests for `vm.migrate` (offline VM migration).

The orchestrator is exercised end-to-end: bytes flow agent-to-agent
between alpha (source) and beta (destination), the daemon flips
`Vm.nodeId` in a single transaction, and the migrated VM is
expected to boot on the destination using the freshly-transferred
disk. See `doc/vm-migration.md` for the design and constraint list.

Topology: `OneDaemonTwoNodesCase`. Alpha runs the daemon + agents;
beta runs agents only. The class fixture below registers beta with
alpha's daemon.

The boot-on-destination tests (`test_migrate_minimal_vm_and_boot`
and `test_migrated_vm_with_cloud_init_boots_on_destination`) are
deliberately the only two that actually start QEMU; the rest are
fast state/DB checks so the class's wall-clock time stays bounded.
"""

from __future__ import annotations

import secrets
import time

import pytest
from corvus_client import ServerError
from corvus_test_harness import OneDaemonTwoNodesCase


def _uniq(stem: str) -> str:
    return f"{stem}-{secrets.token_hex(3)}"


def _qemu_count(node, vm_name: str) -> int:
    """Count qemu-system processes on `node` whose argv mentions `vm_name`.

    Mirrors the helper in `test_multi_node.py`. The `^qemu-system`
    anchor keeps the shell that wraps this very pgrep call from
    matching its own argv.
    """
    r = node.run(
        f"pgrep -af '^qemu-system.*-name {vm_name},' || true",
        check=False,
        timeout_sec=10.0,
    )
    out = r.stdout.decode("utf-8", errors="replace").strip()
    return sum(1 for line in out.splitlines() if line.strip())


def _poll_until(cond, *, timeout_sec: float, msg: str, poll_sec: float = 0.5) -> None:
    deadline = time.monotonic() + timeout_sec
    while time.monotonic() < deadline:
        if cond():
            return
        time.sleep(poll_sec)
    raise AssertionError(f"{msg} (waited {timeout_sec}s)")


def _retry_start(vm, *, attempts: int = 30, sleep_sec: float = 1.0) -> None:
    """Tolerate transient 'nodeagent unavailable' on freshly migrated VMs."""
    last: ServerError | None = None
    for _ in range(attempts):
        try:
            vm.start(wait=False)
            return
        except ServerError as e:
            last = e
            if "unavailable" not in str(e).lower():
                raise
            time.sleep(sleep_sec)
    raise AssertionError(f"vm.start failed after {attempts} attempts: {last}")


def _wait_until_node_ready(client, node_name: str, *, timeout_sec: float = 30.0):
    """Same supervisor-readiness probe as in test_disk_copy_move.py."""
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


def _wait_until_node_stats(client, node_name: str, *, timeout_sec: float = 30.0):
    """Stricter readiness probe — wait until the agent has pushed a
    full stats snapshot (so `ram_mb_free` is populated). Required by
    tests that depend on the capacity gate refusing (the gate is
    optimistic when stats are still NULL)."""
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


class TestVmMigration(OneDaemonTwoNodesCase):
    """End-to-end `vm.migrate` against the two-node fixture."""

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
                description="alpha→beta vm-migration tests",
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

    # ---- common per-test plumbing ------------------------------------------

    def _delete_silent_disk(self, name: str) -> None:
        try:
            self.client_alpha.disks.get(name).delete()
        except Exception:
            pass

    def _delete_silent_vm(self, name: str) -> None:
        try:
            self.client_alpha.vms.get(name).delete()
        except Exception:
            pass

    def _placement_nodes(self, disk_name: str) -> set[str]:
        info = self.client_alpha.disks.get(disk_name).show()
        return {p.node_name for p in info.placements}

    def _assert_migrate_fails(self, vm, to_node: str, *, message_must_match):
        """Drive vm.migrate(to_node) and assert the resulting task
        ends in ``error`` with a message matching any of the
        substrings in ``message_must_match``. Mirrors the
        equivalent helper in test_disk_copy_move.py — vm.migrate is
        an async taskId-returning RPC, so the failure surfaces via
        ``wait_for_task`` rather than synchronously from the
        ``migrate`` call itself.
        """
        tid = vm.migrate(to_node)
        with pytest.raises(AssertionError) as ei:
            self.wait_for_task(self.client_alpha, tid, timeout_sec=30.0)
        lowered = str(ei.value).lower()
        assert any(p.lower() in lowered for p in message_must_match), (
            f"unexpected migrate task error message: {ei.value!r} "
            f"(expected one of: {message_must_match!r})"
        )

    def _make_bootable_vm(
        self,
        vm_name: str,
        overlay_name: str,
        *,
        guest_agent: bool = True,
    ):
        """Build a bootable VM on alpha by creating a qcow2 overlay
        on top of the standard `corvus-test-vm` base image (Alpine
        + qemu-guest-agent + vsock-sshd, baked under the `alpine`
        key in ``base_images.discover``). Suitable for tests that
        need the VM to actually boot, run QGA, and let
        ``vm.start(wait=True)`` return.

        Returns the ``Vm`` capability. Cleanup is the caller's
        job (``self._delete_silent_vm(vm_name)`` +
        ``self._delete_silent_disk(overlay_name)``).
        """
        images = self.register_base_images()
        base_disk = images.get("alpine")
        if base_disk is None:
            pytest.skip(
                "alpine (corvus-test-vm) base image not registered — "
                "run `make test-image-vm` to bake it on the host"
            )
        self.client_alpha.disks.create_overlay(overlay_name, base_disk)
        vm = self.client_alpha.vms.create(
            vm_name,
            cpu_count=1,
            ram_mb=512,
            node=self.alpha_name,
            headless=True,
            guest_agent=guest_agent,
            cloud_init=False,
        )
        vm.attach_disk(overlay_name, interface="virtio")
        return vm

    def _make_stopped_vm_with_disk(
        self,
        vm_name: str,
        disk_name: str,
        *,
        size_mb: int = 16,
        cloud_init: bool = False,
        guest_agent: bool = False,
        attach_disk: bool = True,
    ):
        """Convenience: create a small disk + a VM on alpha, attach
        the disk, leave the VM in `stopped`. Returns the `Vm`
        client capability.
        """
        self.client_alpha.disks.create(disk_name, size_mb=size_mb, format="qcow2")
        vm = self.client_alpha.vms.create(
            vm_name,
            cpu_count=1,
            ram_mb=128,
            node=self.alpha_name,
            headless=True,
            guest_agent=guest_agent,
            cloud_init=cloud_init,
        )
        if attach_disk:
            vm.attach_disk(disk_name, interface="virtio")
        return vm

    # ---- happy paths -------------------------------------------------------

    def test_migrate_minimal_vm_and_boot(self):
        """End-to-end: create a stopped VM on alpha, migrate to beta,
        assert the boot disk's placement moved, then start the VM
        on the destination and verify QEMU spawned only on beta."""
        vm_name = _uniq("mig-boot")
        disk_name = _uniq("mig-boot-disk")
        vm = self._make_stopped_vm_with_disk(vm_name, disk_name)
        try:
            tid = vm.migrate(self.beta_name)
            self.wait_for_task(self.client_alpha, tid, timeout_sec=120.0)
            # r/w drive must have moved to beta (and only beta).
            assert self._placement_nodes(disk_name) == {self.beta_name}
            # VSOCK + SPICE cleared so the next start re-allocates
            # against beta's pools.
            details = vm.show()
            assert details.vsock_cid is None
            assert details.spice_port is None
            # Bring the VM up on beta — qemu must spawn on beta,
            # and NOT on alpha.
            _retry_start(vm)
            _poll_until(
                lambda: _qemu_count(self.node_beta, vm_name) == 1,
                timeout_sec=15.0,
                msg=f"qemu for {vm_name!r} did not spawn on beta",
            )
            assert _qemu_count(self.node_alpha, vm_name) == 0, (
                f"qemu for {vm_name!r} leaked onto alpha after migration"
            )
            vm.reset()
            _poll_until(
                lambda: _qemu_count(self.node_beta, vm_name) == 0,
                timeout_sec=15.0,
                msg=f"qemu for {vm_name!r} did not exit on beta after reset",
            )
        finally:
            self._delete_silent_vm(vm_name)
            self._delete_silent_disk(disk_name)

    def test_migrate_with_user_netif(self):
        """A VM with a `user`-type NIC migrates successfully and the
        NIC follows."""
        vm_name = _uniq("mig-net")
        disk_name = _uniq("mig-net-disk")
        vm = self._make_stopped_vm_with_disk(vm_name, disk_name)
        try:
            vm.add_net_if(type="user")
            tid = vm.migrate(self.beta_name)
            self.wait_for_task(self.client_alpha, tid, timeout_sec=120.0)
            assert self._placement_nodes(disk_name) == {self.beta_name}
            details = vm.show()
            assert len(details.net_ifs) == 1
            assert details.net_ifs[0].type == "user"
        finally:
            self._delete_silent_vm(vm_name)
            self._delete_silent_disk(disk_name)

    def test_migrate_ro_drive_copied_rw_drive_moved(self):
        """A VM with one r/w boot disk + one r/o data disk migrates:
        the r/w disk's placement moves, the r/o disk's is copied."""
        vm_name = _uniq("mig-mix")
        rw_disk = _uniq("mig-mix-rw")
        ro_disk = _uniq("mig-mix-ro")
        vm = self._make_stopped_vm_with_disk(vm_name, rw_disk)
        try:
            self.client_alpha.disks.create(ro_disk, size_mb=8, format="qcow2")
            try:
                vm.attach_disk(ro_disk, interface="virtio", read_only=True)
                tid = vm.migrate(self.beta_name)
                self.wait_for_task(self.client_alpha, tid, timeout_sec=120.0)
                # r/w → moved.
                assert self._placement_nodes(rw_disk) == {self.beta_name}
                # r/o → copied. Both placements stay.
                assert self._placement_nodes(ro_disk) == {
                    self.alpha_name,
                    self.beta_name,
                }
            finally:
                self._delete_silent_disk(ro_disk)
        finally:
            self._delete_silent_vm(vm_name)
            self._delete_silent_disk(rw_disk)

    def test_migrate_clears_vsock_and_spice(self):
        """A VM that was started once on alpha (allocating its
        VSOCK CID) has that field cleared after migration; the
        next start re-allocates against beta.

        Uses the standard `corvus-test-vm` Alpine image so the
        guest actually boots and runs qemu-guest-agent — the
        daemon needs the first QGA ping to land before it
        transitions the row to ``running`` and allocates the
        VSOCK CID. With a non-bootable disk QGA never replies and
        ``vm.start(wait=True)`` hangs to the daemon timeout
        without ever producing the "ran once with vsock_cid set"
        state this test asserts on.

        SPICE port allocation is opt-in (headless VMs don't get
        one); this test runs headless so spice_port is None both
        before and after — we still assert it stays None.
        """
        vm_name = _uniq("mig-alloc")
        overlay_name = _uniq("mig-alloc-ovl")
        vm = self._make_bootable_vm(vm_name, overlay_name, guest_agent=True)
        try:
            # ``wait=True`` makes the daemon block until the agent
            # reports the first QGA ping — when this returns, the
            # row's status is committed as ``running`` and the
            # VSOCK CID has been re-validated against the host
            # kernel.
            vm.start(wait=True)
            _poll_until(
                lambda: _qemu_count(self.node_alpha, vm_name) == 1,
                timeout_sec=15.0,
                msg=f"qemu for {vm_name!r} did not spawn on alpha",
            )
            details_before = vm.show()
            # With guest_agent=True + a guest that runs qemu-ga,
            # the daemon's per-node allocator always assigns a
            # CID; if vsock_cid is None here something is wrong
            # with the test infra (no /dev/vhost-vsock on the
            # node kernel) and the assertions below would be
            # vacuous. Fail fast with a clear message.
            assert details_before.vsock_cid is not None, (
                f"VM {vm_name!r} started with guest_agent=True but no "
                f"vsock_cid was allocated; node kernel may be missing "
                f"/dev/vhost-vsock"
            )
            # Cleanly stop so we can migrate. Use reset (hard
            # kill) — reset commits status=stopped synchronously,
            # works from any state, and doesn't depend on the
            # guest acking ACPI.
            vm.reset()
            _poll_until(
                lambda: _qemu_count(self.node_alpha, vm_name) == 0,
                timeout_sec=15.0,
                msg=f"qemu for {vm_name!r} did not exit on alpha after reset",
            )
            _poll_until(
                lambda: vm.show().status == "stopped",
                timeout_sec=15.0,
                msg=f"vm.show().status for {vm_name!r} did not settle on stopped",
            )
            # Migrate.
            tid = vm.migrate(self.beta_name)
            self.wait_for_task(self.client_alpha, tid, timeout_sec=300.0)
            details_after = vm.show()
            # The orchestrator must clear both fields; the next
            # ``vm.start`` will re-allocate against beta's per-node
            # pools.
            assert details_after.vsock_cid is None, (
                f"vsock_cid still set after migration: {details_after.vsock_cid}"
            )
            assert details_after.spice_port is None
            # The pre-migration vsock_cid value was set; the post
            # one is cleared — proves the orchestrator nulled it
            # rather than incidentally returning the same value.
            assert details_before.vsock_cid != details_after.vsock_cid
        finally:
            self._delete_silent_vm(vm_name)
            self._delete_silent_disk(overlay_name)

    # ---- refusals ----------------------------------------------------------

    def test_migrate_refuses_running_vm(self):
        """A running VM can't be migrated."""
        vm_name = _uniq("mig-run")
        disk_name = _uniq("mig-run-disk")
        vm = self._make_stopped_vm_with_disk(vm_name, disk_name)
        try:
            _retry_start(vm)
            _poll_until(
                lambda: _qemu_count(self.node_alpha, vm_name) == 1,
                timeout_sec=15.0,
                msg=f"qemu for {vm_name!r} did not spawn on alpha",
            )
            try:
                self._assert_migrate_fails(
                    vm,
                    self.beta_name,
                    message_must_match=["stopped", "another migration"],
                )
            finally:
                vm.reset()
                _poll_until(
                    lambda: _qemu_count(self.node_alpha, vm_name) == 0,
                    timeout_sec=15.0,
                    msg=f"qemu for {vm_name!r} did not exit on alpha after reset",
                )
        finally:
            self._delete_silent_vm(vm_name)
            self._delete_silent_disk(disk_name)

    def test_migrate_refuses_shared_dir(self):
        """A VM with a shared directory can't be migrated."""
        vm_name = _uniq("mig-shdir")
        disk_name = _uniq("mig-shdir-disk")
        vm = self._make_stopped_vm_with_disk(vm_name, disk_name)
        try:
            vm.add_shared_dir("/tmp", tag="dummy", read_only=True)
            self._assert_migrate_fails(
                vm,
                self.beta_name,
                message_must_match=["shared director"],
            )
        finally:
            self._delete_silent_vm(vm_name)
            self._delete_silent_disk(disk_name)

    def test_migrate_refuses_managed_nic(self):
        """A VM with a tap/bridge/macvtap NIC can't be migrated.
        We use `type='tap'` with a dummy hostDevice — the daemon's
        add-netif accepts it; the migrate pre-check rejects it."""
        vm_name = _uniq("mig-tap")
        disk_name = _uniq("mig-tap-disk")
        vm = self._make_stopped_vm_with_disk(vm_name, disk_name)
        try:
            vm.add_net_if(type="tap", host_device="corvus-tap-xxxx")
            self._assert_migrate_fails(
                vm,
                self.beta_name,
                message_must_match=["non-user", "network interface"],
            )
        finally:
            self._delete_silent_vm(vm_name)
            self._delete_silent_disk(disk_name)

    def test_migrate_refuses_target_equals_source(self):
        vm_name = _uniq("mig-self")
        disk_name = _uniq("mig-self-disk")
        vm = self._make_stopped_vm_with_disk(vm_name, disk_name)
        try:
            self._assert_migrate_fails(
                vm,
                self.alpha_name,
                message_must_match=["current node"],
            )
        finally:
            self._delete_silent_vm(vm_name)
            self._delete_silent_disk(disk_name)

    def test_migrate_refuses_undersized_ram(self):
        """A VM whose RAM requirement exceeds beta's free RAM is
        refused. We use an absurdly large ram_mb so the request
        outsizes any plausible nested-VM host.

        The capacity check on the destination is optimistic until
        the agent has pushed at least one stats snapshot — wait
        for that explicitly so the assertion isn't flaky on a
        fresh registration.
        """
        _wait_until_node_stats(self.client_alpha, self.beta_name)
        vm_name = _uniq("mig-ram")
        disk_name = _uniq("mig-ram-disk")
        self.client_alpha.disks.create(disk_name, size_mb=16, format="qcow2")
        try:
            vm = self.client_alpha.vms.create(
                vm_name,
                cpu_count=1,
                ram_mb=1024 * 1024,  # 1 TiB — guaranteed too big
                node=self.alpha_name,
                headless=True,
                guest_agent=False,
                cloud_init=False,
            )
            try:
                vm.attach_disk(disk_name, interface="virtio")
                self._assert_migrate_fails(
                    vm,
                    self.beta_name,
                    message_must_match=["insufficient", "ram"],
                )
            finally:
                try:
                    vm.delete()
                except Exception:
                    pass
        finally:
            self._delete_silent_disk(disk_name)

    def test_migrated_vm_with_cloud_init_boots_on_destination(self):
        """A VM created with `cloud_init=True` carries its cloud-init
        ISO across the migration (the daemon's per-VM ISO is a
        normal `DiskImage` row attached r/o, so it follows the
        copy rule). After migration the VM still boots on beta.
        """
        vm_name = _uniq("mig-ci")
        disk_name = _uniq("mig-ci-disk")
        vm = self._make_stopped_vm_with_disk(vm_name, disk_name, cloud_init=True)
        try:
            # Start once on alpha so the cloud-init ISO disk is
            # materialised (it's lazily generated by the agent at
            # first start). Stop afterwards. ``wait=False`` is
            # fine here because cloud-init disks attach BEFORE
            # qemu spawns — the registration happens inside the
            # daemon's start path before agent.vmStart returns.
            _retry_start(vm)
            _poll_until(
                lambda: _qemu_count(self.node_alpha, vm_name) == 1,
                timeout_sec=15.0,
                msg=f"qemu for {vm_name!r} did not spawn on alpha",
            )
            vm.reset()
            _poll_until(
                lambda: _qemu_count(self.node_alpha, vm_name) == 0,
                timeout_sec=15.0,
                msg=f"qemu for {vm_name!r} did not exit on alpha after reset",
            )
            # Wait for the status row to settle on `stopped` —
            # without this poll the still-running async start
            # task can overwrite reset's status (see the
            # comment in test_migrate_clears_vsock_and_spice).
            _poll_until(
                lambda: vm.show().status == "stopped",
                timeout_sec=15.0,
                msg=f"vm.show().status for {vm_name!r} did not settle on stopped",
            )
            # The cloud-init ISO disk is named `<vmName>-cloud-init`
            # (see Corvus.Handlers.CloudInit.ensureCloudInitDiskRegistered).
            ci_disk_name = f"{vm_name}-cloud-init"
            # Pre-migrate: cloud-init disk is on alpha only.
            assert self.alpha_name in self._placement_nodes(ci_disk_name)
            tid = vm.migrate(self.beta_name)
            self.wait_for_task(self.client_alpha, tid, timeout_sec=120.0)
            # The cloud-init ISO is read-only → copied. It must now
            # exist on beta too.
            ci_placements = self._placement_nodes(ci_disk_name)
            assert self.beta_name in ci_placements, (
                f"cloud-init disk not present on beta after migration: "
                f"{ci_placements!r}"
            )
            # The r/w boot disk moved.
            assert self._placement_nodes(disk_name) == {self.beta_name}
            # And the VM boots on beta.
            _retry_start(vm)
            _poll_until(
                lambda: _qemu_count(self.node_beta, vm_name) == 1,
                timeout_sec=15.0,
                msg=f"qemu for {vm_name!r} did not spawn on beta",
            )
            vm.reset()
            _poll_until(
                lambda: _qemu_count(self.node_beta, vm_name) == 0,
                timeout_sec=15.0,
                msg=f"qemu for {vm_name!r} did not exit on beta after reset",
            )
        finally:
            self._delete_silent_vm(vm_name)
            self._delete_silent_disk(disk_name)
            # The cloud-init disk is ephemeral and normally reaped by
            # vm.delete; this is a best-effort cleanup for the case
            # where vm.delete didn't run (e.g. the test failed before
            # we got that far).
            self._delete_silent_disk(f"{vm_name}-cloud-init")
