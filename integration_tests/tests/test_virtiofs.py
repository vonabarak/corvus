"""Virtiofs shared directories against the inner daemon.

Ports the pre-capnp `VirtiofsIntegrationSpec` (see
`doc/integration-tests-pre-capnp.md`) and adds two extras:

  * **Two simultaneous shared directories** — confirms multiple
    `add_shared_dir` calls coexist; both tags mount and round-trip
    independently inside the guest.
  * **Read-only shared directory** — confirms the daemon honours
    `read_only=True` and the guest cannot write through the mount.

All tests stage their host-side directories on the node (the
inner daemon's filesystem) via `self.node.run(...)`; the Alpine
guest mounts via virtiofs and reads/writes from there.
"""

from __future__ import annotations

import secrets

import pytest

from corvus_test_harness import VmSsh, SingleNodeCase


pytestmark = pytest.mark.slow


def _uniq(stem: str) -> str:
    return f"{stem}-{secrets.token_hex(3)}"


class TestVirtiofs(SingleNodeCase):
    """Virtiofs sharing between the node (hosting the inner daemon's
    filesystem) and the Alpine vm.

    Each test stages its host-side directories on the node via
    `self.node.run(...)`, then drives the guest over the nested SSH
    tunnel from `VmSsh`."""

    # ---- port: bidirectional read+write -----------------------------------

    def test_virtiofs_bidirectional(self):
        """Host (node) writes a file under a shared dir; guest reads
        it through virtiofs. Guest writes under the same dir; host
        reads it back. Round-trip both ways through the same tag."""
        host_uuid = secrets.token_hex(6)
        outer_path = f"/tmp/virtiofs-bidi-{secrets.token_hex(4)}"
        self.node.run(f"mkdir -p {outer_path}")
        self.node.run(f"echo UUID:{host_uuid} > {outer_path}/testfile.txt")
        try:

            class _Bidi(VmSsh):
                def _shared_dirs(_self):
                    return [{"path": outer_path, "tag": "share"}]

            with _Bidi(self) as vm:
                # Mount inside guest. doas is configured for corvus
                # (see Alpine image YAML); the mount type is
                # `virtiofs` and the source is the shared-dir tag.
                vm.run("doas mkdir -p /mnt/share")
                vm.run("doas mount -t virtiofs share /mnt/share")

                # Read host content through the mount.
                r = vm.run("cat /mnt/share/testfile.txt")
                assert r.stdout.strip() == f"UUID:{host_uuid}", r

                # Write guest content to the mount.
                guest_marker = f"WRITTEN-BY-GUEST:{secrets.token_hex(4)}"
                vm.run(f"doas sh -c 'echo {guest_marker} > /mnt/share/guest-file.txt'")

            # After the with block: VM is torn down by VmSsh.
            # The guest write should have been persisted on the
            # node's directory.
            r = self.node.run(f"cat {outer_path}/guest-file.txt")
            assert r.stdout.decode().strip() == guest_marker
        finally:
            self.node.run(f"rm -rf {outer_path}", check=False)

    # ---- port: missing-path error surfaces via subtask --------------------

    def test_shared_dir_missing_path_fails(self):
        """Adding a shared dir whose host path does not exist must
        cause `vm.start` to fail. After the Phase 4 VM-abstraction
        refactor virtiofsd is spawned inline by the agent's
        `vmStart` (not as a separate daemon-side subtask), so the
        failure surfaces as a `RespError` from the parent `start`
        task and the `vm` row lands in `VmError`. Cleanup is done
        by hand because we can't go through the Vm context manager
        (its `__enter__` would tear the VM down on the start
        failure and we'd lose the entity id we need for the task
        lookup)."""
        images = self.register_base_images()
        base = images.get("alpine")
        if base is None:
            pytest.skip("alpine base image not registered")

        name = _uniq("virtiofs-missing")
        bad_path = f"/tmp/nonexistent-virtiofs-{secrets.token_hex(4)}"
        self.client.disks.create_overlay(name, base)
        try:
            vm = self.client.vms.create(
                name,
                cpu_count=1,
                ram_mb=512,
                headless=True,
                guest_agent=False,
            )
            try:
                vm.attach_disk(name, interface="virtio")
                vm.add_shared_dir(path=bad_path, tag="broken")
                vm_id = vm.show().id

                with pytest.raises(Exception):
                    vm.start(wait=True)

                # The parent `start` task must be recorded as error
                # and reference virtiofsd in its message — the agent
                # propagates "virtiofsd spawn failed for vmId N: …
                # socket never appeared" up through `vmStart`'s
                # `throwFailed`, which the daemon surfaces as the
                # task error message.
                start_tasks = [
                    t
                    for t in self.client.tasks.list(subsystem="vm", entity_id=vm_id)
                    if t.command == "start"
                ]
                assert start_tasks, "no `start` task recorded for the failed VM"
                errored = [t for t in start_tasks if t.result == "error"]
                assert errored, (
                    "no `start` task recorded as error — daemon didn't "
                    "surface the missing-path failure"
                )
                assert any("virtiofsd" in (t.message or "") for t in errored), (
                    "no errored `start` task mentions virtiofsd in its "
                    "message; messages: " + repr([t.message for t in errored])
                )
            finally:
                try:
                    vm.delete(delete_disks=True)
                except Exception:
                    pass
        finally:
            try:
                self.client.disks.get(name).delete()
            except Exception:
                pass

    # ---- new: two simultaneous shared dirs --------------------------------

    def test_two_shared_dirs_both_mountable(self):
        """Two `add_shared_dir` calls with distinct tags: both should
        appear on `vm.show().shared_dirs`, both should be mountable
        inside the guest with their respective tags, and writes
        through each should land in the correct host directory."""
        token_a = secrets.token_hex(4)
        token_b = secrets.token_hex(4)
        path_a = f"/tmp/virtiofs-a-{token_a}"
        path_b = f"/tmp/virtiofs-b-{token_b}"
        self.node.run(f"mkdir -p {path_a} {path_b}")
        self.node.run(f"echo SHARE-A:{token_a} > {path_a}/file.txt")
        self.node.run(f"echo SHARE-B:{token_b} > {path_b}/file.txt")
        try:

            class _Two(VmSsh):
                def _shared_dirs(_self):
                    return [
                        {"path": path_a, "tag": "share-a"},
                        {"path": path_b, "tag": "share-b"},
                    ]

            with _Two(self) as vm:
                # Daemon records both shared dirs distinctly.
                tags = {s.tag for s in vm.cap.show().shared_dirs}
                assert tags == {"share-a", "share-b"}, (
                    f"shared_dirs roster wrong: {tags!r}"
                )

                # Mount both inside the guest.
                vm.run("doas mkdir -p /mnt/a /mnt/b")
                vm.run("doas mount -t virtiofs share-a /mnt/a")
                vm.run("doas mount -t virtiofs share-b /mnt/b")

                # Each side reads ONLY its own host content (i.e.
                # the tags aren't crossed over and the mounts are
                # genuinely independent).
                ra = vm.run("cat /mnt/a/file.txt").stdout.strip()
                rb = vm.run("cat /mnt/b/file.txt").stdout.strip()
                assert ra == f"SHARE-A:{token_a}", ra
                assert rb == f"SHARE-B:{token_b}", rb

                # Guest writes flow back through the right tag.
                guest_a = f"GUEST-INTO-A:{secrets.token_hex(3)}"
                guest_b = f"GUEST-INTO-B:{secrets.token_hex(3)}"
                vm.run(f"doas sh -c 'echo {guest_a} > /mnt/a/from-guest.txt'")
                vm.run(f"doas sh -c 'echo {guest_b} > /mnt/b/from-guest.txt'")

            # After teardown, host sees each write only in its own dir.
            assert (
                self.node.run(f"cat {path_a}/from-guest.txt").stdout.decode().strip()
                == guest_a
            )
            assert (
                self.node.run(f"cat {path_b}/from-guest.txt").stdout.decode().strip()
                == guest_b
            )
            # And NOT cross-pollinated: file from B doesn't appear in A's dir.
            cross = self.node.run(
                f"test -e {path_a}/from-guest-b-by-mistake.txt; echo $?",
                check=False,
            )
            # File must not exist — this is a sanity check; failure of
            # `test -e` returns 1 from $?.
            assert cross.stdout.decode().strip() == "1"
        finally:
            self.node.run(f"rm -rf {path_a} {path_b}", check=False)

    # ---- new: read-only shared dir blocks writes --------------------------

    def test_shared_dir_read_only_blocks_writes(self):
        """`add_shared_dir(..., read_only=True)` exposes the host
        directory to the guest as read-only. Reads must work; writes
        must fail (EROFS / "Read-only file system")."""
        token = secrets.token_hex(4)
        ro_path = f"/tmp/virtiofs-ro-{token}"
        self.node.run(f"mkdir -p {ro_path}")
        self.node.run(f"echo READ-ONLY:{token} > {ro_path}/file.txt")
        try:

            class _Ro(VmSsh):
                def _shared_dirs(_self):
                    return [
                        {"path": ro_path, "tag": "ro", "read_only": True},
                    ]

            with _Ro(self) as vm:
                # Daemon recorded the read-only flag.
                rec = [s for s in vm.cap.show().shared_dirs if s.tag == "ro"]
                assert rec and rec[0].read_only is True, rec

                vm.run("doas mkdir -p /mnt/ro")
                vm.run("doas mount -t virtiofs ro /mnt/ro")

                # Reads succeed and see host content.
                assert (
                    vm.run("cat /mnt/ro/file.txt").stdout.strip()
                    == f"READ-ONLY:{token}"
                )

                # Writes must fail. We don't care WHICH error — EROFS,
                # permission, etc. — only that the shell command exits
                # non-zero AND the file isn't created on the host.
                w = vm.run(
                    "doas sh -c 'echo nope > /mnt/ro/wrote-me.txt' 2>&1; echo EXIT=$?",
                    check=False,
                )
                # The `EXIT=N` tail tells us how the vm-side doas exited;
                # 0 would be a regression. Anything non-zero (most
                # commonly "Read-only file system") is correct.
                exit_line = [
                    ln for ln in w.stdout.splitlines() if ln.startswith("EXIT=")
                ]
                assert exit_line and exit_line[-1] != "EXIT=0", (
                    f"write to read-only virtiofs unexpectedly succeeded: "
                    f"stdout={w.stdout!r}"
                )

                # Host confirms the file was never created.
                host_check = self.node.run(
                    f"test -e {ro_path}/wrote-me.txt; echo $?",
                    check=False,
                )
                assert host_check.stdout.decode().strip() == "1", (
                    "guest write made it to the host directory despite "
                    "read_only=True — daemon didn't honour the flag"
                )
        finally:
            self.node.run(f"rm -rf {ro_path}", check=False)
