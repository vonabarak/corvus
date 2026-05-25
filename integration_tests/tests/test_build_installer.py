"""Installer-strategy build pipeline end-to-end.

Exercises Corvus's `installer` build strategy (the path taken by
the Windows Server 2025 bake) using a synthetic installer ISO so a
single run takes ~30-60 s instead of ~55 min. The synthetic ISO is
a busybox initramfs whose PID 1 mounts the floppy supplied by
`crv build`, copies a per-run marker payload onto /dev/vda, and
powers off — exactly the contract `runInstallerPhase` (in
`src/Corvus/Handlers/Build.hs`) expects from a real vendor
installer.

The ISO is produced out-of-band by
`scripts/build-synthetic-installer.sh` (invoked by
`make test-image-installer`) and lives under
`~/VMs/BaseImages/SyntheticInstaller/corvus-test-installer-iso.raw`,
so the harness's `register_base_images()` picks it up and
registers it with the inner daemon without any per-test
boilerplate.

What the assertions cover:
  * The full installer pipeline succeeds (apply + build).
  * The artifact disk is registered under the per-run target name.
  * No `__build_*` orphan VMs/disks remain.
  * The marker payload (CORVUS-INSTALLER-OK + a per-run UUID)
    survives publish, proving the floppy was actually attached and
    read this run — which is the exact contract the recent
    `DiskImageNode` placement fix (commit 1c810b8) restored.
"""

from __future__ import annotations

import base64
import secrets
import uuid

import pytest
from corvus_client.types import BuildPipelineEnd
from corvus_test_harness import InstallerImageReady, SingleNodeCase
from corvus_test_harness.host_binary import REPO_ROOT

import yaml as yamlmod

pytestmark = pytest.mark.timeout(900)


_BUILD_YAML = REPO_ROOT / "yaml" / "corvus-test-installer" / "corvus-test-installer.yml"


class TestBuildInstaller(SingleNodeCase):
    @pytest.fixture(scope="class", autouse=True)
    def _installer_image_present(self, crv):
        """Fail fast if the synthetic-installer ISO isn't registered.

        Mirrors `image_ready` for `corvus-test-node`: the bake
        lives in the Makefile (`make test-image-installer`); the
        harness only checks. Without this, the build's apply step
        would fail later with a less actionable error.
        """
        InstallerImageReady.ensure(crv)

    @pytest.fixture(scope="class", autouse=True)
    def _floppy_tools_present(self, _class_topology):
        """Confirm the test-node has mkfs.fat + mcopy on PATH.

        The installer-strategy build path shells out to those tools
        from `Corvus.Handlers.Build.Floppy.buildFloppyImage` to wrap
        the per-build marker into a FAT12 floppy image; without them
        the bake fails with `posix_spawnp: does not exist` mid-
        pipeline. The `corvus-test-node` bake adds them
        (`sys-fs/dosfstools` + `sys-fs/mtools`), but a developer who
        baked the test-node *before* those entries landed needs to
        rebake. Surface that as a clear precondition error rather
        than letting the build fail with a posix_spawnp message.
        """
        r = self.node.run(
            "command -v mkfs.fat && command -v mcopy",
            check=False,
        )
        if r.returncode != 0:
            pytest.fail(
                "test-node is missing mkfs.fat and/or mcopy on PATH. The "
                "installer-strategy build's floppy materialisation step "
                "needs both. Rebake the test-node:\n"
                "  make test-image-node-clean && make test-image-node\n"
                f"(probe stdout={r.stdout!r}, stderr={r.stderr!r})"
            )

    def test_installer_strategy_roundtrip(self):
        """Synthetic installer bakes; marker survives the publish."""
        marker_uuid = uuid.uuid4().hex
        target_token = secrets.token_hex(4)
        artifact_name = f"corvus-it-installer-art-{target_token}"

        # Make the synthetic-installer ISO visible to the inner
        # daemon. `register_base_images` walks the host's
        # ~/VMs/BaseImages tree (virtiofs-shared into the test-
        # node) and registers every disk it finds, with a format
        # hint derived from the file extension so the raw ISO is
        # registered as raw (the daemon-side auto-detect
        # mis-classifies it otherwise).
        self.register_base_images()

        # Patch the in-tree build YAML:
        #   * inject the per-run marker as floppy.contentBase64
        #     (the in-tree YAML leaves it unset so a stray manual
        #     `crv build` of the file fails fast),
        #   * scope the target name to this run so concurrent
        #     workers / repeated runs don't collide.
        marker_payload = f"marker={marker_uuid}\n".encode()
        marker_b64 = base64.b64encode(marker_payload).decode("ascii")
        doc = yamlmod.safe_load(_BUILD_YAML.read_text())
        for step in doc["pipeline"]:
            build = step.get("build")
            if isinstance(build, dict):
                build["target"]["name"] = artifact_name
                build["floppy"] = {
                    "contentBase64": marker_b64,
                    "filename": "marker.txt",
                }
        pipeline_yaml = yamlmod.safe_dump(doc)

        try:
            # The installer strategy doesn't run shell provisioners,
            # so BuildStepStart/BuildStepEnd never fire (those are
            # per-provisioner). We only look at BuildPipelineEnd's
            # per-build summary.
            pipeline_end: BuildPipelineEnd | None = None
            event_trace: list[str] = []
            for ev in self.client.build_stream_text(pipeline_yaml):
                event_trace.append(type(ev).__name__)
                if isinstance(ev, BuildPipelineEnd):
                    pipeline_end = ev

            assert pipeline_end is not None, (
                f"no BuildPipelineEnd event seen; trace={event_trace}"
            )
            # The pipeline always reports one BuildOneResult per
            # pipeline step (apply + build), so we look up the
            # build-step result by name rather than by index.
            bo = next(
                (b for b in pipeline_end.builds if b.name == "corvus-test-installer"),
                None,
            )
            assert bo is not None, (
                f"no 'corvus-test-installer' build result; "
                f"got {[b.name for b in pipeline_end.builds]}"
            )
            if bo.error_message:
                # On a bake-VM error we still hold the bake VM
                # alive via `cleanup: onSuccess`. Surface its
                # serial console + the inner daemon journal so a
                # regression doesn't manifest as an opaque
                # "entered error state".
                diag = self._installer_failure_diagnostics()
                pytest.fail(
                    f"build failed: {bo.error_message}\n--- diagnostics ---\n{diag}"
                )
            assert bo.artifact_disk_id, bo

            # Artifact disk registered under the per-run name.
            disk_names = [d.name for d in self.client.disks.list()]
            assert artifact_name in disk_names

            # `cleanup: always` on the bake means no leftovers.
            vm_names = [v.name for v in self.client.vms.list()]
            assert not any(n.startswith("__build_") for n in vm_names), vm_names

            # Read back the artifact's first 1 KiB via a probe VM.
            # The installer's init script dd'd CORVUS-INSTALLER-OK\n
            # + the marker payload onto /dev/vda, so a `dd` from
            # the same offset must return both markers — proving
            # the floppy was attached AND read this run.
            self._assert_marker_present(
                artifact_name=artifact_name,
                marker_uuid=marker_uuid,
                token=target_token,
            )
        finally:
            for d in self.client.disks.list():
                if d.name == artifact_name:
                    self.client.disks.get(artifact_name, by_name=True).delete()
                    break

    def _installer_failure_diagnostics(self) -> str:
        """Best-effort dump of the inner daemon journal + bake VM
        list on installer-strategy failure. The bake VM is kept
        alive (cleanup: onSuccess) so its qemu PID + workdir
        survive for further manual poking.
        """
        chunks: list[str] = []
        # corvus-nodeagent owns QEMU; its journal carries the
        # qemu spawn line + stderr. The corvus daemon journal
        # just sees "QEMU exited with error code 1" without the
        # actual reason — useful for narrowing the failure
        # point but not the root cause.
        for unit in ("corvus-nodeagent", "corvus"):
            r = self.node.run(
                f"sudo journalctl -u {unit} --no-pager -n 200",
                check=False,
                timeout_sec=30,
            )
            chunks.append(f"[{unit} journal]\n{r.stdout.decode(errors='replace')}")
        try:
            bake_vms = [
                v.name for v in self.client.vms.list() if v.name.startswith("__build_")
            ]
        except Exception as e:
            bake_vms = []
            chunks.append(f"[vm list failed] {e!r}")
        chunks.append(f"[bake VMs alive] {bake_vms}")
        return "\n".join(chunks)

    def _assert_marker_present(
        self,
        *,
        artifact_name: str,
        marker_uuid: str,
        token: str,
    ) -> None:
        """Boot a probe VM with the artifact as a secondary disk
        and read the first few sectors via guest-exec.
        """
        images = self.register_base_images()
        base_disk = images["alpine"]
        overlay_name = f"corvus-it-installer-verify-overlay-{token}"
        self.client.disks.create_overlay(overlay_name, base_disk, ephemeral=True)
        verify_vm = self.client.vms.create(
            f"corvus-it-installer-verify-{token}",
            cpu_count=1,
            ram_mb=512,
            headless=True,
            guest_agent=True,
            cloud_init=False,
        )
        try:
            verify_vm.attach_disk(disk_ref=overlay_name, interface="virtio")
            verify_vm.attach_disk(
                disk_ref=artifact_name,
                interface="virtio",
                read_only=True,
            )
            verify_vm.start(wait=True)
            # Linux agent wraps the command with `/bin/sh -c`, so
            # pipes work directly. Read enough to comfortably
            # cover the 20-byte header + marker line and base64
            # the output so binary trailing zeros don't confuse
            # the QGA round-trip.
            r = verify_vm.guest_exec(
                "dd if=/dev/vdb bs=512 count=4 2>/dev/null | base64"
            )
            assert r.exit_code == 0, r
            decoded = base64.b64decode(r.stdout)
            assert b"CORVUS-INSTALLER-OK" in decoded, decoded[:128]
            assert marker_uuid.encode() in decoded, decoded[:256]
            verify_vm.stop(wait=True)
        finally:
            verify_vm.reset()
            verify_vm.delete()
