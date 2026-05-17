"""Build pipeline against the inner daemon.

Ports `BuildIntegrationSpec` (see
`doc/integration-tests-pre-capnp.md:103-137`):
  * happy path — overlay bake with a shell provisioner writes a
    marker, artifact disk is registered, bake VM torn down, the
    marker survives a fresh boot of the artifact.
  * failure path — a non-zero exit in the shell provisioner
    propagates as a build error, no artifact is registered, and
    `cleanup: always` leaves no `__build_*` VM orphans.

Plus one gap test (`target.ifExists: skip`) absent from the
pre-refactor spec: with the artifact name already registered, the
build should short-circuit before spinning up a bake VM.

Doubly-nested KVM: the inner daemon spawns a bake VM inside the
test-node VM. The host has nested KVM enabled (other integration
tests already rely on the first nesting level); KVM propagates
into the test-node via `cpu=host-passthrough`, so the inner
daemon can launch a bake VM.

Cleanup caveat: build VMs are named `__build_<task>_<name>` and
are NOT caught by `make integration-clean-py` (which only
matches `corvus-it-*`). If a test dies mid-bake, clean by hand:
  ssh corvus@<test-node-ip> 'crv -o json vm list \\
    | jq -r ".[] | select(.name|startswith(\\"__build_\\")) | .name" \\
    | xargs -I{} crv vm delete --delete-disks {}'
"""
from __future__ import annotations

import secrets
import textwrap

import pytest

from corvus_client.types import (
    BuildPipelineEnd,
    BuildStepEnd,
    BuildStepStart,
)
from corvus_test_harness import SingleNodeCase


pytestmark = [pytest.mark.slow, pytest.mark.timeout(1800)]


_BAKE_TEMPLATE = textwrap.dedent("""
    name: {tpl_name}
    cpuCount: 2
    ramMb: 1024
    headless: true
    guestAgent: true
    drives:
      - diskImageName: {base_disk}
        interface: virtio
        strategy: overlay
        sizeMb: 2048
""").strip()


class TestBuildPipeline(SingleNodeCase):
    def test_bake_overlay_marker_roundtrip(self):
        """Happy-path bake produces an artifact whose marker survives.

        Single bake (~5-10 min nested). Verifies:
          * BuildStepStart / BuildStepEnd events arrive for the shell
          * BuildPipelineEnd carries one successful BuildOneResult
          * artifact disk is registered in `disks.list()`
          * no leftover `__build_*` VM remains
          * a fresh VM booted from the artifact can `cat` the marker
        """
        token = secrets.token_hex(4)
        images = self.register_base_images()
        base_disk = images["alpine"]
        tpl_name = f"corvus-it-build-tpl-{token}"
        build_name = f"corvus-it-build-{token}"
        artifact_name = f"corvus-it-build-art-{token}"
        verify_overlay = f"corvus-it-build-verify-{token}"

        tpl = self.client.templates.create(
            _BAKE_TEMPLATE.format(tpl_name=tpl_name, base_disk=base_disk)
        )
        pipeline_yaml = textwrap.dedent(f"""
            pipeline:
              - build:
                  name: {build_name}
                  template: {tpl_name}
                  strategy: overlay
                  target:
                    name: {artifact_name}
                    format: qcow2
                    sizeGb: 2
                    compact: true
                  vm:
                    cpuCount: 2
                    ramMb: 1024
                  provisioners:
                    - shell: |
                        set -eux
                        mkdir -p /var/lib/corvus-test
                        echo bake-marker > /var/lib/corvus-test/marker
                  cleanup: always
                  waitForShutdownSec: 300
        """).strip()

        try:
            step_results = {}
            pipeline_end = None
            saw_step_start = False
            for ev in self.client.build_stream_text(pipeline_yaml):
                if isinstance(ev, BuildStepStart):
                    saw_step_start = True
                elif isinstance(ev, BuildStepEnd):
                    step_results[ev.step_index] = ev.result
                elif isinstance(ev, BuildPipelineEnd):
                    pipeline_end = ev

            # Event-order assertions.
            assert saw_step_start, "no BuildStepStart event seen"
            assert step_results, "no BuildStepEnd events seen"
            assert all(r == "success" for r in step_results.values()), step_results
            assert pipeline_end is not None
            assert len(pipeline_end.builds) == 1
            bo = pipeline_end.builds[0]
            assert bo.name == build_name
            assert bo.artifact_disk_id  # non-zero / non-None
            assert not bo.error_message

            # Artifact disk registered.
            disk_names = [d.name for d in self.client.disks.list()]
            assert artifact_name in disk_names

            # No __build_* orphans (`cleanup: always` honoured).
            vm_names = [v.name for v in self.client.vms.list()]
            assert not any(n.startswith("__build_") for n in vm_names), vm_names

            # Verify: fresh boot of the artifact reads the marker.
            # Attach via overlay so `vm.delete(delete_disks=True)`
            # only touches the overlay; the artifact stays for
            # explicit deletion below.
            self.client.disks.create_overlay(verify_overlay, artifact_name)
            verify_vm = self.client.vms.create(
                f"corvus-it-build-vm-{token}",
                cpu_count=1,
                ram_mb=512,
                headless=True,
                guest_agent=True,
                cloud_init=False,
            )
            try:
                verify_vm.attach_disk(disk_ref=verify_overlay, interface="virtio")
                verify_vm.start(wait=True)
                r = verify_vm.guest_exec("/bin/cat /var/lib/corvus-test/marker")
                assert r.exit_code == 0, r
                assert "bake-marker" in r.stdout, r.stdout
                verify_vm.stop(wait=True)
            finally:
                verify_vm.reset()
                verify_vm.delete(delete_disks=True)

            # Drop the artifact disk so cleanup is complete.
            self.client.disks.get(artifact_name, by_name=True).delete()
        finally:
            tpl.delete()

    def test_failed_provisioner_no_artifact_no_orphans(self):
        """`cleanup: always` + failing provisioner → no artifact, no orphans.

        Single bake; the shell exits 7. Daemon must tear the bake VM
        down and not register an artifact. ~2-3 min nested.
        """
        token = secrets.token_hex(4)
        images = self.register_base_images()
        base_disk = images["alpine"]
        tpl_name = f"corvus-it-build-tpl-{token}"
        artifact_name = f"corvus-it-should-not-exist-{token}"

        tpl = self.client.templates.create(
            _BAKE_TEMPLATE.format(tpl_name=tpl_name, base_disk=base_disk)
        )
        pipeline_yaml = textwrap.dedent(f"""
            pipeline:
              - build:
                  name: corvus-it-build-{token}
                  template: {tpl_name}
                  strategy: overlay
                  target:
                    name: {artifact_name}
                    format: qcow2
                    sizeGb: 2
                  vm:
                    cpuCount: 2
                    ramMb: 1024
                  provisioners:
                    - shell: "exit 7"
                  cleanup: always
                  waitForShutdownSec: 300
        """).strip()

        try:
            pipeline_end = None
            for ev in self.client.build_stream_text(pipeline_yaml):
                if isinstance(ev, BuildPipelineEnd):
                    pipeline_end = ev

            assert pipeline_end is not None
            assert len(pipeline_end.builds) == 1
            bo = pipeline_end.builds[0]
            assert not bo.artifact_disk_id, bo
            assert bo.error_message, bo
            err = bo.error_message.lower()
            # Pre-refactor doc accepted "shell" OR "exited"; allow
            # "provisioner" as a post-refactor variant.
            assert (
                "shell" in err or "exited" in err or "provisioner" in err
            ), err

            # Artifact must not be registered.
            disk_names = [d.name for d in self.client.disks.list()]
            assert artifact_name not in disk_names

            # No leftover bake VM (cleanup: always honoured on failure).
            vm_names = [v.name for v in self.client.vms.list()]
            assert not any(n.startswith("__build_") for n in vm_names), vm_names
        finally:
            tpl.delete()

    def test_target_ifexists_skip_short_circuits(self):
        """`target.ifExists: skip` returns the existing disk id, no bake.

        Pre-stage a stub disk with the artifact name. The build's
        pre-bake check (`Handlers/Build.hs::checkIfExistsPreBake`)
        finds it and short-circuits. No `__build_*` VM ever
        materialises, and the loud-failure provisioner doesn't
        run. ~10 s — no bake.
        """
        token = secrets.token_hex(4)
        images = self.register_base_images()
        base_disk = images["alpine"]
        tpl_name = f"corvus-it-build-tpl-{token}"
        artifact_name = f"corvus-it-build-art-{token}"

        stub = self.client.disks.create(
            artifact_name, size_mb=64, format="qcow2"
        )
        tpl = self.client.templates.create(
            _BAKE_TEMPLATE.format(tpl_name=tpl_name, base_disk=base_disk)
        )
        # Loud-failure provisioner — if the bake runs, build errors
        # out and the test fails.
        pipeline_yaml = textwrap.dedent(f"""
            pipeline:
              - build:
                  name: corvus-it-build-{token}
                  template: {tpl_name}
                  strategy: overlay
                  target:
                    name: {artifact_name}
                    ifExists: skip
                    format: qcow2
                    sizeGb: 1
                  vm:
                    cpuCount: 1
                    ramMb: 512
                  provisioners:
                    - shell: "echo SHOULD NOT RUN; exit 1"
                  cleanup: always
        """).strip()

        try:
            pipeline_end = None
            for ev in self.client.build_stream_text(pipeline_yaml):
                if isinstance(ev, BuildPipelineEnd):
                    pipeline_end = ev

            assert pipeline_end is not None
            bo = pipeline_end.builds[0]
            assert bo.artifact_disk_id, "ifExists: skip should return existing id"
            assert not bo.error_message, bo

            # No __build_* VM should have been created.
            vm_names = [v.name for v in self.client.vms.list()]
            assert not any(n.startswith("__build_") for n in vm_names), vm_names

            # Stub disk still exists with its original size.
            d = self.client.disks.get(artifact_name, by_name=True).show()
            assert d.size_mb == 64, d
        finally:
            stub.delete()
            tpl.delete()
