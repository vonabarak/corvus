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
    | xargs -I{} crv vm delete {}'
"""

from __future__ import annotations

import secrets
import textwrap
import time

import pytest
from corvus_client._async.build import preprocess_build_yaml
from corvus_client.types import (
    BuildPipelineEnd,
    BuildStepEnd,
    BuildStepOutput,
    BuildStepStart,
)
from corvus_test_harness import SingleNodeCase
from corvus_test_harness.host_binary import REPO_ROOT

import yaml as yamlmod

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
            # Attach via overlay so `vm.delete()` reaps the overlay
            # (it's marked ephemeral) and the artifact stays for
            # explicit deletion below — deleting the artifact first
            # would fail with `DiskHasOverlays`.
            self.client.disks.create_overlay(
                verify_overlay, artifact_name, ephemeral=True
            )
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
                verify_vm.delete()

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
            assert "shell" in err or "exited" in err or "provisioner" in err, err

            # Artifact must not be registered.
            disk_names = [d.name for d in self.client.disks.list()]
            assert artifact_name not in disk_names

            # No leftover bake VM (cleanup: always honoured on failure).
            vm_names = [v.name for v in self.client.vms.list()]
            assert not any(n.startswith("__build_") for n in vm_names), vm_names
        finally:
            tpl.delete()

    def test_builds_corvus_test_vm_image(self):
        """End-to-end build of the project's Alpine test image.

        Drives `crv build yaml/corvus-test-vm/corvus-test-vm.yml`
        after staging the `debian12` bake template inline (the
        build uses it as the bake VM for the apk-tools-static
        bootstrap → chroot → GRUB BIOS+UEFI install →
        qemu-guest-agent + vsock-sshd flow).

        We deliberately do *not* apply `yaml/multi-os/multi-os.yml`
        here: that file registers cloud-image disks under names
        like `ubuntu-24.04-server-base` (with dots) whose
        `file_path` collides with the harness's pre-registered
        same-file disks (`register_base_images` sanitises dots to
        hyphens). Staging only what this build needs sidesteps the
        name/path mismatch and keeps the test self-contained.

        We also rewrite the build YAML on the fly to use a unique
        per-test artifact name. The project's hardcoded
        `corvus-test-vm` collides with the harness's `images
        ["alpine"]` alias when the developer's host has a single
        cached `corvus-test-vm.qcow2` under `BaseImages/Alpine/`
        (no `alpine-3.21-base.qcow2`). Deleting that disk in
        finally would break sibling tests that consume the same
        cached `register_base_images()` dict.

        Slow: nested bake runs ~5-10 min under doubly-nested KVM.
        """
        # The bake VM uses Debian's cloud image and cloud-init to
        # install qemu-guest-agent at first boot ('packages: [qemu-
        # guest-agent]'). Without outbound internet on the test-node,
        # apt-get can't reach Debian's mirrors and the agent never
        # starts — the bake VM times out with 'guest agent did not
        # respond within 90000 ms'. Detect the lack of outbound and
        # skip with a clear reason rather than burning 90 s.
        # 'TestNode.run' returns 'subprocess.CompletedProcess'
        # (exit status field is 'returncode').
        probe = self.node.run(
            "ping -c 1 -W 2 1.1.1.1",
            check=False,
        )
        if probe.returncode != 0:
            pytest.skip(
                f"test-node {self.node.name!r} can't reach the internet "
                "on its own; the Debian-cloud bake VM cannot 'apt install "
                "qemu-guest-agent' without it. Set up the host's VDE "
                "uplink to a real NIC + NAT, then re-run."
            )

        token = secrets.token_hex(4)
        artifact_name = f"corvus-it-corvus-test-vm-{token}"
        build_path = REPO_ROOT / "yaml" / "corvus-test-vm" / "corvus-test-vm.yml"

        # Make sure the harness's BaseImages catalogue is registered
        # (gives us `debian-12-generic-base` referenced by the
        # template below).
        self.register_base_images()

        def _drop_artifact() -> None:
            for d in self.client.disks.list():
                if d.name == artifact_name:
                    self.client.disks.get(artifact_name, by_name=True).delete()
                    return

        # Minimal apply: the SSH key, OVMF firmware disks (read
        # from the test-node's local /usr/share/edk2/, no
        # downloads), and the bake template. `skip_existing` makes
        # the test idempotent — re-running in the same topology
        # leaves prior entries untouched.
        bake_template_yaml = textwrap.dedent("""
            sshKeys:
              - name: corvus
                publicKey: "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIDpO6w7latI2iS1Q7SctsaIXa/p4K9DbJfNTmuQwDVOD"

            disks:
              - name: ovmf-code
                register: "/usr/share/edk2/OvmfX64/OVMF_CODE_4M.qcow2"
                format: qcow2
              - name: ovmf-vars
                register: "/usr/share/edk2/OvmfX64/OVMF_VARS_4M.qcow2"
                format: qcow2

            templates:
              - name: debian12
                description: "Debian 12 (bake template for corvus-test-vm)"
                cpuCount: 2
                ramMb: 4096
                cloudInit: true
                guestAgent: true
                cloudInitConfig:
                  userData:
                    packages:
                      - qemu-guest-agent
                    runcmd:
                      - systemctl enable --now qemu-guest-agent
                drives:
                  - diskImageName: debian-12-generic-base
                    interface: virtio
                    strategy: overlay
                    cacheType: writeback
                    discard: true
                  - diskImageName: ovmf-code
                    interface: pflash
                    readOnly: true
                    strategy: direct
                  - diskImageName: ovmf-vars
                    interface: pflash
                    strategy: clone
                networkInterfaces:
                  - type: vde
                    hostDevice: /run/vde2/switch.ctl
                sshKeys:
                  - name: corvus
        """).strip()
        self.client.apply(bake_template_yaml, skip_existing=True, wait=True)

        # Preprocess the build YAML (inlines file refs like the
        # SSH-key pubkey) then patch the target/build names so we
        # don't write back into the shared `corvus-test-vm` slot.
        preprocessed = preprocess_build_yaml(str(build_path))
        doc = yamlmod.safe_load(preprocessed)
        for step in doc["pipeline"]:
            build = step.get("build")
            if isinstance(build, dict):
                build["name"] = f"corvus-it-build-{token}"
                build["target"]["name"] = artifact_name

        try:
            pipeline_end = None
            for ev in self.client.build_stream_text(yamlmod.safe_dump(doc)):
                if isinstance(ev, BuildPipelineEnd):
                    pipeline_end = ev

            assert pipeline_end is not None, "no BuildPipelineEnd event seen"
            assert len(pipeline_end.builds) == 1
            bo = pipeline_end.builds[0]
            assert not bo.error_message, bo
            assert bo.artifact_disk_id, bo

            disk_names = [d.name for d in self.client.disks.list()]
            assert artifact_name in disk_names

            # `cleanup: onSuccess` on the bake means the bake VM
            # is reaped on success — assert there are no
            # __build_* leftovers.
            vm_names = [v.name for v in self.client.vms.list()]
            assert not any(n.startswith("__build_") for n in vm_names), vm_names
        finally:
            _drop_artifact()

    def test_provisioner_output_streams_live(self):
        """Provisioner stdout reaches the client line-by-line, not in
        a single batch at step end.

        Pre-Phase-4 the daemon owned the QGA socket and tailed
        guest-side output via @guest-file-read@, so each line
        emitted by a provisioner showed up in the client live.
        Phase 4 routed guest-exec through @nodeagent.vmGuestExec@,
        which aggregated and returned the entire output on exit
        — provisioner output appeared in a clump only after the
        step finished. The new ``vmGuestExecStream`` RPC restores
        the live behaviour.

        The shell provisioner here prints five lines with one-
        second sleeps between them. Each line arrives as a
        ``BuildStepOutput`` event; we record wall-clock arrival
        times and assert the spread is at least 3 s — well above
        any plausible batching window but well below the 5 s the
        provisioner takes overall. Also assert at least one
        ``BuildStepOutput`` lands *before* the corresponding
        ``BuildStepEnd``: in the regressed implementation the
        outputs were enqueued only AFTER step-end because the
        daemon split them post-completion.
        """

        token = secrets.token_hex(4)
        images = self.register_base_images()
        base_disk = images["alpine"]
        tpl_name = f"corvus-it-build-tpl-{token}"
        build_name = f"corvus-it-build-{token}"
        artifact_name = f"corvus-it-build-art-{token}"

        tpl = self.client.templates.create(
            _BAKE_TEMPLATE.format(tpl_name=tpl_name, base_disk=base_disk)
        )
        # Five iterations × 1 s sleep ≈ 5 s of streamed output.
        # `set +x` keeps the loop's set -x trace from drowning the
        # echo lines we're timing.
        pipeline_yaml = textwrap.dedent(f"""
            pipeline:
              - build:
                  name: {build_name}
                  template: {tpl_name}
                  strategy: overlay
                  target:
                    name: {artifact_name}
                    format: qcow2
                    sizeGb: 1
                  vm:
                    cpuCount: 1
                    ramMb: 512
                  provisioners:
                    - shell: |
                        set +x
                        for i in 1 2 3 4 5; do
                          echo "streaming-marker $i"
                          sleep 1
                        done
                  cleanup: always
                  waitForShutdownSec: 300
        """).strip()

        try:
            # (arrival_time, event) pairs for every event the daemon
            # streams back. Stamping at event-receipt time on the
            # client is the only way to distinguish "arrived live"
            # from "arrived in a final batch".
            timeline: list[tuple[float, object]] = []
            for ev in self.client.build_stream_text(pipeline_yaml):
                timeline.append((time.monotonic(), ev))

            # Pull out the (step_index → BuildStepEnd time) map so
            # we can compare each BuildStepOutput's timestamp to
            # the end of its step.
            step_end_at: dict[int, float] = {
                ev.step_index: t for t, ev in timeline if isinstance(ev, BuildStepEnd)
            }
            # Find the BuildStepStart for our streaming step so we
            # can scope BuildStepOutput events to it.
            step_start_at: dict[int, float] = {
                ev.step_index: t for t, ev in timeline if isinstance(ev, BuildStepStart)
            }
            assert step_start_at, "no BuildStepStart events seen"

            streaming_step = None
            streaming_lines: list[tuple[float, str]] = []
            for t, ev in timeline:
                if isinstance(ev, BuildStepOutput) and "streaming-marker" in ev.line:
                    streaming_step = ev.step_index
                    streaming_lines.append((t, ev.line))

            assert streaming_step is not None, (
                "no BuildStepOutput carrying 'streaming-marker' arrived"
            )
            assert len(streaming_lines) >= 5, (
                f"expected ≥5 streaming-marker lines, got "
                f"{len(streaming_lines)}: {[line for _, line in streaming_lines]}"
            )

            # Live-streaming assertions:
            # (1) Spread between first and last line is at least
            #     3 s — they cannot all have been batched at step
            #     end.
            spread = streaming_lines[-1][0] - streaming_lines[0][0]
            assert spread >= 3.0, (
                f"streaming-marker lines arrived in {spread:.2f}s — "
                f"too compressed; output was likely batched at step end"
            )
            # (2) At least one BuildStepOutput arrives BEFORE the
            #     step's BuildStepEnd. Pre-fix, outputs were
            #     enqueued just before stepEnd, so this would fail.
            end_t = step_end_at.get(streaming_step)
            assert end_t is not None, (
                f"no BuildStepEnd for streaming step {streaming_step}"
            )
            early_lines = [t for t, _ in streaming_lines if t < end_t - 0.5]
            assert early_lines, (
                f"all {len(streaming_lines)} BuildStepOutput events arrived "
                f"within 500 ms of BuildStepEnd — no live streaming"
            )

            # Sanity: the build itself succeeded.
            pipeline_end = next(
                (ev for _, ev in timeline if isinstance(ev, BuildPipelineEnd)),
                None,
            )
            assert pipeline_end is not None
            assert pipeline_end.builds and pipeline_end.builds[0].artifact_disk_id

            # Cleanup the artifact disk.
            self.client.disks.get(artifact_name, by_name=True).delete()
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

        stub = self.client.disks.create(artifact_name, size_mb=64, format="qcow2")
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
