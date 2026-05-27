"""Build pipeline modes: target.ifExists + cleanup + apply-in-pipeline.

Closes a gap in [test_build_pipeline.py](integration_tests/tests/test_build_pipeline.py),
which already exercises happy + failed bakes and ``ifExists: skip``
but leaves ``ifExists: error``, ``ifExists: overwrite``, the
``cleanup: onSuccess`` / ``cleanup: never`` paths, and pipelines
mixing ``apply:`` + ``build:`` untested.

Two strategies in play, picked per sub-test:
  * **Overlay strategy** (Alpine base + shell provisioner): the
    only strategy that exercises ``cleanup`` modes because that's
    the only one with provisioners.  ~2-3 min per bake under
    doubly-nested KVM.
  * **No bake** (``ifExists: error``, ``overwrite``-refused,
    ``apply-in-pipeline`` with ``ifExists: skip``): pre-existing
    disks short-circuit the bake; each test finishes in ~10 s.

Bake VM cleanup: ``__build_*`` VMs that don't reap themselves
(under ``cleanup: onSuccess`` after a failure, or
``cleanup: never``) are deleted in this file's ``finally`` blocks.
``make integration-tests-clean`` doesn't catch them — only
``corvus-it-*`` rows match its filter.
"""

from __future__ import annotations

import secrets
import textwrap

import pytest
from corvus_client.exceptions import CorvusError
from corvus_client.types import BuildPipelineEnd
from corvus_test_harness import SingleNodeCase

pytestmark = pytest.mark.timeout(1800)


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


def _drop_build_orphans(client) -> None:
    """Reap every ``__build_*`` VM in the inner daemon.

    Helper for tests that exercise ``cleanup: onSuccess`` or
    ``cleanup: never`` and intentionally leave bake VMs behind.
    Best-effort: errors are swallowed so a partial leak doesn't
    mask the original test failure."""
    for v in client.vms.list():
        if v.name.startswith("__build_"):
            try:
                cap = client.vms.get(v.name, by_name=True)
                cap.reset()
                cap.delete()
            except Exception:
                pass


def _run_pipeline(client, yaml_text: str) -> BuildPipelineEnd:
    """Drive a build pipeline to its terminal event and return it."""
    end = None
    for ev in client.build_stream_text(yaml_text):
        if isinstance(ev, BuildPipelineEnd):
            end = ev
    assert end is not None, "build stream ended without BuildPipelineEnd"
    return end


class TestBuildModes(SingleNodeCase):
    """ifExists modes + cleanup modes + apply-step-in-pipeline."""

    # ---- ifExists: error (default) ---------------------------------------

    def test_target_ifexists_error_aborts_before_bake(self):
        """A pre-existing target disk with ``ifExists: error``
        (the default) fails the pre-bake check immediately — no
        ``__build_*`` VM is ever created, and the daemon's error
        names ``ifExists: skip or overwrite`` as the remedy."""
        token = secrets.token_hex(3)
        images = self.register_base_images()
        base_disk = images["alpine"]
        artifact_name = f"corvus-it-build-mode-err-{token}"
        tpl_name = f"corvus-it-build-tpl-err-{token}"

        existing = self.client.disks.create(artifact_name, size_mb=64, format="qcow2")
        tpl = self.client.templates.create(
            _BAKE_TEMPLATE.format(tpl_name=tpl_name, base_disk=base_disk)
        )
        # No `ifExists:` field → daemon defaults to error.
        pipeline_yaml = textwrap.dedent(f"""
            pipeline:
              - build:
                  name: corvus-it-build-{token}
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
                    - shell: "echo SHOULD NOT RUN"
                  cleanup: always
        """).strip()
        try:
            end = _run_pipeline(self.client, pipeline_yaml)
            bo = end.builds[0]
            assert bo.error_message, end
            # Daemon's message includes the verbatim remedy hint.
            assert "skip" in bo.error_message and "overwrite" in bo.error_message, (
                bo.error_message
            )
            assert not bo.artifact_disk_id, bo

            # No bake VM came alive.
            vm_names = [v.name for v in self.client.vms.list()]
            assert not any(n.startswith("__build_") for n in vm_names), vm_names

            # Pre-existing disk wasn't touched.
            d = self.client.disks.get(artifact_name, by_name=True).show()
            assert d.size_mb == 64, d
        finally:
            tpl.delete()
            existing.delete()

    # ---- ifExists: overwrite, attach-blocked path ------------------------

    def test_target_ifexists_overwrite_refused_when_attached(self):
        """``ifExists: overwrite`` refuses to delete a disk that's
        still attached to a VM — the daemon names the offending VM
        and tells the user to detach or delete it first."""
        token = secrets.token_hex(3)
        images = self.register_base_images()
        base_disk = images["alpine"]
        artifact_name = f"corvus-it-build-mode-ovw-att-{token}"
        tpl_name = f"corvus-it-build-tpl-ovw-att-{token}"
        vm_name = f"corvus-it-build-mode-attached-{token}"

        existing = self.client.disks.create(artifact_name, size_mb=64, format="qcow2")
        # Attach the pre-existing artifact disk to a stopped VM
        # (read-only — the overwrite check is on attachment, not
        # on r/w semantics).
        vm = self.client.vms.create(
            vm_name,
            cpu_count=1,
            ram_mb=256,
            headless=True,
        )
        try:
            vm.attach_disk(disk_ref=artifact_name, interface="virtio", read_only=True)
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
                        ifExists: overwrite
                        format: qcow2
                        sizeGb: 1
                      vm:
                        cpuCount: 1
                        ramMb: 512
                      provisioners:
                        - shell: "echo SHOULD NOT RUN"
                      cleanup: always
            """).strip()
            try:
                end = _run_pipeline(self.client, pipeline_yaml)
                bo = end.builds[0]
                assert bo.error_message, end
                assert "overwrite" in bo.error_message, bo.error_message
                assert vm_name in bo.error_message, bo.error_message
                assert not bo.artifact_disk_id, bo

                # No bake VM came alive.
                vm_names = [v.name for v in self.client.vms.list()]
                assert not any(n.startswith("__build_") for n in vm_names), vm_names
            finally:
                tpl.delete()
        finally:
            try:
                vm.detach_disk_by_name(artifact_name)
            except Exception:
                pass
            try:
                vm.delete()
            except Exception:
                pass
            try:
                existing.delete()
            except Exception:
                pass

    # ---- ifExists: overwrite, success path -------------------------------

    def test_target_ifexists_overwrite_replaces_disk(self):
        """``ifExists: overwrite`` against an unattached disk
        replaces it with a freshly-baked artifact.  The new disk's
        id may differ — overwrite deletes the old row and the
        daemon mints a new one.

        The bake itself is a minimal Alpine overlay with a no-op
        provisioner; the cost is dominated by Alpine first-boot
        + cloud-init, ~5-10 min under doubly-nested KVM."""
        token = secrets.token_hex(3)
        images = self.register_base_images()
        base_disk = images["alpine"]
        artifact_name = f"corvus-it-build-mode-ovw-{token}"
        tpl_name = f"corvus-it-build-tpl-ovw-{token}"

        stub = self.client.disks.create(artifact_name, size_mb=64, format="qcow2")
        stub_id = stub.show().id
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
                    ifExists: overwrite
                    format: qcow2
                    sizeGb: 1
                  vm:
                    cpuCount: 2
                    ramMb: 1024
                  provisioners:
                    - shell: "true"
                  cleanup: always
                  waitForShutdownSec: 300
        """).strip()
        try:
            end = _run_pipeline(self.client, pipeline_yaml)
            bo = end.builds[0]
            assert not bo.error_message, bo
            assert bo.artifact_disk_id, bo

            # Old stub disk is gone; the new disk owns the name.
            replaced = self.client.disks.get(artifact_name, by_name=True).show()
            assert replaced.id != stub_id, (stub_id, replaced.id)
            assert replaced.size_mb is not None and replaced.size_mb > 64, replaced

            # Bake VM reaped (cleanup: always).
            vm_names = [v.name for v in self.client.vms.list()]
            assert not any(n.startswith("__build_") for n in vm_names), vm_names

            # Drop the replacement so cleanup is complete.
            self.client.disks.get(artifact_name, by_name=True).delete()
        finally:
            tpl.delete()

    # ---- cleanup: onSuccess (failure → keep) -----------------------------

    def test_cleanup_onsuccess_keeps_bake_vm_on_failure(self):
        """``cleanup: onSuccess`` + failing provisioner leaves the
        bake VM behind for inspection.  Test asserts the orphan
        exists and reaps it explicitly so the suite doesn't leak."""
        token = secrets.token_hex(3)
        images = self.register_base_images()
        base_disk = images["alpine"]
        tpl_name = f"corvus-it-build-tpl-onsuccess-{token}"
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
                    sizeGb: 1
                  vm:
                    cpuCount: 2
                    ramMb: 1024
                  provisioners:
                    - shell: "exit 7"
                  cleanup: onSuccess
                  waitForShutdownSec: 300
        """).strip()
        try:
            end = _run_pipeline(self.client, pipeline_yaml)
            bo = end.builds[0]
            assert bo.error_message, bo
            assert not bo.artifact_disk_id, bo

            # Artifact name not registered.
            disk_names = [d.name for d in self.client.disks.list()]
            assert artifact_name not in disk_names

            # Bake VM survived — `cleanup: onSuccess` only reaps
            # on success.
            vm_names = [v.name for v in self.client.vms.list()]
            assert any(n.startswith("__build_") for n in vm_names), vm_names
        finally:
            _drop_build_orphans(self.client)
            tpl.delete()

    # ---- cleanup: never (failure → keep) ---------------------------------

    def test_cleanup_never_keeps_bake_vm_on_failure(self):
        """``cleanup: never`` leaves the bake VM behind regardless
        of outcome.  Equivalent to ``onSuccess`` on the failure
        path but documents the explicit ``never`` opt-in path
        too."""
        token = secrets.token_hex(3)
        images = self.register_base_images()
        base_disk = images["alpine"]
        tpl_name = f"corvus-it-build-tpl-never-{token}"
        artifact_name = f"corvus-it-should-not-exist-never-{token}"

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
                    sizeGb: 1
                  vm:
                    cpuCount: 2
                    ramMb: 1024
                  provisioners:
                    - shell: "exit 7"
                  cleanup: never
                  waitForShutdownSec: 300
        """).strip()
        try:
            end = _run_pipeline(self.client, pipeline_yaml)
            bo = end.builds[0]
            assert bo.error_message, bo
            vm_names = [v.name for v in self.client.vms.list()]
            assert any(n.startswith("__build_") for n in vm_names), vm_names
        finally:
            _drop_build_orphans(self.client)
            tpl.delete()

    # ---- apply step inside pipeline -------------------------------------

    def test_apply_step_inside_pipeline(self):
        """A pipeline interleaving ``apply:`` and ``build:`` steps
        runs both end-to-end.

        Pipeline composition:
          1. ``apply:`` creates a stub disk + an SSH key.
          2. ``build:`` targets that disk with ``ifExists: skip``
             so the daemon short-circuits the bake and returns
             the freshly-applied disk's id.

        Verifies the apply step's side-effects (SSH key, disk)
        survive the pipeline and the build step's returned id
        matches the apply's disk."""
        token = secrets.token_hex(3)
        images = self.register_base_images()
        base_disk = images["alpine"]
        tpl_name = f"corvus-it-build-tpl-mix-{token}"
        artifact_name = f"corvus-it-build-mode-mix-{token}"
        key_name = f"corvus-it-build-mode-key-{token}"
        # Static placeholder; the daemon stores the text verbatim.
        pubkey = (
            "ssh-ed25519 "
            "AAAAC3NzaC1lZDI1NTE5AAAAIH3OAFlPq8wAYIKL3kZx0sMo2krfh1g+OmRkLD1OvBnK"
            f" corvus-it-build-mode-{token}"
        )

        tpl = self.client.templates.create(
            _BAKE_TEMPLATE.format(tpl_name=tpl_name, base_disk=base_disk)
        )
        pipeline_yaml = textwrap.dedent(f"""
            pipeline:
              - apply:
                  sshKeys:
                    - name: {key_name}
                      publicKey: "{pubkey}"
                  disks:
                    - name: {artifact_name}
                      sizeMb: 64
                      format: qcow2
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
            end = _run_pipeline(self.client, pipeline_yaml)
            # Two `BuildOneResult` entries — one per pipeline step.
            assert len(end.builds) == 2, [b.name for b in end.builds]
            # The build step's result name matches the build name;
            # error_message is None and artifact_disk_id is set
            # (returned by the ifExists: skip short-circuit).
            build_step = next(
                (b for b in end.builds if "corvus-it-build" in b.name), None
            )
            assert build_step is not None, [b.name for b in end.builds]
            assert not build_step.error_message, build_step
            assert build_step.artifact_disk_id, build_step

            # Apply step's side-effects landed.
            disk = self.client.disks.get(artifact_name, by_name=True).show()
            assert disk.size_mb == 64, disk
            assert build_step.artifact_disk_id == disk.id, (build_step, disk)
            keys = [k.name for k in self.client.ssh_keys.list()]
            assert key_name in keys, keys

            # No bake VM materialised (ifExists: skip short-circuit).
            vm_names = [v.name for v in self.client.vms.list()]
            assert not any(n.startswith("__build_") for n in vm_names), vm_names
        finally:
            try:
                self.client.disks.get(artifact_name, by_name=True).delete()
            except CorvusError:
                pass
            try:
                self.client.ssh_keys.get(key_name, by_name=True).delete()
            except CorvusError:
                pass
            tpl.delete()
