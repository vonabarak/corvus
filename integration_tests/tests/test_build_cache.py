"""Build-step cache against the inner daemon.

Exercises the per-step caching feature end-to-end against a real
bake VM:

  * **Prime**: a 3-step shell-provisioner pipeline with
    ``--build-cache`` produces three :class:`BuildStepCacheStore`
    events and retains the bake VM after teardown (no ephemeral
    delete; ``cleanupBakeVm`` skips ``VmDelete`` because cache rows
    pin the VM).
  * **Reuse**: re-running the same pipeline with ``--use-cache``
    emits a single :class:`BuildStepCacheRestore` followed by three
    :class:`BuildStepCacheHit` events; no provisioners actually run
    (no :class:`BuildStepStart` for those steps).
  * **Partial reuse**: editing the second step's shell and re-running
    with both flags hits step 1, misses steps 2 & 3.
  * **Stale cache fall-back**: priming the cache, then deleting the
    cached bake VM by hand, then re-running with ``--use-cache``
    triggers the daemon's fallback path — the build completes via a
    fresh bake instead of erroring out.

Doubly-nested KVM: the inner daemon spawns a bake VM inside the
test-node VM (~3-5 min per bake nested).
"""

from __future__ import annotations

import secrets
import textwrap

import pytest
from corvus_client.types import (
    BuildPipelineEnd,
    BuildStepCacheHit,
    BuildStepCacheRestore,
    BuildStepCacheStore,
    BuildStepEnd,
    BuildStepStart,
)
from corvus_test_harness import SingleNodeCase

pytestmark = pytest.mark.timeout(3600)


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


def _three_step_pipeline(
    *,
    tpl_name: str,
    artifact_name: str,
    step2_body: str = "echo step-two-baseline",
) -> str:
    """Three-shell-provisioner pipeline. ``step2_body`` is the only
    field varied between cache-edit scenarios so step 1's chain hash
    stays stable across runs.
    """
    # Pin cacheMode: disk explicitly. The default switched to
    # memory when the vmstate-aware path landed; this test class
    # specifically exercises the disk-mode roundtrip (offline
    # qcow2 rollback + fresh VmStart). Memory-mode reuse has
    # its own targeted test —
    # ``test_memory_mode_preserves_kernel_state_across_cache_resume`` —
    # that proves the snapshot-load lifecycle preserves tmpfs
    # state, which disk mode by definition can't.
    return textwrap.dedent(f"""
        pipeline:
          - build:
              name: {artifact_name}
              template: {tpl_name}
              strategy: overlay
              cacheMode: disk
              target:
                format: qcow2
                sizeGb: 2
                compact: true
                ifExists: overwrite
              vm:
                cpuCount: 2
                ramMb: 1024
              provisioners:
                - shell: |
                    set -eux
                    mkdir -p /var/lib/corvus-cache
                    echo step-one > /var/lib/corvus-cache/step1
                - shell: |
                    set -eux
                    {step2_body} > /var/lib/corvus-cache/step2
                - shell: |
                    set -eux
                    echo step-three > /var/lib/corvus-cache/step3
              cleanup: always
              waitForShutdownSec: 300
    """).strip()


def _bake_vms(client) -> list[str]:
    return [v.name for v in client.vms.list() if v.name.startswith("__build_")]


def _collect_events(stream) -> list:
    out: list = []
    for ev in stream:
        if isinstance(ev, tuple):
            continue
        out.append(ev)
    return out


def _classify(events) -> dict:
    return {
        "starts": [e for e in events if isinstance(e, BuildStepStart)],
        "ends": [e for e in events if isinstance(e, BuildStepEnd)],
        "hits": [e for e in events if isinstance(e, BuildStepCacheHit)],
        "stores": [e for e in events if isinstance(e, BuildStepCacheStore)],
        "restores": [e for e in events if isinstance(e, BuildStepCacheRestore)],
        "pipeline_end": next(
            (e for e in events if isinstance(e, BuildPipelineEnd)), None
        ),
    }


class TestBuildCache(SingleNodeCase):
    def test_cache_prime_reuse_and_partial_reuse(self):
        """End-to-end exercise of the cache write + read paths.

        Three sub-builds against the same artifact name (``ifExists:
        overwrite`` lets each run replace the previous published
        clone):

          1. ``--build-cache`` only — prime the cache. Expect 3
             :class:`BuildStepCacheStore` events; the bake VM
             survives the run.
          2. ``--use-cache`` only — full reuse. Expect 1
             :class:`BuildStepCacheRestore` + 3
             :class:`BuildStepCacheHit`; no provisioners run.
          3. ``--use-cache --build-cache`` after editing step 2's
             shell. Expect 1 cache hit (step 1) + 2 cache stores
             (steps 2 & 3); the bake VM is the SAME VM as before
             (cache reuse hands off to the cached VM, not a fresh one).
        """
        token = secrets.token_hex(4)
        images = self.register_base_images()
        base_disk = images["alpine"]
        tpl_name = f"corvus-it-cache-tpl-{token}"
        artifact_name = f"corvus-it-cache-art-{token}"

        tpl = self.client.templates.create(
            _BAKE_TEMPLATE.format(tpl_name=tpl_name, base_disk=base_disk)
        )
        try:
            # ---- pass 1: prime the cache -------------------------
            pipeline_v1 = _three_step_pipeline(
                tpl_name=tpl_name, artifact_name=artifact_name
            )
            events = _collect_events(
                self.client.build_stream_text(pipeline_v1, build_cache=True)
            )
            c = _classify(events)
            assert c["pipeline_end"] is not None
            bo = next(
                (b for b in c["pipeline_end"].builds if b.name == artifact_name),
                None,
            )
            assert bo is not None, [b.name for b in c["pipeline_end"].builds]
            assert bo.artifact_disk_id, bo
            assert not bo.error_message, bo
            assert len(c["starts"]) == 3, c["starts"]
            assert len(c["stores"]) == 3, c["stores"]
            assert not c["hits"]
            assert not c["restores"]
            store_steps = sorted(s.step_index for s in c["stores"])
            assert store_steps == [1, 2, 3], store_steps
            # Bake VM must survive — cache rows pin it.
            primed_bake_vms = _bake_vms(self.client)
            assert len(primed_bake_vms) >= 1, primed_bake_vms

            # ---- pass 2: full reuse ------------------------------
            events = _collect_events(
                self.client.build_stream_text(pipeline_v1, use_cache=True)
            )
            c = _classify(events)
            assert c["pipeline_end"] is not None
            assert c["pipeline_end"].builds[0].name == artifact_name
            assert c["pipeline_end"].builds[0].artifact_disk_id
            assert len(c["restores"]) == 1, c["restores"]
            assert c["restores"][0].prefix == 3, c["restores"]
            hit_steps = sorted(h.step_index for h in c["hits"])
            assert hit_steps == [1, 2, 3], hit_steps
            # No provisioners actually ran on a full cache hit.
            assert not c["starts"], c["starts"]
            assert not c["stores"], c["stores"]
            # Same bake VM stayed alive (one cached chain == one VM).
            reused_bake_vms = _bake_vms(self.client)
            assert set(primed_bake_vms).issubset(set(reused_bake_vms))

            # ---- pass 3: edit step 2, partial reuse --------------
            pipeline_v2 = _three_step_pipeline(
                tpl_name=tpl_name,
                artifact_name=artifact_name,
                step2_body="echo step-two-CHANGED",
            )
            events = _collect_events(
                self.client.build_stream_text(
                    pipeline_v2, use_cache=True, build_cache=True
                )
            )
            c = _classify(events)
            assert c["pipeline_end"] is not None
            assert c["pipeline_end"].builds[0].artifact_disk_id
            # Step 1 hash unchanged -> hit. Steps 2 & 3 changed -> miss.
            assert len(c["restores"]) == 1, c["restores"]
            assert c["restores"][0].prefix == 1, c["restores"]
            assert [h.step_index for h in c["hits"]] == [1]
            store_steps = sorted(s.step_index for s in c["stores"])
            assert store_steps == [2, 3], store_steps
            # Steps 2 and 3 had real provisioner starts.
            executed = sorted(s.step_index for s in c["starts"])
            assert executed == [2, 3], executed
            # The bake VM is preserved across the edit too.
            after_edit_bake_vms = _bake_vms(self.client)
            assert set(primed_bake_vms).issubset(set(after_edit_bake_vms))
        finally:
            for v in self.client.vms.list():
                if v.name.startswith("__build_"):
                    self.client.vms.get(v.name, by_name=True).delete()
            self.client.disks.get(artifact_name, by_name=True).delete()
            tpl.delete()

    def test_cache_falls_back_to_fresh_when_cached_vm_is_deleted(self):
        """Stale cache rows: cached VM deleted out-of-band.

        After priming the cache, the operator runs ``crv vm delete``
        on the retained bake VM (which cascades the cache rows via
        the FK in ``deleteVm``). A subsequent ``--use-cache`` build
        finds no cache prefix and falls through cleanly to a fresh
        bake — no error, the build completes normally.

        This also covers the case the cache READ design specifically
        guards against: a Vm row that's gone but had cache rows
        referencing it. The cascade in ``deleteVm`` should prevent
        that state, but the runtime fallback covers the gap if a
        future bug ever leaves orphan rows behind.
        """
        token = secrets.token_hex(4)
        images = self.register_base_images()
        base_disk = images["alpine"]
        tpl_name = f"corvus-it-cache-fallback-tpl-{token}"
        artifact_name = f"corvus-it-cache-fallback-art-{token}"

        tpl = self.client.templates.create(
            _BAKE_TEMPLATE.format(tpl_name=tpl_name, base_disk=base_disk)
        )
        try:
            pipeline_yaml = _three_step_pipeline(
                tpl_name=tpl_name, artifact_name=artifact_name
            )

            # Prime the cache.
            events = _collect_events(
                self.client.build_stream_text(pipeline_yaml, build_cache=True)
            )
            c = _classify(events)
            assert len(c["stores"]) == 3
            primed = _bake_vms(self.client)
            assert primed

            # Wipe the bake VM out from under the cache.
            for name in primed:
                self.client.vms.get(name, by_name=True).delete()

            # Re-run with --use-cache. The cache rows are gone via FK
            # cascade, so lookupCachePrefix returns prefix=0 and the
            # runner does a fresh bake. The build should complete.
            events = _collect_events(
                self.client.build_stream_text(pipeline_yaml, use_cache=True)
            )
            c = _classify(events)
            assert c["pipeline_end"] is not None
            bo = c["pipeline_end"].builds[0]
            assert bo.name == artifact_name
            assert bo.artifact_disk_id, bo
            assert not bo.error_message, bo
            # Fresh bake: no cache hits or restores fired this run.
            assert not c["hits"], c["hits"]
            assert not c["restores"], c["restores"]
            # All three provisioners ran from scratch.
            executed = sorted(s.step_index for s in c["starts"])
            assert executed == [1, 2, 3], executed
        finally:
            for v in self.client.vms.list():
                if v.name.startswith("__build_"):
                    self.client.vms.get(v.name, by_name=True).delete()
            try:
                self.client.disks.get(artifact_name, by_name=True).delete()
            except Exception:
                pass
            tpl.delete()

    def test_memory_mode_preserves_kernel_state_across_cache_resume(self):
        """The whole reason ``cacheMode: memory`` exists: a file in
        tmpfs (``/tmp``) created in step 2 must still be there when
        step 3 runs from a cache resume. Disk mode would lose it —
        the cache restore reboots the bake VM and tmpfs vanishes.
        Memory mode loads the vmstate, so the kernel's mount table
        and tmpfs page cache come back intact.

        Test shape:
          1. Prime: 3-step pipeline, step 2 writes a token to
             ``/tmp/memory-marker``, step 3 reads it back. All
             three steps run, three cache rows written.
          2. Edit step 3 to change its hash; re-run with
             ``--use-cache``. Steps 1 + 2 hit cache; step 3 runs
             fresh against the memory-resumed VM. If the
             snapshot-load lifecycle worked, ``/tmp/memory-marker``
             is still present and step 3 succeeds. If not, step 3
             can't find the file and the build errors.
        """
        token = secrets.token_hex(4)
        images = self.register_base_images()
        base_disk = images["alpine"]
        tpl_name = f"corvus-it-mem-cache-tpl-{token}"
        artifact_name = f"corvus-it-mem-cache-art-{token}"

        tpl = self.client.templates.create(
            _BAKE_TEMPLATE.format(tpl_name=tpl_name, base_disk=base_disk)
        )

        def pipeline(step3_extra: str) -> str:
            return textwrap.dedent(f"""
                pipeline:
                  - build:
                      name: {artifact_name}
                      template: {tpl_name}
                      strategy: overlay
                      cacheMode: memory
                      target:
                        format: qcow2
                        sizeGb: 2
                        compact: true
                        ifExists: overwrite
                      vm:
                        cpuCount: 2
                        ramMb: 1024
                      provisioners:
                        - shell: |
                            set -eux
                            echo step-one > /var/lib/corvus-mem-step1
                        - shell: |
                            set -eux
                            echo {token} > /tmp/memory-marker
                        - shell: |
                            set -eux
                            test "$(cat /tmp/memory-marker)" = "{token}"
                            {step3_extra}
                      cleanup: always
                      waitForShutdownSec: 300
            """).strip()

        try:
            # Pass 1: prime the cache. All 3 steps run.
            events = _collect_events(
                self.client.build_stream_text(pipeline(":"), build_cache=True)
            )
            c = _classify(events)
            assert c["pipeline_end"] is not None, events
            assert not c["pipeline_end"].builds[0].error_message, (
                f"prime failed: {c['pipeline_end'].builds[0].error_message}"
            )
            assert len(c["starts"]) == 3, c["starts"]
            assert len(c["stores"]) == 3, c["stores"]

            # Pass 2: edit step 3 so its hash differs, re-run with
            # --use-cache. Steps 1+2 should hit, step 3 runs against
            # the memory-resumed VM. The success of step 3's `test`
            # against /tmp/memory-marker proves vmstate restored the
            # tmpfs state from step 2.
            events = _collect_events(
                self.client.build_stream_text(
                    pipeline("echo VERIFIED"),
                    use_cache=True,
                    build_cache=True,
                )
            )
            c = _classify(events)
            assert c["pipeline_end"] is not None, events
            bo = c["pipeline_end"].builds[0]
            assert bo.artifact_disk_id, bo
            assert not bo.error_message, (
                f"step 3 failed — memory-mode resume didn't preserve "
                f"/tmp/memory-marker: {bo.error_message}"
            )
            assert len(c["restores"]) == 1, c["restores"]
            assert c["restores"][0].prefix == 2, c["restores"]
            hit_steps = sorted(h.step_index for h in c["hits"])
            assert hit_steps == [1, 2], hit_steps
            # Step 3 was the only one that actually ran.
            executed = sorted(s.step_index for s in c["starts"])
            assert executed == [3], executed
        finally:
            for v in self.client.vms.list():
                if v.name.startswith("__build_"):
                    self.client.vms.get(v.name, by_name=True).delete()
            try:
                self.client.disks.get(artifact_name, by_name=True).delete()
            except Exception:
                pass
            tpl.delete()
