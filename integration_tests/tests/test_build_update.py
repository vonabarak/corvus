"""Persistent build input tracking against the real daemon and bake VMs."""

from __future__ import annotations

import secrets
from collections.abc import Generator
from contextlib import contextmanager

import pytest
from corvus_client.types import (
    BuildLogLine,
    BuildPipelineEnd,
    BuildStepCacheRestore,
    BuildStepCacheStore,
    BuildStepStart,
    BuildStreamItem,
)
from corvus_test_harness import SingleNodeCase, SqliteDatabase

import yaml

pytestmark = [pytest.mark.slow, pytest.mark.timeout(3600)]


def _template(name: str, source: str) -> dict[str, object]:
    return {
        "name": name,
        "cpuCount": 2,
        "ram": "1024M",
        "headless": True,
        "guestAgent": True,
        "drives": [
            {
                "diskImage": source,
                "strategy": "overlay",
                "interface": "virtio",
                "size": "2G",
            }
        ],
    }


def _build(
    name: str, template: str, shell: str = "true", policy: str = "update"
) -> dict[str, object]:
    return {
        "name": name,
        "template": template,
        "strategy": "overlay",
        "cacheMode": "disk",
        "target": {"ifExists": policy, "format": "qcow2", "size": "2G"},
        "vm": {"cpuCount": 2, "ram": "1024M"},
        "provisioners": [{"shell": shell}],
        "cleanup": "always",
    }


class TestBuildUpdate(SingleNodeCase):
    @contextmanager
    def _resources(self) -> Generator[str]:
        """Own all versions and cache-retained bake VMs created by one scenario."""
        prefix = f"update-{secrets.token_hex(3)}"
        try:
            yield prefix
        finally:
            for vm in self.client.vms.list():
                if vm.name.startswith("__build_") and prefix in vm.name:
                    self.client.vms.get(vm.id).delete()
            for template in self.client.templates.list():
                if template.name.startswith(prefix):
                    self.client.templates.get(template.id).delete()
            for image in reversed(self.client.disks.list()):
                if image.name.startswith(prefix):
                    self.client.disks.get(image.id).delete()

    def _run(
        self,
        steps: list[dict[str, object]],
        *,
        use_cache: bool = False,
        build_cache: bool = False,
        rebuild_from: int = 0,
    ) -> tuple[dict[str, int], list[BuildStreamItem]]:
        events = list(
            self.client.build_stream_text(
                yaml.safe_dump({"pipeline": steps}),
                use_cache=use_cache,
                build_cache=build_cache,
                rebuild_from=rebuild_from,
            )
        )
        end = next(event for event in events if isinstance(event, BuildPipelineEnd))
        assert not [build for build in end.builds if build.error_message], end
        artifacts = {
            build.name: build.artifact_disk_id
            for build in end.builds
            if build.artifact_disk_id is not None
        }
        return artifacts, events

    @staticmethod
    def _assert_skipped(events: list[BuildStreamItem], count: int) -> None:
        assert not any(
            isinstance(event, (BuildStepStart, BuildStepCacheRestore))
            for event in events
        )
        assert (
            sum(
                isinstance(event, BuildLogLine)
                and "inputs unchanged; skipping bake" in event.line
                for event in events
            )
            == count
        )

    def test_chain_tracks_independent_import_recipe_changes_and_retry_after_restart(
        self,
    ) -> None:
        """Recreated templates do not invalidate inputs. A separately imported
        source version rebuilds its whole chain while an unrelated branch skips.
        A failed downstream build keeps its previous image and provenance; after
        restart, retry skips the completed upstream build and repairs downstream.
        """
        base = self.register_base_images()["alpine"]
        path = self.client.disks.get(base).show().placements[0].file_path
        with self._resources() as prefix, SqliteDatabase(self.node) as database:
            source, first, second, branch = [
                f"{prefix}-{suffix}" for suffix in ("source", "a", "b", "branch")
            ]
            self.wait_for_task(self.client, self.client.disks.import_(source, path))

            def pipeline(
                second_shell: str = "true", first_shell: str = "true"
            ) -> list[dict[str, object]]:
                steps: list[dict[str, object]] = []
                for artifact, dependency, shell in [
                    (first, source, first_shell),
                    (second, first, second_shell),
                    (branch, base, "true"),
                ]:
                    template = artifact + "-tpl"
                    steps.extend(
                        [
                            {
                                "apply": {
                                    "ifExists": "overwrite",
                                    "templates": [_template(template, dependency)],
                                }
                            },
                            {"build": _build(artifact, template, shell)},
                        ]
                    )
                return steps

            initial, _ = self._run(pipeline())
            before = database.query(
                "SELECT COUNT(*) FROM task WHERE command = 'instantiate'"
            )
            same, events = self._run(pipeline(), use_cache=True, build_cache=True)
            assert same == initial
            self._assert_skipped(events, 3)
            assert (
                database.query(
                    "SELECT COUNT(*) FROM task WHERE command = 'instantiate'"
                )
                == before
            )

            self.wait_for_task(self.client, self.client.disks.import_(source, path))
            metadata = database.query(
                "SELECT fingerprint, inputs FROM disk_image_build_identity WHERE disk_image_id = ?",
                (initial[second],),
            )
            failed = list(
                self.client.build_stream_text(
                    yaml.safe_dump({"pipeline": pipeline("exit 7")})
                )
            )
            end = next(event for event in failed if isinstance(event, BuildPipelineEnd))
            assert any(
                result.name == second and result.error_message for result in end.builds
            ), end
            completed_first = self.client.disks.get(first).show().id
            assert completed_first != initial[first]
            assert self.client.disks.get(second).show().id == initial[second]
            assert (
                database.query(
                    "SELECT fingerprint, inputs FROM disk_image_build_identity WHERE disk_image_id = ?",
                    (initial[second],),
                )
                == metadata
            )

            self.client.close()
            self.node._client = None
            self.node.run("sudo systemctl restart corvus.service")
            repaired, events = self._run(pipeline())
            assert repaired[first] == completed_first
            assert repaired[second] != initial[second]
            assert repaired[branch] == initial[branch]
            assert sum(isinstance(event, BuildStepStart) for event in events) == 1

            changed, _ = self._run(
                pipeline(first_shell="echo changed > /tmp/recipe-version")
            )
            assert changed[first] != repaired[first]
            assert changed[second] != repaired[second]
            assert changed[branch] == initial[branch]
            unchanged, events = self._run(
                pipeline(first_shell="echo changed > /tmp/recipe-version")
            )
            assert unchanged == changed
            self._assert_skipped(events, 3)

    def test_missing_metadata_force_and_cache_publication(self) -> None:
        """Old outputs rebuild once. Explicit rebuild and overwrite bypass the
        update check; a named tag checks its selected version independently of
        latest. Cache-resumed publications retain input identities, and changing
        the source cannot restore an old cache. A vanished floating source must
        fail instead of silently reusing output.
        """
        base = self.register_base_images()["alpine"]
        path = self.client.disks.get(base).show().placements[0].file_path
        with self._resources() as prefix, SqliteDatabase(self.node) as database:
            source, artifact, template = [
                f"{prefix}-{suffix}" for suffix in ("source", "image", "tpl")
            ]
            self.wait_for_task(self.client, self.client.disks.import_(source, path))
            self.wait_for_task(self.client, self.client.disks.import_(artifact, path))
            old = self.client.disks.get(artifact).show().id
            self.client.templates.create(yaml.safe_dump(_template(template, source)))

            def pipeline(policy: str = "update") -> list[dict[str, object]]:
                return [{"build": _build(artifact, template, policy=policy)}]

            first, events = self._run(pipeline(), build_cache=True)
            assert first[artifact] != old
            assert any(isinstance(event, BuildStepCacheStore) for event in events)
            same, events = self._run(pipeline(), use_cache=True, build_cache=True)
            assert same == first
            self._assert_skipped(events, 1)
            forced, _ = self._run(pipeline(), rebuild_from=1)
            assert forced[artifact] != first[artifact]
            self.client.disks.get(first[artifact]).tag("stable")
            tagged, events = self._run(
                [{"build": _build(artifact + ":stable", template)}]
            )
            assert tagged[artifact + ":stable"] == first[artifact]
            self._assert_skipped(events, 1)
            assert self.client.disks.get(artifact).show().id == forced[artifact]
            overwritten, events = self._run(pipeline("overwrite"), use_cache=True)
            assert overwritten[artifact] != forced[artifact]
            assert any(isinstance(event, BuildStepCacheRestore) for event in events)
            same, events = self._run(pipeline())
            assert same == overwritten
            self._assert_skipped(events, 1)
            self.wait_for_task(self.client, self.client.disks.import_(source, path))
            changed, events = self._run(pipeline(), use_cache=True)
            assert changed[artifact] != same[artifact]
            assert not any(isinstance(event, BuildStepCacheRestore) for event in events)
            same, events = self._run(pipeline())
            assert same == changed
            self._assert_skipped(events, 1)
            assert database.query(
                "SELECT COUNT(*) FROM disk_image_build_identity WHERE disk_image_id = ?",
                (same[artifact],),
            ) == [[1]]

            # Remove the cache VM's overlays before deleting their source.
            for vm in self.client.vms.list():
                if vm.name.startswith("__build_") and prefix in vm.name:
                    self.client.vms.get(vm.id).delete()
            for image in reversed(self.client.disks.list()):
                if image.name == source:
                    self.client.disks.get(image.id).delete()
            events = list(
                self.client.build_stream_text(yaml.safe_dump({"pipeline": pipeline()}))
            )
            end = next(event for event in events if isinstance(event, BuildPipelineEnd))
            assert (
                end.builds[0].error_message == "build template source image not found"
            )
            assert self.client.disks.get(artifact).show().id == same[artifact]
