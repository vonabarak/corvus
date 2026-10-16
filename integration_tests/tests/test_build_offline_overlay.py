"""Offline image edits preserve the source's first-boot behavior."""

from __future__ import annotations

import json
import secrets
import shlex
from collections.abc import Generator
from contextlib import contextmanager

import pytest
from corvus_client.types import BuildLogLine, BuildPipelineEnd, BuildStreamItem
from corvus_test_harness import SingleNodeCase

import yaml

pytestmark = [pytest.mark.slow, pytest.mark.timeout(3600)]


class TestBuildOfflineOverlay(SingleNodeCase):
    @contextmanager
    def _resources(self) -> Generator[str]:
        prefix = f"offline-{secrets.token_hex(3)}"
        try:
            yield prefix
        finally:
            for vm in self.client.vms.list():
                if prefix in vm.name:
                    cap = self.client.vms.get(vm.id)
                    cap.reset()
                    cap.delete()
            for template in self.client.templates.list():
                if template.name.startswith(prefix):
                    self.client.templates.get(template.id).delete()
            for image in reversed(self.client.disks.list()):
                if prefix in image.name:
                    self.client.disks.get(image.id).delete()

    def _run(
        self, steps: list[dict[str, object]]
    ) -> tuple[BuildPipelineEnd, list[BuildStreamItem]]:
        events = list(
            self.client.build_stream_text(yaml.safe_dump({"pipeline": steps}))
        )
        end = next(event for event in events if isinstance(event, BuildPipelineEnd))
        if any(build.error_message for build in end.builds):
            print(events)
        return end, events

    @pytest.mark.parametrize("output_format", ["qcow2", "raw"])
    def test_firstboot_preserved_and_only_target_published(
        self, output_format: str
    ) -> None:
        base = self.register_base_images()["corvus-test-vm"]
        with self._resources() as prefix:
            template, source, output = (
                f"{prefix}-{suffix}" for suffix in ("helper", "source", "output")
            )
            end, _ = self._run(
                [
                    {
                        "apply": {
                            "templates": [
                                {
                                    "name": template,
                                    "cpuCount": 2,
                                    "ram": "1024M",
                                    "headless": True,
                                    "guestAgent": True,
                                    "drives": [
                                        {
                                            "diskImage": base,
                                            "interface": "virtio",
                                            "strategy": "overlay",
                                            "size": "2G",
                                        }
                                    ],
                                }
                            ]
                        }
                    },
                    {
                        "build": {
                            "name": source,
                            "template": template,
                            "target": {},
                            "provisioners": [
                                {
                                    "shell": """set -eux
export PATH=/usr/sbin:/usr/bin:/sbin:/bin
# QGA starts in the boot runlevel, before the default runlevel finishes.
while pidof openrc >/dev/null; do sleep 1; done
mkdir -p /etc/local.d
printf '#!/bin/sh\necho firstboot > /etc/firstboot-ran\n' > /etc/local.d/firstboot.start
chmod +x /etc/local.d/firstboot.start
rc-update add local default
"""
                                }
                            ],
                        }
                    },
                ]
            )
            assert not [build for build in end.builds if build.error_message], end
            # The fixtures share a filesystem label. Pin the helper root to its
            # first drive so kernel label discovery cannot select the target.
            helper_image = f"{prefix}-bootstrap"
            end, _ = self._run(
                [
                    {
                        "build": {
                            "name": helper_image,
                            "template": template,
                            "target": {},
                            "provisioners": [
                                {
                                    "shell": "sed -i 's#LABEL=root#/dev/vda3#g' /etc/fstab /boot/grub/grub.cfg"
                                }
                            ],
                        }
                    }
                ]
            )
            assert not end.builds[0].error_message, end
            self.client.templates.get(template, by_name=True).delete()
            end, _ = self._run(
                [
                    {
                        "apply": {
                            "templates": [
                                {
                                    "name": template,
                                    "cpuCount": 2,
                                    "ram": "1024M",
                                    "headless": True,
                                    "guestAgent": True,
                                    "drives": [
                                        {
                                            "diskImage": helper_image,
                                            "interface": "virtio",
                                            "strategy": "overlay",
                                            "size": "2G",
                                        }
                                    ],
                                }
                            ]
                        }
                    }
                ]
            )
            assert not [build for build in end.builds if build.error_message], end
            source_disk = self.client.disks.get(source, by_name=True)
            source_disk.tag("stable")
            source_disk.snapshot_create("before-offline")
            source_info = source_disk.show()
            source_path = shlex.quote(source_info.placements[0].file_path)
            before = self.node.run(f"sha256sum {source_path}").stdout
            recipe: dict[str, object] = {
                "name": output,
                "template": template,
                "strategy": "offline-overlay",
                "sourceImage": f"{source}:stable",
                "target": {
                    "format": output_format,
                    "ifExists": "update",
                    "compact": False,
                },
                "provisioners": [
                    {
                        "shell": """set -eux
test "$CORVUS_BUILD_STRATEGY" = offline-overlay
test ! -e /etc/local.d/firstboot.start
printf helper > /etc/helper-only
mkdir -p /mnt/target
mount /dev/vdb3 /mnt/target
trap 'umount /mnt/target' EXIT
test -x /mnt/target/etc/local.d/firstboot.start
test ! -e /mnt/target/etc/firstboot-ran
printf customized > /mnt/target/etc/offline-custom
sync
"""
                    }
                ],
            }
            if output_format == "raw":
                recipe["target"] = {
                    "format": "raw",
                    "size": "3G",
                    "ifExists": "update",
                    "compact": False,
                }
            end, _ = self._run([{"build": recipe}])
            assert not end.builds[0].error_message, end
            artifact_id = end.builds[0].artifact_disk_id
            assert artifact_id is not None
            artifact = self.client.disks.get(artifact_id).show()
            assert artifact.format.lower() == output_format
            assert artifact.size == (
                3 * 1024**3 if output_format == "raw" else source_info.size
            )
            assert artifact.backing_image is None
            info = json.loads(
                self.node.run(
                    f"qemu-img info --output=json {shlex.quote(artifact.placements[0].file_path)}"
                ).stdout
            )
            assert "backing-filename" not in info
            assert not info.get("snapshots")
            assert self.node.run(f"sha256sum {source_path}").stdout == before
            assert len(source_disk.snapshot_list()) == 1
            recipe["sourceImage"] = source_info.id
            end, events = self._run([{"build": recipe}])
            assert end.builds[0].artifact_disk_id == artifact_id
            assert any(
                isinstance(event, BuildLogLine)
                and "inputs unchanged; skipping bake" in event.line
                for event in events
            )

            if output_format == "qcow2":
                # Moving a floating selector changes identity even when image bytes match.
                replacement = self.client.disks.clone(source_info.id, source)
                replacement.tag("stable")
                recipe["sourceImage"] = f"{source}:stable"
                recipe["target"] = {
                    "format": "qcow2",
                    "size": "2G",
                    "ifExists": "update",
                    "compact": False,
                }
                end, _ = self._run([{"build": recipe}])
                assert not end.builds[0].error_message, end
                assert end.builds[0].artifact_disk_id != artifact_id

                # Validation and provisioner failures must reap both helper and target.
                for suffix, target, shell in [
                    ("shrink", {"size": "1G"}, "true"),
                    ("failure", {}, "exit 1"),
                ]:
                    failed = dict(
                        recipe,
                        name=f"{prefix}-{suffix}",
                        target=target,
                        provisioners=[{"shell": shell}],
                    )
                    end, _ = self._run([{"build": failed}])
                    assert end.builds[0].error_message, end
                    assert not any(
                        prefix in vm.name and vm.name.startswith("__build_")
                        for vm in self.client.vms.list()
                    )
                    assert not any(
                        prefix in disk.name and disk.name.startswith("__build_")
                        for disk in self.client.disks.list()
                    )

            # A later consumer boots the customized image and runs the preserved service.
            overlay = f"{prefix}-verify"
            self.client.disks.create_overlay(overlay, artifact_id, ephemeral=True)
            vm = self.client.vms.create(
                f"{prefix}-verify",
                cpu_count=1,
                ram=536870912,
                headless=True,
                guest_agent=True,
            )
            vm.attach_disk(disk_ref=overlay, interface="virtio")
            vm.start(wait=True)
            result = vm.guest_exec(
                "/bin/sh -c 'for i in $(seq 1 60); do test -f /etc/firstboot-ran && break; sleep 1; done; test -f /etc/firstboot-ran && test ! -e /etc/helper-only && test -f /etc/offline-custom'"
            )
            assert result.exit_code == 0, result
            vm.stop(wait=True)
            vm.delete()
            assert not any(
                prefix in vm.name and vm.name.startswith("__build_")
                for vm in self.client.vms.list()
            )
