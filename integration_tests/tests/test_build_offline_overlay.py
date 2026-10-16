"""Offline image edits preserve the source's first-boot behavior."""

from __future__ import annotations

import json
import secrets
import shlex
from collections.abc import Generator
from contextlib import contextmanager
from pathlib import PurePosixPath

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
            images = {
                image.id: self.client.disks.get(image.id).show()
                for image in self.client.disks.list()
                if prefix in image.name
            }
            while images:
                backing_ids = {
                    image.backing_image.id
                    for image in images.values()
                    if image.backing_image is not None
                }
                leaves = set(images) - backing_ids
                assert leaves, "Image dependency cycle during cleanup"
                for image_id in leaves:
                    self.client.disks.get(image_id).delete()
                    del images[image_id]

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

    @pytest.mark.parametrize(
        "output_format,rebase,compact",
        [
            ("qcow2", True, False),
            ("raw", True, False),
            ("qcow2", False, False),
            ("qcow2", False, True),
        ],
    )
    def test_firstboot_preserved_and_only_target_published(
        self, output_format: str, rebase: bool, compact: bool
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
                    "compact": compact,
                    "rebase": rebase,
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
            assert (artifact.backing_image.id if artifact.backing_image else None) == (
                None if rebase else source_info.id
            )
            info = json.loads(
                self.node.run(
                    f"qemu-img info --output=json {shlex.quote(artifact.placements[0].file_path)}"
                ).stdout
            )
            if rebase:
                assert "backing-filename" not in info
            else:
                assert info["backing-filename"] == source_info.placements[0].file_path
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

            if output_format == "qcow2" and rebase:
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

    @pytest.mark.parametrize("compact", [False, True])
    def test_overlay_retains_nested_backing_and_update_policy(
        self, compact: bool
    ) -> None:
        from corvus_client.exceptions import DiskHasOverlays

        base = self.register_base_images()["corvus-test-vm"]
        with self._resources() as prefix:
            template = f"{prefix}-template"

            def apply_template(image: str | int) -> None:
                end, _ = self._run(
                    [
                        {
                            "apply": {
                                "ifExists": "overwrite",
                                "templates": [
                                    {
                                        "name": template,
                                        "cpuCount": 1,
                                        "ram": "512M",
                                        "headless": True,
                                        "guestAgent": True,
                                        "drives": [
                                            {
                                                "diskImage": image,
                                                "strategy": "overlay",
                                                "interface": "virtio",
                                            }
                                        ],
                                    }
                                ],
                            }
                        }
                    ]
                )
                assert not [b for b in end.builds if b.error_message], end

            apply_template(base)
            parent = f"{prefix}-parent"
            target = {"rebase": False, "compact": compact, "ifExists": "update"}
            recipe: dict[str, object] = {
                "name": parent,
                "template": template,
                "target": target,
                "provisioners": [
                    {
                        "shell": "printf inherited > /etc/rebase-inherited; printf old > /etc/rebase-change; dd if=/dev/urandom of=/etc/rebase-zero bs=4096 count=1; sync"
                    }
                ],
            }
            end, _ = self._run([{"build": recipe}])
            assert not end.builds[0].error_message, end
            parent_id = end.builds[0].artifact_disk_id
            assert parent_id is not None
            parent_disk = self.client.disks.get(parent_id)
            parent_info = parent_disk.show()
            assert parent_info.backing_image is not None
            parent_path = shlex.quote(parent_info.placements[0].file_path)
            before = self.node.run(f"sha256sum {parent_path}").stdout
            apply_template(parent_id)
            recipe["name"] = f"{prefix}-child"
            recipe["provisioners"] = [
                {
                    "shell": "printf new > /etc/rebase-change; dd if=/dev/zero of=/etc/rebase-zero bs=4096 count=1 conv=notrunc; sync"
                }
            ]
            end, _ = self._run([{"build": recipe}])
            assert not end.builds[0].error_message, end
            artifact_id = end.builds[0].artifact_disk_id
            assert artifact_id is not None
            artifact = self.client.disks.get(artifact_id).show()
            assert artifact.backing_image is not None
            assert artifact.backing_image.id == parent_id
            path = shlex.quote(artifact.placements[0].file_path)
            info = json.loads(
                self.node.run(f"qemu-img info --output=json {path}").stdout
            )
            assert info["backing-filename"] == parent_info.placements[0].file_path
            assert not info.get("snapshots")
            assert self.node.run(f"sha256sum {parent_path}").stdout == before
            clone = self.client.disks.clone(artifact_id, f"{prefix}-clone")
            assert clone.show().backing_image is None
            clone_info = json.loads(
                self.node.run(
                    f"qemu-img info --output=json {shlex.quote(clone.show().placements[0].file_path)}"
                ).stdout
            )
            assert "backing-filename" not in clone_info
            end, _ = self._run([{"build": recipe}])
            assert end.builds[0].artifact_disk_id == artifact_id
            # A failed publication must preserve the previous image and tags.
            self.client.disks.get(artifact_id).tag("stable")
            failed = dict(
                recipe,
                target=dict(
                    target, path=artifact.placements[0].file_path, ifExists="overwrite"
                ),
            )
            end, _ = self._run([{"build": failed}])
            assert end.builds[0].error_message, end
            assert (
                self.client.disks.get(f"{prefix}-child:stable", by_name=True).show().id
                == artifact_id
            )
            assert (
                self.client.disks.get(f"{prefix}-child", by_name=True).show().id
                == artifact_id
            )
            assert not self.node.run(
                f"find {shlex.quote(str(PurePosixPath(artifact.placements[0].file_path).parent))} -name '.corvus-image*.part*' -print"
            ).stdout.strip()
            target["rebase"] = True
            end, _ = self._run([{"build": recipe}])
            assert not end.builds[0].error_message, end
            assert end.builds[0].artifact_disk_id != artifact_id
            flat_id = end.builds[0].artifact_disk_id
            assert flat_id is not None
            assert self.client.disks.get(flat_id).show().backing_image is None
            del target["rebase"]
            end, _ = self._run([{"build": recipe}])
            assert end.builds[0].artifact_disk_id == flat_id
            self.client.templates.get(template, by_name=True).delete()
            with pytest.raises(DiskHasOverlays):
                parent_disk.delete()
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
                "/bin/sh -c 'test $(cat /etc/rebase-inherited) = inherited && test $(cat /etc/rebase-change) = new && dd if=/dev/zero bs=4096 count=1 | cmp - /etc/rebase-zero'"
            )
            assert result.exit_code == 0, result
            vm.stop(wait=True)
            vm.delete()
