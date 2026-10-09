"""Cleanup through RPC and CLI, with real nodeagent files."""

from __future__ import annotations

import json
import os
import subprocess
from pathlib import Path

import pytest
from corvus_client import AsyncClient

from ._helpers import with_client
from .conftest import _bin_search


def test_cleanup_preview_tags_snapshots_and_files(daemon_socket: Path) -> None:
    async def go(c: AsyncClient) -> None:
        old = await c.disks.create("cleanup-family", size=1048576)
        old_info = await old.show()
        await old.snapshot_create("old-snapshot")
        tagged = await c.disks.create("cleanup-family:stable", size=1048576)
        tagged_info = await tagged.show()
        latest = await c.disks.create("cleanup-family", size=1048576)
        latest_info = await latest.show()
        tasks_before = len(await c.tasks.list())
        preview = await c.disks.cleanup("cleanup-family", dry_run=True)
        assert preview.dry_run and preview.removed_versions == 0
        assert len(await c.tasks.list()) == tasks_before
        assert {v.disk_image.id: v.status for v in preview.versions} == {
            old_info.id: "planned",
            tagged_info.id: "retained",
            latest_info.id: "retained",
        }
        assert (
            daemon_socket.parent / "home" / "VMs" / old_info.placements[0].file_path
        ).exists()
        report = await c.disks.cleanup("cleanup-family")
        assert report.removed_versions == report.removed_placements == 1
        assert report.failures == 0
        assert not (
            daemon_socket.parent / "home" / "VMs" / old_info.placements[0].file_path
        ).exists()
        assert (
            daemon_socket.parent / "home" / "VMs" / tagged_info.placements[0].file_path
        ).exists()
        report = await c.disks.cleanup("cleanup-family", include_tagged=True)
        assert report.removed_versions == 1
        assert not (
            daemon_socket.parent / "home" / "VMs" / tagged_info.placements[0].file_path
        ).exists()
        assert (await latest.show()).id == latest_info.id
        assert (await c.disks.cleanup("cleanup-family")).removed_versions == 0
        await latest.delete()

    with_client(daemon_socket)(go)


def test_cleanup_cli_all_overlay_order(daemon_socket: Path) -> None:
    async def prepare(c: AsyncClient) -> tuple[int, int, list[str], list[int]]:
        base = await c.disks.create("cleanup-base", size=1048576)
        base_info = await base.show()
        overlay = await c.disks.create_overlay(
            "cleanup-overlay", backing_disk_ref=base_info.id
        )
        overlay_info = await overlay.show()
        new_base = await c.disks.create("cleanup-base", size=1048576)
        new_overlay = await c.disks.create("cleanup-overlay", size=1048576)
        return (
            base_info.id,
            overlay_info.id,
            [base_info.placements[0].file_path, overlay_info.placements[0].file_path],
            [(await new_base.show()).id, (await new_overlay.show()).id],
        )

    base, overlay, paths, latest = with_client(daemon_socket)(prepare)
    result = subprocess.run(
        [_bin_search("crv", "CORVUS_CRV"), "-o", "json", "disk", "cleanup", "--all"],
        capture_output=True,
        text=True,
        check=False,
        env=os.environ | {"CORVUS_SOCKET": str(daemon_socket)},
    )
    assert result.returncode == 0, result.stderr
    report = json.loads(result.stdout)
    removed = [
        v["disk_image"]["id"] for v in report["versions"] if v["version_deleted"]
    ]
    assert removed.index(overlay) < removed.index(base)
    assert all(
        not (daemon_socket.parent / "home" / "VMs" / path).exists() for path in paths
    )

    async def finish(c: AsyncClient) -> None:
        for key in latest:
            await (await c.disks.get(key)).delete()

    with_client(daemon_socket)(finish)


@pytest.mark.parametrize(
    "name,all_images",
    [
        (None, False),
        ("family", True),
        ("family:tag", False),
        ("123", False),
        ("", False),
    ],
)
def test_cleanup_target_validation(
    daemon_socket: Path, name: str | None, all_images: bool
) -> None:
    async def go(c: AsyncClient) -> None:
        with pytest.raises(ValueError):
            await c.disks.cleanup(name, all_images=all_images)

    with_client(daemon_socket)(go)


def test_cleanup_protects_templates_and_attachments(daemon_socket: Path) -> None:
    async def go(c: AsyncClient) -> None:
        pinned = await c.disks.create("cleanup-protected:pin", size=1048576)
        floating = await c.disks.create("cleanup-protected:stable", size=1048576)
        attached = await c.disks.create("cleanup-protected", size=1048576)
        latest = await c.disks.create("cleanup-protected", size=1048576)
        pinned_id = (await pinned.show()).id
        floating_id = (await floating.show()).id
        attached_id = (await attached.show()).id
        templates = []
        vm = await c.vms.create("cleanup-protection-vm", headless=True, ram=134217728)
        try:
            for label, selector in [
                ("pinned", str(pinned_id)),
                ("floating", "cleanup-protected:stable"),
            ]:
                templates.append(
                    await c.templates.create(
                        f"name: cleanup-{label}\ncpuCount: 1\nram: 128M\ndrives:\n"
                        f"  - diskImage: {selector}\n    strategy: overlay\n    interface: virtio\n"
                    )
                )
            await vm.attach_disk(attached_id)
            report = await c.disks.cleanup("cleanup-protected", include_tagged=True)
            assert report.removed_versions == 0
            versions = {v.disk_image.id: v for v in report.versions}
            assert (
                versions[pinned_id].reason
                == versions[floating_id].reason
                == "template reference"
            )
            assert versions[attached_id].placements[0].reason == "attached VM on node"
        finally:
            await vm.delete(keep_disks=True)
            for template in templates:
                await template.delete()
            await c.disks.cleanup("cleanup-protected", include_tagged=True)
            await latest.delete()

    with_client(daemon_socket)(go)
