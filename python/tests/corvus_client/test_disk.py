"""Disk CRUD + overlay + clone + snapshot end-to-end against a real daemon."""

from __future__ import annotations

import json
import os
import subprocess
from pathlib import Path

import pytest
from corvus_client import AsyncClient, DiskNotFound
from corvus_client.exceptions import CorvusError

import yaml

from ._helpers import with_client
from .conftest import _bin_search


def test_disk_create_show_delete(daemon_socket: Path) -> None:
    run = with_client(daemon_socket)

    async def go(c: AsyncClient) -> None:
        disk = await c.disks.create("py-disk-1", size=67108864)
        info = await disk.show()
        assert Path(info.placements[0].file_path).name == f"{info.id}-py-disk-1.qcow2"
        assert info.name == "py-disk-1"
        assert info.size == 67108864
        # disks.get by name and by id both find it
        by_name = await c.disks.get("py-disk-1")
        by_id = await c.disks.get(info.id)
        assert (await by_name.show()).id == info.id
        assert (await by_id.show()).name == "py-disk-1"
        # listing shows our disk
        all_disks = await c.disks.list()
        assert any(d.id == info.id for d in all_disks)
        await disk.delete()
        # After deletion, get raises the typed DiskNotFound exception
        # (translated from the daemon's "Disk not found" message).
        with pytest.raises(DiskNotFound):
            await c.disks.get("py-disk-1")

    run(go)


def test_disk_create_with_custom_directory_path(daemon_socket: Path) -> None:
    run = with_client(daemon_socket)
    name = "py-disk-custom-path"

    async def go(c: AsyncClient) -> None:
        disk = await c.disks.create(name, size=16777216, path="custom-create/")
        try:
            info = await disk.show()
            paths = [placement.file_path for placement in info.placements]
            assert any(
                "custom-create/" in path
                and Path(path).name == f"{info.id}-{name}.qcow2"
                for path in paths
            )
        finally:
            await disk.delete()

    run(go)


def test_cli_disk_create_with_custom_directory_path(daemon_socket: Path) -> None:
    name = "cli-disk-custom-path"
    env = os.environ | {"CORVUS_SOCKET": str(daemon_socket)}
    result = subprocess.run(
        [
            _bin_search("crv", "CORVUS_CRV"),
            "disk",
            "create",
            name,
            "--path",
            "cli-create/",
            "--size",
            "16M",
        ],
        capture_output=True,
        check=False,
        env=env,
        text=True,
    )
    assert result.returncode == 0, result.stderr

    run = with_client(daemon_socket)

    async def go(c: AsyncClient) -> None:
        disk = await c.disks.get(name)
        try:
            info = await disk.show()
            paths = [placement.file_path for placement in info.placements]
            assert any(
                "cli-create/" in path and Path(path).name == f"{info.id}-{name}.qcow2"
                for path in paths
            )
        finally:
            await disk.delete()

    run(go)


def test_disk_overlay_and_clone(daemon_socket: Path) -> None:
    run = with_client(daemon_socket)

    async def go(c: AsyncClient) -> None:
        base = await c.disks.create("py-base", size=67108864)
        base_info = await base.show()

        overlay = await c.disks.create_overlay(
            "py-ovl", backing_disk_ref=base_info.id, path="custom-overlay/"
        )
        ovl_info = await overlay.show()
        assert ovl_info.backing_image is not None
        assert ovl_info.backing_image.id == base_info.id
        assert any(
            "custom-overlay/" in path and path.endswith("-py-ovl.qcow2")
            for path in (placement.file_path for placement in ovl_info.placements)
        )

        cloned = await c.disks.clone(source_ref="py-base", new_name="py-clone")
        cloned_info = await cloned.show()
        assert cloned_info.name == "py-clone"

        for d in (overlay, cloned):
            await d.delete()
        await base.delete()

    run(go)


def test_snapshot_create_and_delete(daemon_socket: Path) -> None:
    run = with_client(daemon_socket)

    async def go(c: AsyncClient) -> None:
        disk = await c.disks.create("py-snap-base", size=67108864)
        s1 = await disk.snapshot_create("first")
        s1_info = await s1.show()
        assert s1_info.name == "first"
        snaps = await disk.snapshot_list()
        assert any(s.name == "first" for s in snaps)
        await s1.delete()
        # Deletion drops it from list
        snaps_after = await disk.snapshot_list()
        assert all(s.name != "first" for s in snaps_after)
        await disk.delete()

    run(go)


def test_sub_megabyte_size_and_cli_output(daemon_socket: Path) -> None:
    """Real qemu-img metadata and RPC preserve a disk smaller than one MiB."""
    run = with_client(daemon_socket)
    name = "sub-megabyte-disk"

    async def go(c: AsyncClient) -> None:
        disk = await c.disks.create(name, size=512, format="raw")
        try:
            assert (await disk.show()).size == 512
            await disk.refresh()
            assert (await disk.show()).size == 512
            binary = _bin_search("crv", "CORVUS_CRV")
            env = os.environ | {"CORVUS_SOCKET": str(daemon_socket)}
            for output in ("json", "yaml", "text"):
                result = subprocess.run(
                    [binary, "-o", output, "disk", "show", name],
                    capture_output=True,
                    check=True,
                    env=env,
                    text=True,
                )
                if output == "json":
                    assert json.loads(result.stdout)["size"] == 512
                elif output == "yaml":
                    assert yaml.safe_load(result.stdout)["size"] == "512B"
                else:
                    assert "512B" in result.stdout
            await disk.resize(1537)
            # qemu-img rounds capacity to a whole 512-byte sector.
            assert (await disk.show()).size == 2048
        finally:
            await disk.delete()

    run(go)


def test_created_disk_records_qemu_sector_rounding(daemon_socket: Path) -> None:
    run = with_client(daemon_socket)

    async def go(c: AsyncClient) -> None:
        disk = await c.disks.create("sector-rounded-disk", size=1537, format="qcow2")
        try:
            assert (await disk.show()).size == 2048
            await disk.refresh()
            assert (await disk.show()).size == 2048
        finally:
            await disk.delete()

    run(go)


def test_upload_collision_preserves_bytes_and_latest(
    daemon_socket: Path, tmp_path: Path
) -> None:
    run = with_client(daemon_socket)
    source = tmp_path / "upload.raw"
    source.write_bytes(b"first version" * 1024)

    async def go(c: AsyncClient) -> None:
        old = await c.disks.upload_from_file(
            "py-upload-atomic:v1", source, format="raw"
        )
        try:
            info = await old.show()
            destination = Path(info.placements[0].file_path)
            original = destination.read_bytes()
            source.write_bytes(b"replacement" * 1024)
            with pytest.raises(CorvusError):
                await c.disks.upload_from_file(
                    "py-upload-atomic:v2", source, format="raw", path=str(destination)
                )
            assert destination.read_bytes() == original
            assert (await (await c.disks.get("py-upload-atomic")).show()).id == info.id
            with pytest.raises(DiskNotFound):
                await c.disks.get("py-upload-atomic:v2")
        finally:
            await old.delete()

    run(go)


def test_upload_rejects_format_mismatch_without_publishing(
    daemon_socket: Path, tmp_path: Path
) -> None:
    run = with_client(daemon_socket)
    source = tmp_path / "raw-payload"
    source.write_bytes(b"this is raw data" * 1024)

    async def go(c: AsyncClient) -> None:
        with pytest.raises(CorvusError):
            await c.disks.upload_from_file("py-upload-invalid", source, format="qcow2")
        with pytest.raises(DiskNotFound):
            await c.disks.get("py-upload-invalid")

    run(go)
