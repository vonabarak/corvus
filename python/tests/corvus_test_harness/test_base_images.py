"""Discover only published images available in the host's shared directory."""

from pathlib import Path

from corvus_test_harness.base_images import discover
from corvus_test_harness.outer import JsonObject


def test_remote_only_latest_does_not_block_local_images(tmp_path: Path) -> None:
    local = tmp_path / "Alpine/cloud.qcow2"
    local.parent.mkdir()
    local.touch()
    old = tmp_path / "Gentoo/old.qcow2"
    old.parent.mkdir()
    old.touch()
    catalogue: list[JsonObject] = [
        {
            "name": "gentoo-musl-openrc-headless",
            "id": 27134,
            "format": "qcow2",
            "tags": ["latest"],
            "placements": [{"file_path": str(old.parent / "remote.qcow2")}],
        },
        {
            "name": "gentoo-musl-openrc-headless",
            "id": 1,
            "format": "qcow2",
            "tags": [],
            "placements": [{"file_path": str(old)}],
        },
        {
            "name": "alpine-cloud",
            "id": 2,
            "format": "qcow2",
            "tags": ["latest"],
            "placements": [
                {"file_path": str(local.parent / "remote.qcow2")},
                {"file_path": str(local)},
            ],
        },
    ]
    images = discover(catalogue, tmp_path)
    assert set(images) == {"alpine-cloud", "alpine"}
    assert images["alpine"].host_path == local
    assert images["alpine"].outer_id == 2
