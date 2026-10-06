"""Register pre-baked OS images with the inner Corvus daemon.

The test image's `home-corvus-VMs-BaseImages.mount` (baked in
[`yaml/corvus-test-node/systemd/`](../../yaml/corvus-test-node/systemd/)) virtiofs-mounts
the host's `~/VMs/BaseImages` at `/home/corvus/VMs/BaseImages` inside the test
VM (the daemon runs as the `corvus` user, so this matches its
default $HOME/VMs basePath). This module:

  1. Selects registered versions carrying `latest` in the outer catalogue,
     with a local placement under the host's BaseImages directory.
  2. Ensures the virtiofs share is mounted inside the guest.
  3. Registers those immutable files under stable names and directory aliases.

The selected catalogue is retained for the lifetime of a test class, including
when placements are added on a second node. Publishing new versions during
that class cannot change its backing files.

Tests then refer to the images by short name:

    def test_overlay_on_alpine(single_client, base_images):
        disk = base_images["alpine"]
        single_client.disks.create_overlay("scratch-overlay", disk)
        vm = single_client.vms.create("scratch")
        vm.attach_disk("scratch-overlay", interface="virtio")
"""

from __future__ import annotations

import os
import re
from collections.abc import Sequence
from dataclasses import dataclass
from pathlib import Path

from corvus_client import Client, DiskNotFound

from .outer import Crv, JsonObject

# Host-side root for all pre-baked images. Mirrors `path: BaseImages/…/`
# in yaml/{alpine-test,multi-os,windows-server-2025}/*.yml.
HOST_BASE_IMAGES_DIR = Path(os.path.expanduser("~/VMs/BaseImages"))

# In-guest mount point. Matches `home-corvus-VMs-BaseImages.mount`
# in yaml/corvus-test-node/systemd/. The daemon runs as `corvus`,
# so `/home/corvus/VMs` is its $HOME/VMs basePath.
GUEST_BASE_IMAGES_PATH = Path("/home/corvus/VMs/BaseImages")

# Virtiofs tag wired up in `Topology.add` and the systemd mount unit.
BASE_IMAGES_TAG = "base_images"

# Extensions we recognise as bootable disk images.
_IMAGE_SUFFIXES = (".qcow2", ".img", ".raw")


@dataclass(frozen=True)
class BaseImage:
    """One pre-baked image, ready to register with the inner daemon."""

    name: str
    guest_path: Path
    host_path: Path
    format: str
    outer_id: int


@dataclass(frozen=True)
class RegisteredImages:
    """A fixed catalogue and the inner version IDs registered from it."""

    images: dict[str, BaseImage]
    disk_ids: dict[str, int]

    @property
    def names(self) -> dict[str, str]:
        return {key: image.name for key, image in self.images.items()}


def discover(
    catalogue: Sequence[JsonObject], host_dir: Path = HOST_BASE_IMAGES_DIR
) -> dict[str, BaseImage]:
    """Select latest outer versions with a local file under the shared root.

    Directory aliases prefer a corvus-test-* family; otherwise they use the
    alphabetically first family. Retained versions and unregistered files are
    ignored. File names and numeric IDs never determine which version wins.
    """
    host_dir = host_dir.resolve()
    families: dict[str, BaseImage] = {}
    for disk in catalogue:
        tags = disk.get("tags")
        if not isinstance(tags, list) or "latest" not in tags:
            continue
        name, version, fmt = disk.get("name"), disk.get("id"), disk.get("format")
        placements = disk.get("placements")
        if (
            not isinstance(name, str)
            or not isinstance(version, int)
            or not isinstance(fmt, str)
            or not isinstance(placements, list)
        ):
            raise ValueError(f"Invalid outer image catalogue entry: {disk}")
        paths: set[Path] = set()
        for placement in placements:
            if not isinstance(placement, dict):
                raise ValueError(f"Invalid placement for image {name}:{version}")
            raw_path = placement.get("file_path")
            if not isinstance(raw_path, str):
                raise ValueError(f"Missing placement path for image {name}:{version}")
            path = Path(raw_path).resolve()
            if path.is_relative_to(host_dir) and path.suffix.lower() in _IMAGE_SUFFIXES:
                if not path.is_file():
                    raise FileNotFoundError(
                        f"Published image {name}:{version} is missing: {path}"
                    )
                paths.add(path)
        if not paths:
            continue
        if len(paths) != 1:
            raise ValueError(
                f"Ambiguous local placements for image {name}:{version}: {paths}"
            )
        path = next(iter(paths))
        key = _sanitize_name(name)
        if key in families:
            raise ValueError(f"Ambiguous latest image family or harness name: {name}")
        families[key] = BaseImage(
            name=key,
            host_path=path,
            guest_path=GUEST_BASE_IMAGES_PATH / path.relative_to(host_dir),
            format=fmt,
            outer_id=version,
        )
    images = dict(families)
    for image in sorted(
        families.values(),
        key=lambda image: (not image.name.startswith("corvus-test-"), image.name),
    ):
        relative = image.host_path.relative_to(host_dir)
        if len(relative.parts) < 2:
            continue
        alias = _sanitize_name(relative.parts[0])
        if alias not in images:
            images[alias] = image
    return images


def _sanitize_name(raw: str) -> str:
    """Lowercase + drop characters Corvus's disk-name validator rejects."""
    # validateName rejects empty / all-digit / path-separator-bearing
    # names; here we collapse to lowercase alnum + dashes.
    cleaned = re.sub(r"[^a-z0-9]+", "-", raw.lower()).strip("-")
    return cleaned or "image"


def ensure_mounted(crv: Crv, node_name: str) -> None:
    """Ensure the BaseImages virtiofs share is mounted inside the node.

    The image's `home-corvus-VMs-BaseImages.mount` should mount it
    at boot, but images baked before that unit existed need a
    one-shot manual mount. Idempotent: skips when
    `/home/corvus/VMs/BaseImages` already shows up in
    `/proc/self/mountinfo`.
    """
    script = (
        "mkdir -p /home/corvus/VMs/BaseImages; "
        "mountpoint -q /home/corvus/VMs/BaseImages || "
        f"mount -t virtiofs {BASE_IMAGES_TAG} /home/corvus/VMs/BaseImages"
    )
    crv.vm_exec(node_name, script, timeout_sec=30.0)


def register_all(
    client: Client,
    crv: Crv,
    node_name: str,
    *,
    host_dir: Path = HOST_BASE_IMAGES_DIR,
) -> RegisteredImages:
    """Mount and register a fixed snapshot of the outer latest catalogue."""
    images = discover(crv.disk_list(), host_dir)
    if not images:
        return RegisteredImages(images={}, disk_ids={})
    ensure_mounted(crv, node_name)
    disk_ids: dict[str, int] = {}
    for image in images.values():
        if image.name in disk_ids:
            continue
        try:
            disk = client.disks.get(image.name)
        except DiskNotFound:
            disk = client.disks.register(
                image.name, str(image.guest_path), format=image.format
            )
        info = disk.show()
        if not any(p.file_path == str(image.guest_path) for p in info.placements):
            raise ValueError(
                f"Inner image {image.name} was registered from another file"
            )
        disk_ids[image.name] = info.id
    return RegisteredImages(images=images, disk_ids=disk_ids)


def stage_on_node(
    client: Client,
    crv: Crv,
    *,
    inner_node_name: str,
    outer_vm_name: str,
    registered: RegisteredImages,
) -> None:
    """Stage the exact files and inner IDs selected at initial registration."""
    if not registered.images:
        return
    ensure_mounted(crv, outer_vm_name)
    for name, version in registered.disk_ids.items():
        disk = client.disks.get(version)
        info = disk.show()
        if any(p.node.name == inner_node_name for p in info.placements):
            continue
        image = next(
            image for image in registered.images.values() if image.name == name
        )
        disk.register_placement(str(image.guest_path), node=inner_node_name)
