"""Register pre-baked OS images with the inner Corvus daemon.

The test image's `root-VMs-BaseImages.mount` (baked in
[`tests-integration/images/systemd/`](../images/systemd/)) virtiofs-mounts
the host's `~/VMs/BaseImages` at `/root/VMs/BaseImages` inside the test
VM. This module:

  1. Discovers what image files live under the host directory (one
     directory per OS, e.g. `Alpine/corvus-test.qcow2`,
     `WindowsServer2025/…`, `Debian/…`).
  2. Ensures the virtiofs share is actually mounted inside the guest
     (a fallback `mount -t virtiofs …` covers older test images that
     don't have the systemd unit baked in yet).
  3. Registers each image with the inner daemon under a stable name
     derived from its parent directory (e.g. `alpine`, `debian`,
     `windowsserver2025`).

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
from dataclasses import dataclass
from pathlib import Path
from typing import Iterator

from corvus_client import Client, DiskNotFound

from .outer import Crv


# Host-side root for all pre-baked images. Mirrors `path: BaseImages/…/`
# in yaml/{alpine-test,multi-os,windows-server-2025}/*.yml.
HOST_BASE_IMAGES_DIR = Path(os.path.expanduser("~/VMs/BaseImages"))

# In-guest mount point. Matches `root-VMs-BaseImages.mount` in
# tests-integration/images/systemd/.
GUEST_BASE_IMAGES_PATH = Path("/root/VMs/BaseImages")

# Virtiofs tag wired up in `Topology.add` and the systemd mount unit.
BASE_IMAGES_TAG = "base_images"

# Extensions we recognise as bootable disk images.
_IMAGE_SUFFIXES = (".qcow2", ".img", ".raw")


@dataclass(frozen=True)
class BaseImage:
    """One pre-baked image, ready to register with the inner daemon."""

    # Short key under which the image gets registered with the inner
    # daemon. Derived from the parent directory's name, lowercased.
    name: str
    # Path inside the test VM (after virtiofs mount).
    guest_path: Path
    # Path on the host (under HOST_BASE_IMAGES_DIR).
    host_path: Path


def discover(host_dir: Path = HOST_BASE_IMAGES_DIR) -> dict[str, BaseImage]:
    """Walk the host's BaseImages tree and return the first image found
    per OS subdirectory, keyed by a sanitised directory name.

    Subdirectories with no recognisable image file are skipped silently.
    Returns an empty dict if `host_dir` itself doesn't exist (developer
    hasn't run `make test-image-*` yet).
    """
    if not host_dir.is_dir():
        return {}
    images: dict[str, BaseImage] = {}
    for subdir in sorted(host_dir.iterdir()):
        if not subdir.is_dir():
            continue
        image = next(_image_files(subdir), None)
        if image is None:
            continue
        key = _sanitize_name(subdir.name)
        guest_path = GUEST_BASE_IMAGES_PATH / subdir.name / image.name
        images[key] = BaseImage(name=key, guest_path=guest_path, host_path=image)
    return images


def _image_files(d: Path) -> Iterator[Path]:
    for f in sorted(d.iterdir()):
        if f.is_file() and f.suffix.lower() in _IMAGE_SUFFIXES:
            yield f


def _sanitize_name(raw: str) -> str:
    """Lowercase + drop characters Corvus's disk-name validator rejects."""
    # validateName rejects empty / all-digit / path-separator-bearing
    # names; here we collapse to lowercase alnum + dashes.
    cleaned = re.sub(r"[^a-z0-9]+", "-", raw.lower()).strip("-")
    return cleaned or "image"


def ensure_mounted(crv: Crv, node_name: str) -> None:
    """Ensure the BaseImages virtiofs share is mounted inside the node.

    The image's `root-VMs-BaseImages.mount` should mount it at boot, but
    images baked before that unit existed need a one-shot manual mount.
    Idempotent: skips when `/root/VMs/BaseImages` already shows up in
    `/proc/self/mountinfo`.
    """
    script = (
        "mkdir -p /root/VMs/BaseImages; "
        "mountpoint -q /root/VMs/BaseImages || "
        f"mount -t virtiofs {BASE_IMAGES_TAG} /root/VMs/BaseImages"
    )
    crv.vm_exec(node_name, script, timeout_sec=30.0)


def register_all(
    client: Client,
    crv: Crv,
    node_name: str,
    *,
    host_dir: Path = HOST_BASE_IMAGES_DIR,
) -> dict[str, str]:
    """Mount the share + register every discovered image with `client`.

    Returns a dict mapping short OS key → registered disk name. The
    inner daemon (running on the node) keeps the registration for the
    lifetime of the node, so subsequent calls in the same test are
    cheap no-ops (we treat an existing same-named disk as already
    registered).
    """
    images = discover(host_dir)
    if not images:
        return {}
    ensure_mounted(crv, node_name)
    registered: dict[str, str] = {}
    for key, image in images.items():
        try:
            client.disks.get(image.name)
        except DiskNotFound:
            client.disks.register(image.name, str(image.guest_path))
        registered[key] = image.name
    return registered
