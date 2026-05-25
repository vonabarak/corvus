"""Register pre-baked OS images with the inner Corvus daemon.

The test image's `home-corvus-VMs-BaseImages.mount` (baked in
[`yaml/corvus-test-node/systemd/`](../../yaml/corvus-test-node/systemd/)) virtiofs-mounts
the host's `~/VMs/BaseImages` at `/home/corvus/VMs/BaseImages` inside the test
VM (the daemon runs as the `corvus` user, so this matches its
default $HOME/VMs basePath). This module:

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
from collections.abc import Iterator
from dataclasses import dataclass
from pathlib import Path

from corvus_client import Client, DiskNotFound

from .outer import Crv

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

    # Short key under which the image gets registered with the inner
    # daemon. Derived from the parent directory's name, lowercased.
    name: str
    # Path inside the test VM (after virtiofs mount).
    guest_path: Path
    # Path on the host (under HOST_BASE_IMAGES_DIR).
    host_path: Path


def discover(host_dir: Path = HOST_BASE_IMAGES_DIR) -> dict[str, BaseImage]:
    """Walk the host's BaseImages tree and return every image found.

    Each image is exposed under its sanitised filename stem (e.g.
    `gentoo-base-headless`, `almalinux-10-base`). When a directory
    contains multiple images, ONE also gets the sanitised dir-name
    as an alias (e.g. `gentoo`, `almalinux`) — picked so that
    `Vm(self)`'s default `base_image_key = "alpine"` keeps resolving
    to the bake-time `corvus-test-vm.qcow2` even when an unrelated
    `alpine-*-base.qcow2` upstream cloud image is dropped into the
    same directory.

    Selection rule for the dir alias, in priority order:

      1. A file whose stem starts with ``corvus-test-`` — these are
         the harness's bespoke bake-time images that ship qemu-ga,
         the baked SSH key, and the VSOCK sshd relay. Tests that
         resolve the dir alias (`Vm`, `VmSsh`, …) rely on those
         additions.
      2. Otherwise, the alphabetically-first image in the directory.

    Files ending in `.bak.qcow2` are skipped (backup snapshots).
    Subdirectories with no recognisable image file are skipped
    silently. Returns an empty dict if `host_dir` itself doesn't
    exist (developer hasn't run `make test-image-*` yet).
    """
    if not host_dir.is_dir():
        return {}
    images: dict[str, BaseImage] = {}
    for subdir in sorted(host_dir.iterdir()):
        if not subdir.is_dir():
            continue
        files = list(_image_files(subdir))
        if not files:
            continue
        dir_key = _sanitize_name(subdir.name)
        alias_file = _pick_alias_target(files)
        for image_file in files:
            stem_key = _sanitize_name(image_file.stem)
            # The disk-name registered with the inner daemon is the
            # filename stem — unique per file.
            guest_path = GUEST_BASE_IMAGES_PATH / subdir.name / image_file.name
            base = BaseImage(name=stem_key, guest_path=guest_path, host_path=image_file)
            images[stem_key] = base
            if image_file == alias_file and dir_key not in images:
                images[dir_key] = base
    return images


def _pick_alias_target(files: list[Path]) -> Path:
    """Pick which file the dir-name alias should resolve to."""
    bake = next((f for f in files if f.stem.startswith("corvus-test-")), None)
    return bake if bake is not None else files[0]


def _image_files(d: Path) -> Iterator[Path]:
    for f in sorted(d.iterdir()):
        if not f.is_file():
            continue
        if f.suffix.lower() not in _IMAGE_SUFFIXES:
            continue
        if f.name.lower().endswith(".bak.qcow2"):
            continue
        yield f


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
            # Pass a format hint derived from the extension. The
            # daemon's agent-side `qemu-img info` auto-detection
            # currently mis-classifies some raw images (notably
            # ISO9660 disks like the synthetic installer) as
            # qcow2, which then breaks `qemu -drive
            # format=qcow2,...`. The hint sidesteps that path.
            fmt = _format_from_suffix(image.host_path.suffix.lower())
            client.disks.register(image.name, str(image.guest_path), format=fmt)
        registered[key] = image.name
    return registered


def _format_from_suffix(suffix: str) -> str | None:
    """Map a filename suffix to the matching DriveFormat string.

    Returns None when we don't have a strong hint (the daemon then
    auto-detects via qemu-img). Mirrors the daemon-side mapping in
    `Corvus.Node.Image.detectFormatFromPath`.
    """
    if suffix == ".qcow2":
        return "qcow2"
    if suffix in (".raw", ".img", ".iso"):
        return "raw"
    return None
