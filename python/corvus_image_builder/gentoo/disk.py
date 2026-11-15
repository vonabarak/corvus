"""Create new disk layouts or mount an existing image without changing it."""

import shutil
import subprocess
import time
from pathlib import Path

from ..core import BuildContext
from .base import GentooStage
from .mounts import Mounts


def partition_device(disk: Path, number: int) -> Path:
    """Return a Linux partition path for either vdb or nvme-style disk names."""
    name = str(disk)
    return Path(name + ("p" if name[-1].isdigit() else "") + str(number))


def mount_image(stage: GentooStage, context: BuildContext, disk: Path) -> None:
    """Mount root and ESP by device, never by potentially duplicated labels."""
    mounts = Mounts(stage, context)
    mounts.mount(context.root, str(partition_device(disk, 3)))
    mounts.mount(context.root / "boot/efi", str(partition_device(disk, 2)))
    context.artifacts["disk"] = disk


class DiskStage(GentooStage):
    """Erase and partition a new GPT disk for BIOS and UEFI boot.

    Create a BIOS partition, configurable FAT ESP, and ext4 root. Mounts are
    preserved on build failure; this stage must never be used for an overlay.
    """

    requires = frozenset({"repository"})
    produces = frozenset({"disk"})

    def validate(self, context: BuildContext) -> None:
        for tool in ("sgdisk", "wipefs", "partprobe", "mkfs.vfat", "mkfs.ext4"):
            if not shutil.which(tool):
                raise ValueError(f"Missing prerequisite: {tool}")

    def execute(self, context: BuildContext) -> None:
        disk = str(self.settings.disk)
        self.command(context, "wipefs", "-a", disk)
        self.command(context, "sgdisk", "--zap-all", disk)
        for spec in (
            ("1:0:+1M", "1:ef02"),
            (f"2:0:+{self.settings.esp_size_mib}M", "2:ef00"),
            ("3:0:0", "3:8300"),
        ):
            self.command(context, "sgdisk", "-n", spec[0], "-t", spec[1], disk)
        self.command(context, "partprobe", disk)
        for _ in range(60):
            if all(partition_device(self.settings.disk, i).exists() for i in (1, 2, 3)):
                break
            time.sleep(1)
        else:
            raise RuntimeError("Partition devices did not appear")
        self.command(
            context,
            "mkfs.vfat",
            "-F32",
            "-n",
            "CORVUS-EFI",
            str(partition_device(self.settings.disk, 2)),
        )
        self.command(
            context,
            "mkfs.ext4",
            "-F",
            "-L",
            "corvus-root",
            str(partition_device(self.settings.disk, 3)),
        )
        mount_image(self, context, self.settings.disk)


class ExistingImageStage(GentooStage):
    """Mount an existing GPT artifact and verify its inherited boot files.

    Require the established p2 FAT/p3 ext4 layout. Never wipe, partition, format,
    compile a kernel, or reinstall a bootloader on the overlay path.
    """

    requires = frozenset({"repository"})
    produces = frozenset({"disk", "kernel", "bootloader"})

    def validate(self, context: BuildContext) -> None:
        for number, expected in ((2, "vfat"), (3, "ext4")):
            actual = subprocess.check_output(
                [
                    "blkid",
                    "-s",
                    "TYPE",
                    "-o",
                    "value",
                    str(partition_device(self.settings.disk, number)),
                ],
                text=True,
            ).strip()
            if actual != expected:
                raise ValueError(
                    f"Expected {expected} on partition {number}, got {actual}"
                )

    def execute(self, context: BuildContext) -> None:
        mount_image(self, context, self.settings.disk)
        root = context.root
        kernel = (root / "boot/vmlinuz").readlink()
        if kernel.name != str(kernel) or not kernel.name.startswith("vmlinuz-"):
            raise ValueError("Expected a local versioned kernel symlink")
        version = kernel.name.removeprefix("vmlinuz-")
        for path in (
            root / "boot" / kernel,
            root / "boot" / f"config-{version}",
            root / "lib/modules" / version,
            root / "boot/grub/grub.cfg",
            root / "boot/efi/EFI/BOOT/BOOTX64.EFI",
        ):
            if not path.exists():
                raise ValueError(f"Inherited boot artifact is missing: {path}")
        context.artifacts["kernel"] = kernel.name
        context.artifacts["bootloader"] = root / "boot/grub/grub.cfg"
