"""Export compact rescue installation media without modifying the source disk."""

import os
import re
import subprocess
import tempfile
from pathlib import Path


def export_rescue_image(source: Path, output: Path) -> None:
    """Verify rescue payloads, export an update tar, and shrink a private raw copy.

    Root and Linux loop/filesystem tools are required. Outputs are prepared in
    the destination filesystem and published with atomic individual renames.
    Cleanup never removes a work directory while it remains mounted.
    """
    source = source.resolve(strict=True)
    output = output.resolve()
    if os.geteuid() != 0:
        raise ValueError("Rescue export requires root")
    if not source.is_file() or source == output / "gentoo-rescue.img":
        raise ValueError("Expected a source image distinct from the output")
    output.mkdir(parents=True, exist_ok=True)
    environment = dict(os.environ, LC_ALL="C")

    def run(*argv: str, allowed: tuple[int, ...] = (0,)) -> None:
        result = subprocess.run(argv, env=environment, check=False)
        if result.returncode not in allowed:
            raise subprocess.CalledProcessError(result.returncode, argv)

    def read(*argv: str) -> str:
        return subprocess.check_output(argv, env=environment, text=True).strip()

    loop = ""
    mounted = False
    # Deliberately do not auto-delete work files if unmount/detach fails.
    import shutil

    work = Path(tempfile.mkdtemp(prefix=".export-", dir=output))
    media = work / "media"
    media.mkdir()
    try:
        loop = read(
            "losetup", "--find", "--show", "--partscan", "--read-only", str(source)
        )
        run("udevadm", "settle", "--timeout=30")
        partition = loop + "p3"
        if read("blkid", "-s", "LABEL", "-o", "value", partition) != "rescue-data":
            raise ValueError("Source is not a rescue disk")
        run("mount", "-t", "ext4", "-o", "ro,noload", partition, str(media))
        mounted = True
        subprocess.run(
            ["sha256sum", "-c", "rescue/SHA256SUMS"],
            cwd=media,
            env=environment,
            check=True,
        )
        run(
            "tar",
            "-C",
            str(media),
            "-cf",
            str(work / "gentoo-rescue-update.tar"),
            "boot/vmlinuz",
            "boot/initramfs.img",
            "rescue/root.squashfs",
            "rescue/SHA256SUMS",
        )
        run("umount", str(media))
        mounted = False
        run("losetup", "-d", loop)
        loop = ""
        image = work / "gentoo-rescue.img"
        run("cp", "--reflink=auto", "--sparse=always", str(source), str(image))
        loop = read("losetup", "--find", "--show", "--partscan", str(image))
        run("udevadm", "settle", "--timeout=30")
        partition = loop + "p3"
        run("e2fsck", "-pf", partition, allowed=(0, 1))

        def geometry() -> tuple[int, int]:
            info = read("dumpe2fs", "-h", partition)
            count = re.search(r"^Block count:\s+(\d+)$", info, re.MULTILINE)
            size = re.search(r"^Block size:\s+(\d+)$", info, re.MULTILINE)
            if count is None or size is None:
                raise ValueError("Cannot read ext4 geometry")
            return int(count[1]), int(size[1])

        capacity, blocksize = geometry()
        previous = capacity + 1
        while True:
            run("resize2fs", "-M", partition)
            blocks, blocksize = geometry()
            if blocks > previous:
                raise ValueError("Filesystem unexpectedly grew during shrinking")
            if blocks == previous:
                break
            previous = blocks
        blocks = min(capacity, blocks + 67108864 // blocksize)
        run("resize2fs", partition, str(blocks))
        start = int(read("lsblk", "-ndo", "START", partition))
        guid = read("blkid", "-s", "PART_ENTRY_UUID", "-o", "value", "-p", partition)
        last = int(read("blockdev", "--getsz", loop)) - 34
        end = min(
            last, ((start * 512 + blocks * blocksize + 1048575) // 1048576) * 2048 - 1
        )
        run(
            "sgdisk",
            "--delete=3",
            f"--new=3:{start}:{end}",
            "--typecode=3:8300",
            "--change-name=3:rescue-data",
            f"--partition-guid=3:{guid}",
            loop,
        )
        run("losetup", "-d", loop)
        loop = ""
        size = ((end + 34 + 2047) // 2048) * 1048576
        run("truncate", "-s", str(size), str(image))
        run("sgdisk", "--move-second-header", str(image))
        run("sgdisk", "--verify", str(image))
        image.replace(output / image.name)
        archive = work / "gentoo-rescue-update.tar"
        archive.replace(output / archive.name)
    finally:
        if mounted:
            run("umount", str(media))
        if loop:
            run("losetup", "-d", loop)
        shutil.rmtree(work)
