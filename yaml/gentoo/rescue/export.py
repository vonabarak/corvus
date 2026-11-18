"""Export compact rescue installation media without modifying the source disk."""

import argparse
import json
import os
import re
import shutil
import subprocess
import tempfile
from pathlib import Path


def query_corvus(*args: str) -> str:
    result = subprocess.run(
        [os.environ.get("CRV", "crv"), *args, "-o", "json"],
        capture_output=True,
        text=True,
        check=False,
    )
    if result.returncode:
        detail = result.stderr.strip() or result.stdout.strip()
        raise ValueError(f"Corvus {' '.join(args)} failed: {detail}")
    return result.stdout


def find_source() -> Path:
    nodes = json.loads(query_corvus("node", "list"))
    if not nodes:
        raise ValueError("No Corvus nodes are registered")
    node = nodes[0]

    image = "gentoo-rescue:latest"
    placements = json.loads(query_corvus("disk", "show", image))["placements"]
    placement = next(
        (item for item in placements if item["node"]["id"] == node["id"]), None
    )
    if placement is None:
        raise ValueError(
            f"{image} has no placement on the first Corvus node, {node['name']}"
        )

    source = Path(placement["file_path"])
    if not source.is_file():
        raise ValueError(f"Run the export on {node['name']}, where {source} is stored")
    return source


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
            ["sha256sum", "--strict", "-c", "SHA256SUMS"],
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
            "vmlinuz",
            "initramfs.img",
            "root.squashfs",
            "SHA256SUMS",
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


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("output", type=Path)
    arguments = parser.parse_args()
    if os.geteuid() != 0:
        parser.exit(1, "Rescue export requires root\n")
    try:
        export_rescue_image(find_source(), arguments.output)
    except (OSError, ValueError, subprocess.CalledProcessError) as exc:
        parser.exit(1, f"Rescue export failed: {exc}\n")


if __name__ == "__main__":
    main()
