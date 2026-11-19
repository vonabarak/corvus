"""Download and verify Gentoo rescue payloads through a temporary Corvus VM."""

import argparse
import fcntl
import hashlib
import os
import re
import tempfile
import time
import uuid
from pathlib import Path

import capnp
from corvus_client import Client, CorvusError, GuestAgentError, InvalidTransition
from corvus_client._sync.vm import SyncVm

IMAGE = "gentoo-rescue:latest"
TEMPLATE = "gentoo-musl-openrc-headless"
PAYLOADS = ("vmlinuz", "initramfs.img", "root.squashfs")
MANIFEST = "SHA256SUMS"
FILES = (*PAYLOADS, MANIFEST)
MOUNT = "/run/corvus-rescue-export"
MOUNT_COMMAND = (
    f"\n"
    f"set -eu\n"
    f"partition=\n"
    f"for entry in /sys/class/block/vd*3; do\n"
    f'    [ -f "$entry/partition" ] || continue\n'
    f'    [ "$(cat "$entry/partition")" = 3 ] || continue\n'
    f"    disk=${{entry%3}}\n"
    f'    [ "$(cat "$disk/ro")" = 1 ] || continue\n'
    f"    device=/dev/${{entry##*/}}\n"
    f'    [ "$(blkid -p -s TYPE -o value "$device")" = ext4 ] || continue\n'
    f'    [ "$(blkid -p -s LABEL -o value "$device")" = rescue-data ] || continue\n'
    f'    if [ -n "$partition" ]; then\n'
    f"        echo 'Multiple read-only rescue-data partitions' >&2\n"
    f"        exit 1\n"
    f"    fi\n"
    f"    partition=$device\n"
    f"done\n"
    f'if [ -z "$partition" ]; then\n'
    f"    echo 'No read-only ext4 rescue-data partition 3 found' >&2\n"
    f"    exit 1\n"
    f"fi\n"
    f"mkdir -p {MOUNT}\n"
    f'mount -t ext4 -o ro,noload,nodev,nosuid,noexec "$partition" {MOUNT}\n'
)


def connect() -> Client:
    if socket := os.environ.get("CORVUS_SOCKET"):
        return Client(unix_socket=socket)
    return Client(
        host=os.environ.get("CORVUS_HOST", "127.0.0.1"),
        port=int(os.environ.get("CORVUS_PORT", "9876")),
    )


def guest_command(vm: SyncVm, command: str, operation: str) -> None:
    result = vm.guest_exec(command)
    if result.exit_code:
        detail = result.stderr.strip() or result.stdout.strip()
        raise ValueError(f"{operation} failed (exit {result.exit_code}): {detail}")


def wait_for_guest(vm: SyncVm) -> None:
    deadline = time.monotonic() + 120
    while True:
        try:
            guest_command(vm, "true", "QGA readiness check")
            return
        except (GuestAgentError, InvalidTransition) as exc:
            if time.monotonic() >= deadline:
                raise ValueError(f"QGA did not become ready: {exc}") from exc
            time.sleep(1)


def verify_payloads(work: Path) -> None:
    checksums: dict[str, str] = {}
    for line in (work / MANIFEST).read_text(encoding="ascii").splitlines():
        match = re.fullmatch(r"([0-9a-fA-F]{64}) [ *](.+)", line)
        if match is None or match[2] not in PAYLOADS or match[2] in checksums:
            raise ValueError(f"Invalid {MANIFEST} entry: {line!r}")
        checksums[match[2]] = match[1].lower()
    if checksums.keys() != set(PAYLOADS):
        raise ValueError(f"{MANIFEST} must cover all three rescue payloads")
    for name, expected in checksums.items():
        digest = hashlib.sha256()
        with (work / name).open("rb") as stream:
            for chunk in iter(lambda: stream.read(1024 * 1024), b""):
                digest.update(chunk)
        if digest.hexdigest() != expected:
            raise ValueError(f"Checksum mismatch for {name}")


def download_payloads(client: Client, work: Path) -> None:
    nodes = client.nodes.list()
    if not nodes:
        raise ValueError("No Corvus nodes are registered")
    node = nodes[0]
    image = client.disks.get(IMAGE).show()
    if image.ephemeral:
        raise ValueError(f"{IMAGE} must be a persistent published image")
    if not any(placement.node.id == node.id for placement in image.placements):
        raise ValueError(f"{IMAGE} has no placement on the first node, {node.name}")

    name = f"gentoo-rescue-export-{uuid.uuid4().hex}"
    template = client.templates.get(TEMPLATE)
    for drive in template.show().drives:
        if drive.disk_image is None:
            continue
        backing = client.disks.get(drive.disk_image.id).show()
        if not backing.placements:
            raise ValueError(
                f"Template {TEMPLATE} disk {backing.name} (ID {backing.id}) "
                "has no registered placement. Restore or rebuild the helper image."
            )
    vm = template.instantiate(name, node=node.name)
    started = mounted = False
    failure: BaseException | None = None
    try:
        vm.attach_disk(image.id, interface="virtio", read_only=True)
        started = True
        vm.start(wait=True)
        wait_for_guest(vm)
        guest_command(vm, MOUNT_COMMAND, "Mounting rescue data")
        mounted = True
        for filename in FILES:
            vm.download_to_file(f"{MOUNT}/{filename}", work / filename)
        verify_payloads(work)
    except BaseException as exc:
        failure = exc
        raise
    finally:
        errors = []
        if mounted:
            try:
                guest_command(vm, f"umount {MOUNT}", "Unmounting rescue data")
            except Exception as exc:
                errors.append(str(exc))
        force = False
        if started:
            try:
                vm.stop(wait=True, timeout_sec=30)
            except Exception:
                force = True
        try:
            vm.delete(force=force)
        except Exception as exc:
            errors.append(f"Deleting VM failed: {exc}")
        if errors:
            detail = f"Cleanup of {name} failed: " + ", ".join(errors)
            if failure is None:
                raise ValueError(detail)
            raise ValueError(
                f"{failure}\n"
                f"{detail}"
            ) from failure


def export_rescue_image(client: Client, output: Path) -> None:
    output.mkdir(parents=True, exist_ok=True)
    with tempfile.TemporaryDirectory(prefix=".export-", dir=output) as directory:
        work = Path(directory)
        download_payloads(client, work)
        directory_fd = os.open(output, os.O_RDONLY | os.O_DIRECTORY)
        try:
            fcntl.flock(directory_fd, fcntl.LOCK_EX)
            for name in FILES:
                if (output / name).is_dir():
                    raise IsADirectoryError(output / name)
            for name in FILES:
                (work / name).replace(output / name)
        finally:
            os.close(directory_fd)


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("output", type=Path)
    arguments = parser.parse_args()
    try:
        with connect() as client:
            export_rescue_image(client, arguments.output)
    except (OSError, ValueError, CorvusError, capnp.KjException) as exc:
        parser.exit(1, f"Rescue export failed: {exc}\n")


if __name__ == "__main__":
    main()
