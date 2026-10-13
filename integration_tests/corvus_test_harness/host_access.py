"""Host connection helpers for integration and persistent development nodes."""

from __future__ import annotations

import argparse
import fcntl
import ipaddress
import json
import os
import re
import shlex
import shutil
import stat
import subprocess
import sys
import tempfile
from collections.abc import Iterator
from contextlib import contextmanager
from pathlib import Path
from typing import cast

from .outer import Crv, CrvError, JsonObject
from .ssh import HOST_ALPINE_KEY_PATH

REPO_ROOT = Path(__file__).resolve().parents[2]
CERT_FILES = ("ca.crt", "corvus-client.crt", "corvus-client.key")
RECOVERY = "Run make dev-node-vm-setup for a fresh VM; recreate an existing disposable VM with make dev-node-vm-clean, or configure it manually."


def private_directory(path: Path, *, create: bool = False) -> Path:
    if create:
        try:
            path.mkdir(mode=0o700)
        except FileExistsError:
            pass
    info = path.lstat()
    if (
        not stat.S_ISDIR(info.st_mode)
        or info.st_uid != os.getuid()
        or info.st_mode & 0o077
    ):
        raise RuntimeError(f"Unsafe private directory: {path}")
    return path


def state_root(*, create: bool = False) -> Path:
    return private_directory(
        Path(tempfile.gettempdir()) / f"corvus-devnode-{os.getuid()}", create=create
    )


def validate_state(directory: Path) -> None:
    private_directory(directory)
    for path in directory.rglob("*"):
        info = path.lstat()
        if info.st_uid != os.getuid() or not (
            stat.S_ISDIR(info.st_mode) or stat.S_ISREG(info.st_mode)
        ):
            raise RuntimeError(f"Unsafe dev-node state: {path}")
        if info.st_mode & 0o022 or (path.suffix == ".key" and info.st_mode & 0o077):
            raise RuntimeError(f"Unsafe dev-node state permissions: {path}")


def vm_id(details: JsonObject) -> int:
    value = details.get("id")
    if not isinstance(value, int) or isinstance(value, bool) or value <= 0:
        raise RuntimeError("Outer daemon returned an invalid VM ID")
    return value


def vm_state(details: JsonObject, *, create: bool = False) -> Path:
    return private_directory(
        state_root(create=create) / str(vm_id(details)), create=create
    )


@contextmanager
def state_lock(details: JsonObject) -> Iterator[Path]:
    root = state_root(create=True)
    fd = os.open(
        root / f"{vm_id(details)}.lock", os.O_CREAT | os.O_RDWR | os.O_NOFOLLOW, 0o600
    )
    with os.fdopen(fd, "r+") as lock:
        info = os.fstat(lock.fileno())
        if (
            not stat.S_ISREG(info.st_mode)
            or info.st_uid != os.getuid()
            or info.st_mode & 0o077
        ):
            raise RuntimeError("Unsafe dev-node lock file")
        fcntl.flock(lock, fcntl.LOCK_EX)
        yield root / str(vm_id(details))


def read_metadata(path: Path) -> JsonObject:
    fd = os.open(path, os.O_RDONLY | os.O_NOFOLLOW)
    with os.fdopen(fd) as source:
        value = json.load(source)
    if not isinstance(value, dict):
        raise RuntimeError(f"Invalid dev-node metadata: {path}")
    return cast(JsonObject, value)


def publish_metadata(
    directory: Path, metadata: JsonObject, *, filename: str = "state.json"
) -> None:
    with tempfile.NamedTemporaryFile(mode="w", dir=directory, delete=False) as staging:
        temporary = Path(staging.name)
        try:
            json.dump(metadata, staging)
            staging.flush()
            temporary.replace(directory / filename)
        finally:
            temporary.unlink(missing_ok=True)


def resolve_vm(crv: Crv, target: str, *, running: bool = True) -> JsonObject:
    details = crv.vm_show(target)
    vm_id(details)
    if not isinstance(details.get("name"), str):
        raise RuntimeError("Outer daemon returned an invalid VM name")
    if running and details.get("status") != "running":
        raise RuntimeError(f"VM {target!r} must be started before connecting")
    return details


def ipv4(details: JsonObject) -> str:
    interfaces = details.get("net_ifs")
    if isinstance(interfaces, list):
        for interface in interfaces:
            if not isinstance(interface, dict):
                continue
            addresses = interface.get("guest_ip_addresses")
            if not isinstance(addresses, str):
                continue
            for address in addresses.split(","):
                try:
                    ip = ipaddress.ip_interface(address.strip()).ip
                except ValueError:
                    continue
                if isinstance(ip, ipaddress.IPv4Address) and not (
                    ip.is_loopback
                    or ip.is_link_local
                    or ip.is_unspecified
                    or ip.is_multicast
                ):
                    return str(ip)
    raise RuntimeError(
        "VM has no usable reported IPv4 address; wait for DHCP/guest-agent reporting"
    )


def certificate_directory(details: JsonObject) -> Path:
    name = str(details["name"])
    integration = re.fullmatch(r"corvus-it-(.+)-([0-9a-f]{8})-(\S+)", name)
    if integration:
        cls, run, short = integration.groups()
        directory = (
            Path(tempfile.gettempdir()) / f"corvus-it-pki-{cls}-{run}" / "host" / short
        )
    else:
        try:
            state = vm_state(details)
            validate_state(state)
            metadata = read_metadata(state / "state.json")
            if metadata.get("ready") is not True or metadata.get("vm_id") != vm_id(
                details
            ):
                raise RuntimeError(f"Dev-node setup is incomplete. {RECOVERY}")
            directory = private_directory(state / "client")
        except FileNotFoundError as error:
            raise RuntimeError(
                f"No host certificates for {name!r}. {RECOVERY}"
            ) from error
    for filename in CERT_FILES:
        path = directory / filename
        info = path.lstat()
        if not stat.S_ISREG(info.st_mode) or not info.st_size:
            raise RuntimeError(f"Missing or unsafe certificate: {path}")
        if filename.endswith(".key") and info.st_mode & 0o077:
            raise RuntimeError(f"Private key permissions must be 0600: {path}")
    return directory


def inner_binary() -> Path:
    output = subprocess.check_output(
        [
            "stack",
            *shlex.split(os.environ.get("STACK_BUILD_FLAGS", "")),
            "path",
            "--local-install-root",
        ],
        cwd=REPO_ROOT,
        text=True,
        timeout=60,
    ).strip()
    if not output:
        raise RuntimeError("Stack returned an empty local install root")
    binary = Path(output) / "bin" / "crv"
    if not binary.is_file() or not os.access(binary, os.X_OK):
        raise RuntimeError(
            f"Built crv is missing or not executable at {binary}; run make build"
        )
    return binary


def clean_network(crv: Crv, state: Path) -> None:
    path = state / "network.json"
    if not path.exists():
        return
    metadata = read_metadata(path)
    name = metadata.get("network")
    if name != f"corvus-dev-node-{state.name}":
        raise RuntimeError("Invalid dev-node network ownership metadata")
    networks = crv.network_list()
    for network in networks:
        if network.get("name") == name:
            if network.get("running"):
                crv.network_stop(str(name))
            crv.network_delete(str(name))


def clean(target: str) -> None:
    crv = Crv.autodetect()
    try:
        details = resolve_vm(crv, target, running=False)
    except CrvError as error:
        try:
            envelope = json.loads(error.stdout)
        except (ValueError, TypeError):
            raise error from None
        if envelope.get("code") != "vm_not_found":
            raise
        root = state_root(create=True)
        for state in root.iterdir():
            if (
                state.name.isdecimal()
                and state.is_dir()
                and (state / "network.json").exists()
            ):
                validate_state(state)
                metadata = read_metadata(state / "network.json")
                if target in (str(metadata.get("vm_id")), metadata.get("vm_name")):
                    with state_lock({"id": int(state.name)}):
                        clean_network(crv, state)
                        shutil.rmtree(state)
        print(f"VM {target!r} is already absent")
        return
    with state_lock(details) as state:
        crv.run("vm", "delete", "--force", str(vm_id(details)), timeout=120)
        if state.exists() or state.is_symlink():
            validate_state(state)
            clean_network(crv, state)
            shutil.rmtree(state)


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("action", choices=("crv", "ssh", "clean"))
    parser.add_argument("target")
    parser.add_argument("args", nargs=argparse.REMAINDER)
    args = parser.parse_args(argv)
    try:
        if args.action == "clean":
            clean(args.target)
            return 0
        details = resolve_vm(Crv.autodetect(), args.target)
        address = ipv4(details)
        if args.action == "crv":
            certs = certificate_directory(details)
            command = [
                str(inner_binary()),
                "--host",
                address,
                "--tls-cert-dir",
                str(certs),
                *args.args,
            ]
        else:
            # A dev-node association is required; integration nodes need no client PKI for SSH.
            if not re.fullmatch(
                r"corvus-it-(.+)-([0-9a-f]{8})-(\S+)", str(details["name"])
            ):
                metadata = read_metadata(vm_state(details) / "state.json")
                key = Path(str(metadata["ssh_key"]))
            else:
                key = HOST_ALPINE_KEY_PATH
            key = Path(os.environ.get("DEV_NODE_SSH_KEY", str(key)))
            if not key.is_file():
                raise RuntimeError(
                    f"SSH key missing at {key}; run make image IMAGE=key"
                )
            command = [
                "ssh",
                "-i",
                str(key),
                "-o",
                "StrictHostKeyChecking=no",
                "-o",
                "UserKnownHostsFile=/dev/null",
                "-o",
                "LogLevel=ERROR",
                f"corvus@{address}",
                *args.args,
            ]
        os.execvp(command[0], command)
    except (RuntimeError, OSError, ValueError, subprocess.SubprocessError) as error:
        print(f"{args.action}-it: {error}", file=sys.stderr)
        return 1


if __name__ == "__main__":
    raise SystemExit(main())
