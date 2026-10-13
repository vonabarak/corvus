"""Create a development VM with its own managed host network."""

from __future__ import annotations

import argparse
import ipaddress
import json
import subprocess
import sys
import tempfile
from pathlib import Path

from .host_access import (
    REPO_ROOT,
    inner_binary,
    publish_metadata,
    resolve_vm,
    state_lock,
    vm_state,
)
from .outer import Crv, CrvError, JsonObject


def pick_subnet(networks: list[JsonObject]) -> str:
    existing = [
        ipaddress.ip_network(str(n["subnet"])) for n in networks if n.get("subnet")
    ]
    for octet in range(256):
        candidate = ipaddress.ip_network(f"10.91.{octet}.0/24")
        if all(
            candidate.version != subnet.version or not candidate.overlaps(subnet)
            for subnet in existing
        ):
            return str(candidate)
    raise RuntimeError(
        "No free dev-node subnet in 10.91.0.0/16; remove unused networks"
    )


def create(target: str) -> None:
    crv = Crv.autodetect()
    try:
        resolve_vm(crv, target, running=False)
    except CrvError as error:
        if json.loads(error.stdout).get("code") != "vm_not_found":
            raise
    else:
        print(
            f"VM {target!r} already exists; leaving it alone. Use make dev-node-vm-clean to recreate it."
        )
        return
    binary = inner_binary()
    subnet = pick_subnet(crv.network_list())
    crv.run("template", "instantiate", "corvus-test-node", target)
    details = resolve_vm(crv, target, running=False)
    with state_lock(details):
        state = vm_state(details, create=True)
        name = f"corvus-dev-node-{details['id']}"
        if any(network.get("name") == name for network in crv.network_list()):
            raise RuntimeError(f"Network {name!r} already exists; refusing to adopt it")
        # Record ownership before creation so cleanup also handles partial failures.
        publish_metadata(
            state,
            {"vm_id": details["id"], "vm_name": target, "network": name},
            filename="network.json",
        )
        document = {
            "networks": [
                {
                    "name": name,
                    "subnet": subnet,
                    "dhcp": True,
                    "nat": True,
                    "autostart": True,
                    "dnsServers": ["1.1.1.1", "8.8.8.8"],
                }
            ]
        }
        with tempfile.NamedTemporaryFile(mode="w", suffix=".json") as source:
            json.dump(document, source)
            source.flush()
            crv.apply(source.name, skip_existing=False, wait=True)
        crv.network_start(name)
        interfaces = details.get("net_ifs")
        if not isinstance(interfaces, list):
            raise RuntimeError("Outer daemon returned invalid network interfaces")
        for interface in interfaces:
            if not isinstance(interface, dict) or not isinstance(
                interface.get("id"), int
            ):
                raise RuntimeError(
                    "Outer daemon returned an invalid network interface ID"
                )
            crv.run("net-if", "remove", target, str(interface["id"]))
        crv.run("net-if", "add", target, "--network", name)
        for path, tag in [
            (binary.parent, "corvus_host"),
            (REPO_ROOT, "corvus_src"),
            (Path.home() / "VMs/BaseImages", "base_images"),
        ]:
            crv.run("shared-dir", "add", target, str(path), tag, "--read-only")
        crv.run("vm", "start", target, "--wait", timeout=120)
    print(f"VM {target!r} started on {name} ({subnet})")


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("target")
    args = parser.parse_args()
    try:
        create(args.target)
    except (RuntimeError, OSError, ValueError, subprocess.SubprocessError) as error:
        print(
            f"{error}\nCreation may be incomplete; run make dev-node-vm-clean with the same DEV_NODE_VM before retrying.",
            file=sys.stderr,
        )
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
