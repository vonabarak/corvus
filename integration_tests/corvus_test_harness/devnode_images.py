"""Register the host's published base images with a configured manual node."""

from __future__ import annotations

import argparse
import sys

from . import base_images
from .host_access import certificate_directory, resolve_vm
from .inner import open_client
from .outer import Crv
from .transport import VsockTcpRelay


def register_images(vm: str) -> None:
    crv = Crv.autodetect()
    details = resolve_vm(crv, vm)
    cid = details.get("vsock_cid")
    if not isinstance(cid, int) or isinstance(cid, bool) or cid <= 2:
        raise RuntimeError(f"VM {vm!r} has no usable VSOCK CID")
    cert_dir = certificate_directory(details)
    with VsockTcpRelay.start(cid) as relay:
        client = open_client(relay, cert_dir=cert_dir, tls=True, ensure_self_node=False)
        try:
            registered = base_images.register_all(client, crv, vm, update_existing=True)
            if not registered.disk_ids:
                print("No eligible latest images under ~/VMs/BaseImages.")
            for name, disk_id in registered.disk_ids.items():
                image = registered.images[name]
                print(f"{name}: inner ID {disk_id}, backing {image.guest_path}")
        finally:
            client.close()


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--vm", default="corvus-dev-node")
    args = parser.parse_args(argv)
    try:
        register_images(args.vm)
    except Exception as error:
        print(f"register-images: {error}", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
