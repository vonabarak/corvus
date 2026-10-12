"""Register the host's published base images with a configured manual node."""

from __future__ import annotations

import argparse
import shlex
import sys
import tempfile
from pathlib import Path

from . import base_images
from .inner import open_client
from .outer import Crv
from .ssh import HOST_ALPINE_KEY_PATH, NodeShell
from .transport import VsockTcpRelay


def register_images(vm: str, ssh_key: Path) -> None:
    crv = Crv.autodetect()
    details = crv.vm_show(vm)
    if details.get("status") != "running":
        raise RuntimeError(f"VM {vm!r} must be started before image registration")
    cid = details.get("vsock_cid")
    if not isinstance(cid, int) or isinstance(cid, bool) or cid <= 2:
        raise RuntimeError(f"VM {vm!r} has no usable VSOCK CID")
    shell = NodeShell(cid=cid, user="corvus", key_path=ssh_key)
    with tempfile.TemporaryDirectory(prefix="corvus-devnode-certs-") as temporary:
        cert_dir = Path(temporary)
        for name in ("ca.crt", "corvus-client.crt", "corvus-client.key"):
            result = shell.run(
                'cat "${XDG_CONFIG_HOME:-$HOME/.config}/corvus/"' + shlex.quote(name),
                check=False,
            )
            if result.returncode != 0 or not result.stdout:
                detail = result.stderr.decode(errors="replace").strip()
                raise RuntimeError(
                    f"Cannot fetch {name} from {vm}: {detail}. "
                    "Run bash /mnt/corvus/scripts/devnode/setup.sh inside the VM "
                    "and retry make dev-node-vm-images."
                )
            path = cert_dir / name
            path.touch(mode=0o600)
            path.write_bytes(result.stdout)
        with VsockTcpRelay.start(cid) as relay:
            client = open_client(
                relay, cert_dir=cert_dir, tls=True, ensure_self_node=False
            )
            try:
                registered = base_images.register_all(
                    client, crv, vm, update_existing=True
                )
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
    parser.add_argument("--ssh-key", type=Path, default=HOST_ALPINE_KEY_PATH)
    args = parser.parse_args(argv)
    try:
        register_images(args.vm, args.ssh_key)
    except Exception as error:
        print(f"register-images: {error}", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
