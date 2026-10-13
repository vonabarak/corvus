"""Host-owned PKI and deployment for a persistent development node."""

from __future__ import annotations

import argparse
import shlex
import subprocess
import sys
import uuid
from pathlib import Path

from corvus_admin import ca, deploy, store

from .host_access import (
    CERT_FILES,
    RECOVERY,
    certificate_directory,
    inner_binary,
    private_directory,
    publish_metadata,
    read_metadata,
    resolve_vm,
    state_lock,
    validate_state,
    vm_id,
)
from .outer import Crv, JsonObject
from .runner import NodeShellRunner
from .ssh import HOST_ALPINE_KEY_PATH, NodeShell


class DevRunner(NodeShellRunner):
    """Expand user-service paths on the guest before the harness runner quotes them."""

    def __init__(self, shell: NodeShell) -> None:
        super().__init__(shell)
        result = shell.run('printf %s "$HOME"')
        self.home = result.stdout.decode().strip()
        if not self.home.startswith("/"):
            raise RuntimeError("Cannot resolve guest home directory")

    def expand(self, path: str) -> str:
        return self.home + path[1:] if path.startswith("~/") else path

    def mkdir_p(self, path: str, *, mode: int = 0o755, sudo: bool = False) -> None:
        super().mkdir_p(self.expand(path), mode=mode, sudo=sudo)

    def copy_bytes(
        self, data: bytes, remote_path: str, *, mode: int, sudo: bool = True
    ) -> None:
        super().copy_bytes(data, self.expand(remote_path), mode=mode, sudo=sudo)


def configured_backend(exec_start: str) -> str:
    import re

    match = re.search(r"--database\s+([^\s;]+)", exec_start)
    if not match:
        raise RuntimeError(
            "Cannot determine configured database; inspect the guest daemon service manually"
        )
    database = match[1]
    if database.startswith(("postgresql://", "postgres://")):
        return "postgresql"
    if "://" in database or database.startswith("--"):
        raise RuntimeError(f"Unrecognized configured database: {database}")
    return "sqlite"


def inspect_guest(shell: NodeShell, database: str) -> bool:
    """Validate before changing PKI or guest state; return whether a user unit exists."""
    shell.run("test -d /mnt/corvus && test -d /opt/corvus/bin")
    result = shell.run(
        "systemctl --user show corvus.service --property=ExecStart --value", check=False
    )
    exec_start = result.stdout.decode().strip()
    if exec_start:
        configured = configured_backend(exec_start)
        if configured != database:
            raise RuntimeError(
                f"Configured backend is {configured}, requested {database}; no data was migrated. {RECOVERY}"
            )
        return True
    if (
        shell.run(
            'test -d "${XDG_CONFIG_HOME:-$HOME/.config}/corvus/admin"', check=False
        ).returncode
        == 0
    ):
        raise RuntimeError(
            f"Existing guest admin store has no managed host state. {RECOVERY}"
        )
    return False


def verify(shell: NodeShell, address: str, certs: Path, binary: Path) -> None:
    shell.run(
        "systemctl --user is-active --quiet corvus.service corvus-nodeagent.service && sudo systemctl is-active --quiet corvus-netd.service"
    )
    subprocess.run(
        [str(binary), "--host", address, "--tls-cert-dir", str(certs), "status"],
        check=True,
        timeout=60,
    )


def setup(vm: str, ssh_key: Path, database: str) -> None:
    if database not in ("sqlite", "postgresql"):
        raise ValueError("Database must be sqlite or postgresql")
    if Path("/mnt/corvus").is_dir() and Path("/opt/corvus/bin").is_dir():
        raise RuntimeError(
            "setup.sh now runs on the host; use corvus-admin quickstart for manual guest setup"
        )
    crv = Crv.autodetect()
    details = resolve_vm(crv, vm)
    cid = details.get("vsock_cid")
    if not isinstance(cid, int) or isinstance(cid, bool) or cid <= 2:
        raise RuntimeError(f"VM {vm!r} has no usable VSOCK CID")
    binary = inner_binary()
    ssh_key = ssh_key.resolve()
    if not ssh_key.is_file():
        raise RuntimeError(f"SSH key missing: {ssh_key}")
    shell = NodeShell(cid=cid, user="corvus", key_path=ssh_key)
    # Wait for the managed NIC lease on first boot, before SAN issuance.
    address = shell.outer_ip()
    with state_lock(details) as directory:
        existing = inspect_guest(shell, database)
        metadata: JsonObject | None = None
        if directory.exists() or directory.is_symlink():
            validate_state(directory)
            if (directory / "state.json").exists():
                metadata = read_metadata(directory / "state.json")
        if (
            metadata is None
            and shell.run(
                'test -e "${XDG_CONFIG_HOME:-$HOME/.config}/corvus/ca.crt"',
                check=False,
            ).returncode
            == 0
        ):
            raise RuntimeError(
                f"Guest has credentials without matching host PKI. {RECOVERY}"
            )
        if existing and metadata is None:
            raise RuntimeError(
                f"Guest is already configured but has no matching host PKI. {RECOVERY}"
            )
        if metadata is not None:
            if (
                metadata.get("vm_id") != vm_id(details)
                or metadata.get("database") != database
                or metadata.get("ip") != address
            ):
                raise RuntimeError(
                    f"Host state does not match the VM/backend/IP; no configuration changed. {RECOVERY}"
                )
            guest_ca = shell.run(
                'cat "${XDG_CONFIG_HOME:-$HOME/.config}/corvus/ca.crt"', check=False
            )
            host_ca = directory / "admin" / "ca.crt"
            if (
                guest_ca.returncode == 0 and guest_ca.stdout != host_ca.read_bytes()
            ) or (metadata.get("ready") is True and guest_ca.returncode != 0):
                raise RuntimeError(f"Guest CA does not match the host PKI. {RECOVERY}")
            if metadata.get("ready") is True:
                certs = certificate_directory(details)
                shell.run("sudo loginctl enable-linger corvus")
                verify(shell, address, certs, binary)
                metadata["ssh_key"] = str(ssh_key)
                metadata["name"] = details["name"]
                publish_metadata(directory, metadata)
                print(
                    f"setup: reusing {database} configuration; host certificates: {certs}"
                )
                return
        runner = DevRunner(shell)
        if database == "postgresql":
            shell.run("psql postgresql:///corvus -v ON_ERROR_STOP=1 -c 'SELECT 1'")
            database_value = "postgresql:///corvus"
        else:
            database_value = runner.home + "/.local/share/corvus/corvus.db"
            runner.mkdir_p(str(Path(database_value).parent), mode=0o700)
        private_directory(directory, create=True)
        admin = private_directory(directory / "admin", create=True)
        client_dir = private_directory(directory / "client", create=True)
        admin_store = store.AdminStore(admin)
        if not admin_store.exists():
            ca.init_ca(admin_store)
        if metadata is None:
            metadata = {
                "vm_id": vm_id(details),
                "name": details["name"],
                "ssh_key": str(ssh_key),
                "database": database,
                "ip": address,
                "daemon_uuid": str(uuid.uuid4()),
                "ready": False,
            }
            publish_metadata(directory, metadata)
        if not all((client_dir / name).is_file() for name in CERT_FILES):
            issued = ca.issue_cert(
                admin_store, role=ca.ROLE_CLIENT, name="devnode", ip=None
            )
            for name, content in zip(
                CERT_FILES,
                (
                    admin_store.ca_cert_path.read_bytes(),
                    issued.cert_pem,
                    issued.key_pem,
                ),
                strict=True,
            ):
                path = client_dir / name
                path.write_bytes(content)
                path.chmod(0o600)
        shell.run("sudo loginctl enable-linger corvus")
        # Only role credentials and the public CA cross into the guest.
        runner.mkdir_p("~/.config/corvus", mode=0o700)
        for name in CERT_FILES:
            runner.copy_bytes(
                (client_dir / name).read_bytes(),
                f"~/.config/corvus/{name}",
                mode=0o600,
                sudo=False,
            )
        runner.run(
            ["systemctl", "reset-failed", "corvus-netd.service"], sudo=True, check=False
        )
        runner.run(
            [
                "systemctl",
                "--user",
                "reset-failed",
                "corvus.service",
                "corvus-nodeagent.service",
            ],
            check=False,
        )
        deploy.deploy_netd(
            admin_store,
            runner,
            name="devnode",
            ip="127.0.0.1",
            binary_path="/opt/corvus/bin/corvus-netd",
        )
        deploy.deploy_node(
            admin_store,
            runner,
            name="devnode",
            ip="127.0.0.1",
            user_service=True,
            binary_path="/opt/corvus/bin/corvus-nodeagent",
        )
        deploy.deploy_daemon(
            admin_store,
            runner,
            listen_ip=address,
            additional_ips=("127.0.0.1",),
            reuse_uuid=str(metadata["daemon_uuid"]),
            user_service=True,
            binary_path="/opt/corvus/bin/corvus",
            database=database_value,
        )
        code = (
            "from corvus_admin.register import register_node; r=register_node(name='devnode', host='127.0.0.1', base_path="
            + repr(runner.home + "/VMs")
            + ", crv_path='/opt/corvus/bin/crv'); assert r.healthy, 'node healthcheck timed out'"
        )
        shell.run(
            "PYTHONPATH=/mnt/corvus/python python3 -c " + shlex.quote(code),
            timeout_sec=120,
        )
        verify(shell, address, client_dir, binary)
        metadata["ready"] = True
        publish_metadata(directory, metadata)
        print(f"setup: ready with {database}; host certificates: {client_dir}")


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--vm", default="corvus-dev-node")
    parser.add_argument("--ssh-key", type=Path, default=HOST_ALPINE_KEY_PATH)
    parser.add_argument(
        "--database", choices=("sqlite", "postgresql"), default="sqlite"
    )
    args = parser.parse_args(argv)
    try:
        setup(args.vm, args.ssh_key, args.database)
    except (RuntimeError, OSError, ValueError, subprocess.SubprocessError) as error:
        print(f"setup: {error}", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
