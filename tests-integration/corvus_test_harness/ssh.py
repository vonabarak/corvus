"""Low-level guest shell helpers.

Most tests should drive a VM via its inner pycapnp `Client`. SSH is the
escape hatch for things the daemon can't observe of itself: kernel
sysctl state, filesystem layout, package versions, route tables, etc.

We SSH over the VSOCK transport (`ssh -o ProxyCommand="socat - VSOCK-CONNECT:<cid>:22"`
or systemd-ssh-proxy). The integration-test image ships with sshd
enabled and the `corvus` user authorized by the same key applied via
`crv apply`. Tests need a private key on the host that pairs with the
embedded public key — by convention `.test-images/corvus-test-key`.
"""
from __future__ import annotations

import os
import shutil
import subprocess
from dataclasses import dataclass
from pathlib import Path
from typing import Optional


@dataclass(frozen=True)
class GuestShell:
    cid: int
    user: str = "root"
    key_path: Optional[Path] = None
    port: int = 22

    def run(
        self,
        command: str,
        *,
        timeout_sec: float = 60.0,
        check: bool = True,
    ) -> subprocess.CompletedProcess:
        """Run `command` in the guest via SSH over VSOCK. Returns
        the CompletedProcess (stdout/stderr captured)."""
        socat = shutil.which("socat")
        if not socat:
            raise RuntimeError("`socat` not on PATH; required for VSOCK SSH")
        ssh = shutil.which("ssh")
        if not ssh:
            raise RuntimeError("`ssh` not on PATH")
        argv = [
            ssh,
            "-o", "StrictHostKeyChecking=no",
            "-o", "UserKnownHostsFile=/dev/null",
            "-o", "BatchMode=yes",
            "-o", f"ProxyCommand={socat} - VSOCK-CONNECT:{self.cid}:{self.port}",
        ]
        if self.key_path is not None:
            argv += ["-i", str(self.key_path)]
        argv += [f"{self.user}@vsock-{self.cid}", command]
        return subprocess.run(
            argv,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            timeout=timeout_sec,
            check=check,
        )

    def read_file(self, path: str) -> str:
        out = self.run(f"cat {path}")
        return out.stdout.decode("utf-8", errors="replace")
