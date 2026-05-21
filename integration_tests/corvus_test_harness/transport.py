"""VSOCK-to-TCP relay glue.

The inner Corvus daemon listens on TCP inside its VM; a systemd unit
(`corvus-tcp-relay.service`) exposes that port over AF_VSOCK on the
VM's permanent CID. From the host we run a `socat` in the other
direction: a TCP listener on `127.0.0.1:<auto>` that forks each
connection over VSOCK to the VM's CID.

The Python client connects as if the inner daemon were a local TCP
service. The relay is created per VM by the topology fixture and torn
down on test end.
"""

from __future__ import annotations

import errno
import shutil
import socket
import subprocess
import time
from dataclasses import dataclass

# The TCP port the inner daemon binds; matches corvus.service
# inside the test image. Same number is used as the VSOCK port.
INNER_DAEMON_TCP_PORT = 9876


def _find_free_tcp_port() -> int:
    """Ask the kernel for any unused local TCP port."""
    with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as s:
        s.bind(("127.0.0.1", 0))
        return s.getsockname()[1]


@dataclass
class VsockTcpRelay:
    """Host-side socat that forwards TCP → VSOCK for one inner daemon.

    Construct via `start(cid, ...)`. Stop by calling `close()` or by
    using the relay as a context manager.
    """

    cid: int
    vsock_port: int
    host_port: int
    _proc: subprocess.Popen

    @classmethod
    def start(
        cls,
        cid: int,
        *,
        vsock_port: int = INNER_DAEMON_TCP_PORT,
        host_port: int | None = None,
    ) -> VsockTcpRelay:
        if cid <= 2:
            # CIDs 0/1/2 are reserved by the AF_VSOCK ABI; the harness
            # should never see one of those on a Corvus VM.
            raise ValueError(f"refusing to relay to reserved VSOCK CID {cid}")
        socat = shutil.which("socat")
        if not socat:
            raise RuntimeError("`socat` not on PATH; required to relay vsock ↔ TCP")
        port = host_port if host_port is not None else _find_free_tcp_port()
        proc = subprocess.Popen(
            [
                socat,
                f"TCP-LISTEN:{port},bind=127.0.0.1,reuseaddr,fork",
                f"VSOCK-CONNECT:{cid}:{vsock_port}",
            ],
            stdout=subprocess.DEVNULL,
            stderr=subprocess.PIPE,
        )
        # Wait briefly for socat to bind. If it died (e.g. port collision
        # raced or vsock_loopback isn't loaded), surface the error now.
        deadline = time.monotonic() + 5.0
        while time.monotonic() < deadline:
            if proc.poll() is not None:
                err = (proc.stderr.read() if proc.stderr else b"").decode(
                    errors="replace"
                )
                raise RuntimeError(
                    f"socat relay exited early: {err.strip() or proc.returncode}"
                )
            if _tcp_listen_ready(port):
                break
            time.sleep(0.05)
        else:
            proc.terminate()
            raise RuntimeError(
                f"socat relay didn't start listening on 127.0.0.1:{port}"
            )
        return cls(cid=cid, vsock_port=vsock_port, host_port=port, _proc=proc)

    @property
    def endpoint(self) -> tuple[str, int]:
        """Return (host, port) suitable for `corvus_client.Client(host=..., port=...)`."""
        return ("127.0.0.1", self.host_port)

    def close(self) -> None:
        proc = self._proc
        if proc.poll() is None:
            proc.terminate()
            try:
                proc.wait(timeout=3)
            except subprocess.TimeoutExpired:
                proc.kill()
                proc.wait(timeout=2)

    def __enter__(self) -> VsockTcpRelay:
        return self

    def __exit__(self, exc_type, exc, tb) -> None:
        self.close()


def _tcp_listen_ready(port: int) -> bool:
    """Best-effort check that 127.0.0.1:<port> is accepting connects."""
    try:
        with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as s:
            s.settimeout(0.1)
            s.connect(("127.0.0.1", port))
            return True
    except OSError as e:
        if e.errno in (errno.ECONNREFUSED, errno.EAGAIN, errno.ETIMEDOUT):
            return False
        # Anything else (EADDRNOTAVAIL etc.) — propagate as not-ready;
        # caller will time out and surface the underlying issue.
        return False
