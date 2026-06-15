"""Spawn ``corvus-web`` on the host and point it at one TestNode.

The web gateway is normally a long-running operator process; tests
boot one per scenario, scrape ``/metrics`` (or the WebSocket stats
endpoints), and tear it down.

Why on the host rather than inside the test node:

  * The inner daemon already speaks mTLS-protected Cap'n Proto on
    the host-side TCP relay at ``127.0.0.1:<auto>`` (see
    :mod:`transport`). ``corvus-web`` is a thin client of the daemon
    — running it in the same Python interpreter the harness already
    has is cheaper than bootstrapping a second one inside the VM
    and dealing with VSOCK-forwarded HTTP back to the host.
  * The host-side ``_client_cert_dir`` minted by
    :meth:`Topology.deploy_certs` already has the cert trio
    (``ca.crt``, ``corvus-client.crt``, ``corvus-client.key``)
    ``corvus-web``'s ``--daemon-cert-dir`` flag expects, so the
    gateway authenticates over the exact same chain the pycapnp
    client uses.

We invoke ``corvus-web`` as ``sys.executable -m corvus_web``
(via :mod:`corvus_web.__main__`) so the gateway runs in the same
Python the harness is using — the ``corvus-web`` console script on
``$PATH`` may belong to a separate, system-wide install that
doesn't have the in-tree ``corvus_web`` package on its
``sys.path``. We spawn it with ``subprocess.Popen`` rather than
embedding uvicorn in the test process to keep its event loop fully
isolated from the harness's pycapnp ``kj_loop``.
"""

from __future__ import annotations

import os
import socket
import subprocess
import sys
import tempfile
import time
from typing import TYPE_CHECKING
from urllib.error import URLError
from urllib.request import Request, urlopen

if TYPE_CHECKING:
    from .topology import TestNode


def _find_free_tcp_port() -> int:
    """Pick an unused TCP port on the loopback interface."""
    with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as s:
        s.bind(("127.0.0.1", 0))
        return s.getsockname()[1]


def _tcp_listen_ready(host: str, port: int) -> bool:
    """Best-effort check that ``host:port`` accepts connections."""
    try:
        with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as s:
            s.settimeout(0.1)
            s.connect((host, port))
            return True
    except OSError:
        return False


class WebGateway:
    """Spawn ``corvus-web`` against one ``TestNode``'s inner daemon.

    Usage::

        with WebGateway(self.node) as web:
            web.wait_for_metrics_warmup(timeout_sec=30.0)
            body = web.get("/metrics")
            assert "corvus_vm_up" in body

    The gateway picks a free loopback port, dials the daemon over the
    same TCP relay + mTLS cert dir the pycapnp client uses, and
    blocks ``__enter__`` until the HTTP listener is accepting
    connections. ``__exit__`` terminates the subprocess (SIGTERM,
    then SIGKILL after 3 s).
    """

    def __init__(
        self,
        node: TestNode,
        *,
        bind_host: str = "127.0.0.1",
        bind_port: int | None = None,
        log_level: str = "warning",
    ) -> None:
        self.node = node
        self._bind_host = bind_host
        self._bind_port = bind_port if bind_port is not None else _find_free_tcp_port()
        self._log_level = log_level
        self._proc: subprocess.Popen | None = None
        # corvus-web's stdout + stderr go to this file. We read it
        # back on failure so the diagnostic surfaces in the pytest
        # output instead of disappearing into a dead pipe.
        # Setting Popen(..., stderr=PIPE) would deadlock the child
        # once the pipe buffer fills with debug logging.
        self._log_file: tempfile._TemporaryFileWrapper | None = None

    # ---- public API --------------------------------------------------------

    @property
    def base_url(self) -> str:
        """e.g. ``http://127.0.0.1:38291`` — no trailing slash."""
        return f"http://{self._bind_host}:{self._bind_port}"

    @property
    def host(self) -> str:
        return self._bind_host

    @property
    def port(self) -> int:
        return self._bind_port

    def get(self, path: str, *, timeout_sec: float = 5.0) -> str:
        """GET ``base_url + path``, return the response body as text.

        Raises ``urllib.error.HTTPError`` on non-2xx for behaviour
        identical to the underlying ``urlopen``. Callers that need
        to inspect headers / status code should call
        :meth:`get_response` instead.
        """
        with urlopen(self.base_url + path, timeout=timeout_sec) as resp:
            return resp.read().decode("utf-8")

    def get_response(self, path: str, *, timeout_sec: float = 5.0):
        """Same as :meth:`get` but returns the raw ``HTTPResponse``
        so callers can inspect status + headers. The caller is
        responsible for closing it (use as a context manager)."""
        req = Request(self.base_url + path)
        return urlopen(req, timeout=timeout_sec)

    def wait_for_metrics_warmup(self, *, timeout_sec: float = 30.0) -> None:
        """Block until ``GET /metrics`` returns 200 (not 503).

        ``corvus-web`` returns 503 while the background metrics
        poller hasn't completed its first cycle (see
        ``python/corvus_web/routes/metrics.py:190-195``); under the
        10-second poll cadence the first sample lands within one
        interval. The default 30-second budget covers a slow daemon
        cold-start without flaking on fast hardware.
        """
        deadline = time.monotonic() + timeout_sec
        last_err: str | None = None
        while time.monotonic() < deadline:
            try:
                with self.get_response("/metrics", timeout_sec=2.0) as resp:
                    if resp.status == 200:
                        return
                    last_err = f"HTTP {resp.status}"
            except URLError as e:
                last_err = str(e)
            time.sleep(0.5)
        raise AssertionError(
            f"corvus-web /metrics never returned 200 within {timeout_sec}s; "
            f"last error: {last_err}; "
            f"corvus-web log tail:\n{self.log_tail()}"
        )

    def log_tail(self, *, max_bytes: int = 4096) -> str:
        """Read the last ``max_bytes`` of corvus-web's stdout+stderr.

        Used in failure messages so a broken WebGateway test points
        at the gateway's own diagnostic, not just a generic timeout."""
        if self._log_file is None:
            return "(no log file)"
        try:
            with open(self._log_file.name, "rb") as f:
                f.seek(0, os.SEEK_END)
                size = f.tell()
                f.seek(max(0, size - max_bytes))
                return f.read().decode("utf-8", errors="replace")
        except OSError as e:
            return f"(could not read log: {e})"

    def ws_url(self, path: str) -> str:
        """Build the ``ws://`` URL for a WebSocket endpoint."""
        return f"ws://{self._bind_host}:{self._bind_port}{path}"

    # ---- lifecycle ---------------------------------------------------------

    def __enter__(self) -> WebGateway:
        cert_dir = self.node._client_cert_dir
        if cert_dir is None:
            raise RuntimeError(
                f"node {self.node.short_name!r}: cert dir not set — "
                "Topology.deploy_certs() hasn't run"
            )
        relay_host, relay_port = self.node.relay.endpoint
        # Use sys.executable so we get the venv Python that's
        # running pytest — relying on the `corvus-web` console
        # script picks up whichever copy happens to be earliest on
        # $PATH, which is often a user-level install with the
        # in-tree `corvus_web` package missing from its sys.path.
        argv = [
            sys.executable,
            "-m",
            "corvus_web",
            "--bind-host",
            self._bind_host,
            "--bind-port",
            str(self._bind_port),
            "--daemon-host",
            relay_host,
            "--daemon-port",
            str(relay_port),
            "--daemon-cert-dir",
            str(cert_dir),
            "--log-level",
            self._log_level,
        ]
        # stdout + stderr merge into a tempfile so debug logging
        # doesn't deadlock against a pipe buffer once the kernel
        # ringbuffer fills.
        self._log_file = tempfile.NamedTemporaryFile(
            prefix="corvus-web-",
            suffix=".log",
            delete=False,
        )
        self._proc = subprocess.Popen(
            argv,
            stdout=self._log_file,
            stderr=subprocess.STDOUT,
            env=os.environ.copy(),
        )
        try:
            self._wait_for_listener(timeout_sec=20.0)
        except BaseException:
            self.__exit__(None, None, None)
            raise
        return self

    def __exit__(self, *_args: object) -> None:
        proc = self._proc
        if proc is None:
            return
        if proc.poll() is None:
            proc.terminate()
            try:
                proc.wait(timeout=3)
            except subprocess.TimeoutExpired:
                proc.kill()
                proc.wait(timeout=2)
        self._proc = None
        # Close + remove the log tempfile. Failure path keeps the
        # file open via `log_tail()` long enough for AssertionError
        # to format with it.
        if self._log_file is not None:
            try:
                self._log_file.close()
            except Exception:
                pass
            try:
                os.unlink(self._log_file.name)
            except OSError:
                pass
            self._log_file = None

    # ---- internals ---------------------------------------------------------

    def _wait_for_listener(self, *, timeout_sec: float) -> None:
        """Block until the HTTP listener accepts connections, or fail
        with the captured stderr if ``corvus-web`` exited early."""
        deadline = time.monotonic() + timeout_sec
        proc = self._proc
        assert proc is not None
        while time.monotonic() < deadline:
            if proc.poll() is not None:
                raise RuntimeError(
                    f"corvus-web exited during startup "
                    f"(rc={proc.returncode}); log tail:\n{self.log_tail()}"
                )
            if _tcp_listen_ready(self._bind_host, self._bind_port):
                return
            time.sleep(0.1)
        raise RuntimeError(
            f"corvus-web did not start listening on "
            f"{self._bind_host}:{self._bind_port} within {timeout_sec}s; "
            f"log tail:\n{self.log_tail()}"
        )
