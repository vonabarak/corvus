"""Phase-1 smoke tests for `corvus-netd`, running inside the test node.

Why on the node, not on the host?

The agent will need `CAP_NET_ADMIN` once Phase 2 lands — bridge / TAP /
nftables / dnsmasq are all root-only on a stock kernel. Running pytest
itself as root would be wrong: every other test in the suite runs as a
regular user, and one rogue root pytest run could litter `nft list`
state on a developer's workstation. The outer test node has root
available, is disposable, and already mounts the host's stack-install
bin dir at `/opt/corvus/bin` via virtiofs — so we can launch the
freshly-built binary inside the node without copying it.

The test process on the host stays unprivileged. It:

  1. SSH-launches `sudo corvus-netd …` on the node (one-shot, then
     decoupled with `nohup`).
  2. Opens an SSH `-L` port-forward so a local TCP port lands on the
     agent's TCP port inside the node.
  3. Submits pycapnp coroutines onto the harness's existing
     `SyncRunloop` (the background-thread kj_loop the node's inner
     `Client` already owns). pycapnp's kj_loop is process-global, so
     we MUST reuse the harness's loop instead of spinning up our own
     — otherwise pycapnp aborts the process.

Phase-1 scope is wire shape only. The `Session` cap returned by
`session()` is a real cap, but every privileged method on it
(`createBridge`, `createTap`, …) is still `methodUnimplemented` —
those get filled in during Phase 2.
"""
from __future__ import annotations

import shutil
import socket
import subprocess
import time
from typing import Iterator

import capnp
import pytest

from corvus_client._schema import netagent as NETAGENT_SCHEMA
from corvus_test_harness import SingleNodeCase
from corvus_test_harness.cases import state_for
from corvus_test_harness.ssh import HOST_ALPINE_KEY_PATH


# `corvus_client._schema` already runs `capnp.load("netagent.capnp")`
# at import time (we registered it in `_SCHEMA_FILES`). Re-loading the
# same schema from this test module would re-register every type-id
# and pycapnp aborts the interpreter with `Fatal Python error:
# Aborted`. Reuse the harness-shared instance instead — it points at
# the same in-tree schema file (resolved via `CORVUS_SCHEMA_DIR` or
# the bundled mirror).

# `corvus_host` virtiofs share lands at /opt/corvus/bin in the node
# (see yaml/corvus-test-node/systemd/opt-corvus-bin.mount). The host's
# `stack install` puts `corvus-netd` next to `corvus`, so the same
# mount point exposes both.
NETD_BINARY_ON_NODE = "/opt/corvus/bin/corvus-netd"

# Fixed TCP port inside the node — picked outside the inner daemon's
# 9876 and outside the well-known low range. Per-class node lifecycle
# means there's no parallelism on the same node, so a fixed port is
# fine.
NETD_NODE_PORT = 9899

# Transient systemd unit name used on the node.  systemd-run spawns
# the agent as a fully-detached transient service, which sidesteps
# the SSH-channel hang you get when you try to background a daemon
# via plain `nohup`/`setsid` over a non-interactive SSH session.
NETD_TRANSIENT_UNIT = "corvus-netd-smoke.service"


# ---------------------------------------------------------------------------
# Node-side process lifecycle
# ---------------------------------------------------------------------------


def _start_netd_on_node(node) -> None:
    """Spawn `corvus-netd` as root on the node and return when it's listening.

    Uses `systemd-run` to create a transient service so the daemon is
    decoupled from the SSH session's lifetime — `nohup`+`&` over SSH
    notoriously wedges on closing pipes because backgrounded processes
    inherit the channel's stdio.
    """
    cmd = (
        # Defensive cleanup of any prior run.
        f"sudo systemctl stop {NETD_TRANSIENT_UNIT} 2>/dev/null || true; "
        f"sudo systemctl reset-failed {NETD_TRANSIENT_UNIT} 2>/dev/null || true; "
        f"sudo systemd-run --quiet --unit={NETD_TRANSIENT_UNIT} "
        f"  --property=Type=simple --property=Restart=no "
        f"  {NETD_BINARY_ON_NODE} "
        f"  --host 127.0.0.1 --port {NETD_NODE_PORT} --log-level debug; "
        # Wait up to 5s for the socket to be listening.
        f"for i in $(seq 1 50); do "
        f"  ss -ltn 2>/dev/null | grep -q ':{NETD_NODE_PORT} ' && exit 0; "
        f"  sleep 0.1; "
        f"done; "
        f"echo 'corvus-netd never started listening' >&2; "
        f"sudo journalctl -u {NETD_TRANSIENT_UNIT} --no-pager -n 50 >&2 || true; "
        f"exit 1"
    )
    node.run(cmd, timeout_sec=15)


def _stop_netd_on_node(node) -> None:
    """Stop the transient unit; tolerant of already-stopped / never-started."""
    cmd = (
        f"sudo systemctl stop {NETD_TRANSIENT_UNIT} 2>/dev/null || true; "
        f"sudo systemctl reset-failed {NETD_TRANSIENT_UNIT} 2>/dev/null || true"
    )
    node.run(cmd, timeout_sec=10, check=False)


# ---------------------------------------------------------------------------
# Host-side TCP port forward (host:host_port → node:NETD_NODE_PORT)
# ---------------------------------------------------------------------------


def _pick_free_port() -> int:
    with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as s:
        s.bind(("127.0.0.1", 0))
        return s.getsockname()[1]


def _open_port_forward(cid: int, host_port: int) -> subprocess.Popen:
    """SSH `-N -L` tunnel: host 127.0.0.1:host_port → node 127.0.0.1:NETD_NODE_PORT.

    Uses the same VSOCK-over-SSH proxy as `NodeShell`, but with
    `-N` (no command) and `-L` (forward). Held open until `terminate()`.
    Polls the local TCP port to detect when the forward is actually
    bound — SSH establishes the forward asynchronously after handshake.
    """
    socat = shutil.which("socat")
    ssh = shutil.which("ssh")
    if not socat or not ssh:
        raise RuntimeError("`ssh` and `socat` must both be on PATH")
    argv = [
        ssh,
        "-N",
        "-o", "StrictHostKeyChecking=no",
        "-o", "UserKnownHostsFile=/dev/null",
        "-o", "BatchMode=yes",
        "-o", "ExitOnForwardFailure=yes",
        "-o", f"ProxyCommand={socat} - VSOCK-CONNECT:{cid}:22",
        "-i", str(HOST_ALPINE_KEY_PATH),
        "-L", f"127.0.0.1:{host_port}:127.0.0.1:{NETD_NODE_PORT}",
        f"corvus@vsock-{cid}",
    ]
    proc = subprocess.Popen(
        argv,
        stdout=subprocess.DEVNULL,
        stderr=subprocess.PIPE,
    )
    deadline = time.monotonic() + 10.0
    while time.monotonic() < deadline:
        if proc.poll() is not None:
            err = b""
            if proc.stderr is not None:
                err = proc.stderr.read()
            raise RuntimeError(
                f"ssh port forward exited early: {err.decode(errors='replace')}"
            )
        try:
            with socket.create_connection(("127.0.0.1", host_port), timeout=0.2):
                return proc
        except OSError:
            time.sleep(0.1)
    proc.terminate()
    raise RuntimeError(
        f"ssh port forward never ready on 127.0.0.1:{host_port}"
    )


def _close_port_forward(proc: subprocess.Popen) -> None:
    if proc.poll() is not None:
        return
    proc.terminate()
    try:
        proc.wait(timeout=2.0)
    except subprocess.TimeoutExpired:
        proc.kill()
        proc.wait(timeout=2.0)


# ---------------------------------------------------------------------------
# pycapnp helpers — re-using the harness's background kj_loop.
#
# The IntegrationTestCase fixture has already created a `Client` for
# the node's inner daemon; that Client owns a `SyncRunloop` whose
# background thread is running `capnp.kj_loop()`. pycapnp's kj_loop
# is process-global — creating a second one in our test thread aborts
# the process with `Fatal Python error: Aborted`. We therefore route
# every pycapnp coroutine through the existing runloop's `run()`.
# ---------------------------------------------------------------------------


async def _with_client(host: str, port: int, schema, body):
    stream = await capnp.AsyncIoStream.create_connection(host=host, port=port)
    client = capnp.TwoPartyClient(stream)
    agent = client.bootstrap().cast_as(schema.NetAgent)
    return await body(agent)


def _run_on_node_loop(node, coro_factory):
    """Run `coro_factory()` on the node's existing kj_loop thread.

    `coro_factory` is a zero-arg callable returning a fresh coroutine.
    Building the coroutine outside the loop thread would tie it to the
    wrong asyncio loop; the factory pattern defers creation until
    we're already inside the loop's scheduling path.
    """
    rl = node.client()._rl

    async def wrapper():
        return await coro_factory()

    return rl.run(wrapper())


# ---------------------------------------------------------------------------
# Test class
# ---------------------------------------------------------------------------


class TestNetdSmoke(SingleNodeCase):
    """Cap'n Proto wire-shape checks against the in-node agent.

    Inherits the class-scoped one-node topology from `SingleNodeCase`.
    Adds a second class-scoped fixture that launches `corvus-netd`
    inside the node and opens a TCP port-forward to it. Both fixtures
    fire once per class; the four tests share them.

    Setup ordering relies on `_class_topology` (`autouse=True` on the
    parent) running before our `netd_endpoint` — pytest invokes
    parent-class autouse fixtures first when MROs are linear, which
    they are here. We surface a clear error if the topology isn't
    ready, rather than letting it manifest as a confusing crash.
    """

    NODES = ("netd",)

    @pytest.fixture(scope="class")
    def netd_endpoint(self, request) -> Iterator[tuple[str, int]]:
        state = state_for(request.cls)
        if state.topology is None:
            pytest.skip("class topology not initialised; upstream fixture failed")
        node = state.topology.nodes[0]

        _start_netd_on_node(node)
        host_port = _pick_free_port()
        forward = _open_port_forward(node.cid, host_port)
        try:
            yield ("127.0.0.1", host_port)
        finally:
            _close_port_forward(forward)
            _stop_netd_on_node(node)

    # -- the four tests ------------------------------------------------------

    def test_ping(self, netd_endpoint):
        host, port = netd_endpoint

        async def body(agent):
            await agent.ping()

        _run_on_node_loop(
            self.node,
            lambda: _with_client(host, port, NETAGENT_SCHEMA, body),
        )

    def test_version_reports_semver(self, netd_endpoint):
        host, port = netd_endpoint

        async def body(agent):
            resp = await agent.version()
            return resp.info

        info = _run_on_node_loop(
            self.node,
            lambda: _with_client(host, port, NETAGENT_SCHEMA, body),
        )
        assert info.semver.startswith("0.")
        # Phase 1 advertises an empty capability list; Phase 2 will
        # add "bridge", "tap", "nat", "dnsmasq" etc.
        assert list(info.capabilities) == []

    def test_session_returns_cap(self, netd_endpoint):
        host, port = netd_endpoint

        async def body(agent):
            resp = await agent.session(owner="test-uid-1000")
            # Holding the cap is enough to prove the agent returned
            # one; privileged methods are unimplemented in Phase 1.
            return resp.session is not None

        result = _run_on_node_loop(
            self.node,
            lambda: _with_client(host, port, NETAGENT_SCHEMA, body),
        )
        assert result is True

    def test_session_privileged_method_unimplemented(self, netd_endpoint):
        """Tripwire: Phase 1 Session methods all raise unimplemented.

        Calling `createBridge` should raise pycapnp's wrapper around
        Cap'n Proto's "unimplemented" error. Phase 2 will replace the
        stub with a real implementation — at which point this test
        will start failing, prompting a rewrite against real
        behaviour.
        """
        host, port = netd_endpoint

        async def body(agent):
            session_resp = await agent.session(owner="test-uid-1000")
            session = session_resp.session
            with pytest.raises(capnp.KjException):
                await session.createBridge(
                    params={
                        "name": "br-stub",
                        "cidr": "10.99.0.1/24",
                        "mtu": 1500,
                    }
                )

        _run_on_node_loop(
            self.node,
            lambda: _with_client(host, port, NETAGENT_SCHEMA, body),
        )
