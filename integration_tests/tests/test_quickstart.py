"""End-to-end test for ``corvus-admin quickstart``.

Boots a bare integration-test node (no pre-deployed certs, no
running corvus services), installs corvus + corvus-admin from the
mounted source tree, then runs ``corvus-admin quickstart`` as the
``corvus`` user and verifies the resulting deployment:

* all five Corvus executables resolve on ``$PATH``
* the daemon and nodeagent user services are active
* the netd system service is active
* ``crv`` talks to the daemon over its Unix socket
* the Python ``corvus_client`` library connects over the same
  socket and returns a sensible ``status()``
* the daemon has a Node row recording the just-registered host

The class fixture is custom (not ``SingleNodeCase``) because the
default class fixture would invoke ``corvus-admin deploy …``
itself, defeating the purpose of testing the one-shot path.

**Image prerequisite:** the test-node image must carry the
Python build deps that corvus-admin needs (``dev-python/{pip,
click,cryptography,jinja,pyyaml,pycapnp}``). The portage tree
isn't mounted on a *running* test node, so the deps cannot be
emerged at test time — they have to be baked in by
``yaml/corvus-test-node/corvus-test-node.yml``. Rebuild the
image after pulling this commit:

    make test-image-node
"""

from __future__ import annotations

import shlex
import sys
import time

import pytest
from corvus_test_harness.cases import IntegrationTestCase, state_for
from corvus_test_harness.topology import NodeRole, Topology

# Where the harness mounts the host's repo when ``attach_source=True``.
# Matches the systemd mount unit baked into the test-node image at
# yaml/corvus-test-node/systemd/mnt-corvus.mount.
SRC_MOUNT = "/mnt/corvus"

# User on the node that runs everything (matches the test-node
# image's home dir + Postgres role).
NODE_USER = "corvus"


# ---------------------------------------------------------------------------
# Helpers


def _run(
    node,
    cmd: str,
    *,
    user: str = NODE_USER,
    check: bool = True,
    timeout_sec: float = 120.0,
):
    """``node.run`` wrapper that decodes stdout/stderr and exposes
    both via a small dataclass so assertions read cleanly."""

    cp = node.run(cmd, user=user, check=check, timeout_sec=timeout_sec)
    return cp


def _user_env_prefix() -> str:
    """Shell snippet to export $PATH + $XDG_RUNTIME_DIR so a
    non-interactive SSH session reaches the user-systemd bus
    AND finds binaries pip-installed under ``~/.local/bin``.

    ``loginctl enable-linger corvus`` (run once in the fixture)
    keeps the user manager alive across SSH sessions; this prefix
    just makes sure each command we shell into sees the same env
    the user manager exports."""

    return (
        "export XDG_RUNTIME_DIR=/run/user/$(id -u); "
        "export DBUS_SESSION_BUS_ADDRESS=unix:path=$XDG_RUNTIME_DIR/bus; "
        "export PATH=$HOME/.local/bin:/opt/corvus/bin:$PATH; "
    )


def _shrun(
    node,
    body: str,
    *,
    user: str = NODE_USER,
    check: bool = True,
    timeout_sec: float = 120.0,
):
    """Run a multi-line shell snippet with the user env prefix."""

    full = _user_env_prefix() + body
    return _run(
        node,
        f"bash -c {shlex.quote(full)}",
        user=user,
        check=check,
        timeout_sec=timeout_sec,
    )


# ---------------------------------------------------------------------------
# Test class


class TestQuickstart(IntegrationTestCase):
    """``corvus-admin quickstart`` bootstraps a one-node deployment.

    The class fixture brings up a single node with the source tree
    attached at ``/mnt/corvus`` and pip-installs corvus into the
    ``corvus`` user's site-packages — the test methods then drive
    quickstart and verify the resulting state."""

    NODES = ("quickstart",)

    # ---- Class fixture override ----------------------------------------

    @pytest.fixture(scope="class", autouse=True)
    def _class_topology(self, request, crv, image_ready, host_binary):
        """Bring up the bare node + install corvus-admin from source.

        Differs from the default :class:`IntegrationTestCase`
        fixture in three ways:

        1. ``attach_source=True`` so ``/mnt/corvus`` exposes the
           repo to the node (we need to pip-install corvus there).
        2. We skip ``deploy_certs()`` — quickstart will mint and
           deploy its own certs.
        3. We skip ``node.client()`` — there is no daemon to dial
           until quickstart starts one.
        """

        cls = request.cls
        state = state_for(cls)
        topology: Topology | None = None
        try:
            topology = Topology(
                crv,
                image_ready,
                host_binary,
                class_name=cls.__name__,
                attach_source=True,
            ).__enter__()
            # init_cas is required by Topology's invariants even
            # though we won't call deploy_certs(): the case fixture
            # would otherwise raise on teardown when walking the
            # CA registry.
            topology.init_cas(("shared",))
            sys.stderr.write(f"[harness] booting bare node for {cls.__qualname__}\n")
            sys.stderr.flush()
            for short_name in cls.NODES:
                topology.add(short_name, role=NodeRole.FULL_STACK, ca_key="shared")
            node = topology.nodes[0]
            _bootstrap_node(node)
            state.topology = topology
        except BaseException as exc:
            import traceback

            state.setup_failed = True
            state.setup_error = f"{type(exc).__name__}: {exc}"
            sys.stderr.write(f"[harness] {cls.__qualname__} class fixture failed:\n")
            traceback.print_exc(file=sys.stderr)
            sys.stderr.flush()
            if topology is not None:
                try:
                    topology.finalize(leak_on_failure=True)
                except Exception:
                    pass
                state.topology = None
            yield
            return

        try:
            yield
        finally:
            t = state.topology
            state.topology = None
            if t is not None:
                leak = state.first_failure is not None or state.setup_failed
                try:
                    t.finalize(leak_on_failure=leak)
                except Exception as e:
                    sys.stderr.write(
                        f"[harness] finalize failed for {cls.__qualname__}: {e}\n"
                    )
                    sys.stderr.flush()

    @property
    def node(self):
        return self.nodes[0]

    # ---- Tests ---------------------------------------------------------

    def test_01_all_required_binaries_on_path(self):
        """Every executable quickstart needs resolves via ``which``."""

        for binary in (
            "corvus",
            "corvus-nodeagent",
            "corvus-netd",
            "crv",
            "corvus-admin",
        ):
            cp = _shrun(self.node, f"which {binary}")
            path = cp.stdout.decode().strip()
            assert path, (
                f"{binary!r} not on $PATH (stderr: {cp.stderr.decode().strip()})"
            )

    def test_02_quickstart_succeeds(self):
        """``corvus-admin quickstart`` runs to completion on a fresh node.

        Bump the healthcheck timeout: the test-node VM's user-systemd
        cold-starts the daemon, runs migrations, and waits for the
        nodeagent supervisor's first dial — all of which routinely
        eat past the 15s production default.
        """

        cp = _shrun(
            self.node,
            "corvus-admin quickstart --node-name self --listen-ip 127.0.0.1 "
            "--healthcheck-timeout 60",
            check=False,
            timeout_sec=180.0,
        )
        assert cp.returncode == 0, (
            f"quickstart failed (rc={cp.returncode}).\n"
            f"--- stdout ---\n{cp.stdout.decode(errors='replace')}\n"
            f"--- stderr ---\n{cp.stderr.decode(errors='replace')}"
        )

    def test_03_daemon_user_service_active(self):
        cp = _shrun(
            self.node,
            "systemctl --user is-active corvus.service",
            check=False,
        )
        assert cp.returncode == 0 and b"active" in cp.stdout, (
            f"corvus.service (user) not active.\n"
            f"stdout: {cp.stdout.decode().strip()!r} "
            f"stderr: {cp.stderr.decode().strip()!r}"
        )

    def test_04_nodeagent_user_service_active(self):
        cp = _shrun(
            self.node,
            "systemctl --user is-active corvus-nodeagent.service",
            check=False,
        )
        assert cp.returncode == 0 and b"active" in cp.stdout, (
            f"corvus-nodeagent.service (user) not active.\n"
            f"stdout: {cp.stdout.decode().strip()!r} "
            f"stderr: {cp.stderr.decode().strip()!r}"
        )

    def test_05_netd_system_service_active(self):
        cp = _shrun(
            self.node,
            "systemctl is-active corvus-netd.service",
            check=False,
        )
        assert cp.returncode == 0 and b"active" in cp.stdout, (
            f"corvus-netd.service (system) not active.\n"
            f"stdout: {cp.stdout.decode().strip()!r} "
            f"stderr: {cp.stderr.decode().strip()!r}"
        )

    def test_06_crv_status_works(self):
        """The CLI talks to the user-mode daemon via its Unix
        socket (no TCP listener on a quickstart install)."""

        cp = _shrun(self.node, "crv status", check=False)
        assert cp.returncode == 0, (
            f"`crv status` failed (rc={cp.returncode}).\n"
            f"stdout: {cp.stdout.decode(errors='replace')}\n"
            f"stderr: {cp.stderr.decode(errors='replace')}"
        )
        # Default output includes a version line; be loose to
        # tolerate cosmetic changes.
        assert cp.stdout, "crv status returned empty stdout"

    def test_07_python_client_works_via_unix_socket(self):
        """``corvus_client.Client`` connects over the same socket."""

        snippet = (
            "from corvus_client import Client; "
            "import os, json; "
            "sock = os.path.join(os.environ['XDG_RUNTIME_DIR'], 'corvus', 'corvus.sock'); "
            "c = Client(unix_socket=sock); "
            "info = c.status(); "
            "print(json.dumps({'protocol_version': info.protocol_version})); "
            "c.close()"
        )
        cp = _shrun(self.node, f"python3 -c {shlex.quote(snippet)}", check=False)
        assert cp.returncode == 0, (
            f"corvus_client status call failed (rc={cp.returncode}).\n"
            f"stdout: {cp.stdout.decode(errors='replace')}\n"
            f"stderr: {cp.stderr.decode(errors='replace')}"
        )
        out = cp.stdout.decode().strip()
        assert "protocol_version" in out, f"unexpected output: {out!r}"

    def test_08_quickstart_registered_self_node(self):
        """``corvus-admin quickstart`` should have run ``crv node add``
        for the self node — verify the daemon has a row for it."""

        cp = _shrun(self.node, "crv node list", check=False)
        assert cp.returncode == 0, (
            f"`crv node list` failed: {cp.stderr.decode(errors='replace')}"
        )
        output = cp.stdout.decode()
        assert "self" in output, (
            f"node 'self' not registered. crv node list output:\n{output}"
        )


# ---------------------------------------------------------------------------
# Bootstrap


def _bootstrap_node(node):
    """One-time per-class prep: enable linger, ensure pip, install
    corvus from the mounted source tree.

    Idempotent — re-running over a leaked node is safe.
    """
    sys.stderr.write("[quickstart-test] enabling linger for corvus user\n")
    sys.stderr.flush()
    # loginctl needs root.
    _run(node, "sudo loginctl enable-linger corvus", user=NODE_USER)
    # Wait for the user manager to come up (linger starts it async).
    deadline = time.monotonic() + 30.0
    while time.monotonic() < deadline:
        cp = _run(
            node,
            "test -S /run/user/$(id -u corvus)/bus",
            user=NODE_USER,
            check=False,
        )
        if cp.returncode == 0:
            break
        time.sleep(0.5)
    else:
        raise RuntimeError(
            "user-systemd bus did not appear within 30s of enable-linger"
        )

    sys.stderr.write("[quickstart-test] verifying /mnt/corvus mount\n")
    sys.stderr.flush()
    cp = _run(node, f"test -d {SRC_MOUNT}/python/corvus_admin", check=False)
    if cp.returncode != 0:
        raise RuntimeError(
            f"source tree not visible at {SRC_MOUNT} — "
            "did Topology(attach_source=True) succeed?"
        )

    sys.stderr.write("[quickstart-test] installing corvus into ~corvus/.local\n")
    sys.stderr.flush()
    # The virtiofs mount is read-only, but setuptools' egg_info step
    # wants to write `corvus.egg-info/` inside the source dir. Copy
    # to a writable scratch dir first. --break-system-packages is
    # required so PEP 668 doesn't refuse a user-site install on
    # distros that ship the EXTERNALLY-MANAGED marker.
    _shrun(
        node,
        (
            "rm -rf /tmp/corvus-src && "
            f"cp -r {SRC_MOUNT} /tmp/corvus-src && "
            "python3 -m pip install --user --quiet --break-system-packages /tmp/corvus-src"
        ),
        timeout_sec=300.0,
    )
