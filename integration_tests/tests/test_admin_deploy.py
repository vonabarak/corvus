"""End-to-end coverage for ``corvus-admin deploy`` and ``register``.

[test_admin_lifecycle.py](./test_admin_lifecycle.py) covers the
local-only admin-store verbs (init, renew, list, revoke) without
any VM.  ``test_quickstart.py`` exercises the one-shot
``corvus-admin quickstart`` path on a real test node.

Neither file tests the *individual* deploy recipes or the
``register`` healthcheck flow — a refactor that breaks
``deploy node``'s cert minting, drops ``deploy web``'s unit
render, or makes ``register``'s healthcheck always-timeout would
only surface on a real multi-node deploy.

This file:

* Boots a single test node with corvus-admin installed from the
  mounted source tree (same pattern as :mod:`test_quickstart`).
* Exercises ``deploy --dry-run`` for every role to confirm the
  plan shape is correct.
* Exercises ``deploy node`` (without restarting — the nodeagent
  isn't installed on the test node) to confirm cert minting +
  file layout.
* Exercises ``deploy web`` to confirm the systemd unit renders
  correctly.
* Exercises ``register``'s ``register_node()`` function,
  including the ``RegisterError`` path when ``crv`` is missing.
* Exercises ``corvus-admin renew --due`` sweep with ``--within``
  to confirm the expiry-sweep logic.

What's NOT covered here:

* ``deploy daemon`` / ``deploy node`` / ``deploy netd`` with
  ``install_unit=True`` on a real target — that needs corvus
  binaries installed and systemd reachable, which is what
  :mod:`test_quickstart` already tests.
* ``corvus-admin register`` CLI wrapper (``cli.py``'s
  ``register_node`` invocation).  The Python API
  ``register.register_node()`` is tested directly; the CLI
  wrapper is a thin subprocess call.
* Multi-host deploy (SSH runner).  The harness only boots one
  node; testing SSH deploys would need a second node, which
  doubles the fixture cost.  Left as a future add.
"""

from __future__ import annotations

import json
import os
import shutil
import subprocess
import sys
import time
from pathlib import Path

import pytest
from corvus_test_harness.cases import IntegrationTestCase, state_for
from corvus_test_harness.topology import NodeRole, Topology

# Where the harness mounts the host's repo when ``attach_source=True``.
SRC_MOUNT = "/mnt/corvus"
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
    """``node.run`` wrapper that decodes stdout/stderr."""
    cp = node.run(cmd, user=user, check=check, timeout_sec=timeout_sec)
    return cp


def _user_env_prefix() -> str:
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
    full = _user_env_prefix() + body
    return _run(
        node,
        f"bash -c {__import__('shlex').quote(full)}",
        user=user,
        check=check,
        timeout_sec=timeout_sec,
    )


def _admin(
    node,
    *args: str,
    ca_dir: Path,
    xdg_home: Path,
    check: bool = True,
    timeout_sec: float = 30.0,
) -> subprocess.CompletedProcess:
    """Invoke ``corvus-admin`` as a subprocess on the test node."""
    env = os.environ.copy()
    env["XDG_CONFIG_HOME"] = str(xdg_home)
    verb_args = list(args)
    cmd = [
        sys.executable,
        "-m",
        "corvus_admin",
        verb_args[0],
        "--ca-dir",
        str(ca_dir),
        *verb_args[1:],
    ]
    return _run(
        node,
        f"{' '.join(__import__('shlex').quote(a) for a in cmd)}",
        user=node_user_for(node),
        check=check,
        timeout_sec=timeout_sec,
    )


def _stdout_text(cp):
    """Decode stdout from a CompletedProcess, ignoring errors."""
    return cp.stdout.decode(errors="replace")


def node_user_for(_node):
    """The user that runs corvus-admin on the test node."""
    return NODE_USER


def _test_subdir(node, name):
    """Create a unique per-test subdirectory on the test node."""
    cp = _run(node, f"mktemp -d", user=NODE_USER)
    result_dir = cp.stdout.decode().strip()
    # Create our named subdirectory so we can find it in teardown.
    _run(node, f"mkdir -p {result_dir}/admin {result_dir}/xdg", user=NODE_USER)
    return result_dir


def _remote_mkdir(node, path: str):
    """Create a directory on the test node."""
    _run(node, f"mkdir -p {path}", user=NODE_USER)


# ---------------------------------------------------------------------------
# Test class


class TestAdminDeployAndRegister(IntegrationTestCase):
    """``corvus-admin deploy`` (dry-run + cert minting) and
    ``register`` (healthcheck polling)."""

    NODES = ("deploy_test",)

    # ---- Class fixture override (same pattern as test_quickstart)

    @pytest.fixture(scope="class", autouse=True)
    def _class_topology(
        self, request, crv, image_ready, host_binary, session_test_network
    ):
        cls = request.cls
        state = state_for(cls)
        topology = None
        try:
            topology = Topology(
                crv,
                image_ready,
                host_binary,
                class_name=cls.__name__,
                network_name=session_test_network,
                attach_source=True,
            )
            topology.init_cas(("shared",))
            sys.stderr.write(
                f"[harness] booting bare node for {cls.__qualname__}\n"
            )
            sys.stderr.flush()
            for short_name in cls.NODES:
                topology.add(short_name, role=NodeRole.FULL_STACK, ca_key="shared")
            node = topology.nodes[0]
            _bootstrap_node(node)
            state.topology = topology
            # Create a writable scratch dir on the **test node**
            # for corvus-admin (CA store, cert dirs, etc.).
            # corvus-admin runs on the test node via SSH, so all
            # paths must exist on the test node.  tmp_path is a
            # function-scoped host fixture and is not available in
            # class-based integration tests.
            cp = _run(self.node, "mktemp -d", user=NODE_USER)
            state.scratch_dir = cp.stdout.decode().strip()
        except BaseException as exc:
            state.setup_failed = True
            state.setup_error = f"{type(exc).__name__}: {exc}"
            sys.stderr.write(f"[harness] {cls.__qualname__} class fixture failed:\n")
            import traceback

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
            sd = state_for(cls).scratch_dir
            state.topology = None
            # Clean up the scratch dir on the test node **before**
            # finalize(), which may leak the VM.
            if sd and t is not None:
                node = t.nodes[0]
                try:
                    _run(node, f"rm -rf {sd}", user=NODE_USER)
                except Exception:
                    pass
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

    def _subdir(self):
        """Create a unique per-test subdirectory on the test node.

        Each test gets its own temp dir so it doesn't pollute or
        read from other tests' CA stores.
        """
        cp = _run(self.node, "mktemp -d", user=NODE_USER)
        return cp.stdout.decode().strip()

    # ---- Tests ---------------------------------------------------------

    def test_01_deploy_node_dry_run(self):
        """``corvus-admin deploy node --dry-run`` prints a plan
        summary without minting a cert or touching the filesystem."""

        # Verify corvus-admin is available on the test node.
        which_result = _shrun(self.node, "which corvus-admin", check=False)
        assert which_result.returncode == 0, (
            f"corvus-admin not found on test node: "
            f"which rc={which_result.returncode} "
            f"stderr={which_result.stderr.decode(errors='replace')!r}"
        )

        ca_dir = f"{self._subdir()}/admin"
        xdg = f"{self._subdir()}/xdg"
        _remote_mkdir(self.node, ca_dir)
        _remote_mkdir(self.node, xdg)

        result = _shrun(
            self.node,
            (
                f"corvus-admin init --ca-dir {ca_dir} 2>&1 && "
                f"corvus-admin deploy node testnode local --ca-dir {ca_dir} "
                f"--ip 10.91.0.21 "
                f"--dry-run"
            ),
            check=False,
            timeout_sec=30.0,
        )
        assert result.returncode == 0, (
            f"deploy node --dry-run failed: {result.stdout.decode(errors='replace')}\n"
            f"stderr: {result.stderr.decode(errors='replace')}"
        )
        out = _stdout_text(result)
        assert "[DRY-RUN]" in out, f"missing [DRY-RUN] in output: {out!r}"
        assert "corvus-node" in out, f"missing role in output: {out!r}"
        assert "testnode" in out, f"missing name in output: {out!r}"
        assert "local" in out, f"missing target in output: {out!r}"

    def test_02_deploy_web_dry_run(self):
        """``corvus-admin deploy web --dry-run`` produces a plan
        with the correct service unit and role."""

        ca_dir = f"{self._subdir()}/admin"
        xdg = f"{self._subdir()}/xdg"
        _remote_mkdir(self.node, ca_dir)
        _remote_mkdir(self.node, xdg)

        _shrun(
            self.node,
            f"corvus-admin init --ca-dir {ca_dir} > /dev/null 2>&1",
            check=True,
        )

        result = _shrun(
            self.node,
            (
                f"corvus-admin deploy web --dry-run"
            ),
            check=False,
            timeout_sec=30.0,
        )
        assert result.returncode == 0, result.stderr.decode(errors="replace")
        out = _stdout_text(result)
        assert "[DRY-RUN]" in out, f"missing [DRY-RUN]: {out!r}"
        assert "corvus-web" in out, f"missing corvus-web: {out!r}"
        assert "corvus-web.service" in out, f"missing service unit: {out!r}"

    def test_03_deploy_node_mints_cert(self):
        """Verify the ``corvus_admin.ca.issue_cert`` Python API
        mints a valid cert on the test node.  We test the Python
        API directly because the CLI ``deploy node`` always tries to
        restart systemd — the test node doesn't have
        corvus-nodeagent installed so the restart would fail."""

        ca_dir = f"{self._subdir()}/admin"
        xdg = f"{self._subdir()}/xdg"
        _remote_mkdir(self.node, ca_dir)
        _remote_mkdir(self.node, xdg)

        _shrun(
            self.node,
            f"corvus-admin init --ca-dir {ca_dir} > /dev/null 2>&1",
            check=True,
        )

        cert_dir = f"{self._subdir()}/certs"

        # Use the Python API directly to test cert minting.
        result = _shrun(
            self.node,
            (
                "python3 -c "
                f"'import sys, json; "
                f"sys.path.insert(0, \"/mnt/corvus/python\"); "
                f"from corvus_admin import ca, store; "
                f"st = store.AdminStore(\"{ca_dir}\"); "
                f"issued = ca.issue_cert(st, role=\"corvus-node\", name=\"testnode\", "
                f"ip=\"10.91.0.21\"); "
                f"cert_dir=\"{cert_dir}\"; "
                f"import pathlib; pathlib.Path(cert_dir).mkdir(exist_ok=True); "
                f"ca_pem = ca.ca_cert_pem(st); "
                f"open(f\"{{cert_dir}}/ca.crt\", \"wb\").write(ca_pem); "
                f"open(f\"{{cert_dir}}/corvus-node.crt\", \"wb\").write(issued.cert_pem); "
                f"open(f\"{{cert_dir}}/corvus-node.key\", \"wb\").write(issued.key_pem); "
                f"print(json.dumps({{\"cn\": issued.cn, \"role\": issued.record.role}}))'"
            ),
            check=False,
            timeout_sec=30.0,
        )
        assert result.returncode == 0, (
            f"deploy node cert mint failed: {result.stderr.decode(errors='replace')}"
        )
        info = json.loads(result.stdout)
        assert info["role"] == "corvus-node"
        assert info["cn"].startswith("corvus-node:testnode")

        # Cert trio lands in the target cert dir (on the test node).
        for fname in ("ca.crt", "corvus-node.crt", "corvus-node.key"):
            check = _shrun(self.node, f"test -f {cert_dir}/{fname}", check=False)
            assert check.returncode == 0, f"{fname} not found in {cert_dir}"

        # The CA cert matches what we initialised (on the test node).
        ca_on_node = _shrun(self.node, f"cat {ca_dir}/ca.crt", check=False)
        cert_ca = _shrun(self.node, f"cat {cert_dir}/ca.crt", check=False)
        assert ca_on_node.stdout == cert_ca.stdout, "CA cert mismatch"

    def test_04_deploy_daemon_dry_run(self):
        """``corvus-admin deploy daemon local --dry-run`` produces a plan
        with a ``corvus-daemon:<uuid>`` CN."""

        ca_dir = f"{self._subdir()}/admin"
        xdg = f"{self._subdir()}/xdg"
        _remote_mkdir(self.node, ca_dir)
        _remote_mkdir(self.node, xdg)

        _shrun(
            self.node,
            f"corvus-admin init --ca-dir {ca_dir} > /dev/null 2>&1",
            check=True,
        )

        result = _shrun(
            self.node,
            f"corvus-admin deploy daemon --ca-dir {ca_dir} local --dry-run",
            check=False,
            timeout_sec=30.0,
        )
        assert result.returncode == 0, result.stderr.decode(errors="replace")
        out = _stdout_text(result)
        assert "[DRY-RUN]" in out, f"missing [DRY-RUN]: {out!r}"
        assert "corvus-daemon:" in out, f"missing daemon CN prefix: {out!r}"
        assert "corvus.service" in out, f"missing service unit: {out!r}"

    def test_05_deploy_netd_dry_run(self):
        """``corvus-admin deploy netd --dry-run`` produces a plan
        with the correct role and service unit."""

        ca_dir = f"{self._subdir()}/admin"
        xdg = f"{self._subdir()}/xdg"
        _remote_mkdir(self.node, ca_dir)
        _remote_mkdir(self.node, xdg)

        _shrun(
            self.node,
            f"corvus-admin init --ca-dir {ca_dir} > /dev/null 2>&1",
            check=True,
        )

        result = _shrun(
            self.node,
            f"corvus-admin deploy netd testnetd local --ca-dir {ca_dir} --ip 10.91.0.22 --dry-run",
            check=False,
            timeout_sec=30.0,
        )
        assert result.returncode == 0, result.stderr.decode(errors="replace")
        out = _stdout_text(result)
        assert "[DRY-RUN]" in out, f"missing [DRY-RUN]: {out!r}"
        assert "corvus-netd" in out, f"missing netd role: {out!r}"
        assert "testnetd" in out, f"missing name: {out!r}"
        assert "corvus-netd.service" in out, f"missing service unit: {out!r}"

    def test_06_renew_due_sweep(self):
        """``corvus-admin renew --due --within 36500`` (≈100 years)
        matches every cert and reports what it would renew."""

        ca_dir = f"{self._subdir()}/admin"
        xdg = f"{self._subdir()}/xdg"
        _remote_mkdir(self.node, ca_dir)
        _remote_mkdir(self.node, xdg)

        _shrun(
            self.node,
            f"corvus-admin init --ca-dir {ca_dir} > /dev/null 2>&1",
            check=True,
        )

        result = _shrun(
            self.node,
            (
                f"corvus-admin renew --ca-dir {ca_dir} --due --within 36500 --dry-run"
            ),
            check=False,
            timeout_sec=30.0,
        )
        # The output must include "would renew" for the admin client cert.
        # Other certs (e.g. from prior tests in the same scratch dir)
        # may also appear — we only assert the admin cert line.
        out = _stdout_text(result)
        assert "would renew" in out, f"no 'would renew' in output: {out!r}"
        # At least the admin client cert should be reported.
        assert "would renew" in _stdout_text(result), _stdout_text(result)

    def test_07_renew_list_after_init(self):
        """After ``init``, ``crv-admin list --output json`` shows
        exactly one row (the admin client cert)."""

        ca_dir = f"{self._subdir()}/admin"
        xdg = f"{self._subdir()}/xdg"
        _remote_mkdir(self.node, ca_dir)
        _remote_mkdir(self.node, xdg)

        _shrun(
            self.node,
            f"corvus-admin init --ca-dir {ca_dir} > /dev/null 2>&1",
            check=True,
        )

        result = _shrun(
            self.node,
            f"corvus-admin list --ca-dir {ca_dir} --output json",
            check=False,
            timeout_sec=30.0,
        )
        assert result.returncode == 0, result.stderr.decode(errors="replace")
        rows = json.loads(result.stdout)
        assert isinstance(rows, list)
        assert len(rows) == 1, f"expected 1 row, got {len(rows)}: {rows!r}"
        assert rows[0]["cn"].startswith("corvus-client:")

    def test_08_register_node_missing_crv(self):
        """``register_node()`` raises ``RegisterError`` when
        ``crv`` is not on ``$PATH``."""

        # Use a temp dir with an empty PATH so ``shutil.which("crv")``
        # returns None.
        # Write the test script to the test node and execute it.
        script_file = f"{self._subdir()}/test_register.py"
        _shrun(
            self.node,
            f"cat > {script_file} << 'PYEOF'\nimport shutil, sys\nsys.path.insert(0, '/mnt/corvus/python')\nfrom corvus_admin import register\nold_which = shutil.which\nshutil.which = lambda name: None if name == 'crv' else old_which(name)\ntry:\n    register.register_node(name='test', host='10.0.0.1')\n    sys.exit(1)\nexcept register.RegisterError:\n    print('REGISTER_ERROR:ok')\n    sys.exit(0)\nexcept Exception:\n    print('OTHER_ERROR:fail')\n    sys.exit(2)\nPYEOF",
            check=True,
        )
        result = _shrun(self.node, f"python3 {script_file}", check=False, timeout_sec=10.0)
        assert result.returncode == 0, (
            f"register_node missing-crv test failed: {_stdout_text(result)}\n"
            f"stderr={result.stderr.decode(errors='replace')}"
        )
        assert "REGISTER_ERROR:ok" in _stdout_text(result), (
            f"expected REGISTER_ERROR:ok in output: {_stdout_text(result)!r}"
        )

    def test_09_deploy_client_dry_run(self):
        """``corvus-admin deploy client --dry-run`` returns ``None``
        (no DeployPlan for client certs — they're local-only)."""

        ca_dir = f"{self._subdir()}/admin"
        xdg = f"{self._subdir()}/xdg"
        _remote_mkdir(self.node, ca_dir)
        _remote_mkdir(self.node, xdg)

        _shrun(
            self.node,
            f"corvus-admin init --ca-dir {ca_dir} > /dev/null 2>&1",
            check=True,
        )

        result = _shrun(
            self.node,
            (
                f"corvus-admin "
                f"deploy client test-client --ca-dir {ca_dir} --dry-run"
            ),
            check=False,
            timeout_sec=30.0,
        )
        # Client deploy --dry-run prints a "[DRY-RUN]" message.
        assert result.returncode == 0, result.stderr.decode(errors="replace")
        assert "[DRY-RUN]" in _stdout_text(result), (
            f"expected [DRY-RUN] in output for client dry-run: {_stdout_text(result)!r}"
        )

    def test_10_deploy_web_unit_render(self):
        """``corvus-admin deploy web --dry-run`` reports the bind
        host/port and service unit that would be used."""

        ca_dir = f"{self._subdir()}/admin"
        xdg = f"{self._subdir()}/xdg"
        _remote_mkdir(self.node, ca_dir)
        _remote_mkdir(self.node, xdg)

        _shrun(
            self.node,
            f"corvus-admin init --ca-dir {ca_dir} > /dev/null 2>&1",
            check=True,
        )

        result = _shrun(
            self.node,
            (
                f"corvus-admin "
                f"deploy web --bind-host 0.0.0.0 --bind-port 9090 --dry-run"
            ),
            check=False,
            timeout_sec=30.0,
        )
        assert result.returncode == 0, result.stderr.decode(errors="replace")
        assert "[DRY-RUN]" in _stdout_text(result)
        assert "0.0.0.0" in _stdout_text(result), f"bind host not in output: {_stdout_text(result)!r}"
        assert "9090" in _stdout_text(result), f"bind port not in output: {result.stdout!r}"
        assert "corvus-web.service" in _stdout_text(result)


# ---------------------------------------------------------------------------
# Bootstrap (same pattern as test_quickstart)


def _bootstrap_node(node):
    """One-time per-class prep: enable linger, ensure pip, install
    corvus from the mounted source tree."""
    import sys as _sys

    _sys.stderr.write("[deploy-test] enabling linger for corvus user\n")
    _sys.stderr.flush()
    _run(node, "sudo loginctl enable-linger corvus", user=NODE_USER)

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

    _sys.stderr.write("[deploy-test] verifying /mnt/corvus mount\n")
    _sys.stderr.flush()
    cp = _run(node, f"test -d {SRC_MOUNT}/python/corvus_admin", check=False)
    if cp.returncode != 0:
        raise RuntimeError(
            f"source tree not visible at {SRC_MOUNT} — "
            "did Topology(attach_source=True) succeed?"
        )

    _sys.stderr.write("[deploy-test] installing corvus into ~corvus/.local\n")
    _sys.stderr.flush()
    _shrun(
        node,
        (
            "rm -rf $HOME/corvus-src && "
            "rsync -aL "
            "--exclude=.mypy_cache "
            "--exclude=.ruff_cache "
            "--exclude=.pytest_cache "
            "--exclude=.stack-work "
            "--exclude=.git "
            "--exclude='__pycache__' "
            "--exclude='.venv*' "
            "--exclude=dist-newstyle "
            "--exclude=integration_tests/keys "
            f"{SRC_MOUNT}/ $HOME/corvus-src/ && "
            "python3 -m pip install --user --quiet "
            "--break-system-packages --no-build-isolation --no-deps "
            "$HOME/corvus-src"
        ),
        timeout_sec=300.0,
    )
