"""Shared pytest fixtures for the corvus-admin suite."""

from __future__ import annotations

import os

import pytest
from corvus_admin import deploy, privesc, store
from corvus_admin import systemd as systemd_mod


@pytest.fixture(autouse=True)
def reset_privesc_cache():
    """privesc.detect() caches its result for the lifetime of the
    process. Tests that monkey-patch $PATH or shutil.which need a
    clean slate. Clearing before AND after each test is cheap and
    avoids order-dependent failures."""

    privesc.reset_cache()
    yield
    privesc.reset_cache()


@pytest.fixture()
def admin_store(tmp_path):
    """A fresh AdminStore rooted at a per-test tmpdir. The CA has
    NOT been initialised — call `ca.init_ca(...)` in the test
    body when you want one."""

    return store.AdminStore(tmp_path / "admin")


@pytest.fixture()
def xdg_home(tmp_path, monkeypatch):
    """Pin $XDG_CONFIG_HOME to a per-test dir so client-cert
    deploy doesn't touch the developer's real ~/.config."""

    target = tmp_path / "xdg-config"
    target.mkdir()
    monkeypatch.setenv("XDG_CONFIG_HOME", str(target))
    return target


@pytest.fixture()
def fake_paths(tmp_path, monkeypatch):
    """Redirect the deploy module's system paths into tmp_path and
    stub out systemctl + sudo. Returns ``(cert_dir, systemctl_log)``
    so individual tests can assert on the recorded systemctl argv
    or peek at the deployed cert files.

    Covers four redirections:

    * ``deploy.SYSTEM_CERT_DIR`` → ``<tmp>/etc-corvus``
    * ``systemd_mod.SYSTEMD_SYSTEM_DIR`` → ``<tmp>/etc-systemd``
    * ``systemd_mod.SYSTEMD_USER_DIR`` → ``<tmp>/user-systemd``
    * ``$PATH`` prepends a fake-bin dir containing pass-through
      ``sudo`` and a ``systemctl`` that logs its argv.
    """

    etc = tmp_path / "etc-corvus"
    monkeypatch.setattr(deploy, "SYSTEM_CERT_DIR", str(etc))
    monkeypatch.setattr(
        deploy, "SYSTEM_DATABASE_PATH", str(tmp_path / "var-lib-corvus" / "corvus.db")
    )
    monkeypatch.setattr(
        systemd_mod, "SYSTEMD_SYSTEM_DIR", str(tmp_path / "etc-systemd")
    )
    monkeypatch.setattr(systemd_mod, "SYSTEMD_USER_DIR", str(tmp_path / "user-systemd"))

    bin_dir = tmp_path / "bin"
    bin_dir.mkdir()
    log = bin_dir / "systemctl.log"
    sysctl = bin_dir / "systemctl"
    sysctl.write_text(f'#!/bin/sh\necho "$@" >> {log!s}\n')
    sysctl.chmod(0o755)
    # Pass-through sudo: the deploy code asks for privesc on system
    # paths, but tmp_path is writable so we don't need real
    # escalation — exec the rest of the args as-is.
    sudo = bin_dir / "sudo"
    sudo.write_text('#!/bin/sh\nexec "$@"\n')
    sudo.chmod(0o755)
    # Fake corvus binaries so LocalRunner.which (used by deploy to
    # bake an absolute ExecStart= into the rendered unit) returns
    # a deterministic path regardless of the developer's $PATH.
    for binary in ("corvus", "corvus-nodeagent", "corvus-netd", "corvus-web"):
        stub = bin_dir / binary
        stub.write_text("#!/bin/sh\nexit 0\n")
        stub.chmod(0o755)
    # Put the fake bin dir FIRST on PATH so shutil.which picks the
    # stubs ahead of any real installs in ~/.local/bin or /usr/bin.
    monkeypatch.setenv("PATH", f"{bin_dir}:{os.environ['PATH']}")
    return etc, log
