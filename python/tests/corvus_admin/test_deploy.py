"""Deploy-recipe tests.

We use the LocalRunner but redirect every system path to a
per-test tmpdir, and replace ``systemctl`` with a no-op stub so
the tests don't need real systemd. The end-to-end behaviour with
a real runner is covered by the smoke step on a corvus-test-node
VM (Phase 2 verification).
"""

from __future__ import annotations

import os

import pytest
from corvus_admin import ca, deploy
from corvus_admin.runner import LocalRunner
from cryptography import x509


@pytest.fixture()
def initialised_store(admin_store):
    ca.init_ca(admin_store)
    return admin_store


@pytest.fixture()
def fake_paths(tmp_path, monkeypatch):
    """Redirect SYSTEM_CERT_DIR to tmp_path/etc-corvus and stub
    systemctl with a script that just logs its argv."""

    etc = tmp_path / "etc-corvus"
    monkeypatch.setattr(deploy, "SYSTEM_CERT_DIR", str(etc))

    # Fake systemctl: write its argv to a log file and exit 0.
    bin_dir = tmp_path / "bin"
    bin_dir.mkdir()
    log = bin_dir / "systemctl.log"
    sysctl = bin_dir / "systemctl"
    sysctl.write_text(f'#!/bin/sh\necho "$@" >> {log!s}\n')
    sysctl.chmod(0o755)
    sudo = bin_dir / "sudo"
    # Make `sudo …` a passthrough — the runner no longer passes
    # ``-n`` (so the operator can be prompted for a password); the
    # fake passes the args through as-is.
    sudo.write_text('#!/bin/sh\nexec "$@"\n')
    sudo.chmod(0o755)
    monkeypatch.setenv("PATH", f"{bin_dir}:{os.environ['PATH']}")
    return etc, log


def test_deploy_daemon_drops_three_files_with_right_modes(
    initialised_store, fake_paths
):
    etc, _ = fake_paths
    runner = LocalRunner()
    plan = deploy.deploy_daemon(
        initialised_store,
        runner,
        listen_ip="127.0.0.1",
    )

    ca_pem = (etc / "ca.crt").read_bytes()
    daemon_pem = (etc / "corvus-daemon.crt").read_bytes()
    daemon_key = (etc / "corvus-daemon.key").read_bytes()

    # CA pem in the deployed file matches the admin store's CA.
    assert ca_pem == initialised_store.ca_cert_path.read_bytes()
    # Daemon cert CN starts with corvus-daemon: and ends with the
    # UUID the plan picked.
    cert = x509.load_pem_x509_certificate(daemon_pem)
    cn = cert.subject.get_attributes_for_oid(x509.NameOID.COMMON_NAME)[0].value
    assert cn == f"corvus-daemon:{plan.name}"
    # Key file is mode 0600.
    assert (etc / "corvus-daemon.key").stat().st_mode & 0o777 == 0o600
    # Cert files are mode 0644.
    assert (etc / "corvus-daemon.crt").stat().st_mode & 0o777 == 0o644
    assert (etc / "ca.crt").stat().st_mode & 0o777 == 0o644
    # Sanity: the key parses as a private key.
    from cryptography.hazmat.primitives import serialization

    _ = serialization.load_pem_private_key(daemon_key, password=None)


def test_deploy_daemon_invokes_systemctl_restart(initialised_store, fake_paths):
    _, log = fake_paths
    runner = LocalRunner()
    deploy.deploy_daemon(initialised_store, runner, listen_ip="127.0.0.1")
    lines = log.read_text().splitlines()
    # Expect at least an `enable --now corvus.service` then `restart corvus.service`.
    assert any(line == "enable --now corvus.service" for line in lines), lines
    assert any(line == "restart corvus.service" for line in lines), lines


def test_deploy_node_records_deployment_target(initialised_store, fake_paths):
    runner = LocalRunner()
    plan = deploy.deploy_node(
        initialised_store,
        runner,
        name="alpha",
        ip="10.0.0.21",
    )
    rec = next(
        r for r in initialised_store.iter_records() if r.cn == "corvus-node:alpha"
    )
    assert rec.deployed_to == "local"
    assert plan.service_unit == "corvus-nodeagent.service"


def test_deploy_client_lands_under_xdg_config_home(
    initialised_store, fake_paths, xdg_home
):
    deploy.deploy_client(initialised_store, name="alice")
    client_dir = xdg_home / "corvus"
    assert (client_dir / "ca.crt").is_file()
    assert (client_dir / "corvus-client.crt").is_file()
    assert (client_dir / "corvus-client.key").is_file()
    mode = (client_dir / "corvus-client.key").stat().st_mode & 0o777
    assert mode == 0o600


def test_deploy_client_does_not_invoke_systemctl(
    initialised_store, fake_paths, xdg_home
):
    _, log = fake_paths
    deploy.deploy_client(initialised_store, name="alice")
    # Client deploy is purely local; systemctl should never run.
    assert not log.exists() or log.read_text() == ""
