"""Tests for the renew code path. Uses the shared
``fake_paths`` fixture from ``conftest.py``."""

from __future__ import annotations

import datetime as dt

import pytest
from corvus_admin import ca, deploy, store
from corvus_admin.runner import LocalRunner


@pytest.fixture()
def initialised_store(admin_store):
    ca.init_ca(admin_store)
    return admin_store


def test_renew_daemon_reuses_existing_uuid(initialised_store, fake_paths):
    """After a deploy + renew, the daemon CN UUID must be stable."""

    runner = LocalRunner()
    first = deploy.deploy_daemon(initialised_store, runner, listen_ip="127.0.0.1")
    second = deploy.renew_daemon(initialised_store, target="local", force=True)
    assert first.name == second.name, (
        "renew must reuse the daemon UUID — otherwise every renewal "
        "orphans the daemon's identity"
    )


def test_renew_node_uses_stored_ip(initialised_store, fake_paths):
    """renew_node picks the IP up from the issued record so the
    operator doesn't have to remember it."""

    runner = LocalRunner()
    deploy.deploy_node(initialised_store, runner, name="alpha", ip="10.0.0.21")
    deploy.renew_node(initialised_store, name="alpha", target="local", force=True)

    rec = next(
        r for r in initialised_store.iter_records() if r.cn == "corvus-node:alpha"
    )
    assert rec.ip == "10.0.0.21"


def test_renew_refuses_when_not_due(initialised_store, fake_paths):
    """A freshly-minted cert isn't due for renewal; the default
    refusal must surface as RenewError (and as a non-zero exit
    from the CLI, exercised in test_cli)."""

    runner = LocalRunner()
    deploy.deploy_daemon(initialised_store, runner, listen_ip="127.0.0.1")
    with pytest.raises(deploy.RenewError):
        deploy.renew_daemon(initialised_store, target="local")


def test_renew_proceeds_when_force(initialised_store, fake_paths):
    runner = LocalRunner()
    deploy.deploy_daemon(initialised_store, runner, listen_ip="127.0.0.1")
    # force=True bypasses the not-due-yet check.
    plan = deploy.renew_daemon(initialised_store, target="local", force=True)
    assert plan.role == ca.ROLE_DAEMON


def test_needs_renewal_within_window(initialised_store):
    rec = store.IssuedRecord(
        cn="corvus-daemon:abc",
        role=ca.ROLE_DAEMON,
        name_or_uuid="abc",
        issued_at="2026-01-01T00:00:00+00:00",
        expires_at="2026-06-01T00:00:00+00:00",
        serial=1,
    )
    # 100 days before expiry → not yet due.
    early = dt.datetime(2026, 2, 20, tzinfo=dt.timezone.utc)
    assert deploy.needs_renewal(rec, now=early) is False
    # 10 days before expiry → due.
    late = dt.datetime(2026, 5, 22, tzinfo=dt.timezone.utc)
    assert deploy.needs_renewal(rec, now=late) is True


def test_runner_label_to_target_round_trips():
    assert deploy.runner_label_to_target("local") == "local"
    assert deploy.runner_label_to_target("ssh:root@10.0.0.1") == "root@10.0.0.1"
    # Client deploys store something like "local:/path/..." in
    # deployed_to; renew shouldn't re-run that recipe through the
    # runner factory.
    with pytest.raises(deploy.RenewError):
        deploy.runner_label_to_target("local:/foo")


def test_find_record_raises_when_missing(initialised_store):
    with pytest.raises(deploy.RenewError):
        deploy.find_record(initialised_store, role=ca.ROLE_DAEMON)


def test_renew_client_remints_into_xdg(initialised_store, fake_paths, xdg_home):
    """The admin's local client cert deploy path is reused by
    renew_client; the XDG file is overwritten with the new
    material."""

    first_rec = deploy.deploy_client(initialised_store, name="alice")
    first_cert = (xdg_home / "corvus" / "corvus-client.crt").read_bytes()
    deploy.renew_client(initialised_store, name="alice", force=True)
    second_cert = (xdg_home / "corvus" / "corvus-client.crt").read_bytes()
    assert first_cert != second_cert
    # The index still has the same CN — overwrite, not append.
    cns = [r.cn for r in initialised_store.iter_records()]
    assert cns.count(first_rec.cn) == 1
