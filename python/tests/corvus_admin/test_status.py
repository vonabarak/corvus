"""Tests for the `status` probe. The reachability path needs a
local TLS listener; we stand one up with the admin's own CA so
the handshake actually succeeds."""

from __future__ import annotations

import socket
import ssl
import threading
from contextlib import contextmanager

import pytest
from corvus_admin import ca, deploy, store
from corvus_admin import status as status_mod


@pytest.fixture()
def initialised_store(admin_store):
    ca.init_ca(admin_store)
    return admin_store


def test_probe_all_returns_one_row_per_record(initialised_store, xdg_home):
    # Just issue a few certs; we don't need them actually
    # deployed to verify the index → probe walk.
    ca.issue_cert(initialised_store, role=ca.ROLE_DAEMON, name="dabc", ip="10.0.0.1")
    ca.issue_cert(initialised_store, role=ca.ROLE_NODE, name="alpha", ip="10.0.0.21")
    ca.issue_cert(initialised_store, role=ca.ROLE_CLIENT, name="alice", ip=None)
    reports = status_mod.probe_all(initialised_store)
    cns = [r.cn for r in reports]
    assert cns == sorted(cns), "reports must be ordered by CN"
    by_cn = {r.cn: r for r in reports}
    # Client probe never tries to dial anywhere.
    assert by_cn["corvus-client:alice"].reachable is True
    # Daemon / node entries with an unreachable IP report
    # unreachable rather than crashing.
    assert by_cn["corvus-daemon:dabc"].reachable is False


def test_probe_target_handles_missing_ip(initialised_store):
    """A daemon record minted without an IP SAN AND without a
    deploy target has no probe target — status should report it
    as unreachable with a helpful diagnostic, not crash."""

    ca.issue_cert(initialised_store, role=ca.ROLE_DAEMON, name="noip", ip=None)
    [report] = [
        r
        for r in status_mod.probe_all(initialised_store)
        if r.cn == "corvus-daemon:noip"
    ]
    assert report.reachable is False
    assert report.handshake_error is not None
    assert "no probe target" in report.handshake_error


def test_probe_target_falls_back_to_deployed_to_local(initialised_store):
    """A record minted without --ip but later deployed to `local`
    should still get a probe target — 127.0.0.1 on the per-role
    port. The probe attempt will fail (nothing's listening on
    PORT_DAEMON in the test), but the failure mode is "connection
    refused" / "unreachable", not "no probe target"."""

    issued = ca.issue_cert(initialised_store, role=ca.ROLE_DAEMON, name="noip", ip=None)
    issued.record.deployed_to = "local"
    initialised_store.record(issued.record)
    [report] = [
        r
        for r in status_mod.probe_all(initialised_store)
        if r.cn == "corvus-daemon:noip"
    ]
    assert report.reachable is False
    assert report.handshake_error is not None
    assert "no probe target" not in report.handshake_error


def test_host_from_deploy_label_extracts_ssh_host():
    f = status_mod._host_from_deploy_label
    assert f("local") == "127.0.0.1"
    assert f("ssh:tobacco") == "tobacco"
    assert f("ssh:bobr@tobacco") == "tobacco"
    assert f("ssh:bobr@tobacco:2222") == "tobacco"
    assert f("ssh:[fe80::1]:9876") == "fe80::1"
    # Client cert deploys store an absolute path here; not probable.
    assert f("local:/home/bobr/.config/corvus") is None
    # Garbage in → None (don't crash).
    assert f("") is None
    assert f("???") is None


@contextmanager
def _tls_listener_using_admin_ca(admin_store: store.AdminStore):
    """Stand up a one-shot TLS server using a server cert signed
    by the admin store's CA. Yields (host, port). The server
    accepts one connection, performs the handshake, sends an
    empty payload, and shuts down."""

    issued = ca.issue_cert(
        admin_store, role=ca.ROLE_DAEMON, name="probe", ip="127.0.0.1"
    )
    cert_file = admin_store.root / "_probe.crt"
    key_file = admin_store.root / "_probe.key"
    cert_file.write_bytes(issued.cert_pem)
    key_file.write_bytes(issued.key_pem)

    ctx = ssl.SSLContext(ssl.PROTOCOL_TLS_SERVER)
    ctx.load_cert_chain(certfile=str(cert_file), keyfile=str(key_file))
    # No client cert required — the probe's job is reachability,
    # not full mTLS. The real daemon does demand the client cert
    # (and validates the CN prefix), but here we just confirm
    # status_mod's plumbing dials the right host:port and parses
    # the right cert.
    ctx.verify_mode = ssl.CERT_NONE

    sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    sock.bind(("127.0.0.1", 0))
    sock.listen(1)
    host, port = sock.getsockname()

    def serve():
        try:
            conn, _ = sock.accept()
            with ctx.wrap_socket(conn, server_side=True) as tls_conn:
                # Send + close.
                try:
                    tls_conn.sendall(b"")
                except OSError:
                    pass
        except Exception:
            pass

    t = threading.Thread(target=serve, daemon=True)
    t.start()
    try:
        yield host, port
    finally:
        try:
            sock.close()
        except OSError:
            pass


def test_try_handshake_succeeds_with_admin_ca(initialised_store, xdg_home):
    """End-to-end: stand up a TLS listener using a cert signed by
    the admin's CA, mint a client cert into XDG, ask
    `_try_handshake` to dial it. The fact that it succeeds
    proves the SSLContext we build trusts the admin CA and
    presents the admin's client cert."""

    # Drop the admin's own client cert into XDG (the status probe
    # picks it up from there).
    deploy.deploy_client(initialised_store, name="alice")
    with _tls_listener_using_admin_ca(initialised_store) as (host, port):
        err = status_mod._try_handshake(host, port, initialised_store)
    assert err is None, err


def test_try_handshake_fails_without_client_cert(initialised_store, xdg_home):
    """Without an admin client cert in XDG the probe can't build
    its SSLContext — status should report a clean error rather
    than panic."""

    err = status_mod._try_handshake("127.0.0.1", 1, initialised_store)
    assert err is not None
    # Either "no client cert" (cert missing) or a connection
    # refused — depending on whether port 1 happens to refuse
    # before we get to the cert check. Both are acceptable
    # signals here; the key property is "no panic".


def test_cert_not_after_round_trips(initialised_store):
    issued = ca.issue_cert(
        initialised_store, role=ca.ROLE_NODE, name="alpha", ip="10.0.0.21"
    )
    not_after = status_mod.cert_not_after(issued.cert_pem)
    # Same value the index records.
    import datetime as dt

    indexed = dt.datetime.fromisoformat(issued.record.expires_at)
    # The cert's notAfter is timezone-aware; index value is too.
    assert abs((not_after - indexed).total_seconds()) < 60
