"""CA + cert minting tests."""

from __future__ import annotations

import datetime as dt
import ipaddress
import stat

import pytest
from cryptography import x509
from cryptography.hazmat.primitives import serialization

from corvus_admin import ca


def test_init_ca_creates_cert_and_key(admin_store):
    ca.init_ca(admin_store)
    assert admin_store.ca_cert_path.is_file()
    assert admin_store.ca_key_path.is_file()
    # CA private key MUST be mode 0600 (other bits unreadable).
    mode = admin_store.ca_key_path.stat().st_mode & 0o777
    assert mode == 0o600, f"expected 0600, got {oct(mode)}"


def test_init_ca_is_idempotent_without_force(admin_store):
    ca.init_ca(admin_store)
    with pytest.raises(FileExistsError):
        ca.init_ca(admin_store)


def test_init_ca_overwrites_when_force(admin_store):
    ca.init_ca(admin_store)
    first = admin_store.ca_cert_path.read_bytes()
    ca.init_ca(admin_store, force=True)
    second = admin_store.ca_cert_path.read_bytes()
    assert first != second


def test_issued_cert_has_expected_cn_and_san(admin_store):
    ca.init_ca(admin_store)
    issued = ca.issue_cert(
        admin_store,
        role=ca.ROLE_DAEMON,
        name="abc-123",
        ip="10.0.0.10",
    )
    assert issued.cn == "corvus-daemon:abc-123"
    cert = x509.load_pem_x509_certificate(issued.cert_pem)
    cn = cert.subject.get_attributes_for_oid(x509.NameOID.COMMON_NAME)[0].value
    assert cn == "corvus-daemon:abc-123"
    san = cert.extensions.get_extension_for_class(
        x509.SubjectAlternativeName
    ).value
    ips = list(san.get_values_for_type(x509.IPAddress))
    assert ipaddress.ip_address("10.0.0.10") in ips


def test_issued_cert_validates_against_the_ca(admin_store):
    ca.init_ca(admin_store)
    issued = ca.issue_cert(
        admin_store,
        role=ca.ROLE_NODE,
        name="alpha",
        ip="10.0.0.21",
    )
    cert = x509.load_pem_x509_certificate(issued.cert_pem)
    ca_cert = x509.load_pem_x509_certificate(admin_store.ca_cert_path.read_bytes())
    # The cert's signature is verifiable against the CA's public
    # key. For Ed25519 we use the bytes interface and let the
    # algorithm pick itself based on the cert's signing algorithm.
    ca_cert.public_key().verify(
        cert.signature,
        cert.tbs_certificate_bytes,
    )


def test_issued_cert_records_get_indexed(admin_store):
    ca.init_ca(admin_store)
    ca.issue_cert(admin_store, role=ca.ROLE_NODE, name="alpha", ip="10.0.0.21")
    ca.issue_cert(admin_store, role=ca.ROLE_NETD, name="alpha", ip="10.0.0.21")
    cns = {r.cn for r in admin_store.iter_records()}
    assert cns == {"corvus-node:alpha", "corvus-netd:alpha"}


def test_serial_increments_monotonically(admin_store):
    ca.init_ca(admin_store)
    r1 = ca.issue_cert(admin_store, role=ca.ROLE_NODE, name="a", ip=None)
    r2 = ca.issue_cert(admin_store, role=ca.ROLE_NODE, name="b", ip=None)
    r3 = ca.issue_cert(admin_store, role=ca.ROLE_NODE, name="c", ip=None)
    assert r2.record.serial == r1.record.serial + 1
    assert r3.record.serial == r2.record.serial + 1


def test_issue_cert_rejects_bad_role(admin_store):
    ca.init_ca(admin_store)
    with pytest.raises(ValueError):
        ca.issue_cert(admin_store, role="corvus-nope", name="x", ip=None)


def test_issue_cert_rejects_colon_in_name(admin_store):
    ca.init_ca(admin_store)
    with pytest.raises(ValueError):
        ca.issue_cert(admin_store, role=ca.ROLE_NODE, name="a:b", ip=None)


def test_cert_expiry_uses_default_lifetime(admin_store):
    ca.init_ca(admin_store)
    issued = ca.issue_cert(
        admin_store, role=ca.ROLE_CLIENT, name="alice", ip=None
    )
    cert = x509.load_pem_x509_certificate(issued.cert_pem)
    # Default lifetime is 365 days; allow a generous slop for the
    # wall-clock between issue and the load above.
    nb = cert.not_valid_before_utc
    na = cert.not_valid_after_utc
    delta = na - nb
    assert dt.timedelta(days=364) <= delta <= dt.timedelta(days=366)


def test_atomic_write_leaves_no_tmp_file(admin_store, monkeypatch):
    """Smoke test that a normal init doesn't leave a *.tmp turd."""
    ca.init_ca(admin_store)
    tmp_keys = list(admin_store.root.glob("*.tmp"))
    assert tmp_keys == []
