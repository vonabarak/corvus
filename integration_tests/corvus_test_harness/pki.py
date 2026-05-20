"""Ephemeral CA + per-VM cert minting for the integration suite.

Kept self-contained on purpose: the suite is its own consumer of
the Corvus TLS protocol, so re-using `corvus-admin` would
introduce a circular dependency (the admin tool's own integration
test would need this exact CA-from-scratch path). The cryptography
library is the only dep — same one corvus-admin uses, with the
same Ed25519 + 1-year leaf / 1-day CA conventions baked in.

Lifecycle is per pytest **session**: one CA, many minted certs.
The harness builds the bundle once and hands per-VM cert trios
into ``topology.add`` so each TestNode gets a CN like
``corvus-node:<short-name>``. The host-side client cert is
``corvus-client:harness``.
"""

from __future__ import annotations

import datetime as dt
import ipaddress
import secrets
from dataclasses import dataclass

from cryptography import x509
from cryptography.hazmat.primitives import serialization
from cryptography.hazmat.primitives.asymmetric import ed25519
from cryptography.x509.oid import ExtendedKeyUsageOID, NameOID


# Filename / CN conventions — kept in lock-step with
# ``Corvus.Tls.roleFilename`` (Haskell) and
# ``corvus_admin.ca.ROLE_*`` (Python). Don't import either of
# those here; the harness needs to stay runnable without the
# corvus-admin install.
ROLE_DAEMON = "corvus-daemon"
ROLE_NODE = "corvus-node"
ROLE_NETD = "corvus-netd"
ROLE_CLIENT = "corvus-client"


@dataclass
class CertTrio:
    """The three PEM blobs a Corvus component needs at startup —
    the CA cert, the component's own cert, and its private key."""

    ca_pem: bytes
    cert_pem: bytes
    key_pem: bytes
    cn: str


@dataclass
class ClientBundle:
    """Cert trio for the host-side pycapnp client. Same shape as
    :class:`CertTrio`; named separately to make call sites
    explicit about which leg of mTLS they're holding."""

    ca_pem: bytes
    cert_pem: bytes
    key_pem: bytes
    cn: str


class HarnessPki:
    """Session-scoped ephemeral CA. Construct once; call
    :meth:`issue` per VM and :meth:`issue_client` per host-side
    client. The CA's private key never leaves this process.

    Certs have a generous 1-year lifetime so a test session can
    run as long as it likes without time-bomb flakiness; the CA
    itself lives 10 years to make repeated session runs against
    a long-lived state cache trivial.
    """

    def __init__(self, *, ca_uuid: str | None = None) -> None:
        self._ca_key = ed25519.Ed25519PrivateKey.generate()
        ca_uuid = ca_uuid or secrets.token_hex(8)
        ca_cn = f"corvus-ca:harness-{ca_uuid}"
        now = dt.datetime.now(dt.timezone.utc)
        subject = issuer = x509.Name(
            [x509.NameAttribute(NameOID.COMMON_NAME, ca_cn)]
        )
        self._ca_cert = (
            x509.CertificateBuilder()
            .subject_name(subject)
            .issuer_name(issuer)
            .public_key(self._ca_key.public_key())
            .serial_number(x509.random_serial_number())
            .not_valid_before(now)
            .not_valid_after(now + dt.timedelta(days=10 * 365))
            .add_extension(
                x509.BasicConstraints(ca=True, path_length=0),
                critical=True,
            )
            .add_extension(
                x509.KeyUsage(
                    digital_signature=True,
                    content_commitment=False,
                    key_encipherment=False,
                    data_encipherment=False,
                    key_agreement=False,
                    key_cert_sign=True,
                    crl_sign=True,
                    encipher_only=False,
                    decipher_only=False,
                ),
                critical=True,
            )
            .add_extension(
                x509.SubjectKeyIdentifier.from_public_key(self._ca_key.public_key()),
                critical=False,
            )
            .sign(private_key=self._ca_key, algorithm=None)
        )
        self._ca_pem = self._ca_cert.public_bytes(serialization.Encoding.PEM)

    # ------------------------------------------------------------------
    # Accessors

    @property
    def ca_pem(self) -> bytes:
        return self._ca_pem

    # ------------------------------------------------------------------
    # Component certs (server-side: daemon / node / netd)

    def issue(
        self,
        *,
        role: str,
        name: str,
        ip: str | None = None,
    ) -> CertTrio:
        """Mint a cert trio for the given role. ``role`` is one of
        the ``corvus-*`` constants in this module; ``name`` is
        the daemon UUID or node name. ``ip`` is added as an
        IPAddress SAN when set."""

        if role not in (ROLE_DAEMON, ROLE_NODE, ROLE_NETD, ROLE_CLIENT):
            raise ValueError(f"unknown role {role!r}")
        cn = f"{role}:{name}"
        cert_pem, key_pem = self._mint_leaf(cn=cn, ip=ip)
        return CertTrio(
            ca_pem=self._ca_pem,
            cert_pem=cert_pem,
            key_pem=key_pem,
            cn=cn,
        )

    def issue_client(self, name: str = "harness") -> ClientBundle:
        """Convenience for the host-side client cert."""

        trio = self.issue(role=ROLE_CLIENT, name=name, ip=None)
        return ClientBundle(
            ca_pem=trio.ca_pem,
            cert_pem=trio.cert_pem,
            key_pem=trio.key_pem,
            cn=trio.cn,
        )

    # ------------------------------------------------------------------
    # Internals

    def _mint_leaf(self, *, cn: str, ip: str | None) -> tuple[bytes, bytes]:
        leaf_key = ed25519.Ed25519PrivateKey.generate()
        now = dt.datetime.now(dt.timezone.utc)
        san: list[x509.GeneralName] = [x509.DNSName(cn.split(":", 1)[-1])]
        if ip is not None:
            san.append(x509.IPAddress(ipaddress.ip_address(ip)))
        cert = (
            x509.CertificateBuilder()
            .subject_name(
                x509.Name([x509.NameAttribute(NameOID.COMMON_NAME, cn)])
            )
            .issuer_name(self._ca_cert.subject)
            .public_key(leaf_key.public_key())
            .serial_number(x509.random_serial_number())
            .not_valid_before(now)
            .not_valid_after(now + dt.timedelta(days=365))
            .add_extension(
                x509.BasicConstraints(ca=False, path_length=None),
                critical=True,
            )
            .add_extension(
                x509.KeyUsage(
                    digital_signature=True,
                    content_commitment=False,
                    key_encipherment=False,
                    data_encipherment=False,
                    key_agreement=False,
                    key_cert_sign=False,
                    crl_sign=False,
                    encipher_only=False,
                    decipher_only=False,
                ),
                critical=True,
            )
            .add_extension(
                x509.ExtendedKeyUsage(
                    [
                        ExtendedKeyUsageOID.SERVER_AUTH,
                        ExtendedKeyUsageOID.CLIENT_AUTH,
                    ]
                ),
                critical=False,
            )
            .add_extension(
                x509.SubjectAlternativeName(san),
                critical=False,
            )
            .add_extension(
                x509.SubjectKeyIdentifier.from_public_key(leaf_key.public_key()),
                critical=False,
            )
            .add_extension(
                x509.AuthorityKeyIdentifier.from_issuer_public_key(
                    self._ca_cert.public_key()
                ),
                critical=False,
            )
            .sign(private_key=self._ca_key, algorithm=None)
        )
        return (
            cert.public_bytes(serialization.Encoding.PEM),
            leaf_key.private_bytes(
                encoding=serialization.Encoding.PEM,
                format=serialization.PrivateFormat.PKCS8,
                encryption_algorithm=serialization.NoEncryption(),
            ),
        )
