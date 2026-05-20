"""Certificate-authority operations.

Everything that touches X.509 / private keys lives here. The
public surface is small:

* :func:`init_ca` — generate a fresh CA (idempotent: refuses to
  overwrite an existing one unless ``force=True``).
* :func:`issue_cert` — sign a leaf cert for one of the four
  documented Corvus roles. Returns the cert + key PEM bytes and
  also persists the cert into the admin store's ``issued/`` dir
  and bumps the index.

Conventions baked in:

* Keys are Ed25519. Smaller (32-byte) and faster than RSA, and
  every TLS 1.3 stack we ship (Haskell's ``tls`` + OpenSSL on the
  agent / client side) supports it.
* The CN is exactly ``corvus-<role>:<name|uuid>`` (e.g.
  ``corvus-daemon:b18b3f...``, ``corvus-node:alpha``). The
  Haskell daemon validates the prefix on every handshake.
* Component certs (daemon / node / netd / client) live 365 days
  by default; the CA lives 10 years.
* Daemon, node, and netd certs carry the dial-time IP as an
  ``IPAddress`` SAN so the operator catches a moved host via a
  ``certHasNoMatchingURIs``-style failure rather than a silent
  identity swap. The client cert has no SAN — admins move
  workstations all the time.
* Every leaf carries both ``serverAuth`` and ``clientAuth``
  extended-key-usage so the same cert works whether the holder
  is the active or passive party (the daemon is a server to
  ``crv`` and a client to agents; agents will gain push channels
  in later phases).
"""

from __future__ import annotations

import datetime as dt
import ipaddress
import uuid
from dataclasses import dataclass

from cryptography import x509
from cryptography.hazmat.primitives import hashes, serialization
from cryptography.hazmat.primitives.asymmetric import ed25519
from cryptography.x509.oid import ExtendedKeyUsageOID, NameOID

from corvus_admin.store import AdminStore, IssuedRecord


# ---------------------------------------------------------------------------
# Role table

# Keep these literals identical to TlsRole on the Haskell side
# (Corvus.Tls). Filename = prefix; CN = prefix + ":" + name.
ROLE_DAEMON = "corvus-daemon"
ROLE_NODE = "corvus-node"
ROLE_NETD = "corvus-netd"
ROLE_CLIENT = "corvus-client"

ALL_ROLES = (ROLE_DAEMON, ROLE_NODE, ROLE_NETD, ROLE_CLIENT)

# Lifetimes. Long-lived because we don't auto-renew in v1; the
# operator runs `corvus-admin renew --auto` periodically.
CA_LIFETIME = dt.timedelta(days=10 * 365)
COMPONENT_LIFETIME = dt.timedelta(days=365)


@dataclass
class IssuedCert:
    """A freshly minted cert with everything the deploy step needs."""

    cn: str
    cert_pem: bytes
    key_pem: bytes
    record: IssuedRecord


# ---------------------------------------------------------------------------
# CA

def init_ca(
    store: AdminStore,
    *,
    force: bool = False,
    not_after: dt.datetime | None = None,
) -> None:
    """Generate a fresh CA in *store*. Layout (dir + counter +
    index files) is created if absent. By default refuses to
    overwrite an existing CA; pass ``force=True`` to rotate.

    Returns ``None`` — both the cert and the key are written to
    disk under the store layout.
    """

    store.ensure_layout()

    if store.exists() and not force:
        raise FileExistsError(
            f"CA already initialised at {store.root}; pass force=True to overwrite"
        )

    ca_uuid = uuid.uuid4()
    ca_cn = f"corvus-ca:{ca_uuid}"

    key = ed25519.Ed25519PrivateKey.generate()

    now = dt.datetime.now(dt.timezone.utc)
    not_after = not_after or (now + CA_LIFETIME)

    subject = issuer = x509.Name(
        [x509.NameAttribute(NameOID.COMMON_NAME, ca_cn)]
    )

    cert = (
        x509.CertificateBuilder()
        .subject_name(subject)
        .issuer_name(issuer)
        .public_key(key.public_key())
        .serial_number(x509.random_serial_number())
        .not_valid_before(now)
        .not_valid_after(not_after)
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
            x509.SubjectKeyIdentifier.from_public_key(key.public_key()),
            critical=False,
        )
        .sign(private_key=key, algorithm=None)  # Ed25519 has no hash arg
    )

    # Persist. Files are written with mode 0600 (key) / 0644
    # (cert); ensure_layout() already mode-tightened the dir.
    _atomic_write(
        store.ca_cert_path,
        cert.public_bytes(serialization.Encoding.PEM),
        mode=0o644,
    )
    _atomic_write(
        store.ca_key_path,
        key.private_bytes(
            encoding=serialization.Encoding.PEM,
            format=serialization.PrivateFormat.PKCS8,
            encryption_algorithm=serialization.NoEncryption(),
        ),
        mode=0o600,
    )


def load_ca(store: AdminStore) -> tuple[x509.Certificate, ed25519.Ed25519PrivateKey]:
    """Read the CA cert + private key off disk. Raises
    :class:`FileNotFoundError` when the store hasn't been
    initialised."""

    if not store.exists():
        raise FileNotFoundError(
            f"no CA at {store.root}; run `corvus-admin init` first"
        )
    cert = x509.load_pem_x509_certificate(store.ca_cert_path.read_bytes())
    key_obj = serialization.load_pem_private_key(
        store.ca_key_path.read_bytes(),
        password=None,
    )
    if not isinstance(key_obj, ed25519.Ed25519PrivateKey):
        raise TypeError(
            f"CA private key at {store.ca_key_path} is not Ed25519"
        )
    return cert, key_obj


def ca_cert_pem(store: AdminStore) -> bytes:
    """Read the CA cert PEM, suitable for deployment alongside
    each component's cert/key pair."""

    return store.ca_cert_path.read_bytes()


# ---------------------------------------------------------------------------
# Leaf cert minting

def issue_cert(
    store: AdminStore,
    *,
    role: str,
    name: str,
    ip: str | None = None,
    not_after: dt.datetime | None = None,
) -> IssuedCert:
    """Sign a leaf certificate for the given role / name with the
    admin store's CA. ``role`` is one of the ``corvus-*`` strings
    in :data:`ALL_ROLES`; ``name`` is the daemon UUID, the node
    name, or the client name (no colons).

    When ``ip`` is given it ends up as an ``IPAddress`` SAN; the
    daemon → agent dial validates that the agent's cert was minted
    for the address the daemon resolved. Client certs always pass
    ``ip=None``.

    The cert is also dropped into ``issued/`` and the index is
    updated, so this single call covers everything ``corvus-admin
    deploy …`` needs to hand off to the runner.
    """

    if role not in ALL_ROLES:
        raise ValueError(
            f"unknown role {role!r}; expected one of {ALL_ROLES}"
        )
    if ":" in name:
        raise ValueError(
            f"name {name!r} must not contain ':'; that's the CN separator"
        )

    ca_cert, ca_key = load_ca(store)

    cn = f"{role}:{name}"
    now = dt.datetime.now(dt.timezone.utc)
    not_after = not_after or (now + COMPONENT_LIFETIME)

    leaf_key = ed25519.Ed25519PrivateKey.generate()

    san_list: list[x509.GeneralName] = [x509.DNSName(name)]
    if ip is not None:
        san_list.append(x509.IPAddress(ipaddress.ip_address(ip)))

    builder = (
        x509.CertificateBuilder()
        .subject_name(x509.Name([x509.NameAttribute(NameOID.COMMON_NAME, cn)]))
        .issuer_name(ca_cert.subject)
        .public_key(leaf_key.public_key())
        .serial_number(x509.random_serial_number())
        .not_valid_before(now)
        .not_valid_after(not_after)
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
                [ExtendedKeyUsageOID.SERVER_AUTH, ExtendedKeyUsageOID.CLIENT_AUTH]
            ),
            critical=False,
        )
        .add_extension(
            x509.SubjectAlternativeName(san_list),
            critical=False,
        )
        .add_extension(
            x509.SubjectKeyIdentifier.from_public_key(leaf_key.public_key()),
            critical=False,
        )
        .add_extension(
            x509.AuthorityKeyIdentifier.from_issuer_public_key(
                ca_cert.public_key()
            ),
            critical=False,
        )
    )
    cert = builder.sign(private_key=ca_key, algorithm=None)

    cert_pem = cert.public_bytes(serialization.Encoding.PEM)
    key_pem = leaf_key.private_bytes(
        encoding=serialization.Encoding.PEM,
        format=serialization.PrivateFormat.PKCS8,
        encryption_algorithm=serialization.NoEncryption(),
    )

    serial = store.next_serial()
    _atomic_write(store.issued_cert_path(cn), cert_pem, mode=0o644)

    rec = IssuedRecord(
        cn=cn,
        role=role,
        name_or_uuid=name,
        issued_at=now.isoformat(timespec="seconds"),
        expires_at=not_after.isoformat(timespec="seconds"),
        serial=serial,
        ip=ip,
    )
    store.record(rec)

    return IssuedCert(cn=cn, cert_pem=cert_pem, key_pem=key_pem, record=rec)


# ---------------------------------------------------------------------------
# Helpers

def _atomic_write(path, data: bytes, *, mode: int) -> None:
    """Write *data* to *path* atomically with the given mode. Used
    for cert + key files so a crashed run can't leave a half-written
    key on disk that a later run would silently load."""

    import os

    path = str(path)
    tmp = path + ".tmp"
    fd = os.open(tmp, os.O_WRONLY | os.O_CREAT | os.O_TRUNC, mode)
    try:
        with os.fdopen(fd, "wb") as f:
            f.write(data)
    except Exception:
        try:
            os.unlink(tmp)
        finally:
            raise
    os.replace(tmp, path)
    # Re-apply mode in case umask masked it.
    os.chmod(path, mode)
