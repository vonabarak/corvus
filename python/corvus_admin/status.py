"""Status probe.

For every issued cert with a documented deploy target, try to
connect to the component's TCP listener over TLS and report
whether the handshake succeeds. Helps operators answer "is this
node actually reachable from where I'm standing?" without having
to remember which port belongs to which role.

This is intentionally a cheap probe — handshake only, no RPC
exchange. The real RPC path is the daemon's healthcheck push
(plumbed in via the multi-node feature); this probe just confirms
the network + TLS layer is alive.
"""

from __future__ import annotations

import datetime as dt
import socket
import ssl
from dataclasses import dataclass
from pathlib import Path

from cryptography import x509

from corvus_admin import ca, store

# Default ports the components listen on. Same values as
# corvus_admin.register; kept here so status doesn't import
# register and pull subprocess into this module.
PORT_DAEMON = 9876
PORT_NODE_AGENT = 9878
PORT_NETD = 9877

PROBE_TIMEOUT_SEC = 5.0


@dataclass
class ProbeReport:
    """One row of ``corvus-admin status`` output."""

    cn: str
    role: str
    deployed_to: str | None
    expires_at: str
    days_remaining: int
    reachable: bool
    handshake_error: str | None  # set when reachable=False


def probe_all(admin_store: store.AdminStore) -> list[ProbeReport]:
    """Probe every record in the admin store's index. The order of
    the returned list matches the index file's iteration order
    (sorted by CN), so callers can render it directly."""

    return [
        _probe(rec, admin_store)
        for rec in sorted(admin_store.iter_records(), key=lambda r: r.cn)
    ]


def _probe(rec: store.IssuedRecord, admin_store: store.AdminStore) -> ProbeReport:
    expires = dt.datetime.fromisoformat(rec.expires_at)
    days_remaining = max(
        0,
        int((expires - dt.datetime.now(dt.timezone.utc)).total_seconds() // 86400),
    )

    # Client certs are local-only — no service to probe.
    if rec.role == ca.ROLE_CLIENT:
        return ProbeReport(
            cn=rec.cn,
            role=rec.role,
            deployed_to=rec.deployed_to,
            expires_at=rec.expires_at,
            days_remaining=days_remaining,
            reachable=True,
            handshake_error=None,
        )

    target = _probe_target(rec)
    if target is None:
        return ProbeReport(
            cn=rec.cn,
            role=rec.role,
            deployed_to=rec.deployed_to,
            expires_at=rec.expires_at,
            days_remaining=days_remaining,
            reachable=False,
            handshake_error="no probe target (cert never deployed?)",
        )

    host, port = target
    err = _try_handshake(host, port, admin_store)
    return ProbeReport(
        cn=rec.cn,
        role=rec.role,
        deployed_to=rec.deployed_to,
        expires_at=rec.expires_at,
        days_remaining=days_remaining,
        reachable=err is None,
        handshake_error=err,
    )


def _probe_target(rec: store.IssuedRecord) -> tuple[str, int] | None:
    """Resolve (host, port) the cert points at. Prefers the SAN IP
    recorded at issue time (when the operator passed --ip /
    --listen-ip); falls back to parsing the runner label stored in
    ``deployed_to`` so certs minted without an IP SAN can still be
    probed."""

    port_by_role = {
        ca.ROLE_DAEMON: PORT_DAEMON,
        ca.ROLE_NODE: PORT_NODE_AGENT,
        ca.ROLE_NETD: PORT_NETD,
    }
    port = port_by_role.get(rec.role)
    if port is None:
        return None
    host: str | None = rec.ip
    if host is None and rec.deployed_to:
        host = _host_from_deploy_label(rec.deployed_to)
    if host is None:
        return None
    return host, port


def _host_from_deploy_label(label: str) -> str | None:
    """Extract a dial-able host from a stored runner label.

    * ``"local"`` → ``127.0.0.1`` (the daemon/agent listens on
      loopback in single-host setups).
    * ``"ssh:user@host"`` / ``"ssh:host"`` / ``"ssh:host:port"`` →
      the host portion (we ignore the port — the per-role default
      applies).
    * ``"ssh:[ipv6]:port"`` → the bracketed IPv6 literal.
    * ``"local:<path>"`` and anything else → ``None`` (no probe).
    """

    if label == "local":
        return "127.0.0.1"
    if not label.startswith("ssh:"):
        return None
    target = label[len("ssh:") :]
    if "@" in target:
        target = target.rsplit("@", 1)[1]
    if target.startswith("[") and "]" in target:
        # IPv6 literal — strip the brackets, drop any trailing :port.
        return target[1 : target.index("]")]
    if target.count(":") == 1:
        target = target.split(":", 1)[0]
    return target or None


def _try_handshake(host: str, port: int, admin_store: store.AdminStore) -> str | None:
    """Open a TLS connection to *host:port* using the admin's
    client cert. Returns ``None`` on success, a one-line error
    string on failure. The cert validation passes when the
    component's cert chains to our CA; we don't validate the CN
    here — that's the role-prefix check, exercised in the daemon's
    own handshake. Reachability is the question."""

    if not admin_store.exists():
        return f"admin store {admin_store.root} not initialised"
    client_cert = _find_client_cert(admin_store)
    if client_cert is None:
        return (
            "no client cert in admin store; run "
            "`corvus-admin deploy client <name>` first"
        )
    cert_pem_path, key_pem_path = client_cert

    ctx = ssl.SSLContext(ssl.PROTOCOL_TLS_CLIENT)
    ctx.load_verify_locations(cafile=str(admin_store.ca_cert_path))
    ctx.load_cert_chain(certfile=str(cert_pem_path), keyfile=str(key_pem_path))
    ctx.check_hostname = False
    ctx.verify_mode = ssl.CERT_REQUIRED
    try:
        with socket.create_connection((host, port), timeout=PROBE_TIMEOUT_SEC) as sock:
            with ctx.wrap_socket(sock, server_hostname=None) as tls_sock:
                # Handshake completes on wrap_socket entry. We just
                # need to peek at the peer cert to make sure
                # something useful is on the other side.
                _ = tls_sock.getpeercert()
                return None
    except (ssl.SSLError, OSError) as e:
        return f"{type(e).__name__}: {e}"


def _find_client_cert(admin_store: store.AdminStore) -> tuple[Path, Path] | None:
    """Pick whichever client cert lives next to the admin's own
    client trio (i.e. in $XDG_CONFIG_HOME/corvus/). We deliberately
    don't search ``issued/`` — those files are CA-rooted records
    without a matching private key on disk."""

    client_dir = store.default_client_dir()
    crt = client_dir / "corvus-client.crt"
    key = client_dir / "corvus-client.key"
    if crt.is_file() and key.is_file():
        return crt, key
    return None


# Convenience for tests / external callers: parse a cert's
# notAfter without opening it as a file again. Used by the CLI's
# `status` to cross-check the index's expires_at against the
# actual cert.
def cert_not_after(pem: bytes) -> dt.datetime:
    cert = x509.load_pem_x509_certificate(pem)
    return cert.not_valid_after_utc
