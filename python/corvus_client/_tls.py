"""Python-client TLS plumbing.

Mirror of the Haskell-side ``Corvus.Tls`` module: same role / CN
convention, same search path, same post-handshake CN-prefix check.
Keeps the wire contract a single source of truth — the Haskell
side rejects clients whose CN prefix is wrong, and so do we when
the *peer* (the daemon) doesn't present a ``corvus-daemon:*`` CN.

Used by :mod:`corvus_client._async.client` (and its sync wrapper)
to wrap pycapnp's TCP transport with an :class:`ssl.SSLContext`
plus a tiny post-handshake validator.
"""

from __future__ import annotations

import os
import ssl
from dataclasses import dataclass
from pathlib import Path
from typing import Iterable, Optional


# ---------------------------------------------------------------------------
# Role / CN convention — keep aligned with Corvus.Tls (Haskell)

ROLE_DAEMON = "corvus-daemon"
ROLE_NODE = "corvus-node"
ROLE_NETD = "corvus-netd"
ROLE_CLIENT = "corvus-client"


def role_cn_prefix(role: str) -> str:
    return f"{role}:"


# ---------------------------------------------------------------------------
# Errors


class TlsError(Exception):
    """Base class for cert-handling errors raised by this module."""


class CertNotFoundError(TlsError):
    """The required cert / key / CA trio couldn't be located under
    any of the search-path directories. Carries the missing names
    and the dirs we tried so the diagnostic is actionable."""


class CertificateError(TlsError):
    """The peer's CN didn't match the expected prefix (or, when
    asked, the expected suffix). Raised after the TLS handshake
    has otherwise succeeded — the issue is who they are, not
    whether their cert is well-formed."""


# ---------------------------------------------------------------------------
# Search path


def default_cert_search_path() -> list[Path]:
    """/etc/corvus then $XDG_CONFIG_HOME/corvus (default
    ~/.config/corvus). Mirrors Corvus.Tls.defaultCertSearchPath."""

    return [Path("/etc/corvus"), _xdg_config_home() / "corvus"]


def client_cert_search_path() -> list[Path]:
    """Client certs are user-owned and live only under
    $XDG_CONFIG_HOME/corvus — /etc/corvus is skipped so two
    admins on one workstation never compete for the same file.
    Mirrors Corvus.Tls.clientCertSearchPath."""

    return [_xdg_config_home() / "corvus"]


def _xdg_config_home() -> Path:
    xdg = os.environ.get("XDG_CONFIG_HOME")
    if xdg:
        return Path(xdg)
    home = os.environ.get("HOME") or os.path.expanduser("~")
    return Path(home) / ".config"


def resolve_cert_dir(
    search_path: Iterable[Path],
    needed: Iterable[str],
) -> Optional[Path]:
    """Return the first dir under *search_path* that contains
    every entry in *needed*. ``None`` when nothing matches."""

    needed_list = list(needed)
    for d in search_path:
        if all((d / f).is_file() for f in needed_list):
            return d
    return None


# ---------------------------------------------------------------------------
# SSLContext


@dataclass
class TlsBundle:
    """Loaded TLS bits plus enough context for post-handshake
    validation. Constructed once per :class:`AsyncClient` and
    reused for the lifetime of that client."""

    context: ssl.SSLContext
    cert_dir: Path
    own_cn_prefix: str
    expected_peer_prefix: str
    expected_peer_name: Optional[str]


def build_client_bundle(
    cert_dir: Optional[Path] = None,
    *,
    role: str = ROLE_CLIENT,
    expected_peer_prefix: str = ROLE_DAEMON,
    expected_peer_name: Optional[str] = None,
) -> TlsBundle:
    """Build a TLS bundle for a client of the given *role*.
    Defaults match the most-common case: the CLI / Python client
    talking to the daemon. *cert_dir* overrides the search path
    when set — useful for tests pinning to a temp directory.

    Raises :class:`CertNotFoundError` when the cert trio isn't
    available; the caller is expected to surface a clean error
    pointing at ``corvus-admin``.
    """

    needed = ["ca.crt", f"{role}.crt", f"{role}.key"]
    if cert_dir is not None:
        if not all((cert_dir / f).is_file() for f in needed):
            missing = [f for f in needed if not (cert_dir / f).is_file()]
            raise CertNotFoundError(
                f"cert_dir {cert_dir} is missing required files: {missing}"
            )
        chosen = cert_dir
    else:
        search = (
            client_cert_search_path()
            if role == ROLE_CLIENT
            else default_cert_search_path()
        )
        chosen = resolve_cert_dir(search, needed)
        if chosen is None:
            raise CertNotFoundError(
                f"no directory in {[str(p) for p in search]} contains "
                f"{needed}; run `corvus-admin deploy {role.split('-', 1)[1]} ...` "
                f"or pass tls=False"
            )

    ctx = ssl.SSLContext(ssl.PROTOCOL_TLS_CLIENT)
    ctx.load_verify_locations(cafile=str(chosen / "ca.crt"))
    ctx.load_cert_chain(
        certfile=str(chosen / f"{role}.crt"),
        keyfile=str(chosen / f"{role}.key"),
    )
    ctx.verify_mode = ssl.CERT_REQUIRED
    # We don't run HTTPS; the CN we sign is a Corvus identity, not
    # an FQDN. Hostname matching is done by `validate_peer_cn`
    # below, after the handshake completes.
    ctx.check_hostname = False
    # Constrain to TLS 1.3 + TLS 1.2 (matches Corvus.Tls.corvusSupported
    # on the Haskell side).
    ctx.minimum_version = ssl.TLSVersion.TLSv1_2
    ctx.maximum_version = ssl.TLSVersion.TLSv1_3

    return TlsBundle(
        context=ctx,
        cert_dir=chosen,
        own_cn_prefix=role_cn_prefix(role),
        expected_peer_prefix=role_cn_prefix(expected_peer_prefix),
        expected_peer_name=expected_peer_name,
    )


# ---------------------------------------------------------------------------
# Post-handshake CN validation


def extract_cn(peercert: dict) -> Optional[str]:
    """Pull the CN out of the dict shape :py:meth:`ssl.SSLSocket.getpeercert`
    returns. The dict carries ``subject`` as a tuple of
    ``((('commonName', 'corvus-daemon:...'),),)``-style nested
    tuples; we drill down to the first CN attribute.
    """

    subject = peercert.get("subject") or ()
    for rdn in subject:
        for attr in rdn:
            if attr and attr[0] in ("commonName", "CN"):
                return attr[1]
    return None


def validate_peer_cn(
    peercert: dict | None,
    bundle: TlsBundle,
) -> None:
    """Raise :class:`CertificateError` if the peer's CN doesn't
    match the bundle's expectations. ``peercert`` should be the
    dict returned by ``transport.get_extra_info('peercert')``."""

    if not peercert:
        raise CertificateError("no peer certificate available after TLS handshake")
    cn = extract_cn(peercert)
    if cn is None:
        raise CertificateError(
            f"peer cert subject has no CN; subject={peercert.get('subject')!r}"
        )
    if not cn.startswith(bundle.expected_peer_prefix):
        raise CertificateError(
            f"peer CN {cn!r} does not start with required prefix "
            f"{bundle.expected_peer_prefix!r}"
        )
    if bundle.expected_peer_name is not None:
        suffix = cn[len(bundle.expected_peer_prefix) :]
        if suffix != bundle.expected_peer_name:
            raise CertificateError(
                f"peer CN {cn!r} has name {suffix!r}, expected "
                f"{bundle.expected_peer_name!r}"
            )
