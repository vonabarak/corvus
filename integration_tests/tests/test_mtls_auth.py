"""mTLS authentication failure-path coverage for the daemon's TCP
listener.

[doc/security.md](../../doc/security.md) promises four guarantees on
the TCP path:

  1. The handshake fails if the client cert isn't signed by the
     daemon's CA.
  2. Expired client certs are rejected.
  3. Client CNs must start with ``corvus-client:`` (the
     ``checkPrefixAndName`` check in
     [src/Corvus/Tls.hs:513](src/Corvus/Tls.hs#L513)).
  4. Multiple concurrent clients sharing the same CN both
     authenticate independently (the daemon must not single-track
     on CN identity).

None of these failure paths had a dedicated test. A regression that
loosens any of them is a security incident — operator certs that
should be rejected would silently authenticate.

This file mints the four pathological cert trios via the harness's
existing :class:`CaContext` (plus a second, foreign CA for the
wrong-CA case), drops each into a tempdir, opens a fresh pycapnp
``Client`` against the running daemon, and asserts the connection
fails before the first RPC frame succeeds.

What's NOT covered here:

* Wrong-key-pair (cert PEM + key PEM that don't match each
  other). Mismatch surfaces client-side as an OpenSSL load
  error before any wire packets are sent — would only catch a
  ``corvus_client._tls.build_client_bundle`` regression, not a
  daemon-side change. Tangential to Bucket 4's intent.
* Cert chain depth > 1. Corvus deploys leaf certs signed
  directly by the root CA; multi-intermediate chains aren't a
  supported configuration.
"""

from __future__ import annotations

import datetime as dt
import secrets
from pathlib import Path

import pytest
from corvus_admin import ca
from corvus_admin.store import AdminStore
from corvus_client import Client
from corvus_test_harness import SingleNodeCase


def _stage_cert_dir(
    *,
    dest: Path,
    server_ca_pem: bytes,
    client_cert_pem: bytes,
    client_key_pem: bytes,
) -> Path:
    """Lay out a host-side cert dir in the harness convention:
    ``{ca.crt, corvus-client.crt, corvus-client.key}``. The server CA
    is the daemon's (so the client trusts the SERVER's cert); the
    client cert/key are whichever pathological trio the caller wants
    to test."""
    dest.mkdir(parents=True, exist_ok=True)
    (dest / "ca.crt").write_bytes(server_ca_pem)
    (dest / "corvus-client.crt").write_bytes(client_cert_pem)
    key_path = dest / "corvus-client.key"
    key_path.write_bytes(client_key_pem)
    key_path.chmod(0o600)
    return dest


def _mint_in_harness_ca(
    *,
    role: str,
    name: str,
    not_after: dt.datetime | None,
    admin_store: AdminStore,
):
    """Mint a leaf cert under the harness's existing CA. Used for
    the expired-cert and wrong-CN tests where the only thing
    pathological is the leaf itself (CA trust is intact)."""
    return ca.issue_cert(
        admin_store, role=role, name=name, ip=None, not_after=not_after
    )


class TestMtlsHandshakeFailures(SingleNodeCase):
    """Each test mints a deliberately-broken client cert and tries
    to dial the daemon. The daemon must reject before any RPC
    succeeds — we assert the very first ``status()`` call raises.

    All four pathological trios share the harness's daemon (the
    SingleNodeCase fixture). Tests run quickly: no per-test VM
    boot, just a handful of TLS handshakes."""

    def _try_dial(self, cert_dir: Path) -> None:
        """Try to open a Client and probe ``status()`` once.

        Raises whatever exception the handshake / connect path
        surfaces — pycapnp wraps OpenSSL errors variably, so the
        caller asserts on `pytest.raises(Exception)` rather than a
        narrow type. The point of this helper is keeping the
        try/except shape uniform across the four scenarios.
        """
        host, port = self.node.relay.endpoint
        # Construct may not block; the status() round-trip forces
        # the handshake to complete and surfaces the failure.
        c = Client(host=host, port=port, cert_dir=cert_dir, tls=True)
        try:
            c.status()
        finally:
            try:
                c.close()
            except Exception:
                pass

    def test_wrong_ca_signed_client_cert_rejected(self, tmp_path: Path):
        """A client cert signed by a CA the daemon doesn't trust is
        rejected at handshake (chain validation). We mint a fresh,
        foreign CA, sign a properly-named ``corvus-client:foreign``
        cert with it, then put the FOREIGN client cert next to the
        HARNESS daemon's CA so client-side server verification
        still works — the failure is server-side rejection of the
        unknown client chain."""
        # Foreign CA — not in the daemon's trust store.
        foreign_root = tmp_path / "foreign-ca"
        foreign_store = AdminStore(foreign_root)
        ca.init_ca(foreign_store)
        foreign_client = ca.issue_cert(
            foreign_store, role=ca.ROLE_CLIENT, name="foreign", ip=None
        )

        cert_dir = _stage_cert_dir(
            dest=tmp_path / "wrong-ca-bundle",
            server_ca_pem=self.topology.ca.ca_pem,  # daemon CA — to trust server
            client_cert_pem=foreign_client.cert_pem,
            client_key_pem=foreign_client.key_pem,
        )
        # Handshake errors surface variably (ssl.SSLError,
        # ConnectError, OSError-subclasses, pycapnp wrappers)
        # depending on whether the failure is client-side cert
        # load, OpenSSL chain validation, or the server hanging up
        # post-handshake. We don't pin a narrower type because
        # the cross-stack normalization isn't ours to control.
        with pytest.raises(Exception):  # noqa: B017
            self._try_dial(cert_dir)

    def test_expired_client_cert_rejected(self, tmp_path: Path):
        """``not_valid_after`` set 1 minute in the past: the cert
        was correctly issued by the harness CA but expired before
        we presented it. Modern TLS stacks reject during chain
        validation."""
        now = dt.datetime.now(dt.timezone.utc)
        expired_cert = _mint_in_harness_ca(
            role=ca.ROLE_CLIENT,
            name=f"expired-{secrets.token_hex(3)}",
            not_after=now - dt.timedelta(minutes=1),
            admin_store=self.topology.ca.store,
        )
        cert_dir = _stage_cert_dir(
            dest=tmp_path / "expired-bundle",
            server_ca_pem=self.topology.ca.ca_pem,
            client_cert_pem=expired_cert.cert_pem,
            client_key_pem=expired_cert.key_pem,
        )
        # Handshake errors surface variably (ssl.SSLError,
        # ConnectError, OSError-subclasses, pycapnp wrappers)
        # depending on whether the failure is client-side cert
        # load, OpenSSL chain validation, or the server hanging up
        # post-handshake. We don't pin a narrower type because
        # the cross-stack normalization isn't ours to control.
        with pytest.raises(Exception):  # noqa: B017
            self._try_dial(cert_dir)

    def test_wrong_cn_prefix_rejected(self, tmp_path: Path):
        """A cert signed by the right CA but carrying a
        ``corvus-daemon:`` CN instead of ``corvus-client:`` is
        rejected post-handshake by the daemon's CN-prefix check
        (``checkPrefixAndName`` — Tls.hs:513). Catches a refactor
        that loosens the prefix-validation gate.

        Note: ``ca.issue_cert`` minted with role=ROLE_DAEMON is
        still a legitimate cert chain — the issue isn't trust,
        it's role identity. Without the prefix check the daemon
        would happily accept it."""
        wrong_role_cert = _mint_in_harness_ca(
            role=ca.ROLE_DAEMON,
            name=f"impostor-{secrets.token_hex(3)}",
            not_after=None,
            admin_store=self.topology.ca.store,
        )
        cert_dir = _stage_cert_dir(
            dest=tmp_path / "wrong-prefix-bundle",
            server_ca_pem=self.topology.ca.ca_pem,
            client_cert_pem=wrong_role_cert.cert_pem,
            client_key_pem=wrong_role_cert.key_pem,
        )
        # Handshake errors surface variably (ssl.SSLError,
        # ConnectError, OSError-subclasses, pycapnp wrappers)
        # depending on whether the failure is client-side cert
        # load, OpenSSL chain validation, or the server hanging up
        # post-handshake. We don't pin a narrower type because
        # the cross-stack normalization isn't ours to control.
        with pytest.raises(Exception):  # noqa: B017
            self._try_dial(cert_dir)


class TestMultipleConcurrentClientsSameCn(SingleNodeCase):
    """Two clients sharing the same CN both succeed — the daemon
    must not single-track on CN identity (an operator running
    ``crv`` from two terminals at once is a common case)."""

    def test_two_clients_share_same_cn(self):
        """Open two simultaneous Client instances with the harness's
        shared cert_dir (same CN: ``corvus-client:harness``), assert
        both reach ``status()`` successfully and stay open in
        parallel.

        Catches a regression where the daemon enforces
        single-connection-per-CN (e.g. an over-zealous deduplication
        in the per-client task-attribution layer). Doesn't catch
        a regression that breaks task attribution by CN — that's
        out of scope here (Bucket 9 covers the ``client_name``
        record path)."""
        host, port = self.node.relay.endpoint
        cert_dir = self.node._client_cert_dir

        c1 = Client(host=host, port=port, cert_dir=cert_dir, tls=True)
        try:
            c2 = Client(host=host, port=port, cert_dir=cert_dir, tls=True)
            try:
                # Both round-trip a no-op call. If the daemon
                # single-tracked on CN one of these would fail.
                info1 = c1.status()
                info2 = c2.status()
                assert info1.version == info2.version, (
                    f"two clients reported different versions: "
                    f"{info1.version} vs {info2.version}"
                )
            finally:
                c2.close()
        finally:
            c1.close()
