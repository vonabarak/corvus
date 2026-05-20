"""Per-test-class cert minting + deploy orchestration.

Thin layer on top of :mod:`corvus_admin.ca` and
:mod:`corvus_admin.deploy`. The integration harness uses this
instead of calling deploy_* directly so each call site can stay
short and the bookkeeping (per-VM client cert directory on the
host) lives in one place.

Two shapes a test class might want:

* :func:`deploy_full_stack` — daemon + nodeagent + netd onto one
  VM. Used by :class:`SingleNodeCase` and by every node in
  :class:`TwoDaemonsCase` (each gets its own CA), plus by the
  alpha node in :class:`OneDaemonTwoNodesCase`.

* :func:`deploy_agents_only` — nodeagent + netd onto a VM that
  isn't running a daemon. Used by the beta node in
  :class:`OneDaemonTwoNodesCase`; the daemon side talks to it
  over mTLS as ``corvus-node:beta`` / ``corvus-netd:beta``
  (which is why beta still needs *its own* cert pair, signed by
  the same CA as alpha).
"""

from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path

from corvus_admin import ca, deploy, store

from .runner import NodeShellRunner
from .ssh import NodeShell


# systemd unit names used inside the corvus-test-node image (see
# `yaml/corvus-test-node/systemd/`). The daemon unit is named
# `corvus-test.service` historically (to distinguish from the
# user-systemd `corvus.service` a developer's `make install` drops
# in `~/.config/systemd/user/`); the agent units match
# production naming.
TEST_DAEMON_UNIT = "corvus-test.service"
TEST_NODE_AGENT_UNIT = "corvus-nodeagent.service"
TEST_NETD_UNIT = "corvus-netd.service"


@dataclass
class CaContext:
    """One CA + its admin store + the cached host-side client trio.

    The harness creates one per test class (or two, for
    :class:`TwoDaemonsCase`). Lifetime is class-scoped — the
    pytest tmpdir holding the admin store gets cleaned up by
    pytest's own fixture teardown.
    """

    store: store.AdminStore
    client_cert_pem: bytes
    client_key_pem: bytes

    @classmethod
    def new(cls, root: Path, *, client_name: str = "harness") -> "CaContext":
        """Build a fresh CA rooted at *root* and mint a client
        cert under ``client_name``. The CA's private key never
        leaves *root* — make it a per-class tmpdir."""

        root.mkdir(parents=True, exist_ok=True)
        admin_store = store.AdminStore(root)
        ca.init_ca(admin_store)
        client = ca.issue_cert(
            admin_store, role=ca.ROLE_CLIENT, name=client_name, ip=None
        )
        return cls(
            store=admin_store,
            client_cert_pem=client.cert_pem,
            client_key_pem=client.key_pem,
        )

    @property
    def ca_pem(self) -> bytes:
        """The CA cert PEM, suitable for dropping next to the
        client cert in the host-side cert dir."""
        return self.store.ca_cert_path.read_bytes()

    def mint_host_cert_dir(
        self,
        *,
        role: str,
        name: str,
        root: Path,
    ) -> Path:
        """Mint a fresh cert+key under *role*+name and lay it
        out (alongside the CA cert) in a per-call subdir of
        *root*. Returns the directory.

        Filenames follow the harness-wide convention
        (``ca.crt``, ``<role>.crt``, ``<role>.key``) so the
        result can be passed straight to a Python client's
        ``cert_dir=`` kwarg, or to
        :func:`corvus_client._tls.build_client_bundle`.

        Used by tests that need to dial an agent directly —
        e.g. ``test_netd_smoke``'s direct-to-netd
        :class:`NetdClient`, which must present a
        ``corvus-daemon:…`` cert because netd's CN-prefix
        check refuses anything else. Cert/key are not
        re-deployed remotely: this is a host-side artefact for
        the Python side of the handshake only.
        """

        issued = ca.issue_cert(
            self.store, role=role, name=name, ip=None
        )
        dir_ = root / f"{role}-{name}"
        dir_.mkdir(parents=True, exist_ok=True)
        (dir_ / "ca.crt").write_bytes(self.ca_pem)
        (dir_ / f"{role}.crt").write_bytes(issued.cert_pem)
        key = dir_ / f"{role}.key"
        key.write_bytes(issued.key_pem)
        key.chmod(0o600)
        return dir_


# ---------------------------------------------------------------------------
# Deploy helpers


def deploy_full_stack(
    ca_ctx: CaContext,
    shell: NodeShell,
    *,
    node_name: str,
    node_ip: str,
    host_cert_root: Path,
) -> Path:
    """Mint + push daemon, nodeagent, netd certs onto one VM and
    enable + start the three units.

    Returns the host-side cert directory holding ``ca.crt`` +
    ``corvus-client.{crt,key}`` so the harness's pycapnp client
    can dial this node with ``Client(host=…, cert_dir=…)``.

    ``host_cert_root`` is typically a pytest tmpdir; we create a
    per-VM subdir under it so the cert trio can live alongside
    the other test artefacts for that class.

    All three deploys go through the **same**
    :class:`NodeShellRunner`. The runner's ``label`` flows into
    the admin-store index's ``deployed_to`` column so
    ``corvus-admin list`` / future renew can see where each cert
    landed.
    """

    runner = NodeShellRunner(shell, label=f"vsock:{node_name}")
    # Defensive: in case the previous test run left units in
    # StartLimit hold (a typical first-boot symptom before all
    # certs are in place), clear that state up front so the
    # enable --now calls below see a clean slate.
    _reset_failed(
        runner,
        TEST_DAEMON_UNIT,
        TEST_NODE_AGENT_UNIT,
        TEST_NETD_UNIT,
    )
    # Deploy in dependency order — netd first, then nodeagent,
    # then the daemon. ``corvus-test.service`` Wants both agents,
    # so enabling the daemon early would trigger systemd to
    # start the agents *before* their certs are in place,
    # crash-loop them, and hit StartLimitBurst within 10s. By
    # ordering netd → nodeagent → daemon, each unit has its
    # cert when systemd tries to start it.
    deploy.deploy_netd(
        ca_ctx.store,
        runner,
        name=node_name,
        ip=node_ip,
        user_service=False,
        service_unit=TEST_NETD_UNIT,
    )
    deploy.deploy_node(
        ca_ctx.store,
        runner,
        name=node_name,
        ip=node_ip,
        user_service=False,
        service_unit=TEST_NODE_AGENT_UNIT,
    )
    deploy.deploy_daemon(
        ca_ctx.store,
        runner,
        listen_ip=node_ip,
        user_service=False,
        service_unit=TEST_DAEMON_UNIT,
    )
    return _write_host_cert_dir(ca_ctx, host_cert_root, node_name)


def deploy_agents_only(
    ca_ctx: CaContext,
    shell: NodeShell,
    *,
    node_name: str,
    node_ip: str,
) -> None:
    """Mint + push nodeagent + netd certs onto a VM (no daemon).

    Used for the beta node in :class:`OneDaemonTwoNodesCase`:
    alpha's daemon dials beta's agents over mTLS, so beta still
    needs its own cert pair (one per agent), signed by the
    *same* CA as alpha so alpha's daemon trusts them. No host
    cert dir returned — there's no client to open against this
    node's (non-existent) daemon.
    """

    runner = NodeShellRunner(shell, label=f"vsock:{node_name}")
    _reset_failed(runner, TEST_NODE_AGENT_UNIT, TEST_NETD_UNIT)
    # netd first; nodeagent has no Wants on netd but ordering by
    # dependency keeps both deploy paths symmetric.
    deploy.deploy_netd(
        ca_ctx.store,
        runner,
        name=node_name,
        ip=node_ip,
        user_service=False,
        service_unit=TEST_NETD_UNIT,
    )
    deploy.deploy_node(
        ca_ctx.store,
        runner,
        name=node_name,
        ip=node_ip,
        user_service=False,
        service_unit=TEST_NODE_AGENT_UNIT,
    )


# ---------------------------------------------------------------------------
# Internals


def _reset_failed(runner: NodeShellRunner, *units: str) -> None:
    """``systemctl reset-failed`` for *units*. No-ops cleanly when
    a unit isn't actually in the failed state. We do this before
    the first enable in a deploy pass because corvus-test.service
    Wants both agents — even with the units installed-but-not-
    enabled at image-bake time, an earlier deploy iteration that
    crashed could have left ``corvus-netd.service`` in StartLimit
    hold, and ``enable --now`` refuses to honour the start with
    a 'attempted too often' error in that case.
    """

    if not units:
        return
    runner.run(
        ["systemctl", "reset-failed", *units],
        sudo=True,
        check=False,
    )


def _write_host_cert_dir(
    ca_ctx: CaContext, root: Path, node_name: str
) -> Path:
    """Write the trio that ``corvus_client._tls.build_client_bundle``
    expects under ``<root>/<node_name>/``. Returns that path so
    callers can plug it into ``Client(cert_dir=…)``.

    Same filename convention as :class:`corvus_admin.deploy.deploy_client`:

      * ``ca.crt``                     (mode 0644)
      * ``corvus-client.crt``          (mode 0644)
      * ``corvus-client.key``          (mode 0600)
    """

    dir = root / node_name
    dir.mkdir(parents=True, exist_ok=True)
    (dir / "ca.crt").write_bytes(ca_ctx.ca_pem)
    (dir / "corvus-client.crt").write_bytes(ca_ctx.client_cert_pem)
    (dir / "corvus-client.key").write_bytes(ca_ctx.client_key_pem)
    (dir / "corvus-client.key").chmod(0o600)
    return dir
