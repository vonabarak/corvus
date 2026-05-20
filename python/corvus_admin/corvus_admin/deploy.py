"""High-level deploy recipes.

Each ``deploy_*`` function mints (or reuses) the role's cert,
drops the three files (ca.crt, <role>.crt, <role>.key) into the
target's cert dir, fixes permissions, and (re)starts the relevant
systemd unit. The runner abstraction (local vs SSH) keeps the
recipes generic.

Conventions:

* System paths default to ``/etc/corvus/``. The ``--user-service``
  variant lands under ``~/.config/corvus/`` instead — that
  matches the current ``make install`` flow where the daemon runs
  as a user systemd service.
* Cert files are mode 0644; key files are mode 0600. We do **not**
  chown the key to a corvus group; the deploy step assumes the
  daemon / agent runs as whichever user owns the install. If the
  operator wants a stricter regime they'll bake it into the
  systemd unit's ``User=`` directive.
* Restart commands fail loudly. ``corvus-admin deploy …`` is meant
  to be the operator's smoke test of the whole pipeline; a
  silent restart that did nothing would be worse than a clear
  error.
"""

from __future__ import annotations

import uuid
from dataclasses import dataclass

from corvus_admin import ca, store
from corvus_admin.runner import Runner


# System install paths. Keep these constants in sync with the
# Haskell-side default search path (Corvus.Tls.defaultCertSearchPath
# tries /etc/corvus first, then $XDG_CONFIG_HOME/corvus).
SYSTEM_CERT_DIR = "/etc/corvus"
SYSTEM_CERT_MODE = 0o644
SYSTEM_KEY_MODE = 0o600
SYSTEM_DIR_MODE = 0o755


@dataclass
class DeployPlan:
    """What deploy_* is about to do, before it does it. Useful for
    `corvus-admin deploy --dry-run` (Phase 4) and for tests."""

    role: str
    name: str
    target: str
    cert_dir: str
    service_unit: str
    user_service: bool


# ---------------------------------------------------------------------------
# Daemon


def deploy_daemon(
    admin_store: store.AdminStore,
    runner: Runner,
    *,
    listen_ip: str | None,
    user_service: bool = False,
    reuse_uuid: str | None = None,
) -> DeployPlan:
    """Mint a daemon cert (CN ``corvus-daemon:<uuid>``) and deploy
    it to *runner*'s target. The UUID is fresh each call unless
    *reuse_uuid* is passed; rotating the daemon's identity by
    accident would orphan every node row, so the CLI surfaces
    this as ``--rotate-identity``.
    """

    if reuse_uuid is not None:
        daemon_uuid = reuse_uuid
    else:
        daemon_uuid = str(uuid.uuid4())

    issued = ca.issue_cert(
        admin_store,
        role=ca.ROLE_DAEMON,
        name=daemon_uuid,
        ip=listen_ip,
    )
    plan = _plan(
        role=ca.ROLE_DAEMON,
        name=daemon_uuid,
        target=runner.label,
        user_service=user_service,
        service_unit="corvus.service",
    )
    _drop_cert_trio(admin_store, runner, issued, plan)
    _systemd_restart(runner, plan)
    issued.record.deployed_to = runner.label
    admin_store.record(issued.record)
    return plan


# ---------------------------------------------------------------------------
# Node agent


def deploy_node(
    admin_store: store.AdminStore,
    runner: Runner,
    *,
    name: str,
    ip: str | None,
    user_service: bool = False,
) -> DeployPlan:
    """Mint and deploy a ``corvus-node:<name>`` cert."""

    issued = ca.issue_cert(
        admin_store,
        role=ca.ROLE_NODE,
        name=name,
        ip=ip,
    )
    plan = _plan(
        role=ca.ROLE_NODE,
        name=name,
        target=runner.label,
        user_service=user_service,
        service_unit="corvus-nodeagent.service",
    )
    _drop_cert_trio(admin_store, runner, issued, plan)
    _systemd_restart(runner, plan)
    issued.record.deployed_to = runner.label
    admin_store.record(issued.record)
    return plan


# ---------------------------------------------------------------------------
# Netd


def deploy_netd(
    admin_store: store.AdminStore,
    runner: Runner,
    *,
    name: str,
    ip: str | None,
    user_service: bool = False,
) -> DeployPlan:
    """Mint and deploy a ``corvus-netd:<name>`` cert."""

    issued = ca.issue_cert(
        admin_store,
        role=ca.ROLE_NETD,
        name=name,
        ip=ip,
    )
    plan = _plan(
        role=ca.ROLE_NETD,
        name=name,
        target=runner.label,
        user_service=user_service,
        service_unit="corvus-netd.service",
    )
    _drop_cert_trio(admin_store, runner, issued, plan)
    _systemd_restart(runner, plan)
    issued.record.deployed_to = runner.label
    admin_store.record(issued.record)
    return plan


# ---------------------------------------------------------------------------
# Client (admin's own client cert)


def deploy_client(
    admin_store: store.AdminStore,
    *,
    name: str,
) -> store.IssuedRecord:
    """Mint a client cert and drop it into the admin's own
    ``$XDG_CONFIG_HOME/corvus/`` so ``crv`` finds it without
    further flags. No service to restart, no remote — client
    certs are always local.

    The plan documents client certs as XDG-only (never
    /etc/corvus); this is enforced here by going through
    :func:`store.default_client_dir` and not the runner.
    """

    from pathlib import Path

    issued = ca.issue_cert(
        admin_store,
        role=ca.ROLE_CLIENT,
        name=name,
        ip=None,
    )
    client_dir = store.default_client_dir()
    client_dir.mkdir(mode=0o700, parents=True, exist_ok=True)

    ca_pem = ca.ca_cert_pem(admin_store)
    _local_atomic_write(client_dir / "ca.crt", ca_pem, mode=0o644)
    _local_atomic_write(
        client_dir / "corvus-client.crt", issued.cert_pem, mode=0o644
    )
    _local_atomic_write(
        client_dir / "corvus-client.key", issued.key_pem, mode=0o600
    )

    issued.record.deployed_to = f"local:{client_dir}"
    admin_store.record(issued.record)
    return issued.record


# ---------------------------------------------------------------------------
# Internals


def _plan(
    *,
    role: str,
    name: str,
    target: str,
    user_service: bool,
    service_unit: str,
) -> DeployPlan:
    if user_service:
        # Match the user-systemd layout produced by `make
        # install`: cert dir under ~/.config/corvus, unit under
        # `systemctl --user`.
        cert_dir = "~/.config/corvus"
    else:
        cert_dir = SYSTEM_CERT_DIR
    return DeployPlan(
        role=role,
        name=name,
        target=target,
        cert_dir=cert_dir,
        service_unit=service_unit,
        user_service=user_service,
    )


def _drop_cert_trio(
    admin_store: store.AdminStore,
    runner: Runner,
    issued: ca.IssuedCert,
    plan: DeployPlan,
) -> None:
    """Push ca.crt + <role>.crt + <role>.key into the target's
    cert dir with the right modes. The filenames match the
    Haskell-side ``roleFilename`` constants in ``Corvus.Tls``."""

    sudo = not plan.user_service
    runner.mkdir_p(plan.cert_dir, mode=SYSTEM_DIR_MODE, sudo=sudo)

    cert_basename = issued.record.role  # e.g. "corvus-daemon"
    ca_pem = ca.ca_cert_pem(admin_store)

    runner.copy_bytes(
        ca_pem,
        f"{plan.cert_dir}/ca.crt",
        mode=SYSTEM_CERT_MODE,
    )
    runner.copy_bytes(
        issued.cert_pem,
        f"{plan.cert_dir}/{cert_basename}.crt",
        mode=SYSTEM_CERT_MODE,
    )
    runner.copy_bytes(
        issued.key_pem,
        f"{plan.cert_dir}/{cert_basename}.key",
        mode=SYSTEM_KEY_MODE,
    )


def _systemd_restart(runner: Runner, plan: DeployPlan) -> None:
    if plan.user_service:
        # `--user` units; no sudo. The unit may not be enabled
        # yet (fresh install), so `enable --now` is friendlier
        # than `restart` for first-time deploys.
        runner.run(
            ["systemctl", "--user", "enable", "--now", plan.service_unit],
            sudo=False,
        )
        runner.run(
            ["systemctl", "--user", "restart", plan.service_unit],
            sudo=False,
        )
    else:
        runner.run(
            ["systemctl", "enable", "--now", plan.service_unit],
            sudo=True,
        )
        runner.run(
            ["systemctl", "restart", plan.service_unit],
            sudo=True,
        )


def _local_atomic_write(path, data: bytes, *, mode: int) -> None:
    import os
    from pathlib import Path

    path = Path(path)
    path.parent.mkdir(mode=0o700, parents=True, exist_ok=True)
    tmp = path.with_suffix(path.suffix + ".tmp")
    fd = os.open(str(tmp), os.O_WRONLY | os.O_CREAT | os.O_TRUNC, mode)
    try:
        with os.fdopen(fd, "wb") as f:
            f.write(data)
    except Exception:
        try:
            os.unlink(tmp)
        finally:
            raise
    os.replace(tmp, path)
    os.chmod(path, mode)
