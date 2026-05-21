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

import datetime as dt
import uuid
from dataclasses import dataclass
from pathlib import Path

from corvus_admin import ca, store
from corvus_admin import systemd as systemd_mod
from corvus_admin.runner import Runner, for_target

# Default renewal window. Refusing to renew earlier than this stops
# operators from rolling certs constantly for no reason; --force
# bypasses it for the "I think a key leaked, get me out of here"
# case.
RENEW_WINDOW = dt.timedelta(days=30)


# System install paths. Keep these constants in sync with the
# Haskell-side default search path (Corvus.Tls.defaultCertSearchPath
# tries $XDG_CONFIG_HOME/corvus first, then /etc/corvus).
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
    issued.record.user_service = user_service
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
    issued.record.user_service = user_service
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
    issued.record.user_service = user_service
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
    _local_atomic_write(client_dir / "corvus-client.crt", issued.cert_pem, mode=0o644)
    _local_atomic_write(client_dir / "corvus-client.key", issued.key_pem, mode=0o600)

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
    mode: systemd_mod.InstallMode = "user" if plan.user_service else "system"
    # `enable --now` is friendlier than `restart` for first-time
    # deploys (the unit may not be enabled yet); follow it with an
    # explicit restart to pick up any cert rotation.
    systemd_mod.enable_now(runner, unit=plan.service_unit, mode=mode)
    systemd_mod.restart(runner, unit=plan.service_unit, mode=mode)


# ---------------------------------------------------------------------------
# Renewal


class RenewError(RuntimeError):
    """Raised when renewal can't proceed: cert isn't due yet, no
    matching record exists, or the target label can't be reused."""


def find_record(
    admin_store: store.AdminStore,
    *,
    role: str,
    name: str | None = None,
) -> store.IssuedRecord:
    """Find an issued cert by role and (for non-daemon roles) name.
    Raises :class:`RenewError` when nothing matches. The daemon
    is a special case: there's only one row with role
    ``corvus-daemon``, so the name is ignored."""

    records = list(admin_store.iter_records())
    if role == ca.ROLE_DAEMON:
        matches = [r for r in records if r.role == role]
        if len(matches) > 1:
            raise RenewError(
                "multiple daemon records exist; admin store should have at "
                f"most one — found {len(matches)}"
            )
    else:
        if name is None:
            raise ValueError(f"role {role!r} requires a name argument")
        matches = [r for r in records if r.role == role and r.name_or_uuid == name]
    if not matches:
        raise RenewError(
            f"no issued cert for role={role!r}"
            + (f" name={name!r}" if name is not None else "")
        )
    return matches[0]


def runner_label_to_target(label: str) -> str:
    """Reverse-engineer a runner target from the label we stored
    in :attr:`IssuedRecord.deployed_to`. ``"local"`` stays
    ``"local"``; ``"ssh:user@host"`` becomes ``"user@host"``;
    ``"local:<path>"`` (used for client deploys) raises — clients
    don't go through a runner."""

    if label == "local":
        return "local"
    if label.startswith("ssh:"):
        return label[len("ssh:") :]
    if label.startswith("local:"):
        raise RenewError(
            f"deploy target {label!r} is a local file path, not a runner "
            f"target — use `deploy client` to re-mint a client cert"
        )
    raise RenewError(f"can't reuse target label {label!r} for renew")


def needs_renewal(
    record: store.IssuedRecord, *, now: dt.datetime | None = None
) -> bool:
    """True iff *record* expires within :data:`RENEW_WINDOW`. The
    CLI uses this both for the default behaviour (refuse if still
    well-in-date) and the documented ``--auto`` filter (Phase 4
    follow-up)."""

    now = now or dt.datetime.now(dt.timezone.utc)
    expires = dt.datetime.fromisoformat(record.expires_at)
    return (expires - now) <= RENEW_WINDOW


def renew_daemon(
    admin_store: store.AdminStore,
    *,
    target: str | None = None,
    force: bool = False,
) -> DeployPlan:
    """Re-mint + redeploy the daemon cert. Reuses the existing
    daemon UUID so the identity stays stable across renewals.
    ``target`` defaults to the original deploy target (parsed off
    ``IssuedRecord.deployed_to``); pass an explicit value to
    rehome the daemon to a different host (rare).
    """

    rec = find_record(admin_store, role=ca.ROLE_DAEMON)
    _check_due(rec, force=force)
    runner = for_target(target or runner_label_to_target(rec.deployed_to or ""))
    # rec.user_service is None for legacy records (predates the
    # field). Default to False (system-service) — that's what the
    # old code unconditionally assumed.
    return deploy_daemon(
        admin_store,
        runner,
        listen_ip=rec.ip,
        user_service=bool(rec.user_service),
        reuse_uuid=rec.name_or_uuid,
    )


def renew_node(
    admin_store: store.AdminStore,
    *,
    name: str,
    target: str | None = None,
    force: bool = False,
) -> DeployPlan:
    rec = find_record(admin_store, role=ca.ROLE_NODE, name=name)
    _check_due(rec, force=force)
    runner = for_target(target or runner_label_to_target(rec.deployed_to or ""))
    return deploy_node(
        admin_store,
        runner,
        name=name,
        ip=rec.ip,
        user_service=bool(rec.user_service),
    )


def renew_netd(
    admin_store: store.AdminStore,
    *,
    name: str,
    target: str | None = None,
    force: bool = False,
) -> DeployPlan:
    rec = find_record(admin_store, role=ca.ROLE_NETD, name=name)
    _check_due(rec, force=force)
    runner = for_target(target or runner_label_to_target(rec.deployed_to or ""))
    return deploy_netd(
        admin_store,
        runner,
        name=name,
        ip=rec.ip,
        user_service=bool(rec.user_service),
    )


def renew_client(
    admin_store: store.AdminStore,
    *,
    name: str,
    force: bool = False,
) -> store.IssuedRecord:
    rec = find_record(admin_store, role=ca.ROLE_CLIENT, name=name)
    _check_due(rec, force=force)
    return deploy_client(admin_store, name=name)


def _check_due(record: store.IssuedRecord, *, force: bool) -> None:
    if force:
        return
    if not needs_renewal(record):
        expires = dt.datetime.fromisoformat(record.expires_at)
        remaining = expires - dt.datetime.now(dt.timezone.utc)
        days = max(0, int(remaining.total_seconds() // 86400))
        raise RenewError(
            f"cert {record.cn} is still valid for {days} days "
            f"(renewal window is {RENEW_WINDOW.days}d); pass --force to renew anyway"
        )


def _local_atomic_write(path: Path, data: bytes, *, mode: int) -> None:
    import os

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
