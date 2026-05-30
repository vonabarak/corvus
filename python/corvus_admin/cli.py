"""Click-based CLI for corvus-admin.

Subcommands shipped:

* ``quickstart``         — one-shot single-node bring-up
* ``init``               — generate CA + admin client cert
* ``deploy daemon``      — mint daemon cert + push + restart
* ``deploy node``        — mint node-agent cert + push + restart
* ``deploy netd``        — mint netd cert + push + restart
* ``deploy client``      — mint admin's own client cert (local)
* ``renew {daemon,node,netd,client}`` — re-mint + redeploy
* ``renew --due``        — sweep the admin store, renew everything
                            expiring within ``--within`` days
* ``register``           — `crv node add` shortcut
* ``list``               — show every issued cert and its expiry
* ``status``             — probe each cert's deploy target over TLS
* ``revoke <CN>``        — drop a cert from the admin-store index
* ``completion <SHELL>`` — emit a Click completion script

All ``deploy``/``renew`` subcommands accept ``--dry-run`` for a
no-side-effects preview; ``deploy node`` / ``deploy netd`` accept
``--targets t1,t2,…`` or ``@hosts.txt`` for parallel multi-host
rollouts. ``list`` and ``status`` accept ``--output json`` for
script-friendly output.

Deferred to later phases per the plan: ``ca rotate``.
"""

from __future__ import annotations

import dataclasses
import getpass
import json
import sys
from collections.abc import Callable
from pathlib import Path

import click
from click.shell_completion import BashComplete, FishComplete, ZshComplete

from corvus_admin import (
    ca,
    deploy,
    store,
)
from corvus_admin import (
    quickstart as quickstart_mod,
)
from corvus_admin import (
    register as register_mod,
)
from corvus_admin import (
    status as status_mod,
)
from corvus_admin.runner import RunnerError, for_target

# ---------------------------------------------------------------------------
# Shared options


def _ca_dir_option(f: click.decorators.FC) -> click.decorators.FC:
    return click.option(
        "--ca-dir",
        "ca_dir",
        type=click.Path(file_okay=False, path_type=Path),
        default=None,
        show_default=False,
        help=(
            "Admin-store directory (CA cert+key + index). "
            "Default: $XDG_CONFIG_HOME/corvus/admin."
        ),
    )(f)


def _dry_run_option(f: click.decorators.FC) -> click.decorators.FC:
    return click.option(
        "--dry-run/--no-dry-run",
        "dry_run",
        default=False,
        help=(
            "Print what would be deployed but don't mint a cert, push "
            "files, or restart any unit. Useful before touching prod."
        ),
    )(f)


def _targets_option(f: click.decorators.FC) -> click.decorators.FC:
    return click.option(
        "--targets",
        "targets_csv",
        default=None,
        show_default=False,
        help=(
            "Comma-separated list of targets to deploy to in parallel. "
            "Mutually exclusive with the TARGET positional. Each target follows "
            "the same `local` / `[user@]host[:port]` syntax."
        ),
    )(f)


def _allow_shared_cn_option(f: click.decorators.FC) -> click.decorators.FC:
    return click.option(
        "--allow-shared-cn",
        is_flag=True,
        default=False,
        help=(
            "Deploy the same CN to every target. By default a multi-target "
            "deploy with a single name is refused — see `doc/multi-node.md`."
        ),
    )(f)


def _resolve_targets(target: str | None, targets_csv: str | None) -> list[str]:
    """Decode an argv target into the list of targets to deploy to.

    * ``--targets t1,t2,…`` wins when set; mutually exclusive with the
      positional.
    * A positional starting with ``@`` is read as a file of one
      target per line (blank lines and ``#`` comments are skipped).
    * Anything else is a single literal target.
    """

    if target is not None and targets_csv is not None:
        raise click.UsageError("pass either TARGET or --targets, not both")
    if targets_csv is not None:
        targets = [t.strip() for t in targets_csv.split(",") if t.strip()]
    elif target is None:
        raise click.UsageError("missing TARGET (or --targets)")
    elif target.startswith("@"):
        targets = _read_targets_file(target[1:])
    else:
        targets = [target]
    if not targets:
        raise click.UsageError("no targets resolved")
    return targets


def _read_targets_file(path: str) -> list[str]:
    targets: list[str] = []
    with open(path) as f:
        for line in f:
            s = line.strip()
            if not s or s.startswith("#"):
                continue
            targets.append(s)
    return targets


def _multi_target_deploy(
    targets: list[str],
    deploy_one: Callable[[str], str | None],
    *,
    label: str,
    allow_shared_cn: bool,
) -> int:
    """Run ``deploy_one(target)`` for each target concurrently.

    ``deploy_one`` returns ``None`` on success or a ``str`` error
    message on failure. Returns the count of failed targets (so
    callers can map it onto an exit code)."""

    if len(targets) > 1 and not allow_shared_cn:
        raise click.UsageError(
            f"refusing to deploy the same {label} cert to {len(targets)} hosts; "
            "pass --allow-shared-cn to override (see `doc/multi-node.md`)."
        )
    if len(targets) == 1:
        # Single-target fast path keeps the existing per-target
        # exit-code semantics for the common case.
        err = deploy_one(targets[0])
        return 0 if err is None else 1

    from concurrent.futures import ThreadPoolExecutor

    failed = 0
    with ThreadPoolExecutor(max_workers=min(len(targets), 16)) as pool:
        futs = {pool.submit(deploy_one, t): t for t in targets}
        for fut in futs:
            target = futs[fut]
            try:
                err = fut.result()
            except Exception as e:
                err = f"{type(e).__name__}: {e}"
            if err is None:
                click.echo(f"  {target}: ok")
            else:
                click.echo(f"  {target}: FAILED — {err}", err=True)
                failed += 1
    return failed


def _output_option(f: click.decorators.FC) -> click.decorators.FC:
    return click.option(
        "--output",
        "output",
        type=click.Choice(["text", "json"]),
        default="text",
        show_default=True,
        help="Output format. `json` emits an array of records; `text` is the human-readable table.",
    )(f)


def _resolve_store(ca_dir: Path | None) -> store.AdminStore:
    root = ca_dir if ca_dir is not None else store.default_admin_dir()
    return store.AdminStore(root)


# ---------------------------------------------------------------------------
# Root group


@click.group(context_settings={"help_option_names": ["-h", "--help"]})
@click.version_option(package_name="corvus-admin")
def main() -> None:
    """Manage Corvus mTLS material from the admin workstation."""


# ---------------------------------------------------------------------------
# init


@main.command()
@_ca_dir_option
@click.option(
    "--admin-name",
    default=None,
    show_default=False,
    help="Name embedded in the admin's client cert CN. Default: $USER.",
)
@click.option(
    "--force/--no-force",
    default=False,
    help="Overwrite an existing CA. Use only when rotating; this orphans every previously issued cert.",
)
def init(ca_dir: Path | None, admin_name: str | None, force: bool) -> None:
    """Generate a CA + admin client cert in the admin store.

    Idempotent — re-running without ``--force`` is a no-op when the
    store is already populated. The admin's own client cert lands
    in ``$XDG_CONFIG_HOME/corvus/`` so ``crv`` picks it up
    automatically.
    """

    st = _resolve_store(ca_dir)
    if st.exists() and not force:
        click.echo(
            f"CA already initialised at {st.root}. Re-run with --force to rotate.",
            err=True,
        )
        sys.exit(0)

    ca.init_ca(st, force=force)
    click.echo(f"CA initialised at {st.root}")

    name = admin_name or _default_admin_name()
    rec = deploy.deploy_client(st, name=name)
    assert rec is not None  # init never passes dry_run=True
    click.echo(
        f"Issued client cert {rec.cn} (expires {rec.expires_at}); "
        f"dropped into {store.default_client_dir()}."
    )


def _default_admin_name() -> str:
    try:
        return getpass.getuser()
    except Exception:
        return "admin"


# ---------------------------------------------------------------------------
# quickstart


@main.command("quickstart")
@_ca_dir_option
@click.option(
    "--node-name",
    default=None,
    show_default=False,
    help="Name to register this node under. Default: hostname (short form).",
)
@click.option(
    "--listen-ip",
    default="127.0.0.1",
    show_default=True,
    help="IP the daemon TCP listener binds to and that the daemon dials back through.",
)
@click.option(
    "--base-path",
    default=None,
    show_default=False,
    help="Per-node disk base path. Default: ~/VMs.",
)
@click.option(
    "--database-url",
    default="postgresql://localhost/corvus",
    show_default=True,
    help="Postgres connection URL baked into the generated systemd unit.",
)
@click.option(
    "--skip-netd",
    is_flag=True,
    default=False,
    help="Don't install corvus-netd even when sudo/doas is available.",
)
@click.option(
    "--skip-web",
    is_flag=True,
    default=False,
    help="Don't install corvus-web. By default quickstart brings it up as a user service bound to 127.0.0.1:8080.",
)
@click.option(
    "--web-bind-host",
    default="127.0.0.1",
    show_default=True,
    help="corvus-web HTTP bind host. Use 0.0.0.0 to expose on a routable address (firewall first!).",
)
@click.option(
    "--web-bind-port",
    type=int,
    default=8080,
    show_default=True,
    help="corvus-web HTTP bind port.",
)
@click.option(
    "--force/--no-force",
    default=False,
    help="Overwrite an existing CA. Orphans previously issued certs.",
)
@click.option(
    "--healthcheck-timeout",
    type=float,
    default=15.0,
    show_default=True,
    help="Seconds to wait for the daemon's first healthcheck push after register.",
)
def quickstart(
    ca_dir: Path | None,
    node_name: str | None,
    listen_ip: str,
    base_path: str | None,
    database_url: str,
    skip_netd: bool,
    skip_web: bool,
    web_bind_host: str,
    web_bind_port: int,
    force: bool,
    healthcheck_timeout: float,
) -> None:
    """Bring up a one-node Corvus install in a single command.

    Generates the CA + all component certs, writes systemd unit
    files (user-mode for daemon + nodeagent, system-mode for netd),
    starts the services, and registers the node with the daemon.

    Auto-detects sudo or doas for privilege escalation. When
    neither is available, skips corvus-netd and prints a warning —
    daemon + nodeagent come up as user services with no network
    management.
    """

    try:
        result = quickstart_mod.run(
            node_name=node_name,
            listen_ip=listen_ip,
            base_path=base_path,
            ca_dir=ca_dir,
            skip_netd=skip_netd,
            skip_web=skip_web,
            web_bind_host=web_bind_host,
            web_bind_port=web_bind_port,
            force=force,
            healthcheck_timeout=healthcheck_timeout,
            database_url=database_url,
            log_callback=lambda m: click.echo(m),
        )
    except quickstart_mod.QuickstartError as e:
        click.echo(f"quickstart failed: {e}", err=True)
        sys.exit(1)

    click.echo("")
    click.echo("=" * 60)
    click.echo("Corvus quickstart complete.")
    click.echo(f"  Node:           {result.node_name}")
    click.echo(f"  Listen IP:      {result.listen_ip}")
    click.echo(
        f"  Privesc tool:   {result.privesc_tool or 'none (corvus-netd skipped)'}"
    )
    click.echo(f"  Daemon cert:    {result.daemon_cert_cn}")
    click.echo(f"  Node cert:      {result.node_cert_cn}")
    if result.netd_cert_cn:
        click.echo(f"  Netd cert:      {result.netd_cert_cn}")
    click.echo(f"  Client cert:    {result.client_cert_cn}")
    if result.web_installed:
        click.echo(f"  Web UI:         http://{result.web_bind}/")
    click.echo(
        f"  Healthcheck:    {'received' if result.healthy else 'pending (see logs)'}"
    )
    click.echo("=" * 60)
    sys.exit(0 if result.healthy else 2)


# ---------------------------------------------------------------------------
# deploy


@main.group()
def deploy_group() -> None:
    """Deploy a component's cert + restart its systemd unit.

    The ``<target>`` argument is either ``local`` (run everything
    in-process on the admin's workstation, no SSH) or a standard
    ``[user@]host[:port]`` SSH target.
    """


# click registers the command under the function's name; rename
# so the public verb stays ``deploy``.
main.add_command(deploy_group, name="deploy")


@deploy_group.command("daemon")
@_ca_dir_option
@click.argument("target")
@click.option(
    "--listen-ip",
    default=None,
    show_default=False,
    help="IP the daemon's TCP listener will bind to. Embedded as an IP SAN in the daemon's cert.",
)
@click.option(
    "--user-service/--system-service",
    default=False,
    help="Use the systemd `--user` flow (cert dir ~/.config/corvus, unit started with systemctl --user). Default: system service in /etc/corvus.",
)
@click.option(
    "--binary-path",
    default=None,
    show_default=False,
    help="Path to the corvus binary on the target. Default: ~/.local/bin/corvus (user-service) or /usr/local/bin/corvus (system-service).",
)
@click.option(
    "--database-url",
    default="postgresql://localhost/corvus",
    show_default=True,
    help="Postgres connection URL baked into the rendered systemd unit.",
)
@click.option(
    "--log-level",
    default="info",
    show_default=True,
    help="Log level passed to the daemon via --log-level.",
)
@click.option(
    "--install-unit/--no-install-unit",
    default=True,
    show_default=True,
    help="Render and install the systemd unit on the target. Pass --no-install-unit to keep a pre-existing custom unit; the deploy still pushes certs and restarts the service.",
)
@_dry_run_option
def deploy_daemon(
    ca_dir: Path | None,
    target: str,
    listen_ip: str | None,
    user_service: bool,
    binary_path: str | None,
    database_url: str,
    log_level: str,
    install_unit: bool,
    dry_run: bool,
) -> None:
    """Mint and deploy the daemon cert."""

    st = _ensure_initialised(ca_dir)
    runner = for_target(target)
    try:
        plan = deploy.deploy_daemon(
            st,
            runner,
            listen_ip=listen_ip,
            user_service=user_service,
            binary_path=binary_path,
            database_url=database_url,
            log_level=log_level,
            install_unit=install_unit,
            dry_run=dry_run,
        )
    except RunnerError as e:
        click.echo(f"deploy daemon failed: {e}", err=True)
        if e.stderr:
            click.echo(e.stderr, err=True)
        sys.exit(1)
    _emit_deploy_result("daemon", plan, dry_run=dry_run)


@deploy_group.command("node")
@_ca_dir_option
@click.argument("name")
@click.argument("target", required=False)
@_targets_option
@_allow_shared_cn_option
@click.option(
    "--ip",
    default=None,
    show_default=False,
    help="IP the daemon will dial for this node. Embedded as an IP SAN in the node's cert.",
)
@click.option(
    "--user-service/--system-service",
    default=False,
    help="Use the systemd `--user` flow. Default: system service.",
)
@click.option(
    "--binary-path",
    default=None,
    show_default=False,
    help="Path to the corvus-nodeagent binary on the target. Default: ~/.local/bin/corvus-nodeagent (user-service) or /usr/local/bin/corvus-nodeagent (system-service).",
)
@click.option(
    "--log-level",
    default="info",
    show_default=True,
    help="Log level passed to the nodeagent via --log-level.",
)
@click.option(
    "--install-unit/--no-install-unit",
    default=True,
    show_default=True,
    help="Render and install the systemd unit on the target. Pass --no-install-unit to keep a pre-existing custom unit.",
)
@_dry_run_option
def deploy_node(
    ca_dir: Path | None,
    name: str,
    target: str | None,
    targets_csv: str | None,
    allow_shared_cn: bool,
    ip: str | None,
    user_service: bool,
    binary_path: str | None,
    log_level: str,
    install_unit: bool,
    dry_run: bool,
) -> None:
    """Mint and deploy a corvus-nodeagent cert for the named node.

    TARGET is `local`, an SSH target, or `@hosts.txt` for bulk multi-host
    deploys (one target per line). For an explicit list use `--targets t1,t2`.
    """

    st = _ensure_initialised(ca_dir)
    targets = _resolve_targets(target, targets_csv)

    def deploy_one(t: str) -> str | None:
        runner = for_target(t)
        try:
            plan = deploy.deploy_node(
                st,
                runner,
                name=name,
                ip=ip,
                user_service=user_service,
                binary_path=binary_path,
                log_level=log_level,
                install_unit=install_unit,
                dry_run=dry_run,
            )
        except RunnerError as e:
            return str(e) + (f" — {e.stderr.strip()}" if e.stderr else "")
        if len(targets) == 1:
            _emit_deploy_result("node", plan, dry_run=dry_run)
        return None

    failed = _multi_target_deploy(
        targets, deploy_one, label="node", allow_shared_cn=allow_shared_cn
    )
    if failed:
        sys.exit(1)


@deploy_group.command("netd")
@_ca_dir_option
@click.argument("name")
@click.argument("target", required=False)
@_targets_option
@_allow_shared_cn_option
@click.option(
    "--ip", default=None, show_default=False, help="IP SAN for the netd cert."
)
@click.option(
    "--binary-path",
    default=None,
    show_default=False,
    help="Path to the corvus-netd binary on the target. Default: /usr/local/bin/corvus-netd.",
)
@click.option(
    "--log-level",
    default="info",
    show_default=True,
    help="Log level passed to corvus-netd via --log-level.",
)
@click.option(
    "--install-unit/--no-install-unit",
    default=True,
    show_default=True,
    help="Render and install the systemd unit on the target. Pass --no-install-unit to keep a pre-existing custom unit.",
)
@_dry_run_option
def deploy_netd(
    ca_dir: Path | None,
    name: str,
    target: str | None,
    targets_csv: str | None,
    allow_shared_cn: bool,
    ip: str | None,
    binary_path: str | None,
    log_level: str,
    install_unit: bool,
    dry_run: bool,
) -> None:
    """Mint and deploy a corvus-netd cert for the named node.

    netd needs CAP_NET_ADMIN and is always installed as a system
    service; no --user-service flag is offered.

    TARGET is `local`, an SSH target, or `@hosts.txt` for bulk multi-host
    deploys (one target per line). For an explicit list use `--targets t1,t2`.
    """

    st = _ensure_initialised(ca_dir)
    targets = _resolve_targets(target, targets_csv)

    def deploy_one(t: str) -> str | None:
        runner = for_target(t)
        try:
            plan = deploy.deploy_netd(
                st,
                runner,
                name=name,
                ip=ip,
                user_service=False,
                binary_path=binary_path,
                log_level=log_level,
                install_unit=install_unit,
                dry_run=dry_run,
            )
        except RunnerError as e:
            return str(e) + (f" — {e.stderr.strip()}" if e.stderr else "")
        if len(targets) == 1:
            _emit_deploy_result("netd", plan, dry_run=dry_run)
        return None

    failed = _multi_target_deploy(
        targets, deploy_one, label="netd", allow_shared_cn=allow_shared_cn
    )
    if failed:
        sys.exit(1)


@deploy_group.command("client")
@_ca_dir_option
@click.argument("name")
@_dry_run_option
def deploy_client(ca_dir: Path | None, name: str, dry_run: bool) -> None:
    """Mint an admin client cert and drop it into $XDG_CONFIG_HOME/corvus."""

    st = _ensure_initialised(ca_dir)
    if dry_run:
        click.echo(
            f"[DRY-RUN] would issue client cert corvus-client:{name}; "
            f"would drop into {store.default_client_dir()}."
        )
        return
    rec = deploy.deploy_client(st, name=name)
    assert rec is not None  # dry_run=False path always returns a record
    click.echo(
        f"Issued client cert {rec.cn} (expires {rec.expires_at}); "
        f"dropped into {store.default_client_dir()}."
    )


@deploy_group.command("web")
@click.argument("target", default="local")
@click.option(
    "--user-service/--system-service",
    default=True,
    show_default=True,
    help=(
        "User-mode (~/.config/systemd/user) vs system-mode "
        "(/etc/systemd/system). corvus-web doesn't need root for the "
        "common loopback-to-local-daemon setup, so the default is user."
    ),
)
@click.option(
    "--bind-host",
    default="127.0.0.1",
    show_default=True,
    help="HTTP bind host. Use 0.0.0.0 to expose on a routable address (firewall first!).",
)
@click.option(
    "--bind-port",
    type=int,
    default=8080,
    show_default=True,
    help="HTTP bind port.",
)
@click.option(
    "--binary-path",
    default=None,
    show_default=False,
    help="Path to the corvus-web binary on the target. Default: ~/.local/bin/corvus-web (user) or /usr/local/bin/corvus-web (system).",
)
@click.option(
    "--log-level",
    default="info",
    show_default=True,
    help="Log level passed to corvus-web via --log-level.",
)
@click.option(
    "--install-unit/--no-install-unit",
    default=True,
    show_default=True,
    help="Render and install the systemd unit on the target. Pass --no-install-unit to keep a pre-existing custom unit.",
)
@_dry_run_option
def deploy_web(
    target: str,
    user_service: bool,
    bind_host: str,
    bind_port: int,
    binary_path: str | None,
    log_level: str,
    install_unit: bool,
    dry_run: bool,
) -> None:
    """Install + start the corvus-web HTTP/WS gateway as a systemd
    service. No certificate is minted — corvus-web reaches the
    daemon over its Unix socket by default and inherits the admin's
    client cert from $XDG_CONFIG_HOME/corvus for TLS-over-TCP.

    TARGET is `local` or an SSH target (same shape as deploy daemon).
    """

    runner = for_target(target)
    try:
        plan = deploy.deploy_web(
            runner,
            user_service=user_service,
            binary_path=binary_path,
            bind_host=bind_host,
            bind_port=bind_port,
            log_level=log_level,
            install_unit=install_unit,
            dry_run=dry_run,
        )
    except RunnerError as e:
        click.echo(f"deploy web failed: {e}", err=True)
        if e.stderr:
            click.echo(e.stderr, err=True)
        sys.exit(1)
    if dry_run:
        click.echo(
            f"[DRY-RUN] would deploy corvus-web to {plan.target}:{plan.cert_dir}; "
            f"would bind to {bind_host}:{bind_port}; "
            f"would restart {plan.service_unit}."
        )
    else:
        click.echo(
            f"Deployed corvus-web to {plan.target}; "
            f"listening on {bind_host}:{bind_port}; "
            f"restarted {plan.service_unit}."
        )


# ---------------------------------------------------------------------------
# renew


@main.group("renew", invoke_without_command=True)
@click.option(
    "--due",
    is_flag=True,
    default=False,
    help="Walk the admin store and renew every cert expiring within --within DAYS. Mutually exclusive with naming an explicit role.",
)
@click.option(
    "--within",
    "within_days",
    type=int,
    default=30,
    show_default=True,
    help="Days-from-expiry threshold for --due.",
)
@_ca_dir_option
@_dry_run_option
@click.pass_context
def renew_group(
    ctx: click.Context,
    due: bool,
    within_days: int,
    ca_dir: Path | None,
    dry_run: bool,
) -> None:
    """Re-mint and redeploy a cert. Refuses to renew certs that
    are still >30 days from expiry unless ``--force`` is passed.

    With ``--due``, sweeps the admin store and renews every record
    expiring within ``--within`` days; exit code is non-zero if any
    individual renewal failed.
    """

    if ctx.invoked_subcommand is not None:
        if due:
            raise click.UsageError(
                "--due cannot be combined with an explicit role subcommand"
            )
        # Defer to the named subcommand; its own --ca-dir / --dry-run
        # take precedence (the group-level flags exist only to drive
        # the --due sweep).
        return
    if not due:
        click.echo(ctx.get_help())
        ctx.exit(2)
    _run_renew_due(ca_dir=ca_dir, within_days=within_days, dry_run=dry_run)


def _run_renew_due(*, ca_dir: Path | None, within_days: int, dry_run: bool) -> None:
    import datetime as _dt

    st = _ensure_initialised(ca_dir)
    cutoff = _dt.datetime.now(_dt.timezone.utc) + _dt.timedelta(days=within_days)
    due_records = sorted(
        (
            r
            for r in st.iter_records()
            if _dt.datetime.fromisoformat(r.expires_at) <= cutoff
        ),
        key=lambda r: r.expires_at,
    )
    if not due_records:
        click.echo(f"No certs due for renewal within {within_days} days.")
        return

    failures: list[tuple[str, str]] = []
    for rec in due_records:
        try:
            _renew_one(st, rec, dry_run=dry_run)
        except (deploy.RenewError, RunnerError) as e:
            failures.append((rec.cn, str(e)))
            click.echo(f"renew {rec.cn} failed: {e}", err=True)

    summary = f"{len(due_records) - len(failures)}/{len(due_records)} renewed"
    if failures:
        click.echo(summary + f" ({len(failures)} failed); see stderr above.", err=True)
        sys.exit(1)
    click.echo(summary + ".")


# Dispatch table: role → callable that renews one record of that role.
# Keeps `_renew_one` short and obviates a 4-arm match on the role string.
_RENEW_DISPATCH = {
    ca.ROLE_DAEMON: lambda st, rec, dr: deploy.renew_daemon(st, force=True, dry_run=dr),
    ca.ROLE_NODE: lambda st, rec, dr: deploy.renew_node(
        st, name=rec.name_or_uuid, force=True, dry_run=dr
    ),
    ca.ROLE_NETD: lambda st, rec, dr: deploy.renew_netd(
        st, name=rec.name_or_uuid, force=True, dry_run=dr
    ),
    ca.ROLE_CLIENT: lambda st, rec, dr: deploy.renew_client(
        st, name=rec.name_or_uuid, force=True, dry_run=dr
    ),
}


def _renew_one(st: store.AdminStore, rec: store.IssuedRecord, *, dry_run: bool) -> None:
    """Renew one record, picking the right deploy.renew_* by role.
    Force-renews unconditionally — the caller already filtered on the
    due-window."""

    handler = _RENEW_DISPATCH.get(rec.role)
    if handler is None:
        raise deploy.RenewError(f"unknown role {rec.role!r} in record {rec.cn}")
    result = handler(st, rec, dry_run)
    if isinstance(result, deploy.DeployPlan):
        # daemon / node / netd path — DeployPlan has the redeploy target.
        verb = "would renew" if dry_run else "renewed"
        click.echo(f"  {verb} {rec.cn} (redeploy → {result.target})")
    else:
        # client path — purely local, no DeployPlan.
        verb = "would renew" if dry_run else "renewed"
        click.echo(f"  {verb} {rec.cn} (local client cert)")


@renew_group.command("daemon")
@_ca_dir_option
@click.option(
    "--target",
    default=None,
    show_default=False,
    help="Runner target (`local` or `[user@]host`). Default: reuse the original deploy target.",
)
@click.option(
    "--force/--no-force",
    default=False,
    help="Renew even when the cert is more than 30 days from expiry.",
)
@_dry_run_option
def renew_daemon(
    ca_dir: Path | None, target: str | None, force: bool, dry_run: bool
) -> None:
    st = _ensure_initialised(ca_dir)
    try:
        plan = deploy.renew_daemon(st, target=target, force=force, dry_run=dry_run)
    except (deploy.RenewError, RunnerError) as e:
        click.echo(f"renew daemon failed: {e}", err=True)
        sys.exit(1)
    _emit_renew_result("daemon", plan, dry_run=dry_run)


@renew_group.command("node")
@_ca_dir_option
@click.argument("name")
@click.option("--target", default=None, show_default=False)
@click.option("--force/--no-force", default=False)
@_dry_run_option
def renew_node(
    ca_dir: Path | None, name: str, target: str | None, force: bool, dry_run: bool
) -> None:
    st = _ensure_initialised(ca_dir)
    try:
        plan = deploy.renew_node(
            st, name=name, target=target, force=force, dry_run=dry_run
        )
    except (deploy.RenewError, RunnerError) as e:
        click.echo(f"renew node failed: {e}", err=True)
        sys.exit(1)
    _emit_renew_result("node", plan, dry_run=dry_run)


@renew_group.command("netd")
@_ca_dir_option
@click.argument("name")
@click.option("--target", default=None, show_default=False)
@click.option("--force/--no-force", default=False)
@_dry_run_option
def renew_netd(
    ca_dir: Path | None, name: str, target: str | None, force: bool, dry_run: bool
) -> None:
    st = _ensure_initialised(ca_dir)
    try:
        plan = deploy.renew_netd(
            st, name=name, target=target, force=force, dry_run=dry_run
        )
    except (deploy.RenewError, RunnerError) as e:
        click.echo(f"renew netd failed: {e}", err=True)
        sys.exit(1)
    _emit_renew_result("netd", plan, dry_run=dry_run)


@renew_group.command("client")
@_ca_dir_option
@click.argument("name")
@click.option("--force/--no-force", default=False)
@_dry_run_option
def renew_client(ca_dir: Path | None, name: str, force: bool, dry_run: bool) -> None:
    st = _ensure_initialised(ca_dir)
    try:
        rec = deploy.renew_client(st, name=name, force=force, dry_run=dry_run)
    except deploy.RenewError as e:
        click.echo(f"renew client failed: {e}", err=True)
        sys.exit(1)
    if dry_run:
        click.echo(
            f"[DRY-RUN] would renew client cert corvus-client:{name}; "
            f"would drop into {store.default_client_dir()}."
        )
        return
    assert rec is not None  # dry_run=False path always returns a record
    click.echo(
        f"Renewed client cert {rec.cn} (expires {rec.expires_at}); "
        f"dropped into {store.default_client_dir()}."
    )


# ---------------------------------------------------------------------------
# register


@main.command("register")
@click.argument("name")
@click.option("--host", required=True, help="IP / hostname the daemon will dial.")
@click.option(
    "--node-agent-port",
    type=int,
    default=register_mod.DEFAULT_NODE_AGENT_PORT,
    show_default=True,
)
@click.option(
    "--net-agent-port",
    type=int,
    default=register_mod.DEFAULT_NET_AGENT_PORT,
    show_default=True,
)
@click.option(
    "--base-path",
    default=None,
    show_default=False,
    help="Per-node base path for disk images. Daemon picks a sensible default if omitted.",
)
@click.option(
    "--description",
    default=None,
    show_default=False,
)
@click.option(
    "--crv-path",
    default=None,
    show_default=False,
    help="Path to the `crv` binary. Default: first `crv` on $PATH.",
)
@click.option(
    "--healthcheck-timeout",
    type=float,
    default=15.0,
    show_default=True,
    help="Seconds to wait for the daemon's first healthcheck push after register.",
)
def register_cmd(
    name: str,
    host: str,
    node_agent_port: int,
    net_agent_port: int,
    base_path: str | None,
    description: str | None,
    crv_path: str | None,
    healthcheck_timeout: float,
) -> None:
    """Run `crv node add <name>` and wait for the agent push."""

    try:
        result = register_mod.register_node(
            name=name,
            host=host,
            node_agent_port=node_agent_port,
            net_agent_port=net_agent_port,
            base_path=base_path,
            description=description,
            crv_path=crv_path,
            healthcheck_timeout_sec=healthcheck_timeout,
        )
    except register_mod.RegisterError as e:
        click.echo(f"register failed: {e}", err=True)
        if e.stderr:
            click.echo(e.stderr, err=True)
        sys.exit(1)
    status = "healthy" if result.healthy else "registered (no healthcheck yet)"
    click.echo(f"Node {result.name} → {result.host}:{result.node_agent_port}: {status}")


# ---------------------------------------------------------------------------
# list


@main.command("list")
@_ca_dir_option
@_output_option
def list_cmd(ca_dir: Path | None, output: str) -> None:
    """List every issued cert and where it was deployed."""

    st = _resolve_store(ca_dir)
    if not st.exists():
        click.echo(f"No CA at {st.root}. Run `corvus-admin init`.", err=True)
        sys.exit(1)
    records = sorted(st.iter_records(), key=lambda r: (r.role, r.name_or_uuid))
    if output == "json":
        click.echo(_records_to_json(records))
        return
    if not records:
        click.echo("(no certs issued yet)")
        return
    _print_table(
        ("CN", "EXPIRES", "DEPLOYED-TO", "IP"),
        [(r.cn, r.expires_at, r.deployed_to or "-", r.ip or "-") for r in records],
    )


# ---------------------------------------------------------------------------
# status


@main.command("status")
@_ca_dir_option
@_output_option
def status_cmd(ca_dir: Path | None, output: str) -> None:
    """Probe every deployed component over TLS and report
    reachability + days-to-expiry. Client certs are listed but
    not probed (no remote service to dial)."""

    st = _resolve_store(ca_dir)
    if not st.exists():
        click.echo(f"No CA at {st.root}. Run `corvus-admin init`.", err=True)
        sys.exit(1)

    reports = status_mod.probe_all(st)
    any_unhealthy = any(r.role != ca.ROLE_CLIENT and not r.reachable for r in reports)

    if output == "json":
        click.echo(_records_to_json(reports))
        sys.exit(1 if any_unhealthy else 0)

    if not reports:
        click.echo("(no certs issued yet)")
        return

    rows: list[tuple[str, ...]] = []
    for r in reports:
        if r.role == ca.ROLE_CLIENT:
            status_word = "client"
            detail = r.deployed_to or "-"
        elif r.reachable:
            status_word = "ok"
            detail = r.deployed_to or "-"
        else:
            status_word = "UNREACHABLE"
            detail = r.handshake_error or "?"
        rows.append((r.cn, str(r.days_remaining), status_word, detail))
    _print_table(("CN", "DAYS-LEFT", "STATUS", "DETAIL"), rows)
    sys.exit(1 if any_unhealthy else 0)


# ---------------------------------------------------------------------------
# revoke


@main.command("revoke")
@_ca_dir_option
@click.argument("cn")
@click.option(
    "--yes",
    "yes",
    is_flag=True,
    default=False,
    help="Skip the confirmation prompt.",
)
def revoke_cmd(ca_dir: Path | None, cn: str, yes: bool) -> None:
    """Remove a cert from the admin store's index.

    The matching component on its target host keeps authenticating
    with its existing cert until you redeploy or renew. Corvus does
    not consume a CRL — `revoke` is pure bookkeeping so the next
    `corvus-admin list` no longer shows the row.
    """

    st = _ensure_initialised(ca_dir)
    idx = st.load_index()
    if cn not in idx:
        click.echo(f"No such cert in index: {cn}", err=True)
        sys.exit(1)
    if not yes and not click.confirm(
        f"Revoke {cn}? (the component on {idx[cn].deployed_to or '?'} "
        f"will keep using its existing cert until next deploy/renew)",
        default=False,
    ):
        click.echo("Aborted.")
        return
    rec = st.remove_record(cn)
    click.echo(
        f"Revoked {rec.cn} from the admin store. "
        f"Issue a fresh cert with `corvus-admin deploy {rec.role.split('-', 1)[1]}` "
        f"when you're ready to replace it."
    )


# ---------------------------------------------------------------------------
# completion


_COMPLETERS = {"bash": BashComplete, "zsh": ZshComplete, "fish": FishComplete}


@main.command("completion")
@click.argument("shell", type=click.Choice(list(_COMPLETERS)))
def completion_cmd(shell: str) -> None:
    """Emit a shell-completion script for SHELL (bash, zsh, fish).

    Pipe the output into the appropriate location for your shell, e.g.:

      corvus-admin completion bash > ~/.local/share/bash-completion/completions/corvus-admin
      corvus-admin completion zsh  > ~/.local/share/zsh/site-functions/_corvus-admin
      corvus-admin completion fish > ~/.config/fish/completions/corvus-admin.fish
    """

    cls = _COMPLETERS[shell]
    completer = cls(
        cli=main,
        ctx_args={},
        prog_name="corvus-admin",
        complete_var="_CORVUS_ADMIN_COMPLETE",
    )
    click.echo(completer.source())


# ---------------------------------------------------------------------------
# Helpers


def _ensure_initialised(ca_dir: Path | None) -> store.AdminStore:
    st = _resolve_store(ca_dir)
    if not st.exists():
        click.echo(
            f"No CA at {st.root}. Run `corvus-admin init` first.",
            err=True,
        )
        sys.exit(1)
    return st


def _records_to_json(records: list) -> str:
    """Serialize a list of dataclass records to JSON. `default=str`
    catches anything dataclasses.asdict can't handle natively (Path,
    datetime); records in this codebase only carry primitives, so the
    fallback is just insurance."""

    return json.dumps([dataclasses.asdict(r) for r in records], indent=2, default=str)


def _print_table(header: tuple[str, ...], rows: list[tuple[str, ...]]) -> None:
    """Width-aware columnar print. Picks each column's width from the
    longest cell (header + body). Renders header + dashes-rule + rows."""

    cols = len(header)
    widths = [
        max(len(header[i]), max((len(r[i]) for r in rows), default=0))
        for i in range(cols)
    ]
    fmt = "  ".join(f"{{:<{w}}}" for w in widths)
    line = fmt.format(*header)
    click.echo(line)
    click.echo("-" * len(line))
    for r in rows:
        click.echo(fmt.format(*r))


# Role → CN-prefix for the deploy / renew completion summaries. Keeps the
# success line consistent across "Deployed", "Renewed", and "[DRY-RUN]
# would deploy" wordings without scattering the role-to-prefix mapping.
_ROLE_PREFIX: dict[str, str] = {
    "daemon": "corvus-daemon",
    "node": "corvus-node",
    "netd": "corvus-netd",
}


def _emit_deploy_result(role: str, plan: deploy.DeployPlan, *, dry_run: bool) -> None:
    cn_prefix = _ROLE_PREFIX[role]
    if dry_run:
        click.echo(
            f"[DRY-RUN] would deploy {role} cert (CN {cn_prefix}:{plan.name}) "
            f"to {plan.target}:{plan.cert_dir}; would restart {plan.service_unit}."
        )
        return
    click.echo(
        f"Deployed {role} cert (CN {cn_prefix}:{plan.name}) "
        f"to {plan.target}:{plan.cert_dir}; restarted {plan.service_unit}."
    )


def _emit_renew_result(role: str, plan: deploy.DeployPlan, *, dry_run: bool) -> None:
    cn_prefix = _ROLE_PREFIX[role]
    if dry_run:
        click.echo(
            f"[DRY-RUN] would renew {role} cert (CN {cn_prefix}:{plan.name}); "
            f"would redeploy to {plan.target}."
        )
        return
    click.echo(
        f"Renewed {role} cert (CN {cn_prefix}:{plan.name}); "
        f"redeployed to {plan.target}."
    )


if __name__ == "__main__":
    main()
