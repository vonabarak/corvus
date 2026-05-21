"""Click-based CLI for corvus-admin.

Subcommands shipped:

* ``init``               — generate CA + admin client cert
* ``deploy daemon``      — mint daemon cert + push + restart
* ``deploy node``        — mint node-agent cert + push + restart
* ``deploy netd``        — mint netd cert + push + restart
* ``deploy client``      — mint admin's own client cert (local)
* ``renew {daemon,node,netd,client}`` — re-mint + redeploy
* ``register``           — `crv node add` shortcut
* ``list``               — show every issued cert and its expiry
* ``status``             — probe each cert's deploy target over TLS

Deferred to later phases per the plan: ``ca rotate``, ``revoke``.
"""

from __future__ import annotations

import getpass
import sys
from pathlib import Path

import click

from corvus_admin import ca, deploy, register as register_mod, status as status_mod, store
from corvus_admin.runner import RunnerError, for_target


# ---------------------------------------------------------------------------
# Shared options


def _ca_dir_option(f):
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
def deploy_daemon(
    ca_dir: Path | None,
    target: str,
    listen_ip: str | None,
    user_service: bool,
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
        )
    except RunnerError as e:
        click.echo(f"deploy daemon failed: {e}", err=True)
        if e.stderr:
            click.echo(e.stderr, err=True)
        sys.exit(1)
    click.echo(
        f"Deployed daemon cert (CN corvus-daemon:{plan.name}) "
        f"to {plan.target}:{plan.cert_dir}; restarted {plan.service_unit}."
    )


@deploy_group.command("node")
@_ca_dir_option
@click.argument("name")
@click.argument("target")
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
def deploy_node(
    ca_dir: Path | None,
    name: str,
    target: str,
    ip: str | None,
    user_service: bool,
) -> None:
    """Mint and deploy a corvus-nodeagent cert for the named node."""

    st = _ensure_initialised(ca_dir)
    runner = for_target(target)
    try:
        plan = deploy.deploy_node(
            st,
            runner,
            name=name,
            ip=ip,
            user_service=user_service,
        )
    except RunnerError as e:
        click.echo(f"deploy node failed: {e}", err=True)
        if e.stderr:
            click.echo(e.stderr, err=True)
        sys.exit(1)
    click.echo(
        f"Deployed node cert (CN corvus-node:{plan.name}) "
        f"to {plan.target}:{plan.cert_dir}; restarted {plan.service_unit}."
    )


@deploy_group.command("netd")
@_ca_dir_option
@click.argument("name")
@click.argument("target")
@click.option("--ip", default=None, show_default=False, help="IP SAN for the netd cert.")
@click.option(
    "--user-service/--system-service",
    default=False,
    help="Use the systemd `--user` flow. Default: system service.",
)
def deploy_netd(
    ca_dir: Path | None,
    name: str,
    target: str,
    ip: str | None,
    user_service: bool,
) -> None:
    """Mint and deploy a corvus-netd cert for the named node."""

    st = _ensure_initialised(ca_dir)
    runner = for_target(target)
    try:
        plan = deploy.deploy_netd(
            st,
            runner,
            name=name,
            ip=ip,
            user_service=user_service,
        )
    except RunnerError as e:
        click.echo(f"deploy netd failed: {e}", err=True)
        if e.stderr:
            click.echo(e.stderr, err=True)
        sys.exit(1)
    click.echo(
        f"Deployed netd cert (CN corvus-netd:{plan.name}) "
        f"to {plan.target}:{plan.cert_dir}; restarted {plan.service_unit}."
    )


@deploy_group.command("client")
@_ca_dir_option
@click.argument("name")
def deploy_client(ca_dir: Path | None, name: str) -> None:
    """Mint an admin client cert and drop it into $XDG_CONFIG_HOME/corvus."""

    st = _ensure_initialised(ca_dir)
    rec = deploy.deploy_client(st, name=name)
    click.echo(
        f"Issued client cert {rec.cn} (expires {rec.expires_at}); "
        f"dropped into {store.default_client_dir()}."
    )


# ---------------------------------------------------------------------------
# renew


@main.group("renew")
def renew_group() -> None:
    """Re-mint and redeploy a cert. Refuses to renew certs that
    are still >30 days from expiry unless ``--force`` is passed."""


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
def renew_daemon(ca_dir: Path | None, target: str | None, force: bool) -> None:
    st = _ensure_initialised(ca_dir)
    try:
        plan = deploy.renew_daemon(st, target=target, force=force)
    except (deploy.RenewError, RunnerError) as e:
        click.echo(f"renew daemon failed: {e}", err=True)
        sys.exit(1)
    click.echo(
        f"Renewed daemon cert (CN corvus-daemon:{plan.name}); "
        f"redeployed to {plan.target}."
    )


@renew_group.command("node")
@_ca_dir_option
@click.argument("name")
@click.option("--target", default=None, show_default=False)
@click.option("--force/--no-force", default=False)
def renew_node(
    ca_dir: Path | None, name: str, target: str | None, force: bool
) -> None:
    st = _ensure_initialised(ca_dir)
    try:
        plan = deploy.renew_node(st, name=name, target=target, force=force)
    except (deploy.RenewError, RunnerError) as e:
        click.echo(f"renew node failed: {e}", err=True)
        sys.exit(1)
    click.echo(
        f"Renewed node cert (CN corvus-node:{plan.name}); "
        f"redeployed to {plan.target}."
    )


@renew_group.command("netd")
@_ca_dir_option
@click.argument("name")
@click.option("--target", default=None, show_default=False)
@click.option("--force/--no-force", default=False)
def renew_netd(
    ca_dir: Path | None, name: str, target: str | None, force: bool
) -> None:
    st = _ensure_initialised(ca_dir)
    try:
        plan = deploy.renew_netd(st, name=name, target=target, force=force)
    except (deploy.RenewError, RunnerError) as e:
        click.echo(f"renew netd failed: {e}", err=True)
        sys.exit(1)
    click.echo(
        f"Renewed netd cert (CN corvus-netd:{plan.name}); "
        f"redeployed to {plan.target}."
    )


@renew_group.command("client")
@_ca_dir_option
@click.argument("name")
@click.option("--force/--no-force", default=False)
def renew_client(ca_dir: Path | None, name: str, force: bool) -> None:
    st = _ensure_initialised(ca_dir)
    try:
        rec = deploy.renew_client(st, name=name, force=force)
    except deploy.RenewError as e:
        click.echo(f"renew client failed: {e}", err=True)
        sys.exit(1)
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
    click.echo(
        f"Node {result.name} → {result.host}:{result.node_agent_port}: {status}"
    )


# ---------------------------------------------------------------------------
# list


@main.command("list")
@_ca_dir_option
def list_cmd(ca_dir: Path | None) -> None:
    """List every issued cert and where it was deployed."""

    st = _resolve_store(ca_dir)
    if not st.exists():
        click.echo(f"No CA at {st.root}. Run `corvus-admin init`.", err=True)
        sys.exit(1)
    records = sorted(st.iter_records(), key=lambda r: (r.role, r.name_or_uuid))
    if not records:
        click.echo("(no certs issued yet)")
        return
    # Plain column layout — Phase 2 keeps the output minimal; a
    # tabular formatter can come with `status` in Phase 4.
    header = f"{'CN':40} {'EXPIRES':25} {'DEPLOYED-TO':30} IP"
    click.echo(header)
    click.echo("-" * len(header))
    for r in records:
        click.echo(
            f"{r.cn:40} {r.expires_at:25} {(r.deployed_to or '-'):30} {r.ip or '-'}"
        )


# ---------------------------------------------------------------------------
# status


@main.command("status")
@_ca_dir_option
def status_cmd(ca_dir: Path | None) -> None:
    """Probe every deployed component over TLS and report
    reachability + days-to-expiry. Client certs are listed but
    not probed (no remote service to dial)."""

    st = _resolve_store(ca_dir)
    if not st.exists():
        click.echo(f"No CA at {st.root}. Run `corvus-admin init`.", err=True)
        sys.exit(1)

    reports = status_mod.probe_all(st)
    if not reports:
        click.echo("(no certs issued yet)")
        return

    header = f"{'CN':40} {'DAYS-LEFT':>9} {'STATUS':12} DETAIL"
    click.echo(header)
    click.echo("-" * len(header))
    any_unhealthy = False
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
            any_unhealthy = True
        click.echo(
            f"{r.cn:40} {r.days_remaining:>9} {status_word:12} {detail}"
        )
    sys.exit(1 if any_unhealthy else 0)


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


if __name__ == "__main__":
    main()
