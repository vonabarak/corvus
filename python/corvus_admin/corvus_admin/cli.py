"""Click-based CLI for corvus-admin.

Subcommands implemented in this MVP phase (Phase 2):

* ``init``               — generate CA + admin client cert
* ``deploy daemon``      — mint daemon cert + push + restart
* ``deploy node``        — mint node-agent cert + push + restart
* ``deploy netd``        — mint netd cert + push + restart
* ``deploy client``      — mint admin's own client cert (local)
* ``list``               — show every issued cert and its expiry

Deferred to later phases per the plan: ``renew``, ``register``,
``status``, ``ca rotate``, ``revoke``.
"""

from __future__ import annotations

import getpass
import sys
from pathlib import Path

import click

from corvus_admin import ca, deploy, store
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
