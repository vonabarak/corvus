"""Single-command bring-up of a one-node Corvus install.

``corvus-admin quickstart`` composes the granular building blocks
(``init`` → ``deploy daemon`` → ``deploy node`` → ``deploy netd``
→ ``register``) into a single recipe with sensible defaults for
the most common shape: admin's workstation == the single Corvus
host, daemon + nodeagent running as user-systemd services, netd
running as a system service (since it needs root).

Per design: daemon and nodeagent are *always* user services when
called through quickstart. System-mode layouts continue to be
available via the granular ``deploy ... --system-service``
commands.
"""

from __future__ import annotations

import socket
from collections.abc import Callable
from dataclasses import dataclass
from pathlib import Path
from urllib.parse import urlparse

from corvus_admin import (
    binaries,
    ca,
    deploy,
    privesc,
    register,
    store,
)
from corvus_admin import systemd as systemd_mod
from corvus_admin.runner import LocalRunner


class QuickstartError(RuntimeError):
    """Raised when a quickstart step fails irrecoverably."""


@dataclass
class QuickstartResult:
    """What quickstart did. The CLI uses this to print a one-screen
    summary; tests assert on it."""

    node_name: str
    listen_ip: str
    privesc_tool: str | None
    netd_installed: bool
    web_installed: bool
    web_bind: str  # "host:port" if web installed, "" otherwise
    daemon_cert_cn: str
    node_cert_cn: str
    netd_cert_cn: str | None
    client_cert_cn: str
    units_installed: list[str]
    healthy: bool


def _default_node_name() -> str:
    """Use the hostname (short form) when the operator doesn't
    pass --node-name. Falls back to ``primary`` if hostname lookup
    fails — which it shouldn't on any sane Linux box."""

    try:
        hn = socket.gethostname() or ""
        # Strip any FQDN suffix; node names look like `alpha`, not
        # `alpha.dc1.example.com`.
        short = hn.split(".", 1)[0].strip()
        return short or "primary"
    except OSError:
        return "primary"


def _default_base_path() -> str:
    """Per-node disk base path for a user-service install. The
    daemon (running as the operator) needs to own this; ~/VMs is
    the documented default."""

    home = Path.home()
    return str(home / "VMs")


def _check_postgres(url: str, *, timeout_sec: float = 3.0) -> str | None:
    """Quick reachability probe for the URL the daemon will use.

    Catches the most common quickstart failure — Postgres not
    running — *before* the daemon's systemd unit is started, since
    a daemon stuck on the initial DB connect makes
    ``systemctl --user enable --now`` hang for the full timeout.

    Returns ``None`` on success or a short diagnostic on failure.
    Tries TCP for URLs with a host and the standard Unix-socket
    location otherwise; deliberately does not authenticate or run
    SQL — we just want "is something listening?"."""

    try:
        u = urlparse(url)
    except ValueError as e:
        return f"could not parse {url!r}: {e}"
    if u.scheme not in ("postgresql", "postgres"):
        return f"not a postgres URL: {url!r}"

    port = u.port or 5432
    host = u.hostname

    if not host:
        # Empty host means Unix socket; try the two common dirs.
        for sock_dir in ("/var/run/postgresql", "/tmp"):
            sock_path = Path(sock_dir) / f".s.PGSQL.{port}"
            if sock_path.exists():
                return None
        return (
            f"no postgres Unix socket at /var/run/postgresql/.s.PGSQL.{port} "
            f"or /tmp/.s.PGSQL.{port}"
        )

    try:
        with socket.create_connection((host, port), timeout=timeout_sec):
            return None
    except OSError as e:
        return f"cannot connect to {host}:{port}: {e}"


def run(
    *,
    node_name: str | None = None,
    listen_ip: str = "127.0.0.1",
    base_path: str | None = None,
    ca_dir: Path | None = None,
    skip_netd: bool = False,
    skip_web: bool = False,
    web_bind_host: str = "127.0.0.1",
    web_bind_port: int = 8080,
    force: bool = False,
    healthcheck_timeout: float = 15.0,
    log_callback: Callable[[str], None] | None = None,
    database_url: str = "postgresql://localhost/corvus",
) -> QuickstartResult:
    """Drive the full bring-up. Returns a :class:`QuickstartResult`
    describing what was installed."""

    def log(msg: str) -> None:
        if log_callback is not None:
            log_callback(msg)

    name = node_name or _default_node_name()
    base = base_path or _default_base_path()

    # ------------------------------------------------------------------
    # 1. Detect privesc + binaries

    pe = privesc.detect()
    netd_feasible = pe is not None and not skip_netd
    if pe is None:
        log(
            "WARNING: no privilege-escalation tool (sudo/doas) found on $PATH. "
            "Skipping corvus-netd; no virtual networks (bridges, NAT, dnsmasq) "
            "will be available. To enable later: install sudo or doas, then "
            "run `corvus-admin deploy netd <name> local`."
        )
    elif skip_netd:
        log("Skipping corvus-netd (--skip-netd passed).")

    try:
        # `require_web=False`: corvus-web is best-effort even when
        # not skipped — a pure-agent host or a minimal stack install
        # may not have it on $PATH. We log + continue without it
        # rather than failing the whole quickstart.
        bins = binaries.find_all(
            require_netd=netd_feasible,
            require_web=False,
        )
    except binaries.BinaryNotFound as e:
        raise QuickstartError(str(e)) from e
    log(f"Using daemon binary at {bins.corvus}.")
    log(f"Using nodeagent binary at {bins.nodeagent}.")
    if bins.netd is not None:
        log(f"Using netd binary at {bins.netd}.")

    # Resolve corvus-web separately so we can fall back gracefully.
    import shutil

    web_path: str | None = None
    if not skip_web:
        found = shutil.which("corvus-web")
        if found is not None:
            web_path = found
            log(f"Using web binary at {web_path}.")
        else:
            log(
                "WARNING: corvus-web not on $PATH. Skipping the WebUI install; "
                "the daemon + nodeagent will come up without it. To enable later: "
                "install corvus-web (`pipx install corvus`) and run "
                "`corvus-admin deploy web local`."
            )

    # ------------------------------------------------------------------
    # 2. Probe PostgreSQL — if it isn't up, the daemon's startup
    # blocks the systemd `enable --now` call we'll make below and
    # quickstart appears to hang indefinitely. Fail loud and early
    # instead.

    pg_err = _check_postgres(database_url)
    if pg_err is not None:
        raise QuickstartError(
            f"PostgreSQL not reachable at {database_url}: {pg_err}. "
            f"Start it (e.g. `sudo systemctl start postgresql`) and re-run."
        )
    log(f"PostgreSQL reachable at {database_url}.")

    # ------------------------------------------------------------------
    # 3. Admin store + CA + admin client cert

    st = store.AdminStore(ca_dir if ca_dir is not None else store.default_admin_dir())
    if not st.exists() or force:
        ca.init_ca(st, force=force)
        log(f"CA initialised at {st.root}.")
    else:
        log(f"CA already present at {st.root}; reusing (pass --force to rotate).")

    import getpass

    try:
        admin_name = getpass.getuser()
    except Exception:
        admin_name = "admin"
    client_rec = deploy.deploy_client(st, name=admin_name)
    assert client_rec is not None  # quickstart never uses dry_run
    log(
        f"Issued client cert {client_rec.cn} for {admin_name}; "
        f"dropped into {store.default_client_dir()}."
    )

    # ------------------------------------------------------------------
    # 4. Render + install systemd units

    runner = LocalRunner(privesc_tool=pe)
    units_installed: list[str] = []

    daemon_unit_text = systemd_mod.render_unit(
        "daemon",
        mode="user",
        binary_path=str(bins.corvus),
        database_url=database_url,
    )
    daemon_unit_path = systemd_mod.install_unit(
        runner, component="daemon", mode="user", content=daemon_unit_text
    )
    units_installed.append(daemon_unit_path)
    log(f"Wrote {daemon_unit_path}.")

    nodeagent_unit_text = systemd_mod.render_unit(
        "nodeagent",
        mode="user",
        binary_path=str(bins.nodeagent),
    )
    nodeagent_unit_path = systemd_mod.install_unit(
        runner, component="nodeagent", mode="user", content=nodeagent_unit_text
    )
    units_installed.append(nodeagent_unit_path)
    log(f"Wrote {nodeagent_unit_path}.")

    if netd_feasible and bins.netd is not None:
        netd_unit_text = systemd_mod.render_unit(
            "netd",
            mode="system",
            binary_path=str(bins.netd),
        )
        netd_unit_path = systemd_mod.install_unit(
            runner, component="netd", mode="system", content=netd_unit_text
        )
        units_installed.append(netd_unit_path)
        log(f"Wrote {netd_unit_path}.")

    systemd_mod.daemon_reload(runner, mode="user")
    if netd_feasible:
        systemd_mod.daemon_reload(runner, mode="system")

    # ------------------------------------------------------------------
    # 5. Mint + deploy component certs (which also enables + starts units)

    daemon_plan = deploy.deploy_daemon(
        st,
        runner,
        listen_ip=listen_ip,
        user_service=True,
    )
    log(
        f"Deployed daemon cert (CN corvus-daemon:{daemon_plan.name}); "
        f"started {daemon_plan.service_unit}."
    )

    node_plan = deploy.deploy_node(
        st,
        runner,
        name=name,
        ip=listen_ip,
        user_service=True,
    )
    log(
        f"Deployed node cert (CN corvus-node:{name}); started {node_plan.service_unit}."
    )

    netd_cert_cn: str | None = None
    if netd_feasible and bins.netd is not None:
        netd_plan = deploy.deploy_netd(
            st,
            runner,
            name=name,
            ip=listen_ip,
            user_service=False,
        )
        netd_cert_cn = f"corvus-netd:{name}"
        log(
            f"Deployed netd cert (CN {netd_cert_cn}); started {netd_plan.service_unit}."
        )

    # corvus-web: install + start as a user service by default. No
    # cert work — corvus-web reaches the local daemon over its Unix
    # socket. Skip when --skip-web was passed or the binary wasn't
    # located on $PATH (logged earlier).
    web_installed = False
    web_bind = ""
    if web_path is not None:
        web_plan = deploy.deploy_web(
            runner,
            user_service=True,
            binary_path=web_path,
            bind_host=web_bind_host,
            bind_port=web_bind_port,
        )
        units_installed.append(
            f"{systemd_mod.SYSTEMD_USER_DIR}/{systemd_mod.unit_filename('web')}"
        )
        web_installed = True
        web_bind = f"{web_bind_host}:{web_bind_port}"
        log(
            f"Deployed corvus-web (listening on {web_bind}); "
            f"started {web_plan.service_unit}."
        )
    elif skip_web:
        log("Skipping corvus-web (--skip-web passed).")

    # ------------------------------------------------------------------
    # 6. Register node with daemon

    try:
        result = register.register_node(
            name=name,
            host=listen_ip,
            base_path=base,
            healthcheck_timeout_sec=healthcheck_timeout,
        )
    except register.RegisterError as e:
        raise QuickstartError(
            f"node registration failed: {e}. The daemon and agent are running; "
            f"retry registration with `corvus-admin register {name} --host {listen_ip}`."
        ) from e
    if result.healthy:
        log(f"Node {name} registered and healthcheck received.")
    else:
        log(
            f"Node {name} registered but healthcheck did not arrive within "
            f"{healthcheck_timeout}s. Check `journalctl --user -u corvus-nodeagent`."
        )

    return QuickstartResult(
        node_name=name,
        listen_ip=listen_ip,
        privesc_tool=pe.tool if pe is not None else None,
        netd_installed=netd_feasible and bins.netd is not None,
        web_installed=web_installed,
        web_bind=web_bind,
        daemon_cert_cn=f"corvus-daemon:{daemon_plan.name}",
        node_cert_cn=f"corvus-node:{name}",
        netd_cert_cn=netd_cert_cn,
        client_cert_cn=client_rec.cn,
        units_installed=units_installed,
        healthy=result.healthy,
    )
