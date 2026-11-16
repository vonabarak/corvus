"""Shared development database and nested-network configuration stages."""

from ..core import BuildContext
from ._files import write
from .base import GentooStage


class PostgreSQLInitializationStage(GentooStage):
    """Initialize PostgreSQL 18 and the test database using a socket-only server."""

    requires = frozenset({"system", "interfaces"})
    produces = frozenset({"database"})

    def execute(self, context: BuildContext) -> None:
        self.chroot(context, "eselect", "postgresql", "set", "18")
        self.chroot(context, "emerge", "--config", "dev-db/postgresql:18")
        self.command(
            context,
            "systemctl",
            f"--root={context.root}",
            "enable",
            "postgresql-18.service",
        )
        self.chroot(
            context,
            "install",
            "-d",
            "-o",
            "postgres",
            "-g",
            "postgres",
            "/run/postgresql",
        )
        pg = "/usr/lib64/postgresql-18/bin/"
        control = (
            "runuser",
            "-u",
            "postgres",
            "--",
            pg + "pg_ctl",
            "-D",
            "/var/lib/postgresql/18/data",
        )
        stopped = False

        def stop() -> None:
            nonlocal stopped
            if not stopped:
                # A partially failed startup may have no server. A live server must
                # be stopped successfully before any runtime mount can be released.
                pid = context.root / "var/lib/postgresql/18/data/postmaster.pid"
                if pid.exists():
                    self.chroot(context, *control, "-m", "fast", "-w", "stop")
                stopped = True

        context.own(stop)
        try:
            self.chroot(
                context,
                *control,
                "-w",
                "start",
                "-o",
                "-c listen_addresses='' -c unix_socket_directories=/run/postgresql",
            )
            self.chroot(
                context,
                "runuser",
                "-u",
                "postgres",
                "--",
                pg + "psql",
                "-h",
                "/run/postgresql",
                "-v",
                "ON_ERROR_STOP=1",
                input=b"CREATE USER corvus SUPERUSER;\nCREATE DATABASE corvus OWNER corvus;\n",
            )
        finally:
            stop()
        context.artifacts["database"] = "corvus"


class VdeNetworkConfigurationStage(GentooStage):
    """Configure the isolated VDE subnet, DHCP and outbound NAT."""

    requires = frozenset({"system"})
    produces = frozenset({"network"})

    def execute(self, context: BuildContext) -> None:
        root = context.root
        write(
            root / "etc/systemd/system/vde-switch.service",
            """[Unit]
Description=VDE virtual switch for Corvus test VMs
After=network-pre.target
Before=network.target
Wants=network-pre.target
[Service]
Type=forking
ExecStartPre=/bin/mkdir -p /run/vde2
ExecStart=/usr/bin/vde_switch --sock /run/vde2/switch.ctl --mgmt /run/vde2/switch.mgmt -tap vde0 -m 666 -group kvm --daemon --pidfile /run/vde2/vde_switch.pid
PIDFile=/run/vde2/vde_switch.pid
Restart=on-failure
[Install]
WantedBy=multi-user.target
""",
        )
        write(
            root / "etc/systemd/network/30-vde0.network",
            "[Match]\nName=vde0\n[Network]\nAddress=192.168.92.1/24\nConfigureWithoutCarrier=yes\n",
        )
        write(
            root / "etc/sysctl.d/30-corvus-vde-forward.conf",
            "net.ipv4.ip_forward = 1\n",
        )
        write(
            root / "etc/nftables/rules/main.nft",
            """flush ruleset
table ip nat {
    chain postrouting {
        type nat hook postrouting priority srcnat
        policy accept
        ip saddr 192.168.92.0/24 oifname != "vde0" masquerade
    }
}
""",
        )
        write(
            root / "etc/dnsmasq.conf",
            """interface=vde0
bind-dynamic
dhcp-range=192.168.92.10,192.168.92.200,12h
dhcp-option=option:router,192.168.92.1
dhcp-option=option:dns-server,192.168.92.1
domain-needed
bogus-priv
""",
        )
        write(
            root / "etc/systemd/system/dnsmasq.service.d/wait-vde.conf",
            "[Unit]\nAfter=vde-switch.service systemd-networkd.service\nWants=systemd-networkd.service\n",
        )
        self.command(
            context,
            "systemctl",
            f"--root={root}",
            "enable",
            "vde-switch.service",
            "nftables.service",
            "dnsmasq.service",
        )
        context.artifacts["network"] = root
