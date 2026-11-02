"""Explicit DHCP setup for the default OpenRC test guest."""

from __future__ import annotations

import shlex
from typing import Protocol

from .ssh import SshResult


class _GuestShell(Protocol):
    def run(
        self, command: str, *, timeout_sec: float = 60.0, check: bool = True
    ) -> SshResult: ...


def renew_dhcp(
    guest: _GuestShell, *, interface: str = "eth0", hostname: str | None = None
) -> None:
    """Restart the managed client and wait for IPv4, including DNS/hostname options.

    Keep a single OpenRC-owned daemon rather than racing a second DHCP client
    against the one started at boot. The explicit configuration also makes
    hostname tests independent of the image's default hostname and DHCP hooks.
    """
    for value in (interface, hostname):
        if value is not None and (
            not value
            or not all(c.isascii() and (c.isalnum() or c in "-._") for c in value)
        ):
            raise ValueError(f"Invalid DHCP interface or hostname: {value!r}")
    config = "\n".join(
        (
            "ipv4only",
            "noipv4ll",
            "waitip 4",
            "timeout 20",
            f"allowinterfaces {interface}",
            "clientid",
            "option domain_name_servers, domain_name, domain_search",
            "option routers, subnet_mask, classless_static_routes",
            f"hostname {hostname}" if hostname is not None else "hostname",
            "",
        )
    )
    guest.run(
        f"doas ip link set {shlex.quote(interface)} up && "
        f"printf %s {shlex.quote(config)} | doas tee /etc/dhcpcd.conf >/dev/null && "
        "doas rc-service dhcpcd restart",
        timeout_sec=45.0,
    )
