"""Configure services and locales for musl/OpenRC images."""

import subprocess

from ..core import BuildContext
from ._files import write
from .base import GentooStage
from .disk import partition_device


class OpenRCConfigurationStage(GentooStage):
    """Configure musl/OpenRC networking, logging, SSH, QGA, and serial login.

    Set runlevels, C.UTF-8 locale, UTC, and VSOCK forwarding to the SSH listener.
    """

    requires = frozenset({"kernel"})
    produces = frozenset({"configured"})

    def execute(self, context: BuildContext) -> None:
        root = context.root
        # The builder and the attached image use the same filesystem labels.
        esp_uuid, root_uuid = (
            subprocess.check_output(
                [
                    "blkid",
                    "-s",
                    "UUID",
                    "-o",
                    "value",
                    str(partition_device(self.settings.disk, number)),
                ],
                text=True,
            ).strip()
            for number in (2, 3)
        )
        write(
            root / "etc/fstab",
            (
                f"UUID={root_uuid} / ext4 defaults,noatime 0 1\n"
                f"UUID={esp_uuid} /boot/efi vfat defaults,umask=0077 0 2\n"
            ),
        )
        write(root / "etc/conf.d/hostname", f'hostname="{self.settings.name}"\n')
        write(root / "etc/hosts", (
            "127.0.0.1 localhost\n"
            "::1 localhost\n"
        ))
        write(root / "etc/env.d/02locale", "LANG=C.UTF-8\n")
        write(root / "etc/locale.conf", "LANG=C.UTF-8\n")
        write(root / "etc/env.d/00timezone", "TZ=UTC0\n")
        # Udev can expose the virtio port after OpenRC starts the agent.
        # Keep the agent alive until its transport becomes available.
        with (root / "etc/conf.d/qemu-guest-agent").open("a") as stream:
            stream.write(
                "\n"
                "GA_OPTIONS=\"${GA_OPTIONS} --retry-path\"\n"
            )
        (root / "etc/resolv.conf").unlink(missing_ok=True)
        write(root / "etc/resolv.conf", "")
        clock = root / "etc/conf.d/hwclock"
        write(
            clock,
            "\n".join(
                line
                for line in clock.read_text().splitlines()
                if not line.lstrip(" #\t").startswith("clock_hctosys=")
            )
            + (
                "\n"
                "clock_hctosys=\"NO\"\n"
            ),
        )
        write(
            root / "etc/init.d/vsock-sshd",
            (
                "#!/sbin/openrc-run\n"
                'description="Forward AF_VSOCK:22 to local sshd on 127.0.0.1:22"\n'
                'command="/usr/bin/socat"\n'
                'command_args="VSOCK-LISTEN:22,fork,reuseaddr TCP:127.0.0.1:22"\n'
                "command_background=true\n"
                'pidfile="/run/${RC_SVCNAME}.pid"\n'
                "\n"
                "# Start only after the local SSH listener is available; no IP network is needed.\n"
                "depend() {\n"
                "    need sshd\n"
                "}\n"
                "\n"
                "# The kernel builds virtio-VSOCK as modules; load the transport and dependencies.\n"
                "start_pre() {\n"
                "    modprobe vmw_vsock_virtio_transport\n"
                "}\n"
            ),
        )
        (root / "etc/init.d/vsock-sshd").chmod(0o755)
        with (root / "etc/conf.d/sysklogd").open("a") as stream:
            stream.write(
                "\n"
                "# OpenRC backgrounds syslogd before initialization has created /dev/log.\n"
                "# Keep dependent services waiting until local logging is actually available.\n"
                "start_post() {\n"
                "    local attempt\n"
                "    for attempt in $(seq 1 30); do\n"
                "        [ -S /dev/log ] && return 0\n"
                "        sleep 1\n"
                "    done\n"
                '    eerror "sysklogd did not create /dev/log within 30 seconds"\n'
                "    return 1\n"
                "}\n"
            )
        for level, services in (
            ("sysinit", ("devfs", "dmesg", "udev", "udev-trigger")),
            (
                "boot",
                (
                    "hwclock",
                    "modules",
                    "sysctl",
                    "hostname",
                    "bootmisc",
                    "root",
                    "fsck",
                    "localmount",
                    "sysklogd",
                ),
            ),
            ("default", ("dhcpcd", "sshd", "vsock-sshd", "qemu-guest-agent")),
            ("shutdown", ("savecache", "killprocs", "mount-ro")),
        ):
            for service in services:
                self.command(
                    context,
                    "chroot",
                    str(root),
                    "rc-update",
                    "add",
                    str(service),
                    str(level),
                )
        inittab = root / "etc/inittab"
        if not any(line.startswith("s0:") for line in inittab.read_text().splitlines()):
            with inittab.open("a") as stream:
                stream.write(
                    "\n"
                    "s0:12345:respawn:/sbin/agetty -L 115200 ttyS0 vt100\n"
                )
        context.artifacts["configured"] = root
