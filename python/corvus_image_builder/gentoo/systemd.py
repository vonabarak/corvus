"""Configure services and locales for glibc/systemd images."""

from ..core import BuildContext
from ._files import link, write
from .base import GentooStage


class SystemdConfigurationStage(GentooStage):
    """Configure the glibc/systemd image's filesystems, locales, and boot services.

    Enable DHCP networking, DNS resolution, SSH, QGA, and the serial console;
    arrange SSH host-key generation on first boot.
    """

    requires = frozenset({"kernel"})
    produces = frozenset({"configured"})

    def execute(self, context: BuildContext) -> None:
        root = context.root
        write(
            root / "etc/fstab",
            "LABEL=corvus-root / ext4 defaults,noatime 0 1\n"
            "LABEL=CORVUS-EFI /boot/efi vfat defaults,umask=0077 0 2\n",
        )
        write(root / "etc/hostname", self.settings.name + "\n")
        write(root / "etc/hosts", "127.0.0.1 localhost\n::1 localhost\n")
        link(root / "etc/localtime", "../usr/share/zoneinfo/UTC")
        write(root / "etc/locale.conf", "LANG=en_US.UTF-8\n")
        write(root / "etc/locale.gen", "en_US.UTF-8 UTF-8\nC.UTF-8 UTF-8\n")
        (root / "usr/lib/locale").mkdir(parents=True, exist_ok=True)
        for source, destination in (("en_US", "en_US.UTF-8"), ("C", "C.UTF-8")):
            self.command(
                context,
                "chroot",
                str(root),
                "localedef",
                "-i",
                source,
                "-f",
                "UTF-8",
                destination,
                LC_ALL="C",
            )
        write(
            root / "etc/systemd/network/20-wired.network",
            "[Match]\nName=en* eth*\n\n[Network]\nDHCP=yes\n",
        )
        write(
            root / "etc/systemd/system/sshd-hostkeys.service",
            """[Unit]
Description=Generate SSH host keys
DefaultDependencies=no
RequiresMountsFor=/etc/ssh
After=systemd-remount-fs.service
Conflicts=shutdown.target
Before=shutdown.target
[Service]
Type=oneshot
RemainAfterExit=yes
ExecStart=/usr/bin/ssh-keygen -A
""",
        )
        for consumer in ("sshd.service", "sshd-vsock.socket", "sshd-unix-local.socket"):
            write(
                root / f"etc/systemd/system/{consumer}.d/10-hostkeys.conf",
                "[Unit]\nRequires=sshd-hostkeys.service\nAfter=sshd-hostkeys.service\n",
            )
        self.command(
            context,
            "systemctl",
            f"--root={root}",
            "enable",
            "systemd-networkd.service",
            "systemd-resolved.service",
            "sshd.service",
            "qemu-guest-agent.service",
            "serial-getty@ttyS0.service",
        )
        link(root / "etc/resolv.conf", "/run/systemd/resolve/stub-resolv.conf")
        context.artifacts["configured"] = root
