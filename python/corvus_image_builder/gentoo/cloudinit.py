"""Install and configure cloud-init on an existing headless image."""

from ..core import BuildContext
from ._files import write
from .base import GentooStage


class CloudInitStage(GentooStage):
    """Install NoCloud support and enable services for systemd or OpenRC."""

    requires = frozenset({"disk", "interfaces"})
    produces = frozenset({"configured"})

    def execute(self, context: BuildContext) -> None:
        self.packages(
            context,
            (
                "app-emulation/cloud-init",
                "sys-fs/growpart",
                "app-admin/sudo",
                "app-alternatives/awk",
            ),
        )
        root = context.root
        write(
            root / "etc/cloud/cloud.cfg.d/90_corvus_nocloud.cfg",
            (
                "datasource_list: [NoCloud]\n"
                "system_info:\n"
                "  default_user:\n"
                "    name: gentoo\n"
                "    primary_group: users\n"
                "    groups: [wheel]\n"
                "    lock_passwd: true\n"
                "    shell: /bin/bash\n"
                '    sudo: ["ALL=(ALL) NOPASSWD:ALL"]\n'
                "growpart:\n"
                "  mode: auto\n"
                "  devices: ['/']\n"
                "  ignore_growroot_disabled: false\n"
                "resize_rootfs: true\n"
            ),
        )
        locale = (
            "locale: false\n"
            if self.settings.libc == "glibc"
            else (
                "locale: C.UTF-8\n"
                "locale_configfile: /etc/locale.conf\n"
            )
        )
        write(root / "etc/cloud/cloud.cfg.d/91_corvus_locale.cfg", locale)
        cfg = root / "etc/cloud/cloud.cfg"
        cfg.write_text(
            "\n".join(
                line
                for line in cfg.read_text().splitlines()
                if line.strip() != "- locale"
            )
            + "\n"
        )
        if self.settings.init == "systemd":
            self.command(
                context,
                "systemctl",
                f"--root={root}",
                "enable",
                "cloud-init.target",
                "cloud-init-main.service",
                "cloud-init-local.service",
                "cloud-config.service",
                "cloud-final.service",
                "cloud-init-hotplugd.socket",
            )
        else:
            write(
                root / "etc/cloud/cloud.cfg.d/92_corvus_network.cfg",
                (
                    "network:\n"
                    "  config: disabled\n"
                ),
            )
            write(root / "etc/conf.d/cloud-init-local", 'rc_use="logger"\n')
            candidates = list((root / "usr/lib").glob("**/cloudinit/distros/gentoo.py"))
            if len(candidates) != 1:
                raise ValueError("Expected one installed Gentoo cloud-init backend")
            distro = candidates[0]
            marker = "class Distro(distros.Distro):"
            content = distro.read_text()
            if marker not in content:
                raise ValueError("Unrecognized Gentoo cloud-init backend")
            distro.write_text(
                content.replace(marker, marker + (
                    "\n"
                    "    init_cmd = [\"rc-service\"]"
                ), 1)
            )
            self.chroot_string(context, "rc-update add cloud-init-local boot")
            for service in ("cloud-init", "cloud-config", "cloud-final"):
                self.chroot(context, "rc-update", "add", service, "default")
        context.artifacts["configured"] = root
