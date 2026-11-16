"""Build the integration-test outer node by customizing a Python headless image."""

import os
import shutil
import subprocess
from pathlib import Path

from corvus_image_builder.gentoo.development import (
    PostgreSQLInitializationStage, VdeNetworkConfigurationStage,
)
from corvus_image_builder.core import BuildContext, BuildManager, ImageRecipe
from corvus_image_builder.gentoo import (
    ChrootEnvironmentStage,
    ExistingImageStage,
    FinalizeStage,
    GentooStage,
    PackageInstallStage,
    RepositoryStage,
    Settings,
    WorkspaceStage,
    link,
    write,
)


class NodeWorkspace(WorkspaceStage):
    """Require enough helper disk space to compile the node's runtime packages."""

    def execute(self, context: BuildContext) -> None:
        super().execute(context)
        if shutil.disk_usage("/").total < 30_000_000_000:
            raise ValueError(
                "The test-node builder needs a root filesystem of at least 30 GB"
            )


class NodeRepository(RepositoryStage):
    """Install the node's overlay and package policy before syncing repositories."""

    def execute(self, context: BuildContext) -> None:
        write(
            Path("/etc/portage/repos.conf/corvus.conf"),
            """[vonabarak]
location = /var/db/repos/vonabarak
sync-type = git
sync-uri = https://github.com/vonabarak/gentoo.git

[haskell]
location = /var/db/repos/haskell
sync-type = git
sync-uri = https://github.com/gentoo-haskell/gentoo-haskell.git
""",
        )
        write(
            Path("/etc/portage/package.use/corvus"),
            """app-emulation/qemu spice pipewire pulseaudio usb usbredir virtfs passt vde
net-libs/gnutls pkcs11 tools
dev-db/postgresql python uuid xml
media-libs/libpulse -X -gtk -glib
media-video/pipewire -X -zeroconf -v4l -bluetooth pulseaudio sound-server
""",
        )
        write(
            Path("/etc/portage/package.accept_keywords/corvus"),
            """app-emulation/corvus **
app-emulation/corvus-bin **
dev-python/pycapnp **
dev-python/fastapi **
dev-haskell/supervisors **
dev-haskell/data-default-instances-vector **
dev-haskell/monad-stm **
dev-haskell/zenhack-prelude **
""",
        )
        super().execute(context)


class NodePackages(PackageInstallStage):
    """Extend the inherited system and rebuild pycapnp against its capnproto ABI."""

    requires = frozenset({"disk", "interfaces"})
    atoms = (
        "net-misc/socat",
        "app-emulation/qemu",
        "media-video/pipewire",
        "app-crypt/swtpm",
        "net-misc/vde",
        "net-dns/dnsmasq",
        "net-firewall/nftables",
        "app-emulation/virtiofsd",
        "app-cdr/cdrtools",
        "sys-fs/dosfstools",
        "sys-fs/mtools",
        "sys-firmware/edk2-bin",
        "dev-db/postgresql:18",
        "dev-vcs/git",
        "net-misc/curl",
        "app-arch/zstd",
        "sys-process/htop",
        "app-misc/jq",
        "dev-python/pip",
        "dev-python/click",
        "dev-python/cryptography",
        "dev-python/jinja2",
        "dev-python/pyyaml",
        "dev-python/pycapnp",
        "dev-python/uvicorn",
        "dev-python/fastapi",
        "dev-python/prometheus-client",
        "dev-python/httptools",
        "dev-python/uvloop",
        "dev-python/watchfiles",
        "dev-python/websockets",
    )

    def execute(self, context: BuildContext) -> None:
        # An overlay already has merged-/usr and a complete system: never bootstrap it.
        self.install_system(context)
        self.configure_portage(context)
        context.artifacts["system"] = context.root

    def install_system(self, context: BuildContext) -> None:
        self.packages(context, self.atoms, noreplace=True)
        self.packages(
            context,
            ("dev-python/pycapnp",),
            usepkg_exclude=("dev-python/pycapnp",),
            reinstall_atoms=("dev-python/pycapnp",),
        )
        self.chroot(context, "python3", "-c", "import capnp")


class NodeConfiguration(GentooStage):
    """Configure nested networking, harness access, services and debug commands."""

    requires = frozenset({"database", "kernel"})
    produces = frozenset({"configured"})

    def validate(self, context: BuildContext) -> None:
        assets = Path("/run/corvus-test-node")
        expected = (
            "corvus.service",
            "corvus-netd.service",
            "corvus-nodeagent.service",
            "corvus-tcp-relay.service",
            "home-corvus-VMs-BaseImagesRO.mount",
            "mnt-corvus.mount",
            "opt-corvus-bin.mount",
        )
        if not (assets / "authorized_keys").is_file() or not all(
            (assets / "systemd" / name).is_file() for name in expected
        ):
            raise ValueError("Missing test-node SSH key or systemd unit assets")

    def execute(self, context: BuildContext) -> None:
        root = context.root
        kernel = str(context.artifacts["kernel"]).removeprefix("vmlinuz-")
        config = (root / "boot" / f"config-{kernel}").read_text().splitlines()
        if not any(
            f"CONFIG_KVM_{vendor}={mode}" in config
            for vendor in ("INTEL", "AMD")
            for mode in ("y", "m")
        ):
            raise ValueError("Inherited kernel lacks nested KVM support")
        VdeNetworkConfigurationStage(self.settings).execute(context)
        users = (root / "etc/passwd").read_text().splitlines()
        if any(line.startswith("corvus:") for line in users):
            self.chroot(context, "usermod", "-aG", "wheel,kvm", "corvus")
        else:
            self.chroot(
                context, "useradd", "-m", "-s", "/bin/bash", "-G", "wheel,kvm", "corvus"
            )
        self.chroot(context, "chown", "corvus:corvus", "/home/corvus")
        self.chroot(
            context,
            "install",
            "-d",
            "-m",
            "0700",
            "-o",
            "corvus",
            "-g",
            "corvus",
            "/home/corvus/.ssh",
        )
        self.chroot(
            context,
            "install",
            "-d",
            "-m",
            "0755",
            "-o",
            "corvus",
            "-g",
            "corvus",
            "/home/corvus/VMs",
            "/home/corvus/VMs/BaseImages",
            "/home/corvus/VMs/BaseImagesRO",
            "/etc/corvus",
        )
        shutil.copyfile(
            "/run/corvus-test-node/authorized_keys",
            root / "home/corvus/.ssh/authorized_keys",
        )
        (root / "home/corvus/.ssh/authorized_keys").chmod(0o600)
        self.chroot(
            context, "chown", "corvus:corvus", "/home/corvus/.ssh/authorized_keys"
        )
        write(root / "etc/sudoers.d/wheel-nopasswd", "%wheel ALL=(ALL) NOPASSWD: ALL\n")
        (root / "etc/sudoers.d").chmod(0o750)
        (root / "etc/sudoers.d/wheel-nopasswd").chmod(0o440)
        for directory in ("opt/corvus/bin", "mnt/corvus", "etc/systemd/system"):
            (root / directory).mkdir(parents=True, exist_ok=True)
        for unit in Path("/run/corvus-test-node/systemd").iterdir():
            shutil.copyfile(unit, root / "etc/systemd/system" / unit.name)
        self.command(
            context,
            "systemctl",
            f"--root={root}",
            "enable",
            "vde-switch.service",
            "nftables.service",
            "dnsmasq.service",
            "opt-corvus-bin.mount",
            "mnt-corvus.mount",
            "home-corvus-VMs-BaseImagesRO.mount",
            "corvus-tcp-relay.service",
            "sshd.service",
        )
        self.command(
            context,
            "systemctl",
            f"--root={root}",
            "disable",
            "corvus.service",
            "corvus-netd.service",
            "corvus-nodeagent.service",
        )
        write(root / "etc/profile.d/50-corvus.sh", DEBUG_PROFILE)
        write(
            root / "etc/motd",
            """── Corvus integration-test outer node ───────────────────────
  crv / crv-log / crv-status / crv-psql / crv-procs
  Fresh host binaries are shared at /opt/corvus/bin.
""",
        )
        context.artifacts["configured"] = root


DEBUG_PROFILE = """if [ -d /opt/corvus/bin ]; then
  case ":$PATH:" in
    *:/opt/corvus/bin:*) ;;
    *) PATH="/opt/corvus/bin:$PATH" ;;
  esac
  export PATH CORVUS_HOST=127.0.0.1 CORVUS_PORT=9876
fi
alias crv='/opt/corvus/bin/crv'
alias crv-log='journalctl -u corvus -f --since "5 min ago"'
alias crv-status='systemctl status --no-pager corvus'
alias crv-psql='psql -U corvus corvus'
alias crv-procs='pgrep -af "[c]orvus|[q]emu-system|[v]irtiofsd|[d]nsmasq" | sort'
if [ -x /opt/corvus/bin/crv ]; then
  eval "$(/opt/corvus/bin/crv completion bash 2>/dev/null)"
fi
"""


class NodeFinalize(FinalizeStage):
    """Preserve systemd first-boot semantics and record node runtime tool versions."""

    requires = frozenset({"configured", "bootloader"})

    def reset_identity(self, context: BuildContext) -> None:
        super().reset_identity(context)
        write(context.root / "etc/machine-id", "uninitialized\n")
        link(context.root / "var/lib/dbus/machine-id", "/etc/machine-id")

    def metadata(self, context: BuildContext) -> dict[str, object]:
        info = super().metadata(context)
        info["tools"] = {
            tool: subprocess.check_output(
                ["chroot", str(context.root), *argv],
                stderr=subprocess.STDOUT,
                text=True,
            ).splitlines()[0]
            for tool, argv in (
                ("qemu", ("qemu-system-x86_64", "--version")),
                ("postgresql", ("psql", "--version")),
                ("socat", ("socat", "-V")),
            )
        }
        return info


def build_recipe(settings: Settings) -> ImageRecipe:
    """Compose the overlay recipe without executing commands during planning."""
    return ImageRecipe(
        [
            stage(settings)
            for stage in (
                NodeWorkspace,
                NodeRepository,
                ExistingImageStage,
                ChrootEnvironmentStage,
                NodePackages,
                PostgreSQLInitializationStage,
                NodeConfiguration,
                NodeFinalize,
            )
        ]
    )


if __name__ == "__main__":
    settings = Settings(
        name="corvus-test-node",
        disk=Path(os.environ.get("TARGET_DISK", "/dev/vdb")),
        cache=Path(os.environ.get("IMAGE_CACHE", "/mnt/corvus-cache")),
        kernel_config=None,
        use_binary=os.environ.get("USEPKG", "true") == "true",
        fetch_binary=os.environ.get("GETBINPKG", "true") == "true",
    )
    context = BuildContext(Path(os.environ.get("IMAGE_ROOT", "/mnt/sysroot")).resolve())
    manager = BuildManager()
    recipe = build_recipe(settings)
    if os.environ.get("IMAGE_BUILD_PLAN") == "1":
        print("\n".join(manager.plan(recipe, context)))
    else:
        manager.run(recipe, context)
