"""Build the cloud-init enabled Corvus development image."""

import os
from pathlib import Path

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
from corvus_image_builder.gentoo.development import (
    PostgreSQLInitializationStage,
    VdeNetworkConfigurationStage,
)
from corvus_image_builder.portage import InstallRequest, PortageBackend


class DevelopmentRepository(RepositoryStage):
    """Install the node's overlay and package policy before syncing repositories."""

    def execute(self, context: BuildContext) -> None:
        write(
            Path("/etc/portage/repos.conf/corvus.conf"),
            (
                "[vonabarak]\n"
                "location = /var/db/repos/vonabarak\n"
                "sync-type = git\n"
                "sync-uri = https://github.com/vonabarak/gentoo.git\n"
                "\n"
                "[haskell]\n"
                "location = /var/db/repos/haskell\n"
                "sync-type = git\n"
                "sync-uri = https://github.com/gentoo-haskell/gentoo-haskell.git\n"
            ),
        )
        write(
            Path("/etc/portage/package.use/corvus"),
            (
                "app-emulation/qemu spice usb usbredir virtfs passt vde\n"
                "net-libs/gnutls pkcs11 tools\n"
                "dev-db/postgresql python uuid xml\n"
                "media-libs/libpulse -X -gtk -glib\n"
                "media-video/pipewire -X -zeroconf -v4l -bluetooth pulseaudio sound-server\n"
            ),
        )
        write(
            Path("/etc/portage/package.accept_keywords/corvus"),
            (
                "app-emulation/corvus **\n"
                "app-emulation/corvus-bin **\n"
                "dev-python/pycapnp **\n"
                "dev-python/fastapi **\n"
                "dev-haskell/* ~amd64\n"
                "dev-lang/ghc ~amd64\n"
                "dev-util/ghcup-bin ~amd64\n"
                "app-admin/haskell-updater ~amd64\n"
                "dev-haskell/data-default-instances-vector **\n"
                "dev-haskell/monad-stm **\n"
                "dev-haskell/zenhack-prelude **\n"
            ),
        )
        super().execute(context)


class DevelopmentPackages(PackageInstallStage):
    """Install the live Corvus package and its build dependencies in the artifact."""

    atoms = ("app-emulation/corvus", "app-emulation/cloud-init")

    def execute(self, context: BuildContext) -> None:
        self.install_system(context)
        self.configure_portage(context)
        context.artifacts["system"] = context.root

    def install_system(self, context: BuildContext) -> None:
        backend = PortageBackend(context.runner, context.root)
        common = dict(
            root=context.root,
            use_binary=self.settings.use_binary,
            fetch_binary=self.settings.fetch_binary,
            tmpdir=Path(str(context.artifacts["scratch"])),
        )
        backend.install(
            InstallRequest(
                ("app-emulation/corvus",), onlydeps=True, root_deps=True, **common
            )
        )
        backend.install(
            InstallRequest(
                ("app-emulation/corvus",),
                root_deps=True,
                usepkg_exclude=("app-emulation/corvus",),
                **common,
            )
        )
        self.packages(context, ("app-emulation/cloud-init",), noreplace=True)


class DevelopmentConfiguration(GentooStage):
    """Enable NoCloud, optional shared caches and development login guidance."""

    requires = frozenset({"database", "network"})
    produces = frozenset({"configured"})

    def execute(self, context: BuildContext) -> None:
        root = context.root
        self.command(
            context,
            "systemctl",
            f"--root={root!s}",
            "enable",
            "cloud-init.target",
            "cloud-init-main.service",
            "cloud-init-local.service",
            "cloud-config.service",
            "cloud-final.service",
            "cloud-init-hotplugd.socket",
        )
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
        write(root / "etc/cloud/cloud.cfg.d/91_corvus_locale.cfg", "locale: false\n")
        cfg = root / "etc/cloud/cloud.cfg"
        cfg.write_text(
            "\n".join(
                line
                for line in cfg.read_text().splitlines()
                if line.strip() != "- locale"
            )
            + "\n"
        )
        with (root / "etc/portage/make.conf").open("a") as stream:
            stream.write(
                'DISTDIR="/mnt/corvus-cache/distfiles"\n'
                'PKGDIR="/mnt/corvus-cache/packages/x86-64"\n'
                'FEATURES="-userfetch -userpriv"\n'
            )
        write(
            root / "etc/portage/binrepos.conf/zz-corvus-python.conf",
            (
                "[binhost]\n"
                "location = /mnt/corvus-cache/binhost/x86-64\n"
                "sync-uri = https://distfiles.gentoo.org/releases/amd64/binpackages/23.0/x86-64\n"
                "verify-signature = true\n"
            ),
        )
        (root / "mnt/corvus-cache").mkdir(parents=True, exist_ok=True)
        with (root / "etc/fstab").open("a") as stream:
            stream.write(
                "corvus-cache /mnt/corvus-cache virtiofs nofail,x-systemd.automount 0 0\n"
            )
        write(
            root / "etc/issue",
            (
                "Gentoo Linux with Corvus installed\n"
                "Log in as corvus; run corvus-admin quickstart to configure a local node.\n"
            ),
        )
        link(root / "etc/motd", "/etc/issue")
        write(
            root / "etc/ssh/sshd_config.d/90-corvus-banner.conf",
            (
                "Banner none\n"
                "PrintMotd no\n"
            ),
        )
        context.artifacts["configured"] = root


class DevelopmentFinalize(FinalizeStage):
    """Publish a clean development image after configuration is complete."""

    requires = frozenset({"configured", "bootloader"})


def build_recipe(settings: Settings) -> ImageRecipe:
    """Compose the development overlay without executing it."""
    return ImageRecipe(
        [
            stage(settings)
            for stage in (
                WorkspaceStage,
                DevelopmentRepository,
                ExistingImageStage,
                ChrootEnvironmentStage,
                DevelopmentPackages,
                PostgreSQLInitializationStage,
                VdeNetworkConfigurationStage,
                DevelopmentConfiguration,
                DevelopmentFinalize,
            )
        ]
    )


if __name__ == "__main__":
    settings = Settings(
        name="gentoo-corvus-test",
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
