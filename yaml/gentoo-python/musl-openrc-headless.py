import os
from pathlib import Path

from corvus_image_builder.core import BuildContext, BuildManager, ImageRecipe
from corvus_image_builder.gentoo import (
    BootloaderStage,
    ChrootEnvironmentStage,
    DiskStage,
    FinalizeStage,
    KernelStage,
    MuslPackageInstallStage,
    OpenRCConfigurationStage,
    RepositoryStage,
    Settings,
    WorkspaceStage,
)


class HeadlessPackages(MuslPackageInstallStage):
    """Select runtime packages for the musl/OpenRC headless image."""

    atoms = (
        "sys-apps/kbd",
        "sys-apps/iproute2",
        "sys-process/procps",
        "net-misc/dhcpcd",
        "net-misc/socat",
        "app-admin/sysklogd",
        "app-misc/ca-certificates",
        "sys-fs/e2fsprogs",
        "virtual/udev",
        "app-emulation/qemu-guest-agent",
        "net-misc/openssh",
        "sys-boot/grub",
        "mail-mta/msmtp",
        "app-admin/sudo",
        "sys-fs/growpart",
        "sys-fs/dosfstools",
        "sys-boot/efibootmgr",
        "net-fs/nfs-utils",
        "sys-block/thin-provisioning-tools",
        "app-editors/vim",
        "app-shells/bash-completion",
    )


def build_recipe(settings: Settings) -> ImageRecipe:
    return ImageRecipe(
        [
            stage(settings)
            for stage in (
                WorkspaceStage,
                RepositoryStage,
                DiskStage,
                ChrootEnvironmentStage,
                HeadlessPackages,
                KernelStage,
                OpenRCConfigurationStage,
                BootloaderStage,
                FinalizeStage,
            )
        ]
    )


if __name__ == "__main__":
    settings = Settings(
        libc="musl",
        init="openrc",
        name="gentoo-musl-openrc-headless",
        disk=Path(os.environ.get("TARGET_DISK", "/dev/vdb")),
        cache=Path(os.environ.get("IMAGE_CACHE", "/mnt/corvus-cache")),
        kernel_config=Path(
            os.environ.get("KERNEL_CONFIG", "/run/corvus-kernel-config")
        ),
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
