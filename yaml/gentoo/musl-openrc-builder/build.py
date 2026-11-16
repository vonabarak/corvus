import os
from dataclasses import replace
from pathlib import Path

from corvus_image_builder.core import BuildContext, BuildManager, ImageRecipe
from corvus_image_builder.gentoo import (
    BootloaderStage,
    ChrootEnvironmentStage,
    DiskStage,
    KernelStage,
    OpenRCConfigurationStage,
    RepositoryStage,
    Settings,
    WorkspaceStage,
)
from corvus_image_builder.gentoo.builders import (
    BuilderFinalizeStage,
    MuslBuilderToolsStage,
    Stage3Stage,
)


def build_recipe(settings):
    host = replace(settings, libc="glibc", init="systemd")
    return ImageRecipe(
        [
            WorkspaceStage(host),
            RepositoryStage(host),
            DiskStage(settings),
            Stage3Stage(settings),
            ChrootEnvironmentStage(settings),
            MuslBuilderToolsStage(settings),
            KernelStage(settings),
            OpenRCConfigurationStage(settings),
            BootloaderStage(settings),
            BuilderFinalizeStage(settings),
        ]
    )


if __name__ == "__main__":
    settings = Settings(
        name="gentoo-musl-openrc-builder",
        disk=Path("/dev/vdb"),
        cache=Path("/mnt/corvus-cache"),
        kernel_config=Path("/run/corvus-kernel-config"),
        libc="musl",
        init="openrc",
        use_binary=os.environ.get("USEPKG", "true") == "true",
        fetch_binary=os.environ.get("GETBINPKG", "true") == "true",
    )
    context = BuildContext(Path("/mnt/sysroot"))
    manager = BuildManager()
    recipe = build_recipe(settings)
    if os.environ.get("IMAGE_BUILD_PLAN") == "1":
        print("\n".join(manager.plan(recipe, context)))
    else:
        manager.run(recipe, context)
