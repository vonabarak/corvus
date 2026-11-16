import os
from pathlib import Path

from corvus_image_builder.core import BuildContext, BuildManager, ImageRecipe
from corvus_image_builder.gentoo import (
    ChrootEnvironmentStage,
    ExistingImageStage,
    FinalizeStage,
    RepositoryStage,
    Settings,
    WorkspaceStage,
)
from corvus_image_builder.gentoo.cloudinit import CloudInitStage


def build_recipe(settings: Settings) -> ImageRecipe:
    return ImageRecipe(
        [
            stage(settings)
            for stage in (
                WorkspaceStage,
                RepositoryStage,
                ExistingImageStage,
                ChrootEnvironmentStage,
                CloudInitStage,
                FinalizeStage,
            )
        ]
    )


if __name__ == "__main__":
    settings = Settings(
        libc="musl",
        init="openrc",
        name="gentoo-musl-openrc-headless-cloudinit",
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
