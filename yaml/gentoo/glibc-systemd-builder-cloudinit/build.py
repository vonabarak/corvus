import os
from pathlib import Path

from corvus_image_builder.core import BuildContext, BuildManager, ImageRecipe
from corvus_image_builder.gentoo import RepositoryStage, Settings
from corvus_image_builder.gentoo.builders import (
    BuilderIdentityStage,
    BuilderToolsStage,
    BuilderWorkspaceStage,
)


def build_recipe(settings):
    return ImageRecipe(
        [
            stage(settings)
            for stage in (
                BuilderWorkspaceStage,
                RepositoryStage,
                BuilderToolsStage,
                BuilderIdentityStage,
            )
        ]
    )


if __name__ == "__main__":
    settings = Settings(
        name="gentoo-glibc-systemd-builder-cloudinit",
        disk=Path("/dev/vdb"),
        cache=Path("/mnt/corvus-cache"),
        kernel_config=Path("/run/corvus-kernel-config"),
        libc="glibc",
        init="systemd",
        use_binary=os.environ.get("USEPKG", "true") == "true",
        fetch_binary=os.environ.get("GETBINPKG", "true") == "true",
    )
    context = BuildContext(Path("/"))
    manager = BuildManager()
    recipe = build_recipe(settings)
    if os.environ.get("IMAGE_BUILD_PLAN") == "1":
        print("\n".join(manager.plan(recipe, context)))
    else:
        manager.run(recipe, context)
