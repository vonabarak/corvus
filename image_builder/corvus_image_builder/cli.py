"""Load explicit Python recipe factories."""

import argparse
import importlib
import signal
from pathlib import Path

from .core import BuildContext, BuildManager, ImageRecipe


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("action", choices=("plan", "run"))
    parser.add_argument("recipe", help="module:factory")
    parser.add_argument("--root", type=Path, required=True)
    parser.add_argument("--disk", type=Path)
    parser.add_argument("--cache", type=Path, default=Path("/mnt/corvus-cache"))
    parser.add_argument("--kernel-config", type=Path)
    parser.add_argument("--use-binary", choices=("true", "false"), default="true")
    parser.add_argument("--fetch-binary", choices=("true", "false"), default="true")
    args = parser.parse_args()
    module, factory = args.recipe.split(":", 1)
    if args.recipe == "corvus_image_builder.gentoo:headless":
        from .gentoo import Settings, headless

        if args.disk is None or args.kernel_config is None:
            parser.error("headless requires --disk and --kernel-config")
        recipe = headless(
            Settings(
                args.disk,
                args.cache,
                args.kernel_config,
                args.use_binary == "true",
                args.fetch_binary == "true",
            )
        )
    else:
        recipe = getattr(importlib.import_module(module), factory)()
    if not isinstance(recipe, ImageRecipe):
        parser.error("Factory must return ImageRecipe")
    manager = BuildManager()
    context = BuildContext(args.root.resolve())
    if args.action == "plan":
        print("\n".join(manager.plan(recipe, context)))
    else:

        def interrupted(signum: int, frame: object) -> None:
            raise KeyboardInterrupt(f"Interrupted by signal {signum}")

        signal.signal(signal.SIGTERM, interrupted)
        signal.signal(signal.SIGHUP, interrupted)
        manager.run(recipe, context)
