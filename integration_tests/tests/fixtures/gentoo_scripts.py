"""Helpers for exercising the real, split Gentoo shell modules."""

import re
from pathlib import Path


def library_text(scripts: Path) -> str:
    paths = sorted((scripts / "lib").glob("*.sh"))
    paths.append(scripts / "images/corvus-test-vm/lib.sh")
    return "\n".join(re.sub(r"^\. .*\n", "", p.read_text(), flags=re.M) for p in paths)


def write_fake_libraries(scripts: Path, content: str) -> None:
    """Keep real definitions and replace only boundary operations in isolated tests."""
    for name in (
        "environment",
        "portage",
        "mounts",
        "disk",
        "image",
        "runtime",
        "corvus-build",
        "postgres",
        "vde-network",
    ):
        path = scripts / "lib" / (name + ".sh")
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(content)
