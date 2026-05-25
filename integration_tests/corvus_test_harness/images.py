"""Session-scoped check that the integration-test image is in place.

This module is a *precondition check only* — it confirms the
`corvus-test-node` disk is already registered with the outer daemon
and fails fast otherwise. Image baking lives entirely in the
Makefile (`make test-image` / `make test-image-node`); running it
from inside a pytest fixture used to race under pytest-xdist
because session scope is per-worker, so two workers could both
spawn `crv build` against the same outer daemon when the disk was
missing.

The YAML doesn't declare a VM template — Corvus templates don't yet
support `sharedDirs`, and every test VM needs a per-run share-dir
attached at create time. The harness builds each VM from the base
disk via an inline `apply` YAML; see `topology.py`.

Build path (run once per machine, lives outside pytest):

    make test-image            # umbrella: all integration-test images
    make test-image-node       # just this one
"""

from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path

from .outer import Crv, CrvError

# The base disk name must match the `target.name` in the YAML.
DISK_NAME = "corvus-test-node"

# The YAML file relative to the repo root.
DEFAULT_YAML = (
    Path(__file__).resolve().parents[2]
    / "yaml"
    / "corvus-test-node"
    / "corvus-test-node.yml"
)


@dataclass(frozen=True)
class ImageReady:
    disk_name: str
    yaml_path: Path

    @classmethod
    def ensure(
        cls,
        crv: Crv,
        *,
        yaml_path: Path | None = None,
        disk_name: str = DISK_NAME,
    ) -> ImageReady:
        """Confirm `disk_name` is registered with the outer daemon.

        Returns an `ImageReady` record on success. Raises
        `RuntimeError` with a pointer to the right `make` target
        when the disk is missing — the build path lives in the
        Makefile, not here, so two pytest-xdist workers can't race
        on it.
        """
        path = yaml_path or DEFAULT_YAML
        if not path.is_file():
            raise FileNotFoundError(f"integration-test YAML not found at {path}")
        if not _disk_exists(crv, disk_name):
            raise RuntimeError(
                f"Integration-test image {disk_name!r} is not registered "
                f"with the outer daemon. Run `make test-image` (or "
                f"`make test-image-node` for just this one) before "
                f"`make integration-tests`."
            )
        return cls(disk_name=disk_name, yaml_path=path)


def _disk_exists(crv: Crv, name: str) -> bool:
    try:
        crv.disk_show(name)
        return True
    except CrvError as e:
        if (e.kind or "").endswith("not_found") or "not found" in (
            e.message or ""
        ).lower():
            return False
        raise
