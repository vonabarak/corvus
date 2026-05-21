"""Session-scoped check that the integration-test image is in place.

The harness leaves image bake-out to `crv build`, which Corvus already
makes idempotent via `ifExists: skip` on every YAML stage. Our job
here is just to:

  1. Detect whether the base disk the suite needs is already registered
     in the outer daemon (fast path).
  2. If not, run `crv build` on
     [yaml/corvus-test-node/corvus-test-node.yml](../../yaml/corvus-test-node/corvus-test-node.yml)
     (a `pipeline:`-rooted document — apply + build steps) and wait
     for the bake to finish.

The YAML doesn't declare a VM template — Corvus templates don't yet
support `sharedDirs`, and every test VM needs a per-run share-dir
attached at create time. The harness builds each VM from the base
disk via an inline `apply` YAML; see `topology.py`.

First run takes 30-60 min (kernel + stage3 + emerges); subsequent runs
return in milliseconds.
"""

from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path
from typing import Optional

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
        yaml_path: Optional[Path] = None,
        disk_name: str = DISK_NAME,
    ) -> "ImageReady":
        path = yaml_path or DEFAULT_YAML
        if not path.is_file():
            raise FileNotFoundError(f"integration-test YAML not found at {path}")
        if not _disk_exists(crv, disk_name):
            crv.build(path, wait=True, timeout_sec=4 * 3600)
            if not _disk_exists(crv, disk_name):
                raise RuntimeError(
                    f"`crv build {path}` finished but disk {disk_name!r} not registered"
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
