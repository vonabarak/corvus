"""Load every Cap'n Proto schema file used by the Corvus client.

pycapnp's `capnp.load(path, imports=[dir])` parses an .capnp file and
returns a module-like object whose attributes are the schema's top-level
types (interfaces, structs, enums). We load every file once at import
time and expose the resulting modules under their schema base name.

Schemas are resolved in this order:

  1. `$CORVUS_SCHEMA_DIR` if set (developer override; usually points at
     the live `<repo>/schema/` tree).
  2. The `corvus_client/schema/` directory shipped with this package
     (populated by `make python-schema-sync` and bundled into wheels via
     `tool.setuptools.package-data`).
"""

from __future__ import annotations

import os
from pathlib import Path
from typing import Any

import capnp

_SCHEMA_FILES = (
    "common.capnp",
    "enums.capnp",
    "streams.capnp",
    "cloudinit.capnp",
    "sshkey.capnp",
    "task.capnp",
    "network.capnp",
    "disk.capnp",
    "template.capnp",
    "vm.capnp",
    "node.capnp",
    "corvus.capnp",
    "netagent.capnp",
)


def _resolve_schema_dir() -> Path:
    override = os.environ.get("CORVUS_SCHEMA_DIR")
    if override:
        return Path(override).resolve()
    return Path(__file__).resolve().parent / "schema"


SCHEMA_DIR: Path = _resolve_schema_dir()


def _load_all() -> dict[str, Any]:
    # Each value is whatever pycapnp returns from `capnp.load(...)`:
    # a runtime-generated module-like object whose attributes are the
    # schema's top-level types. Typed as `Any` so attribute access
    # like `_schema.vm.VmCreateParams` doesn't need a stub per type.
    if not SCHEMA_DIR.is_dir():
        raise RuntimeError(
            f"corvus_client: schema directory not found at {SCHEMA_DIR}. "
            "Set CORVUS_SCHEMA_DIR or run `make python-schema-sync`."
        )
    imports = [str(SCHEMA_DIR)]
    modules: dict[str, Any] = {}
    for fname in _SCHEMA_FILES:
        path = SCHEMA_DIR / fname
        if not path.is_file():
            raise RuntimeError(f"corvus_client: missing schema file {path}")
        key = fname[: -len(".capnp")]
        modules[key] = capnp.load(str(path), imports=imports)
    return modules


_MODULES = _load_all()

# Per-schema module attributes — typed Any for the same reason as
# the dict values above.
common: Any = _MODULES["common"]
enums: Any = _MODULES["enums"]
streams: Any = _MODULES["streams"]
cloudinit: Any = _MODULES["cloudinit"]
sshkey: Any = _MODULES["sshkey"]
task: Any = _MODULES["task"]
network: Any = _MODULES["network"]
disk: Any = _MODULES["disk"]
template: Any = _MODULES["template"]
vm: Any = _MODULES["vm"]
node: Any = _MODULES["node"]
corvus: Any = _MODULES["corvus"]
netagent: Any = _MODULES["netagent"]
