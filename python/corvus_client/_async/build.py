"""Client-side YAML preprocessing + build streaming for `Daemon.build`.

The daemon rejects un-preprocessed build payloads — it expects:

  - `shell.script: path` rewritten to `shell.inline: <file contents>`
  - `file.from: path`   rewritten to `file.content: <base64 of bytes>`
  - `floppy.from: path` rewritten to `floppy.contentBase64: <base64>`
    (and `floppy.filename` defaulted to the source basename)

Mirrors `Corvus.Client.Commands.Build.preprocessRoot` in the Haskell client.
"""

from __future__ import annotations

import base64
import os
from collections.abc import AsyncIterator
from pathlib import Path
from typing import Any

import yaml

from .streams import stream_build_events


def _read_text(base_dir: Path, rel: str) -> str:
    path = rel if rel.startswith("/") else str(base_dir / rel)
    with open(path, encoding="utf-8") as f:
        return f.read()


def _read_bytes(base_dir: Path, rel: str) -> bytes:
    path = rel if rel.startswith("/") else str(base_dir / rel)
    with open(path, "rb") as f:
        return f.read()


def _rewrite_shell(prov: dict, base_dir: Path) -> None:
    sh = prov.get("shell")
    if not isinstance(sh, dict):
        return
    script = sh.pop("script", None)
    if isinstance(script, str):
        sh["inline"] = _read_text(base_dir, script)


def _rewrite_file(prov: dict, base_dir: Path) -> None:
    fl = prov.get("file")
    if not isinstance(fl, dict):
        return
    src = fl.pop("from", None)
    if isinstance(src, str):
        data = _read_bytes(base_dir, src)
        fl["content"] = base64.b64encode(data).decode("ascii")


def _rewrite_floppy(build: dict, base_dir: Path) -> None:
    fp = build.get("floppy")
    if not isinstance(fp, dict):
        return
    src = fp.pop("from", None)
    if isinstance(src, str):
        data = _read_bytes(base_dir, src)
        fp.setdefault("filename", os.path.basename(src))
        fp["contentBase64"] = base64.b64encode(data).decode("ascii")


def preprocess_build_yaml(yaml_path: str) -> str:
    """Read `yaml_path`, inline references, return the rewritten YAML text."""
    path = Path(yaml_path).resolve()
    base_dir = path.parent
    with open(path, encoding="utf-8") as f:
        doc = yaml.safe_load(f)
    if not isinstance(doc, dict):
        return yaml.safe_dump(doc, sort_keys=False)
    pipeline = doc.get("pipeline")
    if isinstance(pipeline, list):
        for step in pipeline:
            if not isinstance(step, dict):
                continue
            build = step.get("build")
            if not isinstance(build, dict):
                continue
            provisioners = build.get("provisioners")
            if isinstance(provisioners, list):
                for prov in provisioners:
                    if isinstance(prov, dict):
                        _rewrite_shell(prov, base_dir)
                        _rewrite_file(prov, base_dir)
            _rewrite_floppy(build, base_dir)
    return yaml.safe_dump(doc, sort_keys=False)


async def stream_build_from_file(
    daemon,
    yaml_path: str,
    *,
    use_cache: bool = False,
    build_cache: bool = False,
    rebuild_from: int = 0,
) -> AsyncIterator[Any]:
    """Run `Daemon.build` on a preprocessed YAML file.

    Yields `BuildEvent` dataclasses as they arrive, followed by a final
    `('task_id', N)` tuple once the pipeline completes.
    """
    text = preprocess_build_yaml(yaml_path)
    async for item in stream_build_events(
        daemon,
        text,
        use_cache=use_cache,
        build_cache=build_cache,
        rebuild_from=rebuild_from,
    ):
        yield item
