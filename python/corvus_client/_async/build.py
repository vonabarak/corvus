"""Client-side YAML preprocessing + build streaming for `Daemon.build`.

The daemon rejects un-preprocessed build payloads — it expects:

  - `shell.script: path` rewritten to `shell.inline: <file contents>`
  - `file.from: path`   rewritten to `file.content: <base64 of bytes>`
Mirrors `Corvus.Client.Commands.Build.preprocessRoot` in the Haskell client.
"""

from __future__ import annotations

import base64
from collections.abc import AsyncIterator
from pathlib import Path
from typing import Any

import yaml

from .disk import AsyncDiskManager
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
    path = Path(yaml_path).resolve()
    doc = yaml.safe_load(text)
    if isinstance(doc, dict):
        steps = doc.get("pipeline")
        if isinstance(steps, list):
            uploads: list[dict[str, Any]] = []
            rest: list[Any] = []
            seen_non_upload = False
            for step in steps:
                if isinstance(step, dict) and isinstance(step.get("upload"), dict):
                    if seen_non_upload:
                        raise ValueError(
                            "pipeline upload steps must precede apply/build steps"
                        )
                    uploads.append(step["upload"])
                else:
                    seen_non_upload = True
                    rest.append(step)
            if uploads:
                disks = AsyncDiskManager(daemon)
                for upload in uploads:
                    try:
                        name = upload["name"]
                        source = upload["from"]
                        format = upload["format"]
                    except KeyError as exc:
                        raise ValueError(f"upload.{exc.args[0]} is required") from exc
                    if (
                        not isinstance(name, str)
                        or not isinstance(source, str)
                        or not isinstance(format, str)
                    ):
                        raise ValueError(
                            "upload name, from, and format must be strings"
                        )
                    source_path = Path(source)
                    if not source_path.is_absolute():
                        source_path = path.parent / source_path
                    if upload.get("ifExists", "error") not in {"error", "overwrite"}:
                        raise ValueError(
                            "upload.ifExists must be 'error' or 'overwrite'"
                        )
                    await disks.upload_from_file(
                        name,
                        source_path,
                        format=format,
                        path=upload.get("path"),
                        ephemeral=upload.get("ephemeral", True),
                        node=upload.get("node"),
                        overwrite=upload.get("ifExists") == "overwrite",
                    )
                doc["pipeline"] = rest
                text = yaml.safe_dump(doc, sort_keys=False)
    async for item in stream_build_events(
        daemon,
        text,
        use_cache=use_cache,
        build_cache=build_cache,
        rebuild_from=rebuild_from,
    ):
        yield item
