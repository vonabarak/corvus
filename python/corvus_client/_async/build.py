"""Client-side YAML preprocessing + build streaming for `Daemon.build`.

The daemon rejects un-preprocessed build payloads — it expects:

  - `shell.script: path` rewritten to `shell.inline: <file contents>`
  - `file.from: path`   rewritten to `file.content: <base64 of bytes>`
Mirrors `Corvus.Client.Commands.Build.preprocessRoot` in the Haskell client.
"""

from __future__ import annotations

import base64
import re
from collections.abc import AsyncIterator
from pathlib import Path

import capnp

import yaml

from .. import types
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


def _rewrite_shell(prov: dict[str, object], base_dir: Path) -> None:
    sh = prov.get("shell")
    if not isinstance(sh, dict):
        return
    script = sh.pop("script", None)
    if isinstance(script, str):
        sh["inline"] = _read_text(base_dir, script)


def _rewrite_file(prov: dict[str, object], base_dir: Path) -> None:
    fl = prov.get("file")
    if not isinstance(fl, dict):
        return
    src = fl.pop("from", None)
    if isinstance(src, str):
        data = _read_bytes(base_dir, src)
        fl["content"] = base64.b64encode(data).decode("ascii")


def resolve_build_defaults(text: str) -> str:
    """Expand declared variable defaults in build YAML before RPC submission.

    Required variables must be supplied by the caller in the document's vars
    mapping. Documents without vars are already preprocessed and pass through.
    """
    doc = yaml.safe_load(text)
    if not isinstance(doc, dict) or "vars" not in doc:
        return text
    declarations = doc.pop("vars")
    if declarations is None:
        declarations = {}
    if not isinstance(declarations, dict):
        raise ValueError("build vars must be a mapping")
    values: dict[str, str] = {}
    for name, value in declarations.items():
        if not isinstance(name, str):
            raise ValueError("build variable names must be strings")
        if value is None:
            raise ValueError(f"build variable {name!r} is required")
        if not isinstance(value, (str, int, float, bool)):
            raise ValueError(f"build variable {name!r} must be a scalar")
        values[name] = str(value).lower() if isinstance(value, bool) else str(value)

    def expand(match: re.Match[str]) -> str:
        token = match.group()
        if token in {"{{{{", "}}}}"}:
            return token[:2]
        if not token.endswith("}}"):
            raise ValueError("malformed build variable reference")
        name = token[2:-2].strip()
        if not name.isidentifier() or name not in values:
            raise ValueError(f"unknown build variable {name!r}")
        return values[name]

    def walk(value: object) -> object:
        if isinstance(value, str):
            return re.sub(
                r"\{\{\{\{|\}\}\}\}|\{\{.*?\}\}|\{\{", expand, value, flags=re.DOTALL
            )
        if isinstance(value, list):
            return [walk(item) for item in value]
        if isinstance(value, dict):
            return {key: walk(item) for key, item in value.items()}
        return value

    return yaml.safe_dump(walk(doc), sort_keys=False)


def preprocess_build_yaml(yaml_path: str) -> str:
    """Read `yaml_path`, inline references, return the rewritten YAML text."""
    path = Path(yaml_path).resolve()
    base_dir = path.parent
    with open(path, encoding="utf-8") as f:
        doc = yaml.safe_load(resolve_build_defaults(f.read()))
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
    daemon: capnp.lib.capnp._DynamicCapabilityClient,
    yaml_path: str,
) -> AsyncIterator[types.BuildStreamItem]:
    """Run `Daemon.build` on a preprocessed YAML file.

    Yields `BuildEvent` dataclasses as they arrive, followed by a final
    `('task_id', N)` tuple once the pipeline completes.
    """
    text = preprocess_build_yaml(yaml_path)
    text = await preprocess_uploads(daemon, text, Path(yaml_path).resolve().parent)
    async for item in stream_build_events(
        daemon,
        text,
    ):
        yield item


async def preprocess_uploads(
    daemon: capnp.lib.capnp._DynamicCapabilityClient, text: str, base_dir: Path
) -> str:
    """Upload leading local media and remove those steps from daemon input."""
    doc = yaml.safe_load(text)
    if not isinstance(doc, dict) or not isinstance(doc.get("pipeline"), list):
        return text
    steps = doc["pipeline"]
    first_non_upload = 0
    for step in steps:
        if not isinstance(step, dict) or "upload" not in step:
            break
        first_non_upload += 1
    if any(
        isinstance(step, dict) and "upload" in step for step in steps[first_non_upload:]
    ):
        raise ValueError("pipeline upload steps must precede apply/build steps")
    if not first_non_upload:
        return text
    disks = AsyncDiskManager(daemon)
    for step in steps[:first_non_upload]:
        if len(step) != 1 or not isinstance(step["upload"], dict):
            raise ValueError("pipeline upload step must contain only an upload object")
        await _upload_media(disks, step["upload"], base_dir)
    doc["pipeline"] = steps[first_non_upload:]
    return yaml.safe_dump(doc, sort_keys=False)


async def _upload_media(
    disks: AsyncDiskManager, upload: dict[str, object], base_dir: Path
) -> None:
    def required_text(key: str) -> str:
        value = upload.get(key)
        if not isinstance(value, str):
            raise ValueError(f"upload.{key} is required and must be a string")
        return value

    name = required_text("name")
    source = Path(required_text("from"))
    format = required_text("format")
    if not source.is_absolute():
        source = base_dir / source
    policy = upload.get("ifExists", "error")
    if not isinstance(policy, str) or policy not in {
        "error",
        "skip",
        "overwrite",
        "update",
    }:
        raise ValueError("upload.ifExists must be error, skip, overwrite or update")
    path = upload.get("path")
    if path is not None and not isinstance(path, str):
        raise ValueError("upload.path must be a string")
    ephemeral = upload.get("ephemeral", True)
    if not isinstance(ephemeral, bool):
        raise ValueError("upload.ephemeral must be a boolean")
    node = upload.get("node")
    if node is not None and (
        isinstance(node, bool) or not isinstance(node, (int, str))
    ):
        raise ValueError("upload.node must be an integer or string")
    await disks.upload_from_file(
        name,
        source,
        format=format,
        path=path,
        ephemeral=ephemeral,
        node=node,
        if_exists=policy,
    )
