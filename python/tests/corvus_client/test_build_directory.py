"""Offline tests for client-side directory provisioner expansion."""

from __future__ import annotations

import base64
import os
from pathlib import Path
from unittest.mock import patch

import pytest
from corvus_client._async.build import preprocess_build_yaml

import yaml


def preprocess(tmp_path: Path, steps: list[object]) -> list[object]:
    path = tmp_path / "build.yml"
    path.write_text(yaml.safe_dump({"pipeline": [{"build": {"provisioners": steps}}]}))
    result = yaml.safe_load(preprocess_build_yaml(str(path)))["pipeline"][0]["build"][
        "provisioners"
    ]
    assert isinstance(result, list)
    return result


def test_recursive_expansion_and_order(tmp_path: Path) -> None:
    root = tmp_path / "files"
    (root / "config").mkdir(parents=True)
    (root / "empty").mkdir()
    (root / "z").write_bytes(b"z")
    (root / "config" / "app.ini").write_bytes(b"hi")
    (root / ".hidden").write_bytes(b"\x00\xff")
    (root / "a").write_bytes(b"")
    before, after = {"shell": "echo before"}, {"shell": "echo after"}
    steps = [before, {"directory": {"from": "files", "to": "/opt/app/"}}, after]
    output = preprocess(tmp_path, steps)
    expected = [
        before,
        *[
            {"file": {"to": "/opt/app/" + name, "content": content}}
            for name, content in (
                (".hidden", "AP8="),
                ("a", ""),
                ("config/app.ini", "aGk="),
                ("z", "eg=="),
            )
        ],
        after,
    ]
    assert output == expected
    assert preprocess(tmp_path, steps) == expected


@pytest.mark.parametrize("mode", [None, "0640", "u+x"])
def test_absolute_source_and_mode(tmp_path: Path, mode: str | None) -> None:
    root = tmp_path / "files"
    root.mkdir()
    (root / "x").write_bytes(b"x")
    output = preprocess(
        tmp_path,
        [{"directory": {"from": str(root), "to": "/opt/app", "mode": mode}}],
    )
    fields = {"to": "/opt/app/x", "content": "eA=="}
    if mode is not None:
        fields["mode"] = mode
    assert output == [{"file": fields}]


def test_variables_and_unchanged_pipeline_steps(tmp_path: Path) -> None:
    root = tmp_path / "files"
    root.mkdir()
    (root / "x").write_bytes(b"{{ source }}")
    path = tmp_path / "build.yml"
    apply: dict[str, object] = {"apply": {"templates": []}}
    upload = {"upload": {"from": "image.raw"}}
    path.write_text(
        yaml.safe_dump(
            {
                "vars": {"source": "files"},
                "pipeline": [
                    upload,
                    apply,
                    {
                        "build": {
                            "provisioners": [
                                {"directory": {"from": "{{ source }}", "to": "/x"}}
                            ]
                        }
                    },
                ],
            }
        )
    )
    result = yaml.safe_load(preprocess_build_yaml(str(path)))
    assert result["pipeline"][:2] == [upload, apply]
    file = result["pipeline"][2]["build"]["provisioners"][0]["file"]
    assert base64.b64decode(file["content"]) == b"{{ source }}"


def test_empty_directory(tmp_path: Path) -> None:
    (tmp_path / "empty").mkdir()
    assert preprocess(tmp_path, [{"directory": {"from": "empty", "to": "/x"}}]) == []


@pytest.mark.parametrize(
    "fields",
    [
        None,
        "files",
        {},
        {"from": "", "to": "/x"},
        {"from": "files"},
        {"from": "files", "to": ""},
        {"from": 1, "to": "/x"},
        {"from": "files", "to": 1},
        {"from": "files", "to": "/x", "mode": 644},
        {"from": "files", "to": "/x", "extra": True},
    ],
)
def test_invalid_directory(tmp_path: Path, fields: object) -> None:
    with pytest.raises(ValueError, match="directory:"):
        preprocess(tmp_path, [{"directory": fields}])


@pytest.mark.parametrize("kind", ["shell", "file", "wait-for", "reboot"])
def test_mixed_kinds_rejected_before_empty_expansion(tmp_path: Path, kind: str) -> None:
    (tmp_path / "empty").mkdir()
    with pytest.raises(ValueError, match="exactly one kind"):
        preprocess(
            tmp_path,
            [{"directory": {"from": "empty", "to": "/x"}, kind: {}}],
        )


def test_missing_and_non_directory_source(tmp_path: Path) -> None:
    with pytest.raises(FileNotFoundError, match="missing"):
        preprocess(tmp_path, [{"directory": {"from": "missing", "to": "/x"}}])
    (tmp_path / "plain").write_bytes(b"x")
    with pytest.raises(ValueError, match="plain"):
        preprocess(tmp_path, [{"directory": {"from": "plain", "to": "/x"}}])


@pytest.mark.parametrize("target", ["file", "directory", "missing"])
def test_symlinks_rejected(tmp_path: Path, target: str) -> None:
    root = tmp_path / "files"
    root.mkdir()
    (tmp_path / "file").write_bytes(b"x")
    (tmp_path / "directory").mkdir()
    (root / "link").symlink_to(tmp_path / target)
    with pytest.raises(ValueError, match="link"):
        preprocess(tmp_path, [{"directory": {"from": "files", "to": "/x"}}])


@pytest.mark.parametrize("source", ["link", "link/", "link/."])
def test_source_root_symlink_rejected(tmp_path: Path, source: str) -> None:
    (tmp_path / "files").mkdir()
    (tmp_path / "link").symlink_to(tmp_path / "files")
    with pytest.raises(ValueError, match="link"):
        preprocess(tmp_path, [{"directory": {"from": source, "to": "/x"}}])


def test_special_files_rejected(tmp_path: Path) -> None:
    (tmp_path / "files").mkdir()
    os.mkfifo(tmp_path / "files" / "fifo")
    with pytest.raises(ValueError, match="fifo"):
        preprocess(tmp_path, [{"directory": {"from": "files", "to": "/x"}}])


def test_read_errors_propagate(tmp_path: Path) -> None:
    (tmp_path / "files").mkdir()
    (tmp_path / "files" / "unreadable").touch()
    with (
        patch.object(Path, "read_bytes", side_effect=PermissionError("unreadable")),
        pytest.raises(PermissionError, match="unreadable"),
    ):
        preprocess(tmp_path, [{"directory": {"from": "files", "to": "/x"}}])
