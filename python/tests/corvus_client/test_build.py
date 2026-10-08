"""Build YAML preprocessing + streaming end-to-end."""

from __future__ import annotations

import base64
from pathlib import Path

from corvus_client import AsyncClient
from corvus_client._async.build import preprocess_build_yaml
from corvus_client.types import BuildPipelineEnd

import yaml

from ._helpers import with_client


def test_preprocess_inlines_shell_and_file(tmp_path: Path) -> None:
    script_path = tmp_path / "init.sh"
    script_path.write_text("#!/bin/sh\necho hi\n")
    file_path = tmp_path / "payload.bin"
    file_path.write_bytes(b"\x00\x01\x02hello")

    yaml_path = tmp_path / "pipeline.yml"
    yaml_path.write_text(
        yaml.safe_dump(
            {
                "pipeline": [
                    {
                        "build": {
                            "provisioners": [
                                {"shell": {"script": "init.sh"}},
                                {"file": {"from": "payload.bin", "dest": "/x"}},
                            ],
                        }
                    }
                ]
            }
        )
    )
    out_yaml = preprocess_build_yaml(str(yaml_path))
    out = yaml.safe_load(out_yaml)

    build = out["pipeline"][0]["build"]
    shell = build["provisioners"][0]["shell"]
    file_ = build["provisioners"][1]["file"]

    assert "script" not in shell
    assert shell["inline"] == "#!/bin/sh\necho hi\n"

    assert "from" not in file_
    assert base64.b64decode(file_["content"]) == b"\x00\x01\x02hello"


def test_build_stream_reports_pipeline_end(daemon_socket: Path, tmp_path: Path) -> None:
    """A trivial pipeline with one build step returning an error event.

    We avoid an actual VM boot (which would require KVM + a real image)
    by giving the build step an invalid source image. The daemon still
    emits stepStart / buildEnd / pipelineEnd events, which is what the
    test asserts on.
    """
    yaml_path = tmp_path / "pipeline.yml"
    yaml_path.write_text(
        yaml.safe_dump(
            {
                "pipeline": [
                    {
                        "build": {
                            "name": "py-build-noop",
                            "source": "nonexistent-disk",
                            "target": "py-build-target",
                        }
                    }
                ]
            }
        )
    )

    run = with_client(daemon_socket)

    async def go(c: AsyncClient) -> None:
        saw_pipeline_end = False
        last_task_id = None
        async for item in c.build_stream(str(yaml_path)):
            if isinstance(item, BuildPipelineEnd):
                saw_pipeline_end = True
            elif isinstance(item, tuple) and item[0] == "task_id":
                last_task_id = item[1]
        # Even on an errored build, the daemon emits a pipelineEnd and
        # returns a task id.
        assert saw_pipeline_end, "pipelineEnd event was not seen"
        assert isinstance(last_task_id, int) and last_task_id > 0

    run(go)


def test_upload_preprocessing_forwards_policy_and_removes_only_uploads(
    tmp_path: Path,
) -> None:
    import asyncio
    from unittest.mock import AsyncMock, Mock, patch

    from corvus_client._async.build import preprocess_uploads

    disks = Mock(upload_from_file=AsyncMock())
    remaining: dict[str, object] = {"apply": {"templates": []}}
    text = yaml.safe_dump(
        {
            "pipeline": [
                {
                    "upload": {
                        "name": "answer",
                        "from": "answer.iso",
                        "format": "raw",
                        "ifExists": "update",
                        "ephemeral": False,
                    }
                },
                remaining,
            ]
        }
    )
    with patch("corvus_client._async.build.AsyncDiskManager", return_value=disks):
        result = asyncio.run(preprocess_uploads(Mock(), text, tmp_path))
    assert yaml.safe_load(result)["pipeline"] == [remaining]
    disks.upload_from_file.assert_awaited_once_with(
        "answer",
        tmp_path / "answer.iso",
        format="raw",
        path=None,
        ephemeral=False,
        node=None,
        if_exists="update",
    )
