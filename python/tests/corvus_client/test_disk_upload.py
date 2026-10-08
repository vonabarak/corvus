"""Upload binding behavior using mocked RPC capabilities."""

from __future__ import annotations

import asyncio
import hashlib
from pathlib import Path
from types import SimpleNamespace
from unittest.mock import AsyncMock, Mock

import capnp
import pytest
from corvus_client._async.disk import AsyncDiskManager


def _manager(*, reuse: bool = False) -> tuple[AsyncDiskManager, Mock, Mock]:
    disk = Mock()
    upload = Mock(
        write=AsyncMock(),
        finish=AsyncMock(return_value=SimpleNamespace(disk=disk)),
        abort=AsyncMock(),
    )
    result = Mock(upload=upload, existing=disk)
    result.which.return_value = "existing" if reuse else "upload"
    rpc = Mock(beginUpload=AsyncMock(return_value=SimpleNamespace(result=result)))
    daemon = Mock(disks=AsyncMock(return_value=SimpleNamespace(mgr=rpc)))
    manager = AsyncDiskManager(daemon)
    return manager, rpc, upload


@pytest.mark.parametrize("reuse", [False, True])
def test_update_hashes_chunks_and_reuses_open_file(tmp_path: Path, reuse: bool) -> None:
    source = tmp_path / "answer.raw"
    body = b"a" * (1024 * 1024 + 123)
    source.write_bytes(body)
    manager, rpc, upload = _manager(reuse=reuse)

    response = rpc.beginUpload.return_value

    def inspect_params(
        *, params: capnp.lib.capnp._DynamicStructBuilder
    ) -> SimpleNamespace:
        assert params.ifExists == "update"
        assert params.expectedSha256 == hashlib.sha256(body).hexdigest()
        assert params.sourcePath == str(source.resolve())
        # Replacing the pathname after hashing must not replace the open stream.
        replacement = tmp_path / "replacement"
        replacement.write_bytes(b"different bytes")
        replacement.replace(source)
        return SimpleNamespace(result=response.result)

    rpc.beginUpload.side_effect = inspect_params
    asyncio.run(
        manager.upload_from_file(
            "answer:stable", source, format="raw", if_exists="update"
        )
    )
    if reuse:
        upload.write.assert_not_awaited()
        upload.finish.assert_not_awaited()
    else:
        chunks = [call.kwargs["chunk"] for call in upload.write.await_args_list]
        assert b"".join(chunks) == body
        assert all(len(chunk) <= 1024 * 1024 for chunk in chunks)
        upload.finish.assert_awaited_once()
    upload.abort.assert_not_awaited()


@pytest.mark.parametrize("policy", ["error", "skip", "overwrite"])
def test_policy_forwarding_without_hashing(tmp_path: Path, policy: str) -> None:
    manager, rpc, upload = _manager(reuse=True)
    asyncio.run(
        manager.upload_from_file(
            "answer", tmp_path / "absent", format="raw", if_exists=policy
        )
    )
    params = rpc.beginUpload.await_args.kwargs["params"]
    assert params.ifExists == policy
    assert params.expectedSha256 == ""
    upload.write.assert_not_awaited()


@pytest.mark.parametrize("operation", ["write", "finish"])
def test_failed_stream_aborts_and_preserves_error(
    tmp_path: Path, operation: str
) -> None:
    source = tmp_path / "answer.raw"
    source.write_bytes(b"payload")
    manager, _, upload = _manager()
    getattr(upload, operation).side_effect = ValueError("stream failed")
    upload.abort.side_effect = RuntimeError("abort failed")
    with pytest.raises(ValueError, match="stream failed"):
        asyncio.run(manager.upload_from_file("answer", source, format="raw"))
    upload.abort.assert_awaited_once()


def test_invalid_policy_does_not_begin_upload(tmp_path: Path) -> None:
    manager, rpc, _ = _manager()
    with pytest.raises(ValueError, match="if_exists"):
        asyncio.run(
            manager.upload_from_file(
                "answer", tmp_path / "absent", format="raw", if_exists="unknown"
            )
        )
    rpc.beginUpload.assert_not_awaited()
