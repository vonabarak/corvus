"""File transfer callbacks, completion and atomic local replacement."""

from __future__ import annotations

import asyncio
from pathlib import Path
from types import SimpleNamespace
from unittest.mock import AsyncMock, Mock

import capnp
import pytest
from corvus_client import AsyncClient, VmNotRunning, _schema
from corvus_client._async.files import FileSink, FileSource
from corvus_client._async.vm import AsyncVm

from ._helpers import with_client


@pytest.mark.parametrize(
    "body",
    [b"", bytes(range(256)) * 400, b"x" * 32768],
    ids=["empty", "binary-multichunk", "exact-chunk"],
)
def test_upload_chunks(tmp_path: Path, body: bytes) -> None:
    path = tmp_path / "source"
    path.write_bytes(body)
    received: list[bytes] = []
    saved: list[FileSource] = []

    async def upload(*, path: str, source: FileSource) -> SimpleNamespace:
        assert path == r"C:\Temp\binary"
        saved.append(source)
        while True:
            chunk, eof = await source.read(32768, None)
            assert len(chunk) <= 32768
            received.append(chunk)
            if eof:
                break
        return SimpleNamespace(bytesTransferred=sum(map(len, received)))

    vm = AsyncVm(Mock(uploadFile=AsyncMock(side_effect=upload)))
    assert asyncio.run(vm.upload_from_file(path, r"C:\Temp\binary")) == len(body)
    assert b"".join(received) == body
    with pytest.raises(ValueError, match="closed"):
        asyncio.run(saved[0].read(1, None))


@pytest.mark.parametrize(
    "failure", [None, "rpc", "no_end", "wrong_count", "abort", "cancel"]
)
def test_download_atomic(tmp_path: Path, failure: str | None) -> None:
    path = tmp_path / "destination"
    path.write_bytes(b"original")
    saved: list[FileSink] = []

    async def download(*, path: str, sink: FileSink) -> SimpleNamespace:
        saved.append(sink)
        await sink.write(b"\0\xffpayload", None)
        assert (tmp_path / "destination").read_bytes() == b"original"
        if failure == "rpc":
            raise ValueError("RPC failed")
        if failure == "cancel":
            raise asyncio.CancelledError()
        if failure == "abort":
            await sink.abort(None)
        elif failure != "no_end":
            await sink.end(None)
        return SimpleNamespace(bytesTransferred=0 if failure == "wrong_count" else 9)

    vm = AsyncVm(Mock(downloadFile=AsyncMock(side_effect=download)))
    if failure:
        with pytest.raises((OSError, ValueError, asyncio.CancelledError)):
            asyncio.run(vm.download_to_file("/binary", path))
        assert path.read_bytes() == b"original"
    else:
        assert asyncio.run(vm.download_to_file("/binary", path)) == 9
        assert path.read_bytes() == b"\0\xffpayload"
    assert list(tmp_path.iterdir()) == [path]
    with pytest.raises(ValueError, match="closed"):
        asyncio.run(saved[0].write(b"late", None))


def test_source_real_capability(tmp_path: Path) -> None:
    """Exercise pycapnp's multi-result callback encoding, beyond mocks."""
    path = tmp_path / "source"
    path.write_bytes(b"\0\xff")

    async def go() -> None:
        async with capnp.kj_loop():
            with path.open("rb") as handle:
                source = FileSource(handle)
                client = _schema.streams.ByteSource._new_client(source)
                reply = await client.read(maxBytes=32768)
                assert bytes(reply.chunk) == b"\0\xff"
                assert reply.eof

    asyncio.run(go())


def test_stopped_vm_transfers_preserve_destination(
    daemon_socket: Path, tmp_path: Path
) -> None:
    source = tmp_path / "source"
    source.write_bytes(b"data")
    destination = tmp_path / "destination"
    destination.write_bytes(b"original")

    async def go(client: AsyncClient) -> None:
        vm = await client.vms.create("guest-file-stopped", guest_agent=True)
        with pytest.raises(VmNotRunning):
            await vm.upload_from_file(source, "/tmp/file")
        with pytest.raises(VmNotRunning):
            await vm.download_to_file("/tmp/file", destination)
        assert destination.read_bytes() == b"original"
        assert sorted(p.name for p in tmp_path.iterdir()) == ["destination", "source"]

    with_client(daemon_socket)(go)
