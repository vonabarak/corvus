"""Screenshot completion and atomic, non-replacing local publication."""

from __future__ import annotations

import asyncio
from pathlib import Path
from types import SimpleNamespace
from unittest.mock import AsyncMock, Mock

import pytest
from corvus_client._async.files import FileSink
from corvus_client._async.vm import AsyncVm


@pytest.mark.parametrize(
    "failure", [None, "rpc", "cancel", "abort", "no_end", "count", "race"]
)
def test_screenshot_atomic(tmp_path: Path, failure: str | None) -> None:
    path = tmp_path / "screen.png"
    payload = b"\x89PNG\r\n\x1a\n" + bytes(range(256)) * 300
    saved: list[FileSink] = []

    async def capture(*, sink: FileSink) -> SimpleNamespace:
        saved.append(sink)
        for offset in range(0, len(payload), 32768):
            await sink.write(payload[offset : offset + 32768], None)
        assert not path.exists()
        if failure == "rpc":
            raise ValueError("capture failed")
        if failure == "cancel":
            raise asyncio.CancelledError()
        if failure == "race":
            path.write_bytes(b"original")
        if failure == "abort":
            await sink.abort(None)
        elif failure != "no_end":
            await sink.end(None)
        return SimpleNamespace(
            bytesTransferred=0 if failure == "count" else len(payload)
        )

    vm = AsyncVm(Mock(screenshot=AsyncMock(side_effect=capture)))
    if failure:
        with pytest.raises((OSError, ValueError, asyncio.CancelledError)):
            asyncio.run(vm.screenshot_to_file(path))
        assert list(tmp_path.iterdir()) == ([path] if failure == "race" else [])
        if failure == "race":
            assert path.read_bytes() == b"original"
    else:
        assert asyncio.run(vm.screenshot_to_file(path)) == len(payload)
        assert path.read_bytes() == payload
        assert list(tmp_path.iterdir()) == [path]
    with pytest.raises(ValueError, match="closed"):
        asyncio.run(saved[0].write(b"late", None))


@pytest.mark.parametrize("symlink", [False, True])
def test_screenshot_existing_destination(tmp_path: Path, symlink: bool) -> None:
    path = tmp_path / "screen.png"
    if symlink:
        path.symlink_to(tmp_path / "absent")
    else:
        path.write_bytes(b"original")
    cap = Mock(screenshot=AsyncMock())
    with pytest.raises(FileExistsError):
        asyncio.run(AsyncVm(cap).screenshot_to_file(path))
    cap.screenshot.assert_not_called()
    assert list(tmp_path.iterdir()) == [path]


def test_screenshot_daemon_guards(daemon_socket: Path, tmp_path: Path) -> None:
    from corvus_client import AsyncClient, VmHeadless, VmNotRunning

    from ._helpers import with_client

    async def go(client: AsyncClient) -> None:
        for headless, error in [(False, VmNotRunning), (True, VmHeadless)]:
            vm = await client.vms.create(
                f"screenshot-guard-{headless}", headless=headless
            )
            destination = tmp_path / f"{headless}.png"
            with pytest.raises(error):
                await vm.screenshot_to_file(destination)
            assert not destination.exists()
        assert list(tmp_path.iterdir()) == []

    with_client(daemon_socket)(go)
