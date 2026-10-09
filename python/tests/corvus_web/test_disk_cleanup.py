"""The cleanup route preserves typed reports, including partial failures."""

from __future__ import annotations

import asyncio
from typing import cast
from unittest.mock import AsyncMock, MagicMock

import pytest
from corvus_client import AsyncClient
from corvus_client.types import DiskCleanupReport
from fastapi import HTTPException

from corvus_web.routes.disks import CleanupBody, cleanup_disks


def test_cleanup_route_returns_partial_report() -> None:
    client = MagicMock(spec=AsyncClient)
    client.disks.cleanup = AsyncMock(return_value=DiskCleanupReport(False, [], 2, 3, 1))
    result = asyncio.run(
        cleanup_disks(
            CleanupBody(all_images=True, node="worker"), cast(AsyncClient, client)
        )
    )
    assert result["failures"] == 1
    assert result["removed_versions"] == 2
    client.disks.cleanup.assert_awaited_once_with(
        None, all_images=True, node="worker", include_tagged=False, dry_run=False
    )


def test_cleanup_route_rejects_invalid_target() -> None:
    client = MagicMock(spec=AsyncClient)
    client.disks.cleanup = AsyncMock(
        side_effect=ValueError("Select exactly one image name")
    )
    with pytest.raises(HTTPException) as error:
        asyncio.run(cleanup_disks(CleanupBody(), cast(AsyncClient, client)))
    assert error.value.status_code == 400
