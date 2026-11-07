"""Storage availability has a paired sentinel: only zero total means unknown."""

from __future__ import annotations

import pytest
from corvus_client import _schema
from corvus_client import types as t
from corvus_client._async import _convert


@pytest.mark.parametrize(
    "total,free,expected", [(100, 0, 0), (100, 50, 50), (0, 0, None)]
)
def test_storage_capacity_conversion(
    total: int, free: int, expected: int | None
) -> None:
    info = _schema.node.NodeInfo.new_message(
        storageBytesTotal=total, storageBytesFree=free
    )
    details = _schema.node.NodeDetails.new_message(
        storageBytesTotal=total, storageBytesFree=free
    )
    results: list[t.NodeInfo | t.NodeDetails] = [
        _convert.node_info(info.as_reader()),
        _convert.node_details(details.as_reader()),
    ]
    for result in results:
        assert result.storage_bytes_free == expected
        assert result.storage_bytes_total == (total or None)
