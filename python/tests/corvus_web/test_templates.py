"""Template list visibility and YAML editing."""

from __future__ import annotations

import asyncio
from typing import cast
from unittest.mock import AsyncMock, MagicMock

import pytest
from corvus_client import AsyncClient

from corvus_web.routes.templates import list_templates


@pytest.mark.parametrize("with_hidden", [False, True])
def test_list_templates_visibility(with_hidden: bool) -> None:
    client = MagicMock(spec=AsyncClient)
    client.templates.list = AsyncMock(return_value=[])
    assert asyncio.run(list_templates(cast(AsyncClient, client), with_hidden)) == []
    client.templates.list.assert_awaited_once_with(with_hidden=with_hidden)
