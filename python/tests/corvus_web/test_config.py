"""The SPA reads lifecycle actions from the gateway config route."""

from __future__ import annotations

from corvus_web.app import create_app
from corvus_web.config import CorvusWebConfig


def test_config_route_matches_spa_request() -> None:
    app = create_app(CorvusWebConfig())
    paths = app.openapi()["paths"]

    assert "get" in paths["/api/config"]
