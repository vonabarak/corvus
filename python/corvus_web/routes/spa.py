"""Static-file serving for the React SPA.

The SPA uses HTML5 history routing, so any path that doesn't match a
real file in the build output should fall back to ``index.html`` — the
client-side router then takes over. ``/api/*`` routes are mounted
earlier and won't reach this router."""

from __future__ import annotations

from pathlib import Path

from fastapi import APIRouter, Request
from fastapi.responses import FileResponse, PlainTextResponse, Response
from fastapi.staticfiles import StaticFiles


def build_router(frontend_dir: Path) -> APIRouter:
    """Construct the SPA-serving router for a given build directory.

    Two cases:

    * The directory contains an ``index.html`` — mount ``StaticFiles``
      under ``/assets`` for vite's hashed asset URLs and serve
      ``index.html`` for everything else (HTML5 history fallback).
    * The directory is empty or missing ``index.html`` — return a
      plain-text 404 explaining how to build the frontend. This is
      the dev experience when running from a fresh source checkout
      before ``make web-build``."""

    router = APIRouter()
    index_html = frontend_dir / "index.html"
    assets_dir = frontend_dir / "assets"

    if assets_dir.is_dir():
        # vite places hashed JS/CSS/images under /assets/. Mount with
        # an extended TTL since the filenames are content-addressed.
        router.mount(
            "/assets",
            StaticFiles(directory=assets_dir, html=False),
            name="assets",
        )

    @router.get("/{path:path}", include_in_schema=False)
    async def serve_spa(request: Request, path: str) -> Response:
        if not index_html.is_file():
            return PlainTextResponse(
                (
                    "corvus-web: frontend bundle missing.\n"
                    f"Expected {index_html} to exist.\n"
                    "Run `make web-build` (or `npm run build` inside frontend/).\n"
                ),
                status_code=404,
            )
        # Try a direct file hit first (favicon.ico, robots.txt, etc.).
        candidate = frontend_dir / path if path else index_html
        if candidate.is_file() and candidate.resolve().is_relative_to(
            frontend_dir.resolve()
        ):
            return FileResponse(candidate)
        # Anything else: hand back index.html and let react-router
        # render the right view.
        return FileResponse(index_html)

    # `serve_spa` is registered on the router by the decorator; the
    # local binding is unused but FastAPI's machinery keeps the
    # handler alive via the router itself.
    _ = serve_spa
    return router
