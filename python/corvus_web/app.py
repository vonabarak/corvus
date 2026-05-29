"""FastAPI app factory + lifespan.

The lifespan opens the pycapnp kj loop and a long-lived
``AsyncClient`` connection to the daemon, stashes the client on
``app.state``, and tears down in reverse on shutdown. All routes
reach the client via the :func:`corvus_web.deps.get_client`
dependency.
"""

from __future__ import annotations

from collections.abc import AsyncIterator
from contextlib import AsyncExitStack, asynccontextmanager

import capnp
from corvus_client import AsyncClient
from fastapi import FastAPI

from .config import CorvusWebConfig
from .routes import spa, system


def create_app(config: CorvusWebConfig) -> FastAPI:
    """Build a FastAPI application bound to ``config``.

    The returned app owns no resources until uvicorn drives its
    lifespan; ``create_app`` itself is cheap and idempotent, which
    makes it safe to call from tests with a fake daemon."""

    @asynccontextmanager
    async def lifespan(app: FastAPI) -> AsyncIterator[None]:
        # ``capnp.kj_loop()`` must wrap every Cap'n Proto call on this
        # thread. uvicorn already gives us a running asyncio loop; we
        # only need to enter the kj integration once here and tear it
        # down after the AsyncClient is closed. AsyncExitStack keeps
        # the unwind order correct even if AsyncClient.__aenter__ fails
        # part-way through.
        async with AsyncExitStack() as stack:
            await stack.enter_async_context(capnp.kj_loop())
            client_kwargs: dict[str, object] = {}
            if config.daemon_unix_socket is not None:
                client_kwargs["unix_socket"] = config.daemon_unix_socket
            else:
                # Validated in CorvusWebConfig construction (the CLI
                # rejects "neither set"), so this branch is safe.
                assert config.daemon_host is not None
                client_kwargs["host"] = config.daemon_host
                client_kwargs["port"] = config.daemon_port
                if config.daemon_tls is not None:
                    client_kwargs["tls"] = config.daemon_tls
                if config.daemon_cert_dir is not None:
                    client_kwargs["cert_dir"] = config.daemon_cert_dir
            client = await stack.enter_async_context(AsyncClient(**client_kwargs))  # type: ignore[arg-type]
            app.state.client = client
            app.state.config = config
            yield

    app = FastAPI(
        title="Corvus Web",
        version="0.1.0",
        # /docs and /redoc remain on by default — useful for the
        # operator inspecting the REST surface from a browser. They
        # add no overhead and don't expose anything the routes don't.
        lifespan=lifespan,
    )

    # API routes mounted under /api/. The SPA mount is last so it
    # catches everything else and serves index.html for client-side
    # routing.
    app.include_router(system.router, prefix="/api")
    app.include_router(spa.build_router(config.frontend_dir))

    return app
