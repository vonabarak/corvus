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
from corvus_client.exceptions import (
    ConnectError,
    CorvusError,
    DiskHasOverlays,
    DiskInUse,
    DiskNotFound,
    DriveNotFound,
    FormatNotSupported,
    GuestAgentError,
    GuestAgentNotEnabled,
    InvalidTransition,
    NetIfNotFound,
    NetworkAlreadyRunning,
    NetworkInUse,
    NetworkNotFound,
    NetworkNotRunning,
    NodeInUse,
    NodeNotFound,
    SharedDirNotFound,
    SnapshotNotFound,
    SshKeyInUse,
    SshKeyNotFound,
    TaskNotFound,
    TemplateNotFound,
    VmMustBeStopped,
    VmNotFound,
    VmRunning,
)
from fastapi import FastAPI, Request
from fastapi.responses import JSONResponse

from .config import CorvusWebConfig
from .routes import (
    apply,
    disks,
    metrics,
    networks,
    nodes,
    spa,
    spice,
    ssh_keys,
    system,
    tasks,
    templates,
    vms,
)

# Map daemon-typed exceptions to HTTP status codes. Anything outside
# these tuples falls back to 400 (the daemon rejected the request).
_NOT_FOUND_EXCEPTIONS: tuple[type[CorvusError], ...] = (
    VmNotFound,
    DiskNotFound,
    NetworkNotFound,
    SshKeyNotFound,
    TemplateNotFound,
    TaskNotFound,
    NodeNotFound,
    SnapshotNotFound,
    DriveNotFound,
    NetIfNotFound,
    SharedDirNotFound,
)

_CONFLICT_EXCEPTIONS: tuple[type[CorvusError], ...] = (
    InvalidTransition,
    VmRunning,
    VmMustBeStopped,
    DiskInUse,
    DiskHasOverlays,
    NetworkInUse,
    NetworkAlreadyRunning,
    NetworkNotRunning,
    SshKeyInUse,
    NodeInUse,
    GuestAgentNotEnabled,
    FormatNotSupported,
)


def _corvus_error_status(exc: CorvusError) -> int:
    """HTTP status code for a typed daemon exception.

    The split mirrors REST semantics: 404 for "the resource isn't
    there", 409 for "the resource is there but the request conflicts
    with its current state", 503 for "the guest agent isn't currently
    reachable" (a transient downstream condition), 502 for
    daemon-unreachable, and 400 as the catch-all for any other
    daemon-side rejection that survives the client's exception
    translator.
    """
    if isinstance(exc, ConnectError):
        return 502
    if isinstance(exc, _NOT_FOUND_EXCEPTIONS):
        return 404
    if isinstance(exc, _CONFLICT_EXCEPTIONS):
        return 409
    if isinstance(exc, GuestAgentError):
        return 503
    return 400


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
            # Background task polling the daemon's per-VM stats
            # cache; populates the in-memory map the /metrics
            # exposition endpoint walks on every scrape.
            poller = await metrics.start_metrics_poller(app)
            try:
                yield
            finally:
                await metrics.shutdown_metrics_poller(poller)

    app = FastAPI(
        title="Corvus Web",
        version="0.1.0",
        # /docs and /redoc remain on by default — useful for the
        # operator inspecting the REST surface from a browser. They
        # add no overhead and don't expose anything the routes don't.
        lifespan=lifespan,
    )

    # Centralised translation of typed daemon exceptions into
    # structured HTTP errors. Without this any route that doesn't
    # explicitly catch a 'CorvusError' subclass turns into a silent
    # 500 with an empty body — the WebUI then has no message to show
    # the operator. Per-route `try / except` catches still take
    # precedence; this handler only fires when the route doesn't
    # handle the exception itself.
    @app.exception_handler(CorvusError)
    async def _corvus_error_handler(_req: Request, exc: CorvusError) -> JSONResponse:
        return JSONResponse(
            status_code=_corvus_error_status(exc),
            content={"detail": str(exc)},
        )

    # API routes mounted under /api/. The SPA mount is last so it
    # catches everything else and serves index.html for client-side
    # routing.
    app.include_router(system.router, prefix="/api")
    app.include_router(vms.router, prefix="/api")
    app.include_router(disks.router, prefix="/api")
    app.include_router(networks.router, prefix="/api")
    app.include_router(ssh_keys.router, prefix="/api")
    app.include_router(templates.router, prefix="/api")
    app.include_router(tasks.router, prefix="/api")
    app.include_router(apply.router, prefix="/api")
    app.include_router(nodes.router, prefix="/api")
    app.include_router(spice.router, prefix="/api")
    app.include_router(metrics.router)
    app.include_router(spa.build_router(config.frontend_dir))

    return app
