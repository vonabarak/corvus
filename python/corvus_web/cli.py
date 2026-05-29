"""``corvus-web`` console entry point.

Parses argv, builds a :class:`CorvusWebConfig`, hands the FastAPI app
to uvicorn. Kept deliberately small — uvicorn already handles signals,
reload, log formatting, and worker bootstrapping.
"""

from __future__ import annotations

import argparse
import logging
import sys
from pathlib import Path

import uvicorn

from . import __version__
from .app import create_app
from .config import CorvusWebConfig, _default_unix_socket

logger = logging.getLogger("corvus_web")


def _build_parser() -> argparse.ArgumentParser:
    p = argparse.ArgumentParser(
        prog="corvus-web",
        description=(
            "HTTP/WebSocket gateway in front of the corvus daemon. "
            "Serves a React SPA plus a REST + WS bridge over the daemon's "
            "Cap'n Proto RPC."
        ),
    )
    p.add_argument(
        "--version",
        action="version",
        version=f"corvus-web {__version__}",
    )
    # Bind. Defaults are intentionally localhost-only: v1 has no auth,
    # so binding to a routable address would expose the daemon.
    p.add_argument(
        "--bind-host",
        default="127.0.0.1",
        help="HTTP bind host (default: %(default)s).",
    )
    p.add_argument(
        "--bind-port",
        type=int,
        default=8080,
        help="HTTP bind port (default: %(default)s).",
    )

    # Daemon connection. Exactly one of --daemon-socket / --daemon-host
    # may be specified; the default is the daemon's Unix socket under
    # $XDG_RUNTIME_DIR.
    src = p.add_mutually_exclusive_group()
    src.add_argument(
        "--daemon-socket",
        default=None,
        help=(
            "Unix socket path for the daemon "
            "(default: $XDG_RUNTIME_DIR/corvus/corvus.sock)."
        ),
    )
    src.add_argument(
        "--daemon-host",
        default=None,
        help="TCP host of the daemon. Mutually exclusive with --daemon-socket.",
    )
    p.add_argument(
        "--daemon-port",
        type=int,
        default=9876,
        help="TCP port of the daemon when --daemon-host is set (default: %(default)s).",
    )

    # TLS knobs for TCP daemon connections. ``--no-daemon-tls`` is the
    # mirror of crv's --no-tls flag (src/Corvus/Client/Parser.hs).
    tls = p.add_mutually_exclusive_group()
    tls.add_argument(
        "--daemon-tls",
        dest="daemon_tls",
        action="store_const",
        const=True,
        default=None,
        help="Force TLS on (TCP only). Default: TLS auto-on for TCP.",
    )
    tls.add_argument(
        "--no-daemon-tls",
        dest="daemon_tls",
        action="store_const",
        const=False,
        help="Disable TLS to the daemon. Use only against a daemon started with --no-tls.",
    )
    p.add_argument(
        "--daemon-cert-dir",
        type=Path,
        default=None,
        help=(
            "Directory containing client cert/key/CA used to authenticate to the "
            "daemon over TLS. Defaults to the corvus-admin search path."
        ),
    )

    # SPA assets. Tests + dev override the default to point at frontend/dist/.
    p.add_argument(
        "--frontend-dir",
        type=Path,
        default=None,
        help=(
            "Directory containing the built SPA (vite output). "
            "Defaults to the static/ directory bundled in the wheel."
        ),
    )

    p.add_argument(
        "--log-level",
        default="info",
        choices=["critical", "error", "warning", "info", "debug", "trace"],
        help="uvicorn + corvus-web log level (default: %(default)s).",
    )
    return p


def _resolve_config(args: argparse.Namespace) -> CorvusWebConfig:
    """Turn parsed argv into a :class:`CorvusWebConfig`."""
    # Resolve the daemon transport. If neither flag was given, default
    # to the Unix socket — same default the crv CLI uses.
    unix = args.daemon_socket
    host = args.daemon_host
    if unix is None and host is None:
        unix = _default_unix_socket()

    kwargs: dict[str, object] = {
        "bind_host": args.bind_host,
        "bind_port": args.bind_port,
        "daemon_unix_socket": unix,
        "daemon_host": host,
        "daemon_port": args.daemon_port,
        "daemon_tls": args.daemon_tls,
        "daemon_cert_dir": args.daemon_cert_dir,
        "log_level": args.log_level,
    }
    if args.frontend_dir is not None:
        kwargs["frontend_dir"] = args.frontend_dir
    return CorvusWebConfig(**kwargs)  # type: ignore[arg-type]


def main(argv: list[str] | None = None) -> int:
    """Console entry point. Returns a process exit code."""
    args = _build_parser().parse_args(argv)
    config = _resolve_config(args)

    logging.basicConfig(
        level=args.log_level.upper() if args.log_level != "trace" else "DEBUG",
        format="%(asctime)s %(levelname)-7s %(name)s: %(message)s",
    )
    logger.info(
        "corvus-web %s starting on %s:%d (daemon: %s)",
        __version__,
        config.bind_host,
        config.bind_port,
        config.daemon_unix_socket or f"{config.daemon_host}:{config.daemon_port}",
    )

    app = create_app(config)
    # uvicorn.run blocks until the server shuts down. We pass the
    # already-constructed app rather than an import string because the
    # config-bound lifespan can't be rebuilt from a module path.
    uvicorn.run(
        app,
        host=config.bind_host,
        port=config.bind_port,
        log_level=config.log_level,
        access_log=True,
    )
    return 0


if __name__ == "__main__":  # pragma: no cover
    sys.exit(main())
