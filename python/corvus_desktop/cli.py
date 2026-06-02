"""``corvus-desktop`` console entry point.

Parses argv into a :class:`DesktopConfig`, then hands off to
:func:`corvus_desktop.app.run`. PySide6 is imported lazily inside
``run`` so ``corvus-desktop --help`` works on a slim install and
the missing-dep error message points at the right ``pip install``.
"""

from __future__ import annotations

import argparse
import logging
import os
import sys
from dataclasses import dataclass
from pathlib import Path

from . import __version__

logger = logging.getLogger("corvus_desktop")


@dataclass(frozen=True)
class DesktopConfig:
    """Resolved CLI args. Consumed by :func:`corvus_desktop.app.run`."""

    daemon_unix_socket: str | None
    daemon_host: str | None
    daemon_port: int
    daemon_tls: bool | None
    daemon_cert_dir: Path | None
    log_level: str


def _default_unix_socket() -> str:
    """Mirror the daemon default: ``$XDG_RUNTIME_DIR/corvus/corvus.sock``.

    Same fallback to ``/tmp/corvus/corvus.sock`` as the Haskell code (see
    ``src/Corvus/Types.hs``) and as ``corvus_web.config._default_unix_socket``.
    """
    base = os.environ.get("XDG_RUNTIME_DIR") or "/tmp"
    return str(Path(base) / "corvus" / "corvus.sock")


def _build_parser() -> argparse.ArgumentParser:
    p = argparse.ArgumentParser(
        prog="corvus-desktop",
        description=(
            "Native desktop GUI for the corvus daemon. Talks Cap'n Proto RPC "
            "directly via corvus_client.AsyncClient. Requires the [desktop] "
            "extra (PySide6 + pyte)."
        ),
    )
    p.add_argument(
        "--version",
        action="version",
        version=f"corvus-desktop {__version__}",
    )

    # Daemon connection. Mirrors corvus-web's flag names so a user
    # who knows one knows the other.
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
            "Directory containing client cert/key/CA used to authenticate to "
            "the daemon over TLS. Defaults to the corvus-admin search path."
        ),
    )

    p.add_argument(
        "--log-level",
        default="info",
        choices=["critical", "error", "warning", "info", "debug"],
        help="corvus-desktop log level (default: %(default)s).",
    )
    return p


def _resolve_config(args: argparse.Namespace) -> DesktopConfig:
    unix = args.daemon_socket
    host = args.daemon_host
    if unix is None and host is None:
        unix = _default_unix_socket()
    return DesktopConfig(
        daemon_unix_socket=unix,
        daemon_host=host,
        daemon_port=args.daemon_port,
        daemon_tls=args.daemon_tls,
        daemon_cert_dir=args.daemon_cert_dir,
        log_level=args.log_level,
    )


def main(argv: list[str] | None = None) -> int:
    """Console entry point. Returns a process exit code."""
    args = _build_parser().parse_args(argv)
    config = _resolve_config(args)

    logging.basicConfig(
        level=config.log_level.upper(),
        format="%(asctime)s %(levelname)-7s %(name)s: %(message)s",
    )

    try:
        from .app import run
    except ImportError as e:
        # PySide6 / pyte missing — give the user the exact pip command
        # to recover. Don't let the ImportError out: the message is
        # cryptic without context, and we want the user to land on the
        # extras hint, not a stack trace.
        sys.stderr.write(
            f"corvus-desktop requires the [desktop] extra ({e.name or 'PySide6'} "
            "not installed).\n"
            "Install with:\n"
            "    pip install 'corvus[desktop]'\n"
        )
        return 2

    return run(config)


if __name__ == "__main__":  # pragma: no cover
    sys.exit(main())
