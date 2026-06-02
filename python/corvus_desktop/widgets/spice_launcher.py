"""Launch ``remote-viewer`` for a SPICE :class:`ViewGrant`.

If ``remote-viewer`` is not on PATH, fall back to a dialog with the
host / port / password so the user can copy-paste into their preferred
client.
"""

from __future__ import annotations

import shutil
import subprocess
import tempfile
from typing import Any

from corvus_client.types import ViewGrant
from PySide6.QtWidgets import QMessageBox, QWidget


def launch_remote_viewer(grant: ViewGrant, parent: QWidget | None = None) -> None:
    """Try to spawn ``remote-viewer``; degrade to a dialog on failure."""
    remote_viewer = shutil.which("remote-viewer")
    if remote_viewer is None:
        _show_grant_dialog(grant, parent)
        return

    # remote-viewer wants either a URI or a `.vv` file. The URI form
    # is simpler: spice://host:port plus the password via env or
    # a one-shot file. We write a minimal .vv file to a tmp path so
    # the password lives in the file (remote-viewer reads & deletes
    # it on launch for SPICE_AUTO).
    body = (
        "[virt-viewer]\n"
        "type=spice\n"
        f"host={grant.host}\n"
        f"port={grant.port}\n"
        f"password={grant.password}\n"
        "delete-this-file=1\n"
    )
    try:
        with tempfile.NamedTemporaryFile(mode="w", suffix=".vv", delete=False) as tmp:
            tmp.write(body)
            vv_path = tmp.name
        subprocess.Popen([remote_viewer, vv_path])
    except OSError as e:
        QMessageBox.warning(
            parent,
            "Could not launch remote-viewer",
            f"Failed to spawn remote-viewer: {e}\n\n"
            f"Connect manually to spice://{grant.host}:{grant.port}",
        )


def _show_grant_dialog(grant: ViewGrant, parent: Any) -> None:
    QMessageBox.information(
        parent,
        "SPICE connection details",
        "remote-viewer is not installed.\n\n"
        f"Host: {grant.host}\n"
        f"Port: {grant.port}\n"
        f"Password: {grant.password}\n"
        f"Valid for: {grant.ttl_seconds} seconds\n\n"
        "Use any SPICE client (virt-viewer, spicy, …) to connect.",
    )
