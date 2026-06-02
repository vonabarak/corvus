"""Qt application factory and entry point.

Imports PySide6 at module top so a missing dep raises ``ImportError``
inside :func:`corvus_desktop.cli.main`'s lazy import, where it gets
translated into a friendly ``pip install 'corvus[desktop]'`` hint.

Phase 5 adds the native-polish layer: a :class:`DesktopTray` instance
(no-ops on stock GNOME via the platform probe), plus a hook on
:attr:`QApplication.applicationStateChanged` that asks the bridge to
pause its push subscriptions while the user is on another workspace.
"""

from __future__ import annotations

import logging

from PySide6.QtCore import Qt
from PySide6.QtWidgets import QApplication

from .cli import DesktopConfig
from .client_bridge import CorvusBridge
from .widgets.tray import DesktopTray
from .windows.main_window import MainWindow

logger = logging.getLogger("corvus_desktop.app")


def run(config: DesktopConfig) -> int:
    """Build the QApplication, wire the bridge, run the event loop."""
    existing = QApplication.instance()
    app = existing if isinstance(existing, QApplication) else QApplication([])
    app.setApplicationName("corvus-desktop")
    app.setOrganizationName("corvus")

    target = (
        config.daemon_unix_socket
        if config.daemon_unix_socket
        else f"{config.daemon_host}:{config.daemon_port}"
    )
    logger.info("corvus-desktop connecting to %s", target)

    bridge = CorvusBridge(config)
    window = MainWindow(bridge, target)
    # Tray is fire-and-forget; the QObject parent keeps it alive for
    # the QApplication lifetime.
    DesktopTray(bridge, app)

    # Idle suspend: pause push subscriptions while the user is on
    # another workspace; resume when they come back. The bridge
    # remembers the active subscription set between the two.
    def _on_state_changed(state: Qt.ApplicationState) -> None:
        if state == Qt.ApplicationState.ApplicationInactive:
            logger.debug("app inactive — pausing subscriptions")
            bridge.pause_subscriptions()
        elif state == Qt.ApplicationState.ApplicationActive:
            logger.debug("app active — resuming subscriptions")
            bridge.resume_subscriptions()

    app.applicationStateChanged.connect(_on_state_changed)

    # Tear the bridge down before Qt destroys the QApplication —
    # otherwise the worker thread's pycapnp finalisers race the
    # interpreter's final-GC and can SIGABRT.
    app.aboutToQuit.connect(bridge.shutdown)

    window.show()
    bridge.start()
    return app.exec()
