"""Optional system-tray icon + desktop notifications.

GNOME's stock shell has dropped tray-icon support; the platform
fallback ``QSystemTrayIcon.isSystemTrayAvailable()`` returns ``False``
and we noop the tray entirely. Linux desktops with KDE Plasma, XFCE,
LXQt, MATE, Cinnamon, and any GNOME shell with the AppIndicator
extension installed still get tray + notifications.

Notification trigger (MVP): every ``TaskProgressFinished`` the bridge
sees triggers a balloon. This piggybacks on the task-detail view's
subscription pipeline — the user has to be watching a task for it to
fire, which is the right semantics for "long-running ops" without
introducing a separate notification subscription channel.
"""

from __future__ import annotations

from typing import TYPE_CHECKING, Any

from corvus_client.types import TaskProgressFinished
from PySide6.QtCore import QObject
from PySide6.QtGui import QIcon
from PySide6.QtWidgets import QApplication, QMenu, QSystemTrayIcon

if TYPE_CHECKING:
    from ..client_bridge import CorvusBridge


def system_tray_supported() -> bool:
    """Cheap probe used by callers to guard tray construction."""
    return QSystemTrayIcon.isSystemTrayAvailable()


class DesktopTray(QObject):
    """Tray icon + balloon-notification surface.

    Lifecycle: construct after :class:`CorvusBridge` is wired up but
    before the main window is shown. The tray observes ``task_event``
    and ``vm_action_completed`` signals and translates final events
    into ``QSystemTrayIcon.showMessage`` calls.
    """

    def __init__(self, bridge: CorvusBridge, app: QApplication) -> None:
        super().__init__(app)
        self._bridge = bridge
        self._app = app
        self._tray: QSystemTrayIcon | None = None

        if not system_tray_supported():
            return

        icon = app.windowIcon() if not app.windowIcon().isNull() else QIcon()
        self._tray = QSystemTrayIcon(icon, self)
        self._tray.setToolTip("corvus-desktop")

        menu = QMenu()
        menu.addAction("Show window", self._on_show)
        menu.addSeparator()
        menu.addAction("Quit", app.quit)
        self._tray.setContextMenu(menu)
        self._tray.activated.connect(self._on_activated)
        self._tray.show()

        bridge.task_event.connect(self._on_task_event)
        bridge.vm_action_completed.connect(self._on_vm_action)

    # ---------------------------------------------------- internals

    def _on_show(self) -> None:
        # Iterate every top-level window: any that's currently hidden
        # (the user closed it) gets shown again; minimized ones get
        # restored; then we raise + activate so it ends up frontmost.
        # We can't filter on `isVisible()` here — that's exactly the
        # state we're trying to recover from.
        for widget in QApplication.topLevelWidgets():
            if not widget.isWindow():
                continue
            if widget.isMinimized():
                widget.showNormal()
            elif not widget.isVisible():
                widget.show()
            widget.raise_()
            widget.activateWindow()

    def _on_activated(self, reason: Any) -> None:
        if reason == QSystemTrayIcon.ActivationReason.Trigger:
            self._on_show()

    def _on_task_event(self, task_id: int, event: Any) -> None:
        if not isinstance(event, TaskProgressFinished):
            return
        title = f"Task #{task_id} {event.result}"
        body = event.message or ""
        self._notify(title, body, success=event.result == "success")

    def _on_vm_action(self, vm_id: int, action: str, new_status: str) -> None:
        # Send_ctrl_alt_del is too low-stakes to interrupt the user.
        if action == "send_ctrl_alt_del":
            return
        title = f"VM #{vm_id}: {action} → {new_status or 'completed'}"
        self._notify(title, "", success=True)

    def _notify(self, title: str, body: str, *, success: bool) -> None:
        if self._tray is None:
            return
        icon = (
            QSystemTrayIcon.MessageIcon.Information
            if success
            else QSystemTrayIcon.MessageIcon.Warning
        )
        self._tray.showMessage(title, body, icon, 5000)
