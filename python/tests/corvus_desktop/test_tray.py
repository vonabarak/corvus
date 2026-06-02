"""Ring 2 widget tests: :class:`DesktopTray`.

Validates the tray construction degrades gracefully when the platform
doesn't expose a system tray (offscreen QPA, stock GNOME shell), and
that the notification routing translates ``TaskProgressFinished``
events from the bridge into a single ``showMessage`` call.
"""

from __future__ import annotations

from typing import Any

from corvus_client.types import (
    TaskProgressFinished,
    TaskProgressProgress,
    TaskProgressStarted,
)
from corvus_desktop.widgets import tray as tray_mod
from corvus_desktop.widgets.tray import DesktopTray
from PySide6.QtCore import QObject, Signal
from PySide6.QtWidgets import QApplication


class _MockBridge(QObject):
    task_event = Signal(int, object)
    vm_action_completed = Signal(int, str, str)

    def __init__(self) -> None:
        super().__init__()


class _FakeActivated:
    def connect(self, _slot: Any) -> None:
        pass


def _make_fake_tray(calls: list[tuple[str, str]]) -> type:
    """Return a ``QSystemTrayIcon`` stand-in that captures
    ``showMessage`` calls into ``calls``."""

    class _FakeTray:
        ActivationReason = type("ActivationReason", (), {"Trigger": 1})
        MessageIcon = type(
            "MessageIcon",
            (),
            {"Information": 0, "Warning": 1},
        )

        def __init__(self, *args: Any, **kwargs: Any) -> None:
            self.activated = _FakeActivated()

        def setToolTip(self, _t: str) -> None:
            pass

        def setContextMenu(self, _m: Any) -> None:
            pass

        def show(self) -> None:
            pass

        def showMessage(self, title: str, body: str, *_args: Any) -> None:
            calls.append((title, body))

        @staticmethod
        def isSystemTrayAvailable() -> bool:
            return True

    return _FakeTray


def test_tray_constructs_without_crashing(qapp: Any) -> None:
    """Even on an offscreen QPA (no real tray), construction must
    not raise and the tray attribute is None."""
    bridge = _MockBridge()
    desktop_tray = DesktopTray(bridge, qapp)
    assert isinstance(desktop_tray, DesktopTray)


def test_tray_notifies_on_finished_event(qapp: Any, monkeypatch: Any) -> None:
    """Bypass the platform probe so the tray actually constructs a
    QSystemTrayIcon, then capture showMessage calls."""

    monkeypatch.setattr(tray_mod, "system_tray_supported", lambda: True)
    calls: list[tuple[str, str]] = []
    monkeypatch.setattr(tray_mod, "QSystemTrayIcon", _make_fake_tray(calls))

    bridge = _MockBridge()
    DesktopTray(bridge, qapp)

    # Non-final events do nothing.
    bridge.task_event.emit(
        1, TaskProgressStarted(task_id=1, command="x", subsystem="vm")
    )
    bridge.task_event.emit(
        1, TaskProgressProgress(task_id=1, completed=1, total=2, label="…")
    )
    assert calls == []

    bridge.task_event.emit(
        1, TaskProgressFinished(task_id=1, result="success", message="ok")
    )
    assert len(calls) == 1
    assert "#1" in calls[0][0]
    assert "success" in calls[0][0]
    assert calls[0][1] == "ok"


def test_tray_skips_low_stakes_vm_actions(qapp: Any, monkeypatch: Any) -> None:
    """Ctrl+Alt+Del shouldn't pop a notification — too low-stakes."""

    monkeypatch.setattr(tray_mod, "system_tray_supported", lambda: True)
    calls: list[tuple[str, str]] = []
    monkeypatch.setattr(tray_mod, "QSystemTrayIcon", _make_fake_tray(calls))

    bridge = _MockBridge()
    DesktopTray(bridge, qapp)
    bridge.vm_action_completed.emit(1, "send_ctrl_alt_del", "")
    assert calls == []

    bridge.vm_action_completed.emit(1, "start", "running")
    assert len(calls) == 1


def test_system_tray_supported_returns_a_bool(qapp: QApplication) -> None:
    assert isinstance(tray_mod.system_tray_supported(), bool)
