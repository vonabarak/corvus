"""Ring 2 widget tests: ``StatusBadge``, ``SerialConsoleWidget``.

These exercise widget logic without a bridge — feed bytes / set
status / dispatch a key event and assert on internal state."""

from __future__ import annotations

from typing import Any

import pytest
from corvus_desktop.widgets.serial_console import SerialConsoleWidget
from corvus_desktop.widgets.status_badge import StatusBadge
from PySide6.QtCore import QEvent, Qt
from PySide6.QtGui import QKeyEvent

# --------------------------------------------------------- StatusBadge


def test_status_badge_renders_known_states(qapp: Any) -> None:
    badge = StatusBadge("vm")
    for state in ("stopped", "running", "paused", "error"):
        badge.set_status(state)
        assert badge.text() == state
        # Stylesheet must reflect the state in colour terms.
        assert "background" in badge.styleSheet()


def test_status_badge_unknown_state_degrades(qapp: Any) -> None:
    badge = StatusBadge("vm")
    badge.set_status("totally-made-up")
    assert badge.text() == "totally-made-up"
    # Doesn't crash; falls back to the neutral palette.
    assert "background" in badge.styleSheet()


def test_status_badge_task_kind_uses_task_palette(qapp: Any) -> None:
    badge = StatusBadge("task")
    for state in ("running", "success", "error"):
        badge.set_status(state)
        assert badge.text() == state


# --------------------------------------------------------- SerialConsoleWidget


@pytest.fixture
def console(qapp: Any) -> SerialConsoleWidget:
    return SerialConsoleWidget()


def test_console_feed_writes_to_screen(console: SerialConsoleWidget) -> None:
    console.feed_bytes(b"hello\n")
    # pyte renders the first row as "hello" left-aligned.
    first_row = console._screen.display[0]
    assert first_row.startswith("hello")


def test_console_handles_partial_utf8(console: SerialConsoleWidget) -> None:
    # Split a 2-byte UTF-8 char ("é" = 0xC3 0xA9) across feeds.
    console.feed_bytes(b"\xc3")
    console.feed_bytes(b"\xa9")
    # The replacement char or the recovered grapheme is fine; what
    # matters is no exception was raised.
    assert console._screen.cursor.x >= 0


def test_console_reset_clears_screen(console: SerialConsoleWidget) -> None:
    console.feed_bytes(b"hello")
    console.reset_screen()
    # After reset, the first row is all spaces.
    assert console._screen.display[0].strip() == ""


def test_console_emits_input_for_printable_keys(
    console: SerialConsoleWidget,
) -> None:
    captured: list[bytes] = []
    console.input_bytes.connect(captured.append)
    ev = QKeyEvent(
        QEvent.Type.KeyPress, Qt.Key.Key_A, Qt.KeyboardModifier.NoModifier, "a"
    )
    console.keyPressEvent(ev)
    assert captured == [b"a"]


def test_console_emits_csi_for_arrow_keys(console: SerialConsoleWidget) -> None:
    captured: list[bytes] = []
    console.input_bytes.connect(captured.append)
    for key in (
        Qt.Key.Key_Up,
        Qt.Key.Key_Down,
        Qt.Key.Key_Right,
        Qt.Key.Key_Left,
    ):
        ev = QKeyEvent(QEvent.Type.KeyPress, key, Qt.KeyboardModifier.NoModifier, "")
        console.keyPressEvent(ev)
    assert captured == [b"\x1b[A", b"\x1b[B", b"\x1b[C", b"\x1b[D"]


def test_console_emits_control_chars(console: SerialConsoleWidget) -> None:
    captured: list[bytes] = []
    console.input_bytes.connect(captured.append)
    # Ctrl+C → 0x03
    ev = QKeyEvent(
        QEvent.Type.KeyPress, Qt.Key.Key_C, Qt.KeyboardModifier.ControlModifier, ""
    )
    console.keyPressEvent(ev)
    assert captured == [b"\x03"]
