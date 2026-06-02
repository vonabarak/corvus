"""Native serial console widget — ``pyte`` emulator + Qt renderer.

Why not ``qtermwidget``?  See the plan: LXQt's terminal widget has no
PySide6 bindings, and shiboken-ing one is a multi-week project. Konsole's
``KPart`` needs the entire KDE Frameworks. ``pyte`` is a pure-Python
VT102 emulator (~5 kLoC, on PyPI), so the work boils down to writing a
~200-line ``QAbstractScrollArea`` that paints ``pyte.Screen``'s rows.

Lifecycle (set up by ``VmDetailWidget``):

* ``feed_bytes`` is called for every ``serial_data`` signal payload —
  bytes flow into ``pyte.Stream``, which mutates ``pyte.Screen``.
* The widget calls :meth:`viewport().update` after each feed, which
  triggers ``paintEvent`` to repaint affected rows.
* Keypresses are translated to bytes (with arrow / function keys
  mapped to their xterm CSI sequences) and emitted via
  :attr:`input_bytes` for the parent to forward to
  ``CorvusBridge.send_serial``.
"""

from __future__ import annotations

from typing import Any

import pyte
from PySide6.QtCore import QSize, Qt, Signal
from PySide6.QtGui import QColor, QFont, QFontDatabase, QFontMetrics, QPainter
from PySide6.QtWidgets import QWidget

# 16-color xterm palette by index. Matches pyte's `Char.fg`/`bg` strings
# ("default", "red", "brightred", …) — we resolve them to QColor below.
_PALETTE: dict[str, QColor] = {
    "default": QColor("#dcdcdc"),
    "black": QColor("#000000"),
    "red": QColor("#cd0000"),
    "green": QColor("#00cd00"),
    "yellow": QColor("#cdcd00"),
    "blue": QColor("#0000ee"),
    "magenta": QColor("#cd00cd"),
    "cyan": QColor("#00cdcd"),
    "white": QColor("#e5e5e5"),
    "brightblack": QColor("#7f7f7f"),
    "brightred": QColor("#ff0000"),
    "brightgreen": QColor("#00ff00"),
    "brightyellow": QColor("#ffff00"),
    "brightblue": QColor("#5c5cff"),
    "brightmagenta": QColor("#ff00ff"),
    "brightcyan": QColor("#00ffff"),
    "brightwhite": QColor("#ffffff"),
}
_BG_DEFAULT = QColor("#000000")


def _resolve_color(name: str, *, is_bg: bool) -> QColor:
    """Map a pyte color name to a QColor. Hex strings (``"#rrggbb"``)
    pass through; named colors lookup the palette; unknowns degrade."""
    if not name or name == "default":
        return _BG_DEFAULT if is_bg else _PALETTE["default"]
    if name.startswith("#") and len(name) == 7:
        return QColor(name)
    if len(name) == 6 and all(c in "0123456789abcdefABCDEF" for c in name):
        return QColor(f"#{name}")
    return _PALETTE.get(name, _BG_DEFAULT if is_bg else _PALETTE["default"])


# Map Qt key codes → byte sequences. Covers the keys a Linux serial
# login + bootloader uses; vim won't have full coverage but the
# common arrow/Home/End/PgUp/PgDn/Fn are here. CSI sequences match
# xterm's defaults.
_KEY_MAP: dict[int, bytes] = {
    Qt.Key.Key_Return: b"\r",
    Qt.Key.Key_Enter: b"\r",
    Qt.Key.Key_Backspace: b"\x7f",
    Qt.Key.Key_Tab: b"\t",
    Qt.Key.Key_Escape: b"\x1b",
    Qt.Key.Key_Up: b"\x1b[A",
    Qt.Key.Key_Down: b"\x1b[B",
    Qt.Key.Key_Right: b"\x1b[C",
    Qt.Key.Key_Left: b"\x1b[D",
    Qt.Key.Key_Home: b"\x1b[H",
    Qt.Key.Key_End: b"\x1b[F",
    Qt.Key.Key_PageUp: b"\x1b[5~",
    Qt.Key.Key_PageDown: b"\x1b[6~",
    Qt.Key.Key_Insert: b"\x1b[2~",
    Qt.Key.Key_Delete: b"\x1b[3~",
    Qt.Key.Key_F1: b"\x1bOP",
    Qt.Key.Key_F2: b"\x1bOQ",
    Qt.Key.Key_F3: b"\x1bOR",
    Qt.Key.Key_F4: b"\x1bOS",
    Qt.Key.Key_F5: b"\x1b[15~",
    Qt.Key.Key_F6: b"\x1b[17~",
    Qt.Key.Key_F7: b"\x1b[18~",
    Qt.Key.Key_F8: b"\x1b[19~",
    Qt.Key.Key_F9: b"\x1b[20~",
    Qt.Key.Key_F10: b"\x1b[21~",
    Qt.Key.Key_F11: b"\x1b[23~",
    Qt.Key.Key_F12: b"\x1b[24~",
}


class SerialConsoleWidget(QWidget):
    """Fixed-size VT102 terminal. Emits :attr:`input_bytes` on keypress."""

    # Emitted for every keystroke the user types. The parent wires
    # this to ``CorvusBridge.send_serial(vm_id, payload)``.
    input_bytes = Signal(bytes)

    # Default screen dimensions. Boot menus and dmesg fit comfortably
    # at 100x30; the daemon-side serial PTY isn't told about this size
    # — we just render whatever the guest sends.
    DEFAULT_COLS = 100
    DEFAULT_ROWS = 30

    def __init__(self, parent: QWidget | None = None) -> None:
        super().__init__(parent)
        self.setFocusPolicy(Qt.FocusPolicy.StrongFocus)
        self.setAutoFillBackground(False)

        font = QFontDatabase.systemFont(QFontDatabase.SystemFont.FixedFont)
        font.setPointSize(max(font.pointSize(), 10))
        font.setStyleStrategy(QFont.StyleStrategy.PreferAntialias)
        self.setFont(font)
        self._metrics = QFontMetrics(font)
        self._cell_w = self._metrics.horizontalAdvance("M")
        self._cell_h = self._metrics.height()

        self._screen = pyte.HistoryScreen(self.DEFAULT_COLS, self.DEFAULT_ROWS)
        self._stream = pyte.Stream(self._screen)

    # -------------------------------------------------------- public API

    def feed_bytes(self, chunk: bytes) -> None:
        """Push bytes through the emulator and trigger a repaint."""
        try:
            text = chunk.decode("utf-8", errors="replace")
        except Exception:
            text = chunk.decode("latin-1", errors="replace")
        self._stream.feed(text)
        self.update()

    def reset_screen(self) -> None:
        """Drop the emulator state (used when the session closes)."""
        self._screen.reset()
        self.update()

    # ---------------------------------------------------------- Qt API

    def sizeHint(self) -> QSize:
        return QSize(self._cell_w * self.DEFAULT_COLS, self._cell_h * self.DEFAULT_ROWS)

    def keyPressEvent(self, event: Any) -> None:
        key = event.key()
        mods = event.modifiers()
        # Ctrl+letter → control character
        if (
            mods & Qt.KeyboardModifier.ControlModifier
            and Qt.Key.Key_A <= key <= Qt.Key.Key_Z
        ):
            self.input_bytes.emit(bytes([key - Qt.Key.Key_A + 1]))
            return
        if key in _KEY_MAP:
            self.input_bytes.emit(_KEY_MAP[key])
            return
        text = event.text()
        if text:
            self.input_bytes.emit(text.encode("utf-8"))
            return
        super().keyPressEvent(event)

    def paintEvent(self, _event: Any) -> None:
        painter = QPainter(self)
        painter.fillRect(self.rect(), _BG_DEFAULT)
        painter.setFont(self.font())

        cell_w = self._cell_w
        cell_h = self._cell_h
        baseline = self._metrics.ascent()
        for y, row in enumerate(self._screen.display):
            # pyte.HistoryScreen.buffer is a defaultdict whose rows are
            # also defaultdicts of Char, so out-of-range lookups give a
            # default-styled space — no None handling needed.
            line = self._screen.buffer[y]
            for x in range(min(len(row), self._screen.columns)):
                ch = line[x]
                glyph = row[x] if x < len(row) else " "
                fg = _resolve_color(ch.fg, is_bg=False)
                bg = _resolve_color(ch.bg, is_bg=True)
                if ch.reverse:
                    fg, bg = bg, fg
                px = x * cell_w
                py = y * cell_h
                if bg != _BG_DEFAULT:
                    painter.fillRect(px, py, cell_w, cell_h, bg)
                painter.setPen(fg)
                if glyph and glyph != " ":
                    painter.drawText(px, py + baseline, glyph)

        # Cursor as a filled rectangle (block style).
        if not self._screen.cursor.hidden:
            cx = self._screen.cursor.x * cell_w
            cy = self._screen.cursor.y * cell_h
            painter.fillRect(cx, cy, cell_w, cell_h, _PALETTE["default"])
            ch_under = self._screen.buffer[self._screen.cursor.y][self._screen.cursor.x]
            if ch_under.data and ch_under.data != " ":
                painter.setPen(_BG_DEFAULT)
                painter.drawText(cx, cy + baseline, ch_under.data)
        painter.end()
