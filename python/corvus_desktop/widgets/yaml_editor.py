"""YAML editor widget — QPlainTextEdit with a YAML syntax highlighter.

QScintilla would give "real" syntax highlighting + a line-number margin,
but its binding ships against PyQt5 and PyQt5 won't safely coexist with
PySide6 in the same process. So we get by with a hand-written
``QSyntaxHighlighter`` and a fixed-width font; plenty good for the
templates / cloud-init / apply / build editors that need it.

Single public API:

* ``set_text(text)`` / ``text()`` for content
* ``validate()`` → ``True`` if ``yaml.safe_load`` accepts the body
* ``validation_changed`` signal emits the error message (or empty
  string on success) after every text change — let callers gate a
  Save button on it.
"""

from __future__ import annotations

import re

from PySide6.QtCore import QRegularExpression, Signal
from PySide6.QtGui import (
    QColor,
    QFont,
    QFontDatabase,
    QSyntaxHighlighter,
    QTextCharFormat,
    QTextDocument,
)
from PySide6.QtWidgets import QFileDialog, QMessageBox, QPlainTextEdit, QWidget

import yaml


def _fmt(color: str, *, bold: bool = False, italic: bool = False) -> QTextCharFormat:
    f = QTextCharFormat()
    f.setForeground(QColor(color))
    if bold:
        f.setFontWeight(QFont.Weight.Bold)
    if italic:
        f.setFontItalic(True)
    return f


class _YamlHighlighter(QSyntaxHighlighter):
    """Minimal YAML highlighter.

    Recognises: comments, document markers, block-mapping keys,
    quoted strings, block scalar indicators, booleans / nulls,
    numbers. Not a real parser; runs once per line, which is fine
    for our editor sizes (templates / cloud-init are <1k lines).
    """

    def __init__(self, document: QTextDocument) -> None:
        super().__init__(document)
        self._rules: list[tuple[QRegularExpression, QTextCharFormat]] = [
            # Comments to end of line.
            (QRegularExpression(r"#.*$"), _fmt("#6a737d", italic=True)),
            # Block-mapping keys — word characters followed by ":" with a
            # space or end-of-line. Stops before flow-collection braces.
            (
                QRegularExpression(r"^\s*-?\s*([A-Za-z_][\w.-]*)(?=\s*:)"),
                _fmt("#005cc5", bold=True),
            ),
            # Document separators / directives.
            (QRegularExpression(r"^---\s*$|^\.\.\.\s*$"), _fmt("#d73a49")),
            # Booleans / null tokens (YAML 1.1 keywords).
            (
                QRegularExpression(
                    r"\b(true|false|yes|no|on|off|null|~)\b",
                    QRegularExpression.PatternOption.CaseInsensitiveOption,
                ),
                _fmt("#005cc5"),
            ),
            # Numbers.
            (QRegularExpression(r"\b\d+(\.\d+)?\b"), _fmt("#005cc5")),
            # Block scalar indicators (| and >, optionally with - or +).
            (QRegularExpression(r"[|>][+-]?\s*$"), _fmt("#d73a49")),
            # Anchors / aliases / tags.
            (QRegularExpression(r"[&*][\w.-]+"), _fmt("#6f42c1")),
            (QRegularExpression(r"!{1,2}[\w.-]+"), _fmt("#6f42c1")),
        ]
        # Quoted strings — handled separately so they win over keyword
        # matches inside the string.
        self._string_rules: list[tuple[QRegularExpression, QTextCharFormat]] = [
            (QRegularExpression(r"\"([^\"\\]|\\.)*\""), _fmt("#22863a")),
            (QRegularExpression(r"'([^'\\]|\\.)*'"), _fmt("#22863a")),
        ]

    def highlightBlock(self, text: str) -> None:
        for pattern, fmt in self._rules:
            it = pattern.globalMatch(text)
            while it.hasNext():
                m = it.next()
                self.setFormat(m.capturedStart(), m.capturedLength(), fmt)
        # Strings last so they override keyword matches inside quotes.
        for pattern, fmt in self._string_rules:
            it = pattern.globalMatch(text)
            while it.hasNext():
                m = it.next()
                self.setFormat(m.capturedStart(), m.capturedLength(), fmt)


# Strip ANSI control characters that are sometimes pasted from terminal
# output and confuse PyYAML. Keep whitespace + printable.
_CONTROL_CHARS_RE = re.compile(r"[\x00-\x08\x0b-\x1f\x7f]")


class YamlEditor(QPlainTextEdit):
    """A ``QPlainTextEdit`` preconfigured for YAML editing."""

    # Empty string on success, error message otherwise.
    validation_changed = Signal(str)

    def __init__(self, initial: str = "", parent: QWidget | None = None) -> None:
        super().__init__(parent)
        font = QFontDatabase.systemFont(QFontDatabase.SystemFont.FixedFont)
        font.setPointSize(max(font.pointSize(), 10))
        self.setFont(font)
        self.setLineWrapMode(QPlainTextEdit.LineWrapMode.NoWrap)
        self.setTabChangesFocus(False)
        # 4-space soft-tabs — YAML's grammar requires spaces, never tabs.
        metrics = self.fontMetrics()
        self.setTabStopDistance(metrics.horizontalAdvance(" ") * 4)
        self._highlighter = _YamlHighlighter(self.document())
        self.setPlainText(initial)
        self.textChanged.connect(self._on_text_changed)
        self._last_error: str | None = None

    # -------------------------------------------------------- public API

    def set_text(self, text: str) -> None:
        self.setPlainText(text)

    def text(self) -> str:
        return self.toPlainText()

    def validate(self) -> str:
        """Return an empty string on success, an error message on failure."""
        body = self.toPlainText()
        body = _CONTROL_CHARS_RE.sub("", body)
        try:
            yaml.safe_load(body)
        except yaml.YAMLError as e:
            # PyYAML's __str__ already produces "while parsing... in line N, ..."
            return str(e)
        except Exception as e:
            return f"{type(e).__name__}: {e}"
        return ""

    # ---------------------------------------------------------- internals

    def _on_text_changed(self) -> None:
        err = self.validate()
        if err != self._last_error:
            self._last_error = err
            self.validation_changed.emit(err)


def load_yaml_into(editor: YamlEditor, parent: QWidget | None = None) -> str | None:
    """Pop a ``QFileDialog`` and replace ``editor``'s text with the
    chosen file's contents.

    Returns the chosen path on success, ``None`` if the user cancelled
    or the read failed (an error dialog has already been shown in the
    latter case).
    """
    path, _filter = QFileDialog.getOpenFileName(
        parent,
        "Load YAML file",
        "",
        "YAML files (*.yml *.yaml);;All files (*)",
    )
    if not path:
        return None
    try:
        with open(path, encoding="utf-8") as f:
            body = f.read()
    except OSError as e:
        QMessageBox.warning(
            parent,
            "Could not load file",
            f"Failed to read {path}:\n{e}",
        )
        return None
    editor.set_text(body)
    return path
