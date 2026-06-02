"""Task detail screen — subscribes to live ``TaskProgress*`` events.

Lifecycle:

1. ``set_task_id`` is called by the main window when a row is activated.
2. We tell the bridge to ``subscribe_task(task_id)``.
3. ``CorvusBridge.task_event`` deliveries are filtered by task id and
   rendered into a status label, a progress bar, and a small log.
4. The first ``TaskProgressFinished`` we see updates the result badge
   and the bridge auto-keeps the subscription open — closing it is a
   no-op for finished tasks, but we tear down explicitly on close to
   free the daemon's subscriber slot.
"""

from __future__ import annotations

from typing import TYPE_CHECKING, Any

from corvus_client.types import (
    TaskInfo,
    TaskProgressFinished,
    TaskProgressProgress,
    TaskProgressStarted,
)
from PySide6.QtCore import QSize, Qt, Signal
from PySide6.QtWidgets import (
    QFormLayout,
    QLabel,
    QPlainTextEdit,
    QProgressBar,
    QPushButton,
    QSizePolicy,
    QVBoxLayout,
    QWidget,
)

if TYPE_CHECKING:
    from ..client_bridge import CorvusBridge


class TaskDetailWidget(QWidget):
    """Live view of a single task's progress."""

    # Forwarded when the user clicks Back. The main window swaps in
    # the task list view in response.
    back_requested = Signal()

    def __init__(self, bridge: CorvusBridge, parent: QWidget | None = None) -> None:
        super().__init__(parent)
        self._bridge = bridge
        self._task_id: int | None = None

        # --------------- header row ---------------
        self._back_btn = QPushButton("← Back to tasks")
        self._back_btn.clicked.connect(self.back_requested)
        self._title = QLabel("(no task)")
        self._title.setStyleSheet("font-size: 14pt; font-weight: 600;")

        # --------------- field grid ---------------
        self._subsystem = QLabel("—")
        self._command = QLabel("—")
        self._result = QLabel("—")
        self._result.setStyleSheet("font-weight: 600;")
        # Message can be a long error trace; QLabel + word-wrap in a
        # QFormLayout doesn't grow vertically. The custom widget below
        # is a borderless read-only QPlainTextEdit that resizes itself
        # to fit whatever's currently in the document.
        self._message = _AutoSizingPlainTextEdit("—")
        form = QFormLayout()
        form.addRow("Subsystem:", self._subsystem)
        form.addRow("Command:", self._command)
        form.addRow("Result:", self._result)
        form.addRow("Message:", self._message)

        # --------------- progress ---------------
        self._progress = QProgressBar()
        self._progress.setRange(0, 0)  # indeterminate by default
        self._progress.setTextVisible(True)
        self._progress.setFormat("waiting for first progress event…")

        # --------------- event log ---------------
        self._log = QPlainTextEdit()
        self._log.setReadOnly(True)
        self._log.setMaximumBlockCount(5000)  # cap memory on chatty tasks

        # --------------- layout ---------------
        layout = QVBoxLayout(self)
        layout.addWidget(self._back_btn)
        layout.addWidget(self._title)
        layout.addLayout(form)
        layout.addWidget(self._progress)
        layout.addWidget(self._log, 1)

        bridge.task_event.connect(self._on_event)
        bridge.task_detail_ready.connect(self._on_detail)
        bridge.operation_failed.connect(self._on_op_failed)

    # ---------------------------------------------------- public

    def set_task_id(self, task_id: int) -> None:
        """Switch to ``task_id``: unsubscribe from the previous one
        (if any), reset widgets, fetch the static record, and request
        a fresh subscription.

        The static fetch is what populates fields for already-finished
        tasks (which produce no further progress events); the
        subscription handles live updates if the task is still running.
        """
        self.clear()
        self._task_id = task_id
        self._title.setText(f"Task #{task_id}")
        self._progress.setFormat("loading…")
        self._bridge.request_task_detail(task_id)
        self._bridge.subscribe_task(task_id)

    def clear(self) -> None:
        """Drop any active subscription and reset the UI."""
        if self._task_id is not None:
            self._bridge.unsubscribe_task(self._task_id)
        self._task_id = None
        self._title.setText("(no task)")
        for label in (self._subsystem, self._command, self._result):
            label.setText("—")
        self._message.setPlainText("—")
        self._progress.setRange(0, 0)
        self._progress.setValue(0)
        self._progress.setFormat("")
        self._log.clear()

    # ---------------------------------------------------- slots

    def _on_detail(self, info: Any) -> None:
        """Populate fields from the static :class:`TaskInfo` record.

        Always runs on set_task_id; for live / running tasks the
        subscription's progress events later overwrite the relevant
        bits. For already-finished tasks this is the only data we
        ever get.
        """
        if not isinstance(info, TaskInfo) or info.id != self._task_id:
            return
        self._subsystem.setText(info.subsystem)
        self._command.setText(info.command)
        self._result.setText(info.result)
        self._message.setPlainText(info.message or "—")
        # Reflect the result in the progress bar too.
        if info.result == "running":
            self._progress.setRange(0, 0)
            self._progress.setFormat("running…")
        else:
            self._progress.setRange(0, 1)
            self._progress.setValue(1)
            self._progress.setFormat(info.result)

    def _on_event(self, task_id: int, event: Any) -> None:
        if task_id != self._task_id:
            return  # event for a different task — ignore
        if isinstance(event, TaskProgressStarted):
            self._subsystem.setText(event.subsystem)
            self._command.setText(event.command)
            self._log.appendPlainText(f"[started] {event.subsystem} {event.command}")
        elif isinstance(event, TaskProgressProgress):
            label = event.label or ""
            if event.total:
                self._progress.setRange(0, event.total)
                self._progress.setValue(event.completed)
                self._progress.setFormat(
                    f"{event.completed}/{event.total}"
                    + (f" — {label}" if label else "")
                )
            else:
                self._progress.setRange(0, 0)
                self._progress.setFormat(label or "in progress…")
            if label:
                self._log.appendPlainText(f"[progress] {label}")
        elif isinstance(event, TaskProgressFinished):
            self._result.setText(event.result)
            if event.message:
                self._message.setPlainText(event.message)
            self._progress.setRange(0, 1)
            self._progress.setValue(1)
            self._progress.setFormat(event.result)
            self._log.appendPlainText(
                f"[finished] {event.result}"
                + (f": {event.message}" if event.message else "")
            )

    def _on_op_failed(self, op: str, message: str) -> None:
        if op not in ("task_subscribe", "task_detail"):
            return
        if self._task_id is None:
            return
        self._log.appendPlainText(f"[bridge] {op}: {message}")
        self._progress.setFormat(f"{op} failed")


class _AutoSizingPlainTextEdit(QPlainTextEdit):
    """Borderless, scrollbar-less QPlainTextEdit that grows vertically
    to fit its document, wrapping to the widget's current width.

    Uses Qt's standard ``hasHeightForWidth`` / ``heightForWidth``
    mechanism so the parent layout sizes the row correctly — without
    that, a QPlainTextEdit just keeps its default sizeHint and the
    rest of the text falls off the bottom. ``textChanged`` calls
    ``updateGeometry`` so the layout re-asks for a fresh
    ``heightForWidth`` whenever new text lands.
    """

    def __init__(self, text: str = "", parent: QWidget | None = None) -> None:
        super().__init__(text, parent)
        self.setReadOnly(True)
        self.setFrameShape(QPlainTextEdit.Shape.NoFrame)
        self.setLineWrapMode(QPlainTextEdit.LineWrapMode.WidgetWidth)
        self.setVerticalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAlwaysOff)
        self.setHorizontalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAlwaysOff)
        # Tell the layout system to consult heightForWidth(); without
        # this the form layout fixes our height to the default size
        # hint and the wrapped tail of the text gets hidden.
        policy = QSizePolicy(QSizePolicy.Policy.Preferred, QSizePolicy.Policy.Preferred)
        policy.setHeightForWidth(True)
        self.setSizePolicy(policy)
        self.textChanged.connect(self._on_text_changed)

    def hasHeightForWidth(self) -> bool:
        return True

    def heightForWidth(self, width: int) -> int:
        margin = self.contentsMargins()
        doc_margin = int(self.document().documentMargin())
        # Text wraps at: widget width - widget contents margins -
        # frame thickness - the document's own internal margin on
        # both sides. Miss any of those and the calc is short by a
        # line (the previous round forgot doc_margin).
        usable = (
            width
            - margin.left()
            - margin.right()
            - self.frameWidth() * 2
            - 2 * doc_margin
        )
        if usable <= 0:
            return self.fontMetrics().lineSpacing() + 8
        # QFontMetrics.boundingRect with TextWordWrap measures the
        # rendered height the same way Qt's text layout does — more
        # reliable than cloning the QTextDocument and reading
        # ``doc.size()`` (which silently rounded down here).
        rect = self.fontMetrics().boundingRect(
            0,
            0,
            usable,
            10_000,
            int(Qt.TextFlag.TextWordWrap),
            self.toPlainText(),
        )
        return (
            rect.height()
            + margin.top()
            + margin.bottom()
            + self.frameWidth() * 2
            + 2 * doc_margin
            + 4
        )

    def sizeHint(self) -> QSize:
        w = self.width() or 200
        return QSize(w, self.heightForWidth(w))

    def minimumSizeHint(self) -> QSize:
        return QSize(0, self.fontMetrics().lineSpacing() + 8)

    def resizeEvent(self, event: Any) -> None:
        super().resizeEvent(event)
        self._lock_height_to_content()

    # Called from textChanged as well, since the layout may not
    # re-query sizeHint after content changes.
    def _on_text_changed(self) -> None:
        self.updateGeometry()
        self._lock_height_to_content()

    def _lock_height_to_content(self) -> None:
        """Force the widget's height to exactly what
        :meth:`heightForWidth` reports. QFormLayout silently ignores
        ``heightForWidth`` on field widgets, so we have to plant the
        height ourselves via ``setFixedHeight``."""
        h = self.heightForWidth(self.width())
        if h <= 0 or h == self.height():
            return
        self.setFixedHeight(h)
