"""Dialog: run a single command via the QEMU guest agent.

Stays open after Run so the user can re-execute. Bridge result feeds
into the stdout / stderr / exit-code panes.
"""

from __future__ import annotations

from typing import TYPE_CHECKING, Any

from corvus_client.types import GuestExecResult
from PySide6.QtCore import Qt
from PySide6.QtGui import QFontDatabase
from PySide6.QtWidgets import (
    QDialog,
    QDialogButtonBox,
    QHBoxLayout,
    QLabel,
    QLineEdit,
    QPlainTextEdit,
    QPushButton,
    QSplitter,
    QVBoxLayout,
    QWidget,
)

if TYPE_CHECKING:
    from ..client_bridge import CorvusBridge


class GuestExecDialog(QDialog):
    def __init__(
        self, bridge: CorvusBridge, vm_id: int, parent: QWidget | None = None
    ) -> None:
        super().__init__(parent)
        self.setWindowTitle("Guest exec")
        self.setModal(True)
        self.resize(900, 600)
        self._bridge = bridge
        self._vm_id = vm_id

        self._command = QLineEdit()
        self._command.setPlaceholderText("e.g. uptime")
        self._command.returnPressed.connect(self._on_run)
        self._run_btn = QPushButton("Run")
        self._run_btn.setDefault(True)
        self._run_btn.clicked.connect(self._on_run)

        toolbar = QHBoxLayout()
        toolbar.addWidget(self._command, 1)
        toolbar.addWidget(self._run_btn)

        self._stdout = QPlainTextEdit()
        self._stdout.setReadOnly(True)
        self._stderr = QPlainTextEdit()
        self._stderr.setReadOnly(True)
        mono = QFontDatabase.systemFont(QFontDatabase.SystemFont.FixedFont)
        for w in (self._stdout, self._stderr):
            w.setFont(mono)

        split = QSplitter(Qt.Orientation.Vertical)
        split.addWidget(_labeled("stdout", self._stdout))
        split.addWidget(_labeled("stderr", self._stderr))

        self._exit_code = QLabel("(not yet run)")

        buttons = QDialogButtonBox(QDialogButtonBox.StandardButton.Close)
        buttons.rejected.connect(self.accept)

        layout = QVBoxLayout(self)
        layout.addLayout(toolbar)
        layout.addWidget(split, 1)
        layout.addWidget(self._exit_code)
        layout.addWidget(buttons)

        bridge.guest_exec_result.connect(self._on_result)
        bridge.operation_failed.connect(self._on_op_failed)

    def _on_run(self) -> None:
        cmd = self._command.text().strip()
        if not cmd:
            return
        self._exit_code.setText("running…")
        self._stdout.clear()
        self._stderr.clear()
        self._run_btn.setEnabled(False)
        self._bridge.vm_guest_exec(self._vm_id, cmd)

    def _on_result(self, vm_id: int, result: Any) -> None:
        if vm_id != self._vm_id:
            return
        self._run_btn.setEnabled(True)
        if isinstance(result, GuestExecResult):
            self._stdout.setPlainText(result.stdout or "")
            self._stderr.setPlainText(result.stderr or "")
            self._exit_code.setText(f"exit code: {result.exit_code}")

    def _on_op_failed(self, op: str, message: str) -> None:
        if op != "vm_guest_exec":
            return
        self._run_btn.setEnabled(True)
        self._exit_code.setText(f"failed: {message}")


def _labeled(title: str, widget: QWidget) -> QWidget:
    holder = QWidget()
    layout = QVBoxLayout(holder)
    layout.setContentsMargins(0, 0, 0, 0)
    layout.addWidget(QLabel(title))
    layout.addWidget(widget, 1)
    return holder
