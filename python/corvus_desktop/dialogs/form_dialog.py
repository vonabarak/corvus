"""Reusable form-dialog base.

Subclasses populate a :class:`QFormLayout` via :meth:`build_form` and
override :meth:`result_payload` to gate Save. Returning ``None`` blocks
the accept and keeps the dialog open (callers should also show an
error message).
"""

from __future__ import annotations

from typing import Any

from PySide6.QtCore import Qt
from PySide6.QtWidgets import (
    QDialog,
    QDialogButtonBox,
    QFormLayout,
    QLabel,
    QVBoxLayout,
    QWidget,
)


class FormDialog(QDialog):
    """Base class. Subclasses override :meth:`build_form` and
    :meth:`result_payload`."""

    def __init__(
        self,
        title: str,
        *,
        save_label: str = "Save",
        parent: QWidget | None = None,
    ) -> None:
        super().__init__(parent)
        self.setWindowTitle(title)
        self.setModal(True)

        self._error_label = QLabel("")
        self._error_label.setStyleSheet("color: #991b1b;")
        self._error_label.setWordWrap(True)
        self._error_label.setVisible(False)

        self._form = QFormLayout()
        self._buttons = QDialogButtonBox(QDialogButtonBox.StandardButton.Cancel)
        self._save_btn = self._buttons.addButton(
            save_label, QDialogButtonBox.ButtonRole.AcceptRole
        )
        self._save_btn.setDefault(True)
        self._buttons.accepted.connect(self._on_accept)
        self._buttons.rejected.connect(self.reject)

        outer = QVBoxLayout(self)
        outer.addLayout(self._form)
        outer.addWidget(self._error_label)
        outer.addWidget(self._buttons)

        self.build_form(self._form)

    # ---------------------------------------------------- API

    def build_form(self, form: QFormLayout) -> None:
        """Override to populate the form. Default no-op."""

    def result_payload(self) -> dict[str, Any] | None:
        """Override to return the payload to deliver on Save.

        Return ``None`` to block accept (also set an error message via
        :meth:`show_error`).
        """
        return None

    def show_error(self, message: str) -> None:
        """Display ``message`` under the form."""
        self._error_label.setText(message)
        self._error_label.setVisible(bool(message))

    # ---------------------------------------------------- internals

    def _on_accept(self) -> None:
        payload = self.result_payload()
        if payload is None:
            return  # show_error already called by subclass
        # Cache for caller pickup via :meth:`payload`.
        self._cached_payload = payload
        self.accept()

    def payload(self) -> dict[str, Any]:
        """Return the payload from the last successful Save."""
        return getattr(self, "_cached_payload", {})


# Ensure the import is visible to type checkers even if no method
# directly mentions it.
_ = Qt
