"""Colored badges for VM status + task result.

Mirrors the React badges (frontend/src/components/ui/vm-status-badge.tsx
and task-result-badge.tsx) so the desktop and web UIs agree on color
language at a glance: green = stable/success, yellow = transitional /
running, red = error, grey = inactive.
"""

from __future__ import annotations

from PySide6.QtCore import Qt
from PySide6.QtWidgets import QLabel, QWidget

# Status → (background, foreground). Matches the web's color tokens.
_VM_STATUS_COLORS: dict[str, tuple[str, str]] = {
    "stopped": ("#e5e7eb", "#374151"),
    "starting": ("#fef3c7", "#92400e"),
    "running": ("#d1fae5", "#065f46"),
    "stopping": ("#fef3c7", "#92400e"),
    "paused": ("#dbeafe", "#1e40af"),
    "saved": ("#dbeafe", "#1e40af"),
    "error": ("#fee2e2", "#991b1b"),
}
_TASK_RESULT_COLORS: dict[str, tuple[str, str]] = {
    "running": ("#fef3c7", "#92400e"),
    "success": ("#d1fae5", "#065f46"),
    "error": ("#fee2e2", "#991b1b"),
}


def _badge_stylesheet(bg: str, fg: str) -> str:
    return (
        "QLabel {"
        f" background: {bg};"
        f" color: {fg};"
        " border-radius: 4px;"
        " padding: 2px 8px;"
        " font-weight: 600;"
        " font-size: 11px;"
        "}"
    )


class StatusBadge(QLabel):
    """A small colored pill for a single status value."""

    def __init__(self, kind: str = "vm", parent: QWidget | None = None) -> None:
        super().__init__(parent)
        self._kind = kind
        self.setAlignment(Qt.AlignmentFlag.AlignCenter)
        self.setText("")
        self._apply("", _VM_STATUS_COLORS.get("stopped", ("#e5e7eb", "#374151")))

    def set_status(self, status: str) -> None:
        table = _TASK_RESULT_COLORS if self._kind == "task" else _VM_STATUS_COLORS
        colors = table.get(status, ("#e5e7eb", "#374151"))
        self._apply(status, colors)

    def _apply(self, status: str, colors: tuple[str, str]) -> None:
        self.setText(status or "?")
        self.setStyleSheet(_badge_stylesheet(*colors))
