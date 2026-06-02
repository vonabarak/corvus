"""Disk detail screen — summary + snapshots + resize/delete actions.

Snapshot operations (create / delete / rollback / merge) are exposed
inline so the user doesn't have to leave the page. Resize prompts via
``QInputDialog`` (qcow2 disks can only grow — the daemon rejects a
shrink with a clear error that we route through ``operation_failed``).
"""

from __future__ import annotations

from typing import TYPE_CHECKING, Any

from corvus_client.types import DiskImageInfo, SnapshotInfo
from PySide6.QtCore import Qt, Signal
from PySide6.QtGui import QFont, QFontMetrics
from PySide6.QtWidgets import (
    QFormLayout,
    QGroupBox,
    QHBoxLayout,
    QInputDialog,
    QLabel,
    QLineEdit,
    QMessageBox,
    QPlainTextEdit,
    QPushButton,
    QTableWidget,
    QTableWidgetItem,
    QVBoxLayout,
    QWidget,
)

from ..dialogs.disk_rebase_copy_move import DiskCopyMoveDialog, DiskRebaseDialog

if TYPE_CHECKING:
    from ..client_bridge import CorvusBridge


def _fmt_size_mb(size_mb: int | None) -> str:
    if size_mb is None:
        return "—"
    if size_mb < 1024:
        return f"{size_mb} MB"
    return f"{size_mb / 1024:.1f} GB"


class DiskDetailWidget(QWidget):
    back_requested = Signal()

    def __init__(self, bridge: CorvusBridge, parent: QWidget | None = None) -> None:
        super().__init__(parent)
        self._bridge = bridge
        self._disk_id: int | None = None
        self._size_mb: int | None = None

        # ---------------- header ----------------
        self._back_btn = QPushButton("← Back to disks")
        self._back_btn.clicked.connect(self.back_requested)
        self._title = QLabel("(no disk)")
        f = self._title.font()
        f.setPointSize(f.pointSize() + 4)
        f.setWeight(QFont.Weight.DemiBold)
        self._title.setFont(f)

        # ---------------- action buttons ----------------
        self._btn_resize = QPushButton("Resize…")
        self._btn_rebase = QPushButton("Rebase…")
        self._btn_copy = QPushButton("Copy…")
        self._btn_move = QPushButton("Move…")
        self._btn_delete = QPushButton("Delete")
        self._btn_resize.clicked.connect(self._on_resize)
        self._btn_rebase.clicked.connect(self._on_rebase)
        self._btn_copy.clicked.connect(self._on_copy)
        self._btn_move.clicked.connect(self._on_move)
        self._btn_delete.clicked.connect(self._on_delete)
        action_row = QHBoxLayout()
        for b in (
            self._btn_resize,
            self._btn_rebase,
            self._btn_copy,
            self._btn_move,
            self._btn_delete,
        ):
            action_row.addWidget(b)
        action_row.addStretch(1)

        # ---------------- summary fields ----------------
        self._format = QLabel("—")
        self._size = QLabel("—")
        self._created = QLabel("—")
        self._backing = QLabel("—")
        self._attached = QLabel("—")
        self._attached.setWordWrap(True)
        # Placement is a list of "<node>: <path>" lines that can run
        # arbitrarily long (long paths, many replicated nodes). A QLabel
        # inside QFormLayout doesn't get the height it needs to show
        # them all; use a read-only QPlainTextEdit and resize it to fit
        # whatever the daemon hands us.
        self._placements = QPlainTextEdit("—")
        self._placements.setReadOnly(True)
        self._placements.setFrameShape(QPlainTextEdit.Shape.NoFrame)
        self._placements.setLineWrapMode(QPlainTextEdit.LineWrapMode.NoWrap)
        # Always size to content — never show inner scrollbars; the
        # field grows with however many placements come back.
        self._placements.setVerticalScrollBarPolicy(
            Qt.ScrollBarPolicy.ScrollBarAlwaysOff
        )
        self._placements.setHorizontalScrollBarPolicy(
            Qt.ScrollBarPolicy.ScrollBarAlwaysOff
        )
        form = QFormLayout()
        form.addRow("Format:", self._format)
        form.addRow("Size:", self._size)
        form.addRow("Created:", self._created)
        form.addRow("Backing:", self._backing)
        form.addRow("Attached to:", self._attached)
        form.addRow("Placement:", self._placements)
        summary = QGroupBox("Summary")
        summary_layout = QVBoxLayout(summary)
        summary_layout.addLayout(form)

        # ---------------- snapshots ----------------
        snap_box = QGroupBox("Snapshots")
        snap_layout = QVBoxLayout(snap_box)

        # New snapshot inline form
        self._new_snap_name = QLineEdit()
        self._new_snap_name.setPlaceholderText("Snapshot name")
        self._btn_create_snap = QPushButton("Create snapshot")
        self._btn_create_snap.clicked.connect(self._on_create_snapshot)
        new_row = QHBoxLayout()
        new_row.addWidget(self._new_snap_name, 1)
        new_row.addWidget(self._btn_create_snap)
        snap_layout.addLayout(new_row)

        self._snap_table = QTableWidget(0, 4)
        self._snap_table.setHorizontalHeaderLabels(["Name", "Created", "Size", ""])
        self._snap_table.verticalHeader().setVisible(False)
        self._snap_table.horizontalHeader().setStretchLastSection(True)
        self._snap_table.setEditTriggers(QTableWidget.EditTrigger.NoEditTriggers)
        snap_layout.addWidget(self._snap_table, 1)

        # ---------------- root layout ----------------
        layout = QVBoxLayout(self)
        layout.addWidget(self._back_btn)
        layout.addWidget(self._title)
        layout.addLayout(action_row)
        layout.addWidget(summary)
        layout.addWidget(snap_box, 1)

        bridge.disk_detail_ready.connect(self._on_detail)
        bridge.snapshot_list_ready.connect(self._on_snapshots)
        bridge.disk_action_completed.connect(self._on_disk_action_completed)
        bridge.snapshot_action_completed.connect(self._on_snapshot_action_completed)

    # ---------------------------------------------------- public

    def set_disk_id(self, disk_id: int) -> None:
        self.clear()
        self._disk_id = disk_id
        self._title.setText(f"Disk #{disk_id}")
        self._bridge.request_disk_detail(disk_id)
        self._bridge.request_snapshot_list(disk_id)

    def clear(self) -> None:
        self._disk_id = None
        self._size_mb = None
        self._title.setText("(no disk)")
        for label in (
            self._format,
            self._size,
            self._created,
            self._backing,
            self._attached,
        ):
            label.setText("—")
        self._set_placements_text("—")
        self._new_snap_name.clear()
        self._snap_table.setRowCount(0)

    # ---------------------------------------------------- bridge slots

    def _on_detail(self, info: Any) -> None:
        if not isinstance(info, DiskImageInfo) or info.id != self._disk_id:
            return
        self._size_mb = info.size_mb
        self._title.setText(info.name)
        self._format.setText(info.format)
        self._size.setText(_fmt_size_mb(info.size_mb))
        self._created.setText(info.created_at.isoformat(sep=" ", timespec="seconds"))
        self._backing.setText(info.backing_image.name if info.backing_image else "—")
        self._attached.setText(", ".join(a.vm.name for a in info.attached_to) or "—")
        self._set_placements_text(
            "\n".join(f"{p.node.name}: {p.file_path}" for p in info.placements) or "—"
        )
        # Can't delete a disk that's attached to a VM.
        self._btn_delete.setEnabled(not info.attached_to)
        # Backing-file (overlay) and clone disks can be resized; raw
        # CD-ROM media usually can't. The daemon enforces this — we
        # keep the button enabled and let `operation_failed` toast
        # surface any rejection.
        self._btn_resize.setEnabled(True)

    def _on_snapshots(self, disk_id: int, snaps: Any) -> None:
        if disk_id != self._disk_id:
            return
        self._fill_snapshots(snaps)

    def _on_disk_action_completed(self, disk_id: int, action: str) -> None:
        if disk_id != self._disk_id:
            return
        if action == "delete":
            self.back_requested.emit()
            return
        # Resize → re-fetch detail.
        self._bridge.request_disk_detail(disk_id)

    def _on_snapshot_action_completed(
        self, disk_id: int, _action: str, _snap_name: str
    ) -> None:
        if disk_id != self._disk_id:
            return
        self._bridge.request_snapshot_list(disk_id)
        # Size may have changed on rollback/merge.
        self._bridge.request_disk_detail(disk_id)

    # ---------------------------------------------------- ui actions

    def _on_resize(self) -> None:
        if self._disk_id is None:
            return
        current = self._size_mb or 0
        new_size, ok = QInputDialog.getInt(
            self,
            "Resize disk",
            f"New size in MB (current: {current} MB; cannot shrink):",
            value=max(current + 1024, 1),
            minValue=1,
            maxValue=4 * 1024 * 1024,  # 4 TB
            step=1024,
        )
        if not ok:
            return
        self._bridge.disk_resize(self._disk_id, new_size)

    def _on_rebase(self) -> None:
        if self._disk_id is None:
            return
        dlg = DiskRebaseDialog(self._bridge, self)
        if dlg.exec():
            p = dlg.payload()
            self._bridge.disk_rebase(self._disk_id, p["new_backing_disk_ref"])

    def _on_copy(self) -> None:
        if self._disk_id is None:
            return
        dlg = DiskCopyMoveDialog("copy", self)
        if dlg.exec():
            p = dlg.payload()
            self._bridge.disk_copy(
                self._disk_id,
                p["to_node_ref"],
                to_path=p["to_path"],
                with_backing_chain=p["with_backing_chain"],
            )

    def _on_move(self) -> None:
        if self._disk_id is None:
            return
        dlg = DiskCopyMoveDialog("move", self)
        if dlg.exec():
            p = dlg.payload()
            self._bridge.disk_move(
                self._disk_id,
                p["to_node_ref"],
                to_path=p["to_path"],
                with_backing_chain=p["with_backing_chain"],
            )

    def _on_delete(self) -> None:
        if self._disk_id is None:
            return
        reply = QMessageBox.question(
            self,
            "Delete disk",
            "Delete this disk image? This cannot be undone.",
            QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No,
            QMessageBox.StandardButton.No,
        )
        if reply == QMessageBox.StandardButton.Yes:
            self._bridge.disk_delete(self._disk_id)

    def _set_placements_text(self, text: str) -> None:
        """Resize the placements box to fit every line — no scrollbars."""
        self._placements.setPlainText(text)
        metrics = QFontMetrics(self._placements.font())
        line_count = max(1, text.count("\n") + 1)
        margin = self._placements.contentsMargins()
        height = (
            metrics.lineSpacing() * line_count
            + margin.top()
            + margin.bottom()
            + self._placements.frameWidth() * 2
            + 4
        )
        self._placements.setFixedHeight(height)

    def _on_create_snapshot(self) -> None:
        if self._disk_id is None:
            return
        name = self._new_snap_name.text().strip()
        if not name:
            return
        self._bridge.snapshot_create(self._disk_id, name)
        self._new_snap_name.clear()

    # ---------------------------------------------------- internals

    def _fill_snapshots(self, snaps: list[SnapshotInfo]) -> None:
        self._snap_table.setRowCount(len(snaps))
        for row, s in enumerate(snaps):
            self._snap_table.setItem(row, 0, QTableWidgetItem(s.name))
            self._snap_table.setItem(
                row,
                1,
                QTableWidgetItem(s.created_at.isoformat(sep=" ", timespec="seconds")),
            )
            self._snap_table.setItem(row, 2, QTableWidgetItem(_fmt_size_mb(s.size_mb)))
            # Per-row action button cluster.
            cell = QWidget()
            cell_layout = QHBoxLayout(cell)
            cell_layout.setContentsMargins(2, 2, 2, 2)
            cell_layout.setSpacing(4)
            btn_rb = QPushButton("Rollback")
            btn_mg = QPushButton("Merge")
            btn_rm = QPushButton("Delete")
            snap_name = s.name
            btn_rb.clicked.connect(
                lambda _checked=False, n=snap_name: self._snap_action(n, "rollback")
            )
            btn_mg.clicked.connect(
                lambda _checked=False, n=snap_name: self._snap_action(n, "merge")
            )
            btn_rm.clicked.connect(
                lambda _checked=False, n=snap_name: self._snap_action(n, "delete")
            )
            cell_layout.addWidget(btn_rb)
            cell_layout.addWidget(btn_mg)
            cell_layout.addWidget(btn_rm)
            cell_layout.addStretch(1)
            self._snap_table.setCellWidget(row, 3, cell)
        self._snap_table.resizeColumnsToContents()

    def _snap_action(self, snap_name: str, action: str) -> None:
        if self._disk_id is None:
            return
        if action in ("rollback", "delete"):
            verb = action
            reply = QMessageBox.question(
                self,
                f"{verb.capitalize()} snapshot",
                f"{verb.capitalize()} snapshot '{snap_name}'? "
                "This may affect VMs using this disk.",
                QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No,
                QMessageBox.StandardButton.No,
            )
            if reply != QMessageBox.StandardButton.Yes:
                return
        self._bridge.snapshot_action(self._disk_id, snap_name, action)
