"""Rolling sparkline chart backed by ``PySide6.QtCharts``.

Stores up to ``max_points`` Y values, auto-scales the Y axis, and slides
the X axis as samples are appended. Used by VM detail to render live
CPU / RAM / disk-I/O / net-I/O traces alongside the existing summary.

The chart is intentionally minimal — no legend, no titles, thin margins
— because the surrounding ``QGroupBox`` already labels it. The result
is a real sparkline rather than a full QtCharts dashboard.
"""

from __future__ import annotations

from PySide6.QtCharts import QChart, QChartView, QLineSeries, QValueAxis
from PySide6.QtCore import QMargins, Qt
from PySide6.QtGui import QPainter
from PySide6.QtWidgets import QWidget


class StatsSparkline(QChartView):
    """Single-series sparkline. ``add_point(y)`` appends, dropping the
    oldest point once ``max_points`` is exceeded."""

    def __init__(
        self,
        *,
        max_points: int = 60,
        y_min: float = 0.0,
        y_unit: str = "",
        parent: QWidget | None = None,
    ) -> None:
        super().__init__(parent)
        self._max_points = max_points
        self._y_unit = y_unit
        self._values: list[float] = []

        self._series = QLineSeries()
        chart = QChart()
        chart.addSeries(self._series)
        chart.legend().setVisible(False)
        chart.setMargins(QMargins(2, 2, 2, 2))
        chart.setBackgroundRoundness(0)

        self._x_axis = QValueAxis()
        self._x_axis.setRange(0, max_points - 1)
        self._x_axis.setLabelFormat(" ")
        self._x_axis.setTickCount(2)
        self._x_axis.setGridLineVisible(False)
        chart.addAxis(self._x_axis, Qt.AlignmentFlag.AlignBottom)
        self._series.attachAxis(self._x_axis)

        self._y_axis = QValueAxis()
        self._y_axis.setRange(y_min, y_min + 1.0)
        self._y_axis.setLabelFormat(f"%.1f{y_unit}" if y_unit else "%.0f")
        self._y_axis.setGridLineVisible(True)
        chart.addAxis(self._y_axis, Qt.AlignmentFlag.AlignLeft)
        self._series.attachAxis(self._y_axis)

        self.setChart(chart)
        self.setRenderHint(QPainter.RenderHint.Antialiasing)
        self.setMinimumHeight(80)

    # -------------------------------------------------------- public API

    def add_point(self, value: float) -> None:
        """Append one Y value at the next X position. Slides the window
        once we pass ``max_points``."""
        self._values.append(value)
        if len(self._values) > self._max_points:
            del self._values[0]
        self._series.clear()
        for x, y in enumerate(self._values):
            self._series.append(float(x), float(y))
        if self._values:
            ymax = max(self._values)
            ymin = min(self._values)
            # Keep a small headroom so the line isn't pinned to the top.
            span = max(ymax - ymin, 1.0)
            self._y_axis.setRange(max(0.0, ymin - 0.1 * span), ymax + 0.1 * span)

    def clear(self) -> None:
        self._values.clear()
        self._series.clear()
        self._y_axis.setRange(0.0, 1.0)

    def latest(self) -> float | None:
        return self._values[-1] if self._values else None
