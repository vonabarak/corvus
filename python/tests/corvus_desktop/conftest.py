"""Shared fixtures for the corvus-desktop test suite.

Provides an offscreen ``QApplication`` so tests can construct widgets
and use ``QSignalSpy`` without requiring a display. Pinning the
``QT_QPA_PLATFORM`` env var has to happen *before* PySide6 is imported;
the module-top assignment here runs at conftest collection time, which
is early enough.
"""

from __future__ import annotations

import os

# Must precede the first PySide6 import in this process.
os.environ.setdefault("QT_QPA_PLATFORM", "offscreen")

from collections.abc import Iterator

import pytest
from PySide6.QtCore import QCoreApplication
from PySide6.QtWidgets import QApplication


@pytest.fixture(scope="session")
def qapp() -> Iterator[QApplication]:
    """Process-wide ``QApplication``. Qt forbids more than one."""
    app = QCoreApplication.instance()
    if app is None:
        app = QApplication([])
    assert isinstance(app, QApplication)
    yield app
    # Don't quit the app — pytest reuses it across the session.
