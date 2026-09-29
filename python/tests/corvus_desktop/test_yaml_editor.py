"""Ring 2 tests: :class:`YamlEditor`."""

from __future__ import annotations

from corvus_desktop.widgets.yaml_editor import YamlEditor
from PySide6.QtWidgets import QApplication


def test_validate_good_yaml(qapp: QApplication) -> None:
    ed = YamlEditor("a: 1\nb: [2, 3]\n")
    assert ed.validate() == ""


def test_validate_bad_yaml_returns_error(qapp: QApplication) -> None:
    ed = YamlEditor("a: [unterminated\n")
    err = ed.validate()
    assert err != ""
    assert "expected" in err or "found" in err or "stream" in err.lower()


def test_validation_changed_fires(qapp: QApplication) -> None:
    """Editing should fire ``validation_changed`` with the latest state."""
    ed = YamlEditor("a: 1\n")
    events: list[str] = []
    ed.validation_changed.connect(events.append)
    ed.set_text("a:\n  - oops:\n  -")
    # Validation fires synchronously on textChanged.
    assert len(events) >= 1


def test_text_round_trip(qapp: QApplication) -> None:
    ed = YamlEditor("foo: bar\n")
    assert ed.text() == "foo: bar\n"
    ed.set_text("baz: 7\n")
    assert ed.text() == "baz: 7\n"
