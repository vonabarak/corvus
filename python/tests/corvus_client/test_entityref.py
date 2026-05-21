"""entity_ref heuristic tests (pure-Python; no daemon needed)."""
from __future__ import annotations

import pytest

from corvus_client._entityref import entity_ref


def test_int_value_sets_id():
    r = entity_ref(42)
    assert r.which() == "id"
    assert r.id == 42


def test_digit_string_sets_id():
    r = entity_ref("42")
    assert r.which() == "id"
    assert r.id == 42


def test_name_string_sets_name():
    r = entity_ref("web-1")
    assert r.which() == "name"
    assert r.name == "web-1"


def test_by_name_override():
    r = entity_ref("42", by_name=True)
    assert r.which() == "name"
    assert r.name == "42"


def test_bool_rejected():
    with pytest.raises(TypeError):
        entity_ref(True)


def test_other_type_rejected():
    with pytest.raises(TypeError):
        entity_ref(3.14)  # type: ignore[arg-type]
