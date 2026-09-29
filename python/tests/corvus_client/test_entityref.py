"""entity_ref heuristic tests (pure-Python; no daemon needed)."""

from __future__ import annotations

from typing import cast

import pytest
from corvus_client._entityref import entity_ref


def test_int_value_sets_id() -> None:
    r = entity_ref(42)
    assert r.which() == "id"
    assert r.id == 42


def test_digit_string_sets_id() -> None:
    r = entity_ref("42")
    assert r.which() == "id"
    assert r.id == 42


def test_name_string_sets_name() -> None:
    r = entity_ref("web-1")
    assert r.which() == "name"
    assert r.name == "web-1"


def test_by_name_override() -> None:
    r = entity_ref("42", by_name=True)
    assert r.which() == "name"
    assert r.name == "42"


def test_bool_rejected() -> None:
    with pytest.raises(TypeError):
        entity_ref(True)


def test_other_type_rejected() -> None:
    with pytest.raises(TypeError):
        entity_ref(cast(int | str, 3.14))
