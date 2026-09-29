"""Shared utilities for corvus-web route handlers."""

from dataclasses import asdict, is_dataclass
from datetime import date, datetime
from typing import TypeAlias, cast

from pydantic import JsonValue

JsonObject: TypeAlias = dict[str, JsonValue]


def to_dict(obj: object) -> JsonObject:
    """Convert a top-level response dataclass to a JSON object."""
    return cast(JsonObject, _to_json(obj))


def _to_json(obj: object) -> JsonValue:
    """Recursively convert a dataclass tree (incl. datetimes, nested
    lists, tuples) into JSON-friendly primitives.

    Used by every route handler to serialise response payloads for
    JSON transmission over HTTP.  The input is deliberately ``object``:
    serializers validate it into the recursive JSON value domain instead of
    silently accepting an ``Any`` value.
    """
    if isinstance(obj, datetime):
        return obj.isoformat()
    if isinstance(obj, date):
        return obj.isoformat()
    if isinstance(obj, tuple):
        return [_to_json(v) for v in obj]
    if is_dataclass(obj) and not isinstance(obj, type):
        return {k: _to_json(v) for k, v in asdict(obj).items()}
    if isinstance(obj, list):
        return [_to_json(v) for v in obj]
    return cast(JsonValue, obj)
