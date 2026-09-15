"""Shared utilities for corvus-web route handlers."""

from dataclasses import asdict, is_dataclass
from datetime import date, datetime
from typing import Any


def to_dict(obj: Any) -> Any:
    """Recursively convert a dataclass tree (incl. datetimes, nested
    lists, tuples) into JSON-friendly primitives.

    Used by every route handler to serialise response payloads for
    JSON transmission over HTTP.  Type is ``Any`` because the input
    can be any dataclass type and the recursive structure is
    inherently heterogeneous.
    """
    if isinstance(obj, datetime):
        return obj.isoformat()
    if isinstance(obj, date):
        return obj.isoformat()
    if isinstance(obj, tuple):
        return [to_dict(v) for v in obj]
    if is_dataclass(obj) and not isinstance(obj, type):
        return {k: to_dict(v) for k, v in asdict(obj).items()}
    if isinstance(obj, list):
        return [to_dict(v) for v in obj]
    return obj
