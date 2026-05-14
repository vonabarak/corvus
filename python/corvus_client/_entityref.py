"""Build EntityRef union values for the Corvus schema.

The schema's `EntityRef` is a union of `id :Int64` and `name :Text`
(see `schema/common.capnp`). The Haskell client's heuristic in
`Corvus.Client.Capnp.Rpc.entityRefFromText` is: if the bare token
matches `^[0-9]+$`, populate `id`; otherwise populate `name`. We
mirror that here so `crv vm show 42` and `Client.vms.get("42")` reach
the same VM (the one whose id is 42, not a VM literally named "42").

Callers who must address a digit-only name use `by_name=True`.
"""
from __future__ import annotations

from typing import Union

from . import _schema


def entity_ref(value: Union[int, str], *, by_name: bool = False):
    """Build an EntityRef message ready to pass as a cap method arg.

    - `int` → `EntityRef.id`
    - `str` of all digits → `EntityRef.id` (the `crv` heuristic),
      unless `by_name=True`, which forces `EntityRef.name`.
    - any other `str` → `EntityRef.name`.
    """
    ref = _schema.common.EntityRef.new_message()
    if isinstance(value, bool):  # bool is a subclass of int in Python
        raise TypeError("entity_ref does not accept bool")
    if isinstance(value, int):
        ref.id = value
        return ref
    if not isinstance(value, str):
        raise TypeError(f"entity_ref: unsupported type {type(value).__name__}")
    if not by_name and value.isdigit():
        ref.id = int(value)
    else:
        ref.name = value
    return ref
