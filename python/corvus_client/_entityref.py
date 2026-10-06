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

import capnp

from . import _schema


def entity_ref(
    value: int | str, *, by_name: bool = False
) -> capnp.lib.capnp._DynamicStructBuilder:
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


def disk_entity_ref(
    value: int | str, *, by_name: bool = False
) -> capnp.lib.capnp._DynamicStructBuilder:
    """Resolve an image ID or name[:tag]; bare names select latest."""
    if isinstance(value, bool):
        raise TypeError("disk selector does not accept bool")
    if isinstance(value, int):
        if not 0 < value <= (1 << 63) - 1:
            raise ValueError("image ID must be a positive Int64")
        return entity_ref(value)
    if not isinstance(value, str) or not value:
        raise ValueError("disk selector must be a nonempty name or ID")
    if "0" <= value[0] <= "9":
        if not value.isascii() or not value.isdecimal():
            raise ValueError("digit-leading disk selectors must be decimal image IDs")
        return disk_entity_ref(int(value))
    parts = value.split(":")
    if (
        len(parts) > 2
        or not parts[0]
        or any(x in parts[0] for x in ("/", "\\", "..", "\0"))
    ):
        raise ValueError("invalid image name")
    if len(parts) == 2:
        import re

        if not re.fullmatch(r"[A-Za-z0-9_][A-Za-z0-9_.-]{0,127}", parts[1]):
            raise ValueError("invalid image tag")
    return entity_ref(value, by_name=True)
