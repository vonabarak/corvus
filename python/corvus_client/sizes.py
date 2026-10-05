"""Exact binary sizes: API quantities are integers in bytes."""

import re

MAX_SIZE = (1 << 63) - 1
MIB = 1 << 20


def validate_size(value: int, *, ram: bool = False) -> int:
    if type(value) is not int or not 0 < value <= MAX_SIZE:
        raise ValueError("Size must be a positive integer byte count fitting Int64")
    if ram and value % MIB:
        raise ValueError("RAM must be a whole multiple of 1M (1048576 bytes)")
    return value


def parse_size(value: str, *, ram: bool = False) -> int:
    match = re.fullmatch(r"([0-9]+)([BKMGT])", value, re.IGNORECASE)
    if match is None:
        raise ValueError("Expected an integer with a B/K/M/G/T suffix (e.g. 1G)")
    size = int(match[1]) * 1024 ** "BKMGT".index(match[2].upper())
    return validate_size(size, ram=ram)


def format_size(value: int | None) -> str:
    if value is None:
        return "—"
    if value == 0:
        return "0B"
    for power in range(4, -1, -1):
        factor = 1024**power
        if value % factor == 0:
            return f"{value // factor}{'BKMGT'[power]}"
    raise AssertionError("unreachable")
