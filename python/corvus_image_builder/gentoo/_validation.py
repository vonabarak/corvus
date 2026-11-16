"""Validate identifiers before using them in commands or artifact paths."""

import re
from pathlib import Path


def validate_disk(disk: Path) -> None:
    """Require a direct, single-name Linux device path."""
    if re.fullmatch(r"/dev/[A-Za-z0-9][A-Za-z0-9._+-]*", str(disk)) is None:
        raise ValueError(f"Invalid target disk name: {disk}")


def validate_kernel(kernel: str) -> None:
    """Require a kernel identifier without separators or quoting characters."""
    if re.fullmatch(r"[A-Za-z0-9][A-Za-z0-9._+-]*", kernel) is None:
        raise ValueError(f"Invalid kernel identifier: {kernel}")
