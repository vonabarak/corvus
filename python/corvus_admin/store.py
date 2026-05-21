"""On-disk admin-store layout.

Everything corvus-admin persists lives under one directory (the
"admin store"):

    $XDG_CONFIG_HOME/corvus/admin/
    ├── ca.crt                       — CA cert (also deployed to every host)
    ├── ca.key                       — CA private key (mode 0600, never leaves admin's box)
    ├── serial                       — monotonic serial counter, text
    ├── issued/                      — one file per minted cert (PEM)
    │   ├── corvus-daemon:<uuid>.crt
    │   ├── corvus-node:<name>.crt
    │   ├── corvus-netd:<name>.crt
    │   └── corvus-client:<name>.crt
    └── index.json                   — { full_cn → record } for `corvus-admin list`

Functions in this module manipulate that tree but stay agnostic
of the contents of the cert files themselves; cert generation
lives in :mod:`corvus_admin.ca`.
"""

from __future__ import annotations

import json
import os
import re
from collections.abc import Iterator
from dataclasses import asdict, dataclass, field
from pathlib import Path

# Filenames used in the admin dir. Keep these constants in sync
# with the Haskell-side defaults in Corvus.Tls.
CA_CERT_NAME = "ca.crt"
CA_KEY_NAME = "ca.key"
SERIAL_NAME = "serial"
INDEX_NAME = "index.json"
ISSUED_DIRNAME = "issued"


@dataclass
class IssuedRecord:
    """One row in the admin-store index. Anything we want
    `corvus-admin list` (and future `renew` / `status`) to display
    lives here. Cert / key file paths are derived from the CN, so
    we don't store them again."""

    cn: str
    role: str
    name_or_uuid: str
    issued_at: str  # ISO 8601 UTC
    expires_at: str  # ISO 8601 UTC
    serial: int
    deployed_to: str | None = None  # e.g. "root@10.0.0.21" or "local"
    ip: str | None = None  # SAN IP, when applicable
    extra: dict = field(default_factory=dict)


def default_admin_dir() -> Path:
    """Default location for the admin store: $XDG_CONFIG_HOME/corvus/admin.

    Falls back to ~/.config/corvus/admin when XDG_CONFIG_HOME is
    unset (matching the Haskell-side search-path resolver in
    Corvus.Tls.resolveXdgConfigDir)."""

    xdg = os.environ.get("XDG_CONFIG_HOME")
    if xdg:
        return Path(xdg) / "corvus" / "admin"
    home = os.environ.get("HOME") or os.path.expanduser("~")
    return Path(home) / ".config" / "corvus" / "admin"


def default_client_dir() -> Path:
    """Where the admin's own client cert lands so `crv` finds it
    automatically (search path is XDG-only for client certs).

    Same XDG resolution as :func:`default_admin_dir`, minus the
    final ``admin/`` segment."""

    xdg = os.environ.get("XDG_CONFIG_HOME")
    if xdg:
        return Path(xdg) / "corvus"
    home = os.environ.get("HOME") or os.path.expanduser("~")
    return Path(home) / ".config" / "corvus"


class AdminStore:
    """File-system view of the admin store. Constructing one is
    cheap (just stores the root path); the methods do the I/O."""

    def __init__(self, root: Path) -> None:
        self.root = Path(root)

    # ------------------------------------------------------------------
    # Layout

    @property
    def ca_cert_path(self) -> Path:
        return self.root / CA_CERT_NAME

    @property
    def ca_key_path(self) -> Path:
        return self.root / CA_KEY_NAME

    @property
    def serial_path(self) -> Path:
        return self.root / SERIAL_NAME

    @property
    def index_path(self) -> Path:
        return self.root / INDEX_NAME

    @property
    def issued_dir(self) -> Path:
        return self.root / ISSUED_DIRNAME

    def issued_cert_path(self, cn: str) -> Path:
        # Filenames carry the full CN (including the colon), which
        # is illegal on Windows but fine on the Linux machines
        # this tool is intended for. Sanity-check anyway:
        if not re.match(r"^corvus-(daemon|node|netd|client):[^/\x00]+$", cn):
            raise ValueError(f"invalid CN {cn!r}")
        return self.issued_dir / f"{cn}.crt"

    # ------------------------------------------------------------------
    # Initialisation

    def exists(self) -> bool:
        """True iff the admin store has been initialised (`init` ran)."""
        return self.ca_cert_path.exists() and self.ca_key_path.exists()

    def ensure_layout(self) -> None:
        """Create the directory skeleton with safe permissions if
        absent. Idempotent. Files inside (ca.key, certs) are
        managed by the caller."""

        self.root.mkdir(mode=0o700, parents=True, exist_ok=True)
        self.issued_dir.mkdir(mode=0o700, parents=True, exist_ok=True)
        if not self.serial_path.exists():
            self.serial_path.write_text("1\n")
        if not self.index_path.exists():
            self.index_path.write_text("{}\n")
        # Tighten existing perms — `mkdir(exist_ok=True)` honours
        # whatever mode the caller already had.
        try:
            os.chmod(self.root, 0o700)
            os.chmod(self.issued_dir, 0o700)
        except OSError:
            pass

    # ------------------------------------------------------------------
    # Serial counter

    def next_serial(self) -> int:
        """Read-modify-write the serial counter. Not concurrency-
        safe — corvus-admin is expected to be run by one
        operator at a time."""

        try:
            current = int(self.serial_path.read_text().strip())
        except (FileNotFoundError, ValueError):
            current = 1
        self.serial_path.write_text(f"{current + 1}\n")
        return current

    # ------------------------------------------------------------------
    # Index

    def load_index(self) -> dict[str, IssuedRecord]:
        if not self.index_path.exists():
            return {}
        raw = json.loads(self.index_path.read_text() or "{}")
        return {k: IssuedRecord(**v) for k, v in raw.items()}

    def save_index(self, index: dict[str, IssuedRecord]) -> None:
        serialised = {k: asdict(v) for k, v in index.items()}
        text = json.dumps(serialised, indent=2, sort_keys=True) + "\n"
        # Atomic write: rename over the existing file.
        tmp = self.index_path.with_suffix(".json.tmp")
        tmp.write_text(text)
        os.replace(tmp, self.index_path)

    def record(self, rec: IssuedRecord) -> None:
        """Add or overwrite a single index entry. The CN is the
        primary key; redeploying / renewing a cert overwrites
        the previous row instead of growing a history."""

        idx = self.load_index()
        idx[rec.cn] = rec
        self.save_index(idx)

    def iter_records(self) -> Iterator[IssuedRecord]:
        yield from self.load_index().values()
