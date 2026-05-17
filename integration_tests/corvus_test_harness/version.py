"""Session-start sanity checks for the outer environment.

Two things must hold before any integration test is meaningful:

1. The outer Corvus daemon is recent enough that the harness's `crv`
   calls match what the daemon understands. Right now we just verify
   that `crv status` succeeds and surfaces the daemon's version; a
   strict minimum-version gate is left as a TODO until the version
   string semantics stabilise.

2. The host kernel has nested virtualization enabled — without it,
   QEMU-in-QEMU is unusably slow at best and breaks at worst. The
   check is a one-shot file read of
   `/sys/module/kvm_{intel,amd}/parameters/nested`.
"""
from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path
from typing import Optional

from .outer import Crv, CrvError


@dataclass(frozen=True)
class NestedKvmStatus:
    available: bool
    vendor: Optional[str]  # 'intel' or 'amd'
    reason: Optional[str]  # human-readable explanation when unavailable


def check_nested_kvm() -> NestedKvmStatus:
    """Inspect /sys/module/kvm_*/parameters/nested.

    On most kernels this is world-readable and we can confirm nested
    is on. On hardened kernels the parameter file is 0400 (root-only);
    we treat that as "presumed available" if the module is loaded —
    the actual boot will fail loudly if it isn't, but we don't want
    sysfs permissions to block the entire suite.
    """
    permission_denied = False
    for vendor in ("intel", "amd"):
        module_dir = Path(f"/sys/module/kvm_{vendor}")
        path = module_dir / "parameters" / "nested"
        try:
            value = path.read_text().strip()
        except FileNotFoundError:
            # Module not loaded for this vendor; try the other.
            continue
        except PermissionError:
            # Some hardened kernels chmod 0400 on /sys/module/kvm_*.
            # We can't know definitively from userspace; remember and
            # try the other vendor first.
            permission_denied = True
            continue
        except OSError as e:
            return NestedKvmStatus(False, vendor, f"could not read {path}: {e}")
        if value in ("Y", "1", "y"):
            return NestedKvmStatus(True, vendor, None)
        return NestedKvmStatus(
            False,
            vendor,
            f"kvm_{vendor}.nested={value!r}; reload module with `nested=1`",
        )
    if permission_denied:
        # KVM module present (we got PermissionError, not FileNotFound)
        # but we can't read its parameter. The boot will fail clearly
        # if nested is actually off; don't block the suite over sysfs.
        return NestedKvmStatus(
            True,
            None,
            "/sys/module/kvm_* not readable as $USER — presuming nested KVM is on",
        )
    return NestedKvmStatus(
        False,
        None,
        "no kvm_intel or kvm_amd module loaded; KVM unavailable on this host",
    )


def check_outer_version(crv: Crv) -> dict:
    """Confirm the outer daemon is reachable and capture its version.

    Returns the parsed status envelope on success. Raises a
    `RuntimeError` with a clear, single-line message on failure —
    pytest's session-startup error reporting then surfaces that to
    the developer.
    """
    try:
        info = crv.status()
    except CrvError as e:
        if e.kind in ("connection_error", "connect"):
            raise RuntimeError(
                "outer Corvus daemon is unreachable. "
                "Check that the corvus systemd service is running, "
                "or set $CORVUS_SOCKET to a different daemon."
            ) from None
        raise RuntimeError(
            f"`crv status` failed: {e.message or e.stderr.strip()}"
        ) from None
    version = info.get("version") or info.get("info", {}).get("version")
    if not version:
        raise RuntimeError(
            "outer Corvus `status` envelope had no version field. "
            f"got: {info!r}"
        )
    return info
