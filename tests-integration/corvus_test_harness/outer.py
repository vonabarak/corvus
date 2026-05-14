"""`crv` CLI wrapper used to drive the outer Corvus daemon.

The test harness deliberately never opens a pycapnp connection to the
outer daemon: outer and inner Corvus versions may differ (the inner is
freshly compiled from this source tree; the outer is whatever the
developer has installed). Talking to the outer through its versioned
CLI insulates the tests from outer-side protocol drift.

Every method returns parsed JSON (`crv -o json …`) so callers get
Python dicts/lists, not text to scrape. On non-zero exit, the wrapped
`CrvError` carries the JSON error envelope (if any) plus stderr.
"""
from __future__ import annotations

import json
import os
import shlex
import shutil
import subprocess
from dataclasses import dataclass
from pathlib import Path
from typing import Any, Optional, Sequence


class CrvError(RuntimeError):
    """A `crv` invocation exited non-zero.

    Attributes:
      returncode:  the exit status.
      kind:        the daemon's error kind if the JSON envelope had one
                   (e.g. "vm_not_found", "connection_error"); else None.
      message:     the daemon's error message; falls back to stderr.
      stdout:      raw stdout from the failing invocation.
      stderr:      raw stderr from the failing invocation.
      args:        the argv that was executed.
    """

    def __init__(
        self,
        args: Sequence[str],
        returncode: int,
        stdout: str,
        stderr: str,
    ) -> None:
        self.args = list(args)
        self.returncode = returncode
        self.stdout = stdout
        self.stderr = stderr
        self.kind: Optional[str] = None
        self.message: Optional[str] = None
        # Try to parse a JSON error envelope from stdout (crv -o json
        # writes the envelope there even on error).
        try:
            envelope = json.loads(stdout) if stdout.strip() else None
        except json.JSONDecodeError:
            envelope = None
        if isinstance(envelope, dict) and envelope.get("status") == "error":
            self.kind = envelope.get("error") or envelope.get("kind")
            self.message = envelope.get("message")
        msg = self.message or stderr.strip() or f"crv exited {returncode}"
        super().__init__(f"{msg} (argv: {' '.join(shlex.quote(a) for a in args)})")


@dataclass
class Crv:
    """Synchronous `crv` CLI driver.

    Construct with the path to the `crv` binary (defaults to the
    user-installed one under ~/.local/bin; system /usr/bin is a
    fallback, but harness session-start probably wants to assert the
    user binary is in use). Optional `unix_socket` overrides which
    daemon `crv` talks to — useful when running multiple outer
    daemons side-by-side for harness self-tests.
    """

    binary: str
    unix_socket: Optional[str] = None
    extra_env: Optional[dict[str, str]] = None
    # Default subprocess timeout for "quick" calls; long-running ones
    # (build/apply with --wait) pass their own timeout explicitly.
    default_timeout_sec: float = 60.0

    @classmethod
    def autodetect(cls) -> "Crv":
        """Find a usable `crv` binary on PATH and the current user's
        local install. Prefers ~/.local/bin/crv (the binary built from
        this dev tree) over any system install."""
        local = Path.home() / ".local" / "bin" / "crv"
        if local.is_file() and os.access(local, os.X_OK):
            return cls(binary=str(local))
        found = shutil.which("crv")
        if not found:
            raise RuntimeError("`crv` not found on PATH or in ~/.local/bin (run `make install`)")
        return cls(binary=found)

    # ---- raw subprocess machinery ----------------------------------------

    def _argv(self, *extra: str) -> list[str]:
        argv = [self.binary]
        if self.unix_socket:
            argv += ["--socket", self.unix_socket]
        argv.append("-o")
        argv.append("json")
        argv.extend(extra)
        return argv

    def run(
        self,
        *args: str,
        timeout: Optional[float] = None,
        check: bool = True,
        input_bytes: Optional[bytes] = None,
    ) -> Any:
        """Run `crv -o json <args>` and return the parsed JSON payload.

        On non-zero exit (when `check=True`), raises `CrvError`. The
        returned payload is whatever the daemon emitted: a dict for
        show/status envelopes, a list for `vm list`/`disk list`/etc.
        Callers that need a specific shape coerce it locally.
        """
        argv = self._argv(*args)
        env = os.environ.copy()
        if self.extra_env:
            env.update(self.extra_env)
        proc = subprocess.run(
            argv,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            input=input_bytes,
            env=env,
            timeout=timeout or self.default_timeout_sec,
        )
        stdout = proc.stdout.decode("utf-8", errors="replace")
        stderr = proc.stderr.decode("utf-8", errors="replace")
        if proc.returncode != 0:
            if check:
                raise CrvError(argv, proc.returncode, stdout, stderr)
            return {"status": "error", "stderr": stderr, "returncode": proc.returncode}
        # Some commands (`apply --wait`) emit progress events one-per-line
        # before the final envelope. We want the last well-formed JSON
        # object from stdout.
        return _last_json_object(stdout)

    # ---- daemon-level ----------------------------------------------------

    def status(self) -> dict[str, Any]:
        return self.run("status")

    def ping(self) -> None:
        self.run("ping")

    # ---- vm subsystem ----------------------------------------------------

    def vm_list(self) -> list[dict[str, Any]]:
        return _as_list(self.run("vm", "list"), key="vms")

    def vm_show(self, name_or_id: str | int) -> dict[str, Any]:
        return self.run("vm", "show", str(name_or_id))

    def vm_create(
        self,
        name: str,
        *,
        cpu_count: int = 1,
        ram_mb: int = 1024,
        description: Optional[str] = None,
        headless: bool = False,
        guest_agent: bool = False,
        cloud_init: bool = False,
        autostart: bool = False,
    ) -> dict[str, Any]:
        args = ["vm", "create", name, "-c", str(cpu_count), "-m", str(ram_mb)]
        if description is not None:
            args += ["-d", description]
        if headless:
            args.append("--headless")
        if guest_agent:
            args.append("--guest-agent")
        if cloud_init:
            args.append("--cloud-init")
        if autostart:
            args.append("--autostart")
        return self.run(*args)

    def vm_start(self, name_or_id: str | int, *, wait: bool = False) -> dict[str, Any]:
        args = ["vm", "start", str(name_or_id)]
        if wait:
            args.append("--wait")
        # Boots with cloud-init can comfortably take 90s on a cold cache.
        return self.run(*args, timeout=300)

    def vm_stop(
        self,
        name_or_id: str | int,
        *,
        wait: bool = False,
        timeout_sec: Optional[int] = None,
    ) -> dict[str, Any]:
        # `crv vm stop` is graceful only — there's no `--force` (that
        # option only exists on `network stop`). For uncooperative
        # guests, lower `timeout_sec` so the daemon's own watchdog
        # SIGKILLs QEMU once the ACPI shutdown deadline elapses.
        args = ["vm", "stop", str(name_or_id)]
        if wait:
            args.append("--wait")
        if timeout_sec is not None:
            args += ["--timeout", str(timeout_sec)]
        return self.run(*args, timeout=180)

    def vm_delete(self, name_or_id: str | int, *, delete_disks: bool = False) -> dict[str, Any]:
        args = ["vm", "delete", str(name_or_id)]
        if delete_disks:
            args.append("--delete-disks")
        return self.run(*args, timeout=120)

    def vm_exec(
        self,
        name_or_id: str | int,
        command: str,
        *,
        timeout_sec: float = 30.0,
    ) -> dict[str, Any]:
        """Run a shell command inside the VM via the QEMU guest agent.

        Returns the parsed envelope: `{exit_code, stdout, stderr}` (the
        daemon's GuestExecResult shape). Used by tests to drive in-guest
        one-shots (mounting virtiofs shares, probing systemd units) that
        the inner Corvus daemon itself doesn't expose.
        """
        return self.run("vm", "exec", str(name_or_id), command, timeout=timeout_sec)

    # ---- shared-dir ------------------------------------------------------

    def shared_dir_add(
        self,
        vm: str | int,
        path: str,
        tag: str,
        *,
        cache: Optional[str] = None,
        read_only: bool = False,
    ) -> dict[str, Any]:
        args = ["shared-dir", "add", str(vm), str(path), tag]
        if cache is not None:
            args += ["--cache", cache]
        if read_only:
            args.append("--read-only")
        return self.run(*args)

    def shared_dir_list(self, vm: str | int) -> list[dict[str, Any]]:
        """List a VM's shared directories.

        `crv vm show` deliberately omits sharedDirs (the protocol-side
        VmDetails record doesn't carry them), so this is the canonical
        way to inspect them.
        """
        return _as_list(self.run("shared-dir", "list", str(vm)), key="sharedDirs")

    # ---- template / disk -------------------------------------------------

    def template_list(self) -> list[dict[str, Any]]:
        return _as_list(self.run("template", "list"), key="templates")

    def disk_list(self) -> list[dict[str, Any]]:
        return _as_list(self.run("disk", "list"), key="disks")

    def disk_show(self, name_or_id: str | int) -> dict[str, Any]:
        return self.run("disk", "show", str(name_or_id))

    # ---- apply / build ---------------------------------------------------

    def apply(
        self,
        yaml_path: str | Path,
        *,
        skip_existing: bool = True,
        wait: bool = True,
        timeout_sec: float = 3600.0,
    ) -> dict[str, Any]:
        args = ["apply", str(yaml_path)]
        if skip_existing:
            args.append("--skip-existing")
        if wait:
            args.append("--wait")
        return self.run(*args, timeout=timeout_sec)

    def build(
        self,
        yaml_path: str | Path,
        *,
        wait: bool = True,
        timeout_sec: float = 7200.0,
    ) -> dict[str, Any]:
        args = ["build", str(yaml_path)]
        if wait:
            args.append("--wait")
        return self.run(*args, timeout=timeout_sec)


def _last_json_object(s: str) -> Any:
    """Parse the last well-formed JSON value printed in `s`.

    The `crv apply --wait` and `crv build --wait` paths interleave
    streaming events (one JSON object per line) with the final
    envelope. Non-streaming commands like `crv vm list` emit a single
    JSON value at the top level — which may be an object (envelope)
    or an array (list-of-items).
    """
    s = s.strip()
    if not s:
        return {}
    # Cheap fast path: a single top-level JSON value.
    try:
        return json.loads(s)
    except json.JSONDecodeError:
        pass
    # Fallback: the last non-empty line that parses as JSON.
    for line in reversed(s.splitlines()):
        line = line.strip()
        if not line:
            continue
        try:
            return json.loads(line)
        except json.JSONDecodeError:
            continue
    return {"raw": s}


def _as_list(payload: Any, *, key: str) -> list[dict[str, Any]]:
    """Coerce a list command's response to a list-of-dicts.

    `crv -o json` for list commands currently returns a top-level
    JSON array; older daemon versions may instead return an envelope
    `{"<key>": [...], "status": "ok"}`. We accept either shape.
    """
    if isinstance(payload, list):
        return payload
    if isinstance(payload, dict):
        nested = payload.get(key)
        if isinstance(nested, list):
            return nested
        nested = payload.get("data")
        if isinstance(nested, list):
            return nested
    return []
