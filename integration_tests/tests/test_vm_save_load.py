"""Integration tests for `vm save` / `vm start`-resumes-saved.

`crv vm save` writes the VM's RAM image to disk via QEMU's external
migration (`migrate file:…`) and terminates QEMU. `crv vm start` on
a saved VM spawns a fresh QEMU with `-incoming "file:…"`, waits for
the restore to finish, and `cont`s the guest — same verb as a cold
boot. The state file lives at `<basePath>/<vmName>/state.qemu`.

These tests exercise the full round-trip on a real Alpine VM with
qemu-guest-agent — proving that RAM survives the save/restore cycle
by writing a sentinel through QGA, saving, restoring, and reading
the sentinel back. Behavioural coverage:

  * Round-trip preserves RAM (sentinel test).
  * State file appears under the conventional path after save.
  * `vm stop` refuses a saved VM; the file stays in place.
  * `vm reset` discards the state file and lands at stopped.
  * `vm migrate` refuses a saved VM with an actionable error.
  * Autostart resumes a saved VM after a daemon restart.

See `doc/vm-management.md` (the "Save / Resume" subsection) for
the operator-level reference.
"""

from __future__ import annotations

import shlex
import time

from corvus_test_harness import SingleNodeCase, Vm


def _saved_state_path(client, node_name: str, vm_name: str) -> str:
    """Reproduce the conventional saved-state path the node-agent uses.

    Mirrors `Corvus.Node.Runtime.getSavedStateFile` — the daemon
    publishes the per-node `basePath` via `nodes.get(name).show()`,
    so we can compute the same path the agent computes without
    parsing config files on the node. The `.zst` extension reflects
    the on-disk format: QEMU's `migrate "exec:zstd …"` writes a
    zstd-compressed migration stream.
    """
    base = client.nodes.get(node_name).show().base_path.rstrip("/")
    return f"{base}/{vm_name}/state.qemu.zst"


def _file_exists_on(node, path: str) -> bool:
    """SSH into the test node, return True iff `path` exists.

    Plain `test -f` returns non-zero when missing — we wrap with
    `|| echo MISSING` so the SSH call itself stays green and the
    presence/absence shows up in stdout.
    """
    r = node.run(f"test -f {shlex.quote(path)} && echo PRESENT || echo MISSING")
    return r.stdout.decode("utf-8", errors="replace").strip() == "PRESENT"


def _qemu_count(node, vm_name: str) -> int:
    """Count qemu-system processes whose argv mentions `vm_name`.

    Mirrors the helper in test_vm_migration.py. The `^qemu-system`
    anchor keeps the shell that wraps this pgrep call from
    matching its own argv.
    """
    r = node.run(
        f"pgrep -af '^qemu-system.*-name {vm_name},' || true",
        check=False,
        timeout_sec=10.0,
    )
    out = r.stdout.decode("utf-8", errors="replace").strip()
    return sum(1 for line in out.splitlines() if line.strip())


def _poll_until(cond, *, timeout_sec: float, msg: str, poll_sec: float = 0.5) -> None:
    deadline = time.monotonic() + timeout_sec
    while time.monotonic() < deadline:
        if cond():
            return
        time.sleep(poll_sec)
    raise AssertionError(f"{msg} (waited {timeout_sec}s)")


class TestVmSaveLoadRoundtrip(SingleNodeCase):
    """Round-trip a real Alpine VM through save/start and verify RAM
    survives. The other refusal/discard cases hang off this same
    fixture pattern to share the bake cost."""

    def test_save_then_start_preserves_ram(self):
        """The whole point of save: a value written to RAM survives
        a save → start cycle. Drop a sentinel into `/tmp/sentinel`
        via QGA (no fsync — RAM is the only thing we care about),
        save, restart, read the sentinel back."""
        with Vm(self) as vm:
            sentinel = f"corvus-save-{int(time.monotonic_ns())}"
            r = vm.cap.guest_exec(
                f"/bin/sh -c {shlex.quote(f'echo {sentinel} > /tmp/sentinel')}"
            )
            assert r.exit_code == 0, r

            state_path = _saved_state_path(self.client, self.node.short_name, vm.name)
            assert not _file_exists_on(self.node, state_path), (
                f"setup: state file pre-existed at {state_path}"
            )

            # Save. The cap blocks until the agent has finished the
            # migrate + quit dance, so as soon as save() returns
            # the status is committed and QEMU is gone.
            vm.cap.save()
            _poll_until(
                lambda: vm.cap.show().status == "saved",
                timeout_sec=10.0,
                msg="vm.show().status did not become 'saved'",
            )
            _poll_until(
                lambda: _qemu_count(self.node, vm.name) == 0,
                timeout_sec=10.0,
                msg=f"qemu for {vm.name!r} did not exit after save",
            )
            assert _file_exists_on(self.node, state_path), (
                f"save did not produce a state file at {state_path}"
            )

            # Resume via `start`. The agent spawns QEMU with
            # `-incoming "file:…"`, polls query-migrate, issues
            # cont, and unlinks the file. Wait for the inner
            # daemon's status sink to flip the row to 'running'.
            vm.cap.start(wait=True)
            _poll_until(
                lambda: vm.cap.show().status == "running",
                timeout_sec=60.0,
                msg="vm.show().status did not become 'running' after start (saved)",
            )
            _poll_until(
                lambda: _qemu_count(self.node, vm.name) == 1,
                timeout_sec=10.0,
                msg=f"qemu for {vm.name!r} did not respawn after start (saved)",
            )
            assert not _file_exists_on(self.node, state_path), (
                f"state file at {state_path} was not unlinked after successful resume"
            )

            r = vm.cap.guest_exec("/bin/cat /tmp/sentinel")
            assert r.exit_code == 0, r
            assert sentinel in r.stdout, (
                f"sentinel {sentinel!r} did not survive save→start round trip; "
                f"guest stdout={r.stdout!r}"
            )

    def test_stop_refuses_saved_vm(self):
        """`vm stop` on a saved VM must refuse — operators land at
        `vm reset` (discard) or `vm start` (resume), not at a
        silent half-state. The FSM rule emits a message naming
        both verbs; the daemon surfaces it as RespInvalidTransition
        which (today) flows back as `status=error` to the caller.
        Either shape — explicit exception or `error` status —
        proves the daemon refused; what we assert is that the
        VM stays saved and the file remains.
        """
        with Vm(self) as vm:
            vm.cap.save()
            _poll_until(
                lambda: vm.cap.show().status == "saved",
                timeout_sec=10.0,
                msg="vm.show().status did not become 'saved'",
            )
            state_path = _saved_state_path(self.client, self.node.short_name, vm.name)
            assert _file_exists_on(self.node, state_path)

            stop_resp = None
            stop_exc: Exception | None = None
            try:
                stop_resp = vm.cap.stop(wait=False)
            except Exception as e:
                stop_exc = e

            # The daemon must NOT report a successful transition
            # ("stopping" / "stopped"). It either raises (preferred
            # — the FSM rejection percolates as a typed exception)
            # or returns "error". Anything else means a transition
            # we explicitly disallowed silently happened.
            assert stop_resp not in {"stopped", "stopping"}, (
                f"vm.stop() on saved VM reported {stop_resp!r}; should refuse"
            )
            if stop_exc is not None:
                msg = str(stop_exc).lower()
                # Sanity: the rejection message should at least
                # mention 'saved' so operators know which state
                # they're stuck in.
                assert "saved" in msg, f"stop refusal didn't mention 'saved': {msg!r}"

            assert vm.cap.show().status == "saved", "status drifted after refused stop"
            assert _file_exists_on(self.node, state_path), (
                "saved-state file disappeared after a refused stop"
            )

    def test_reset_drops_state_file_and_lands_stopped(self):
        """`vm reset` is the discard verb. On a saved VM it asks
        the agent to unlink the state file, then flips the row to
        stopped. Idempotent: a missing file is still success."""
        with Vm(self) as vm:
            vm.cap.save()
            _poll_until(
                lambda: vm.cap.show().status == "saved",
                timeout_sec=10.0,
                msg="vm.show().status did not become 'saved'",
            )
            state_path = _saved_state_path(self.client, self.node.short_name, vm.name)
            assert _file_exists_on(self.node, state_path)

            vm.cap.reset()
            _poll_until(
                lambda: vm.cap.show().status == "stopped",
                timeout_sec=10.0,
                msg="vm.show().status did not become 'stopped' after reset",
            )
            assert not _file_exists_on(self.node, state_path), (
                "reset on saved VM did not unlink the state file"
            )

    def test_saved_state_file_is_zstd_compressed(self):
        """The saved-state file on disk is zstd-compressed, not raw
        QEMU migration stream. The agent's `qmpMigrate` issues
        `migrate "exec:zstd …"`, so the file's first 4 bytes must
        be the zstd frame magic (0x28 0xb5 0x2f 0xfd, little-endian).

        Cheap structural check — proves the URI switch from
        `file:` to `exec:zstd` actually reached QEMU. The full
        round-trip (sentinel survives save → start) in
        ``test_save_then_start_preserves_ram`` covers the
        decompression side.
        """
        with Vm(self) as vm:
            vm.cap.save()
            _poll_until(
                lambda: vm.cap.show().status == "saved",
                timeout_sec=10.0,
                msg="vm.show().status did not become 'saved'",
            )
            state_path = _saved_state_path(self.client, self.node.short_name, vm.name)
            # `xxd -p -l 4` keeps the output to 8 hex chars, no
            # newlines — easy to compare. Quote the path to defend
            # against operator base-paths with spaces (no agent
            # control over this from the test side).
            r = self.node.run(f"xxd -p -l 4 {shlex.quote(state_path)}")
            magic = r.stdout.decode("utf-8", errors="replace").strip()
            assert magic == "28b52ffd", (
                f"saved state file at {state_path} is not zstd-compressed: "
                f"first-4-bytes magic={magic!r} (expected '28b52ffd')"
            )


# Cross-host migration of saved state moved to
# integration_tests/tests/test_vm_migration.py::TestVmMigrationBootableGuest
# (alongside the other bootable-Alpine migrate tests). That class
# pre-stages the Alpine base image on both nodes so the per-test
# migration plan carries only the overlay + state file, dropping the
# wall-clock from ~5 min to ~2 min for the three saved-VM tests.
#
# Autostart coverage (cold boot + resume-from-saved) lives in
# integration_tests/tests/test_autostart.py.
