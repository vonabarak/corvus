"""Serial-console replay + capture against the inner daemon.

Ports `SerialConsoleIntegrationSpec` from the pre-capnp suite (see
`doc/integration-tests-pre-capnp.md:417-468`):
  - replay on reconnect (ring-buffer history is preserved)
  - capture while disconnected
  - reboot persistence (buffer survives guest `system_reset`)
  - buffer cleaned up on VM stop
  - rejection: non-headless VM
  - rejection: stopped VM

All four positive behaviours share one Alpine `Vm` boot — the daemon's
serial ring buffer is per-VM TVar state and the assertions are
sequential reads separated by tiny actions (close, qga-write, reset,
stop), so a single boot suffices.

The pre-capnp "RPC protocol upgrade" assertion is dropped: under
Cap'n Proto, `serialConsole` is a regular cap method returning
bidirectional byte sinks — there's no protocol upgrade to verify.

Uses the `SyncByteStream` wrapper around `serialConsole` (added in
`python/corvus_client/_sync/vm.py`).
"""
from __future__ import annotations

import secrets
import time

import pytest

from corvus_client import ServerError
from corvus_client.exceptions import GuestAgentError
from corvus_test_harness import SingleNodeCase, Vm


pytestmark = pytest.mark.slow

LOGIN_PROMPT = b"login:"


def _drain_until(stream, needle: bytes, timeout: float) -> bytes:
    """Read chunks until `needle` appears or `timeout` elapses.

    Returns the accumulated bytes. Raises `AssertionError` on timeout
    with the captured tail so failures are diagnosable.
    """
    deadline = time.monotonic() + timeout
    buf = bytearray()
    while time.monotonic() < deadline:
        chunk = stream.read(timeout=1.0)
        if chunk is None:
            break  # EOF — daemon closed the stream
        if chunk:
            buf.extend(chunk)
            if needle in buf:
                return bytes(buf)
    raise AssertionError(
        f"timed out waiting for {needle!r} after {timeout}s; "
        f"tail={bytes(buf[-512:])!r}"
    )


class TestSerialConsole(SingleNodeCase):
    """Five phases over one Alpine VM, plus two cheap negative tests."""

    def test_buffer_lifecycle(self):
        """Replay, capture-while-disconnected, reboot, and stop-cleanup.

        Five sequential phases inside a single `with Vm(self) as vm:`
        block. Boot cost is paid once; each phase is a small action
        and a fresh serial-console connection.
        """
        with Vm(self) as vm:
            # ── Phase A: initial connect — replay sees the boot output.
            with vm.cap.serial_console() as stream1:
                data = _drain_until(stream1, LOGIN_PROMPT, timeout=60.0)
                assert LOGIN_PROMPT in data

            # ── Phase B: write a marker via QGA with no client attached.
            # `Vm` defaults give us headless + qemu-guest-agent but no SSH
            # plumbing. `guest_exec` synchronously runs the command and
            # `echo … > /dev/ttyS0` lands in the same chardev the
            # daemon's ring buffer reads from.
            marker = f"SERIAL-MARKER-{secrets.token_hex(4)}"
            r = vm.cap.guest_exec(
                f"/bin/sh -c 'echo {marker} > /dev/ttyS0'"
            )
            assert r.exit_code == 0, r
            time.sleep(0.5)  # let the buffer absorb the bytes

            # ── Phase C: reconnect — replay must include the marker.
            with vm.cap.serial_console() as stream2:
                data = _drain_until(
                    stream2, marker.encode(), timeout=10.0
                )
                assert marker.encode() in data

            # ── Phase D: in-guest reboot survives.
            # `vm.cap.reset()` is a misleading name — it hard-kills
            # QEMU (see `Handlers/Vm.hs::handleVmReset:509-535`), so we
            # can't use it to test buffer survival across a reboot.
            # Issue `reboot -f` inside the guest instead: the kernel
            # restarts, QEMU stays alive, the daemon's buffer thread
            # keeps reading from the same serial chardev.
            # Flush first so the next "login:" we observe is from the
            # post-reboot kernel, not leftover replay.
            vm.cap.serial_console_flush()
            try:
                vm.cap.guest_exec("/sbin/reboot -f")
            except ServerError:
                # `reboot -f` kills init while QGA is mid-exec; the
                # QGA→daemon communication can timeout or error out.
                # That's expected — we only care about the side-effect.
                pass
            with vm.cap.serial_console() as stream3:
                data = _drain_until(stream3, LOGIN_PROMPT, timeout=120.0)
                assert LOGIN_PROMPT in data

            # ── Phase E: stop the VM; the buffer is torn down.
            # `handleVmStop` removes the buffer handle from
            # `ssSerialBuffers`, so a subsequent `serial_console` call
            # raises `GuestAgentError("Serial console buffer not
            # available")` — the cap method in `Rpc/Vm.hs::vm'serialConsole`
            # only consults the buffer map and emits this single
            # error for any non-running or non-headless VM.
            vm.cap.stop(wait=True)
            with pytest.raises(GuestAgentError) as excinfo:
                vm.cap.serial_console()
            assert "buffer not available" in str(excinfo.value)

    def test_rejects_non_headless_vm(self):
        """Daemon refuses serial console for graphical VMs.

        Non-headless VMs get a SPICE port and no serial buffer thread
        (see `Handlers/Vm.hs:386` — the buffer thread is started only
        for headless VMs). `vm'serialConsole` finds no buffer in
        `ssSerialBuffers` and raises "Serial console buffer not
        available", surfaced client-side as `GuestAgentError`.
        """

        class _GraphicalVm(Vm):
            headless = False
            guest_agent = True
            wait_for_qga = True  # gate __enter__ on QGA so the VM is up

        with _GraphicalVm(self) as vm:
            with pytest.raises(GuestAgentError) as excinfo:
                vm.cap.serial_console()
            assert "buffer not available" in str(excinfo.value)

    def test_rejects_stopped_vm(self):
        """Daemon refuses serial console for stopped VMs.

        A bare VM record with no QEMU process has no serial buffer in
        `ssSerialBuffers` — `vm'serialConsole` raises "Serial console
        buffer not available", surfaced as `GuestAgentError`.
        """
        name = f"corvus-it-serial-stopped-{secrets.token_hex(4)}"
        vm_cap = self.client.vms.create(
            name,
            cpu_count=1,
            ram_mb=512,
            headless=True,
            guest_agent=False,
            cloud_init=False,
        )
        try:
            with pytest.raises(GuestAgentError) as excinfo:
                vm_cap.serial_console()
            assert "buffer not available" in str(excinfo.value)
        finally:
            vm_cap.delete(delete_disks=False)
