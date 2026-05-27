"""HMP monitor replay + flush + rejection against the inner daemon.

Mirrors [test_serial_console.py](integration_tests/tests/test_serial_console.py)
for the HMP (Human Monitor Protocol) byte stream documented in
[doc/vm-management.md](doc/vm-management.md): a 64 KiB ring buffer
that replays on reconnect, can be flushed, and rejects attach
attempts against stopped VMs.

The HMP cap method is symmetric to ``serialConsole``:
``vm.hmpMonitor`` returns a bidirectional ``ByteSink``; writes are
forwarded to QEMU's monitor and the daemon tees QEMU's stdout
into the ring buffer.  The whole machinery is exercised at the
process level (`crv vm monitor`) but never asserted programmatically
— this file closes that gap.

One Alpine VM boot covers every positive phase (replay, write/read
round-trip, flush, headless+non-headless attach).  Each negative
case (stopped, missing VM) uses a tiny stopped-only VM.
"""

from __future__ import annotations

import secrets
import time

import pytest
from corvus_client.exceptions import VmNotFound, VmRunning
from corvus_test_harness import SingleNodeCase, Vm


def _drain_until(stream, needle: bytes, *, timeout: float) -> bytes:
    """Read chunks until ``needle`` appears or ``timeout`` elapses.

    Mirrors the same-named helper in
    [test_serial_console.py](integration_tests/tests/test_serial_console.py)
    — no harness promotion to keep the two test files independent
    (per the plan)."""
    deadline = time.monotonic() + timeout
    buf = bytearray()
    while time.monotonic() < deadline:
        chunk = stream.read(timeout=1.0)
        if chunk is None:
            break
        if chunk:
            buf.extend(chunk)
            if needle in buf:
                return bytes(buf)
    raise AssertionError(
        f"timed out waiting for {needle!r} after {timeout}s; tail={bytes(buf[-512:])!r}"
    )


class TestHmpMonitor(SingleNodeCase):
    """HMP monitor ring buffer + flush + rejection paths."""

    def test_hmp_lifecycle(self):
        """Replay-on-reconnect + write/read round-trip + flush.

        Single boot, multiple HMP attach/detach cycles. The HMP
        banner (``QEMU <version> monitor`` or ``(qemu)`` prompt)
        is the most stable replay marker — both have stayed
        stable across QEMU 6.x → 9.x.
        """
        with Vm(self) as vm:
            # ── Phase A: first connect. QEMU has been running
            # since `vm.start` returned; the daemon's ring buffer
            # holds whatever QEMU has emitted so far. The `(qemu)`
            # prompt shows up after init and is enduringly stable
            # across releases.
            with vm.cap.hmp_monitor() as stream1:
                _drain_until(stream1, b"(qemu)", timeout=10.0)
                # Issue a cheap command and read the response.
                stream1.write(b"info status\n")
                data = _drain_until(stream1, b"VM status", timeout=10.0)
                assert b"running" in data, data

            # ── Phase B: reconnect — ring buffer replays. The
            # previous `info status` request + response must be
            # in the replay because the daemon doesn't trim past
            # a disconnect.
            with vm.cap.hmp_monitor() as stream2:
                data = _drain_until(stream2, b"info status", timeout=10.0)
                assert b"info status" in data, data

            # ── Phase C: flush, then reconnect. Buffer must be
            # empty of the historical command. We DO expect the
            # next prompt from QEMU (which will arrive as soon as
            # the daemon's buffer thread reads from QEMU), so we
            # search only the bytes that arrived in a short read
            # window.
            vm.cap.hmp_monitor_flush()
            with vm.cap.hmp_monitor() as stream3:
                early = stream3.read(timeout=2.0) or b""
                assert b"info status" not in early, early

    def test_hmp_rejected_on_stopped_vm(self):
        """Attaching HMP to a stopped VM raises with the
        documented "not running" daemon message — the cap method
        gates on VM status before reaching the buffer map (see
        `Handlers/Vm.hs::handleHmpMonitor`)."""
        name = f"corvus-it-hmp-stopped-{secrets.token_hex(3)}"
        vm = self.client.vms.create(
            name,
            cpu_count=1,
            ram_mb=256,
            headless=True,
        )
        try:
            with pytest.raises(VmRunning) as excinfo:
                vm.hmp_monitor()
            msg = str(excinfo.value)
            assert "not running" in msg, msg
            assert "stopped" in msg, msg
        finally:
            vm.delete(keep_disks=True)

    def test_hmp_works_on_non_headless_vm(self):
        """`crv vm monitor` is documented to work for every
        running VM regardless of `headless`. Verify the
        non-headless branch reaches the same buffer path the
        headless smoke covers."""

        class _GraphicalVm(Vm):
            headless = False
            guest_agent = True
            wait_for_qga = True

        with _GraphicalVm(self) as vm:
            with vm.cap.hmp_monitor() as stream:
                _drain_until(stream, b"(qemu)", timeout=10.0)

    def test_hmp_get_on_missing_vm(self):
        """Resolving a non-existent VM by name surfaces as
        :class:`VmNotFound`. This is upstream of the HMP path
        but documents the failure mode end users hit when they
        typo a name to `crv vm monitor`."""
        with pytest.raises(VmNotFound):
            # 16-hex-char token chosen so it's astronomically
            # unlikely to alias any in-flight test VM.
            self.client.vms.get(
                f"corvus-it-does-not-exist-{secrets.token_hex(8)}",
                by_name=True,
            )
