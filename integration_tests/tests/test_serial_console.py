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

import os
import pty
import secrets
import select
import subprocess
import time

import pytest
from corvus_client import ServerError
from corvus_client.exceptions import VmRunning
from corvus_test_harness import SingleNodeCase, Vm

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
        f"timed out waiting for {needle!r} after {timeout}s; tail={bytes(buf[-512:])!r}"
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
            r = vm.cap.guest_exec(f"/bin/sh -c 'echo {marker} > /dev/ttyS0'")
            assert r.exit_code == 0, r
            time.sleep(0.5)  # let the buffer absorb the bytes

            # ── Phase C: reconnect — replay must include the marker.
            with vm.cap.serial_console() as stream2:
                data = _drain_until(stream2, marker.encode(), timeout=10.0)
                assert marker.encode() in data

            # ── Phase D: flush actually empties the ring buffer.
            # `serial_console_flush` is the same daemon call that
            # `Ctrl+] f` triggers from `crv vm view`. After flushing,
            # a fresh reconnect must not see the Phase-B marker —
            # otherwise the keybinding has no observable effect.
            vm.cap.serial_console_flush()
            with vm.cap.serial_console() as stream_post_flush:
                early = stream_post_flush.read(timeout=2.0) or b""
                assert marker.encode() not in early, early

            # ── Phase E: in-guest reboot survives.
            # `vm.cap.reset()` is misleadingly named — it hard-kills
            # QEMU (see `Handlers/Vm.hs::handleVmReset:509-535`), so
            # it can't prove the buffer survives a real reboot.
            # Issue `reboot -f` inside the guest: kernel restarts,
            # QEMU stays alive, the daemon's buffer thread keeps
            # reading from the same serial chardev.
            try:
                vm.cap.guest_exec("/sbin/reboot -f")
            except ServerError:
                # `reboot -f` kills init while QGA is mid-exec; the
                # QGA→daemon path may timeout. We only care about
                # the side-effect.
                pass
            with vm.cap.serial_console() as stream3:
                data = _drain_until(stream3, LOGIN_PROMPT, timeout=120.0)
                assert LOGIN_PROMPT in data

            # ── Phase F: stop the VM; the cap rejects with
            # "VM is not running (status: stopped)" — the rich
            # message restored by routing `vm'serialConsole` through
            # `handleSerialConsole`.
            vm.cap.stop(wait=True)
            with pytest.raises(VmRunning) as excinfo:
                vm.cap.serial_console()
            msg = str(excinfo.value)
            assert "not running" in msg, msg
            assert "stopped" in msg, msg

    def test_rejects_non_headless_vm(self):
        """Daemon refuses serial console for graphical VMs.

        The cap method routes through `handleSerialConsole`, whose
        headlessness check emits "VM is not headless — use SPICE
        viewer instead" before the buffer lookup. The Python client
        classifies the message as `VmRunning` (see exceptions table).
        """

        class _GraphicalVm(Vm):
            headless = False
            guest_agent = True
            wait_for_qga = True  # gate __enter__ on QGA so the VM is up

        with _GraphicalVm(self) as vm:
            with pytest.raises(VmRunning) as excinfo:
                vm.cap.serial_console()
            assert "not headless" in str(excinfo.value)

    def test_rejects_stopped_vm(self):
        """Daemon refuses serial console for stopped VMs.

        `handleSerialConsole` rejects on status before reaching the
        buffer map — error reads "VM is not running (status: stopped)".
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
            with pytest.raises(VmRunning) as excinfo:
                vm_cap.serial_console()
            msg = str(excinfo.value)
            assert "not running" in msg, msg
            assert "stopped" in msg, msg
        finally:
            vm_cap.delete(keep_disks=True)

    def test_view_session_banner(self):
        """`crv vm view` prints connection banner + keybinding help.

        Pure client-side stdout; only an end-to-end subprocess test
        observes it. Drive the host `crv` binary against the outer
        daemon's orchestrator-node VM (a real headless VM that lives
        for the duration of the test class). Attach for ~2 s, send
        `Ctrl+] q` to exit cleanly, assert banner text.
        """
        crv = self.topology.crv.binary
        node_name = self.topology._nodes[0].name
        master, slave = pty.openpty()
        proc = subprocess.Popen(
            [crv, "vm", "view", node_name],
            stdin=slave,
            stdout=slave,
            stderr=slave,
            close_fds=True,
        )
        os.close(slave)
        captured = bytearray()
        deadline = time.monotonic() + 5.0
        try:
            # Pull stdout until we see the banner (or timeout).
            while time.monotonic() < deadline:
                rl, _, _ = select.select([master], [], [], 0.2)
                if not rl:
                    continue
                try:
                    chunk = os.read(master, 4096)
                except OSError:
                    break
                if not chunk:
                    break
                captured.extend(chunk)
                if b"f=flush" in captured:
                    break
            # Ctrl+] q exits the raw-mode loop cleanly.
            try:
                os.write(master, b"\x1dq")
            except OSError:
                pass
            try:
                proc.wait(timeout=10)
            except subprocess.TimeoutExpired:
                proc.kill()
                proc.wait(timeout=5)
        finally:
            try:
                os.close(master)
            except OSError:
                pass
            if proc.poll() is None:
                proc.kill()
                proc.wait(timeout=5)

        text = captured.decode("utf-8", errors="replace")
        assert "serial console" in text, text
        assert "Escape: Ctrl+]" in text, text
        assert "q=quit" in text, text
        assert "d=Ctrl+Alt+Del" in text, text
        assert "f=flush" in text, text
        assert "?=help" in text, text

    def test_serial_console_login(self):
        """Log in as `corvus` over the serial console.

        Uses the test image's password-based login (corvus:corvus,
        baked at `yaml/alpine-test/alpine-test.yml:179` via
        `chpasswd`). Sends username + password through the
        bidirectional `serial_console()` stream, then issues
        `whoami` and reads the response from the same stream.
        Image-agnostic w.r.t. cloud-init — the standard `Vm`
        Alpine image is enough.
        """
        with Vm(self) as vm, vm.cap.serial_console() as stream:
            # 1. Wait for the getty `login:` prompt. The boot
            #    output may have already scrolled past it — the
            #    ring buffer replay carries it.
            _drain_until(stream, b"login:", timeout=60.0)
            # 2. Send the username. CR is what real terminals
            #    send for ENTER.
            stream.write(b"corvus\r")
            _drain_until(stream, b"Password:", timeout=10.0)
            # 3. Password. Local echo is off so we don't expect
            #    the bytes back; we expect a shell prompt next.
            stream.write(b"corvus\r")
            # 4. Shell prompt. Alpine /bin/sh prints `$ ` for an
            #    unprivileged user. If the password was wrong
            #    we'd see `Login incorrect` and a fresh `login:`.
            data = _drain_until(stream, b"$ ", timeout=15.0)
            assert b"Login incorrect" not in data, (
                f"login rejected; tail={data[-256:]!r}"
            )
            # 5. Smoke: `whoami` echoes through the shell.
            stream.write(b"whoami\r")
            data = _drain_until(stream, b"corvus", timeout=10.0)
            assert b"corvus" in data
