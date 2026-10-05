"""Verify that the VirtIO balloon changes guest memory and reported VM stats."""

from __future__ import annotations

import shlex
import time

from corvus_test_harness import SingleNodeCase, VmSsh


def _available_kb(vm: VmSsh) -> int:
    return int(vm.run("awk '/^MemAvailable:/{print $2}' /proc/meminfo").stdout.strip())


def _balloon_actual_bytes(vm: VmSsh) -> int | None:
    stats = vm.cap.show().stats
    return stats.balloon_actual_bytes if stats is not None else None


def _set_balloon(vm: VmSsh, mb: int) -> None:
    # Drive QEMU's QMP Unix socket from the node. This exercises the live
    # balloon device without involving the client's bidirectional HMP stream.
    script = f"""import socket, sys
s = socket.socket(socket.AF_UNIX)
s.settimeout(10)
s.connect(sys.argv[1])
f = s.makefile('rwb', buffering=0)
f.readline()  # QMP greeting
f.write(b'{{"execute":"qmp_capabilities"}}\\n')
f.readline()
f.write(b'{{"execute":"balloon","arguments":{{"value":{mb * 1024 * 1024}}}}}\\n')
reply = f.readline()
assert b'"return"' in reply, reply
"""
    qmp_socket = vm.cap.show().monitor_socket.replace("monitor.sock", "qmp.sock")
    result = vm.case.nodes[0].run(
        f"python3 -c {shlex.quote(script)} {shlex.quote(qmp_socket)}",
        check=False,
    )
    assert result.returncode == 0, result.stderr.decode(errors="replace")


class TestVmBalloon(SingleNodeCase):
    def test_balloon_reclaims_and_restores_guest_memory(self) -> None:
        with VmSsh(self) as vm:
            details = vm.cap.show()
            assert details.balloon
            before = _available_kb(vm)

            try:
                _set_balloon(vm, 768)
                deadline = time.monotonic() + 60
                while time.monotonic() < deadline:
                    actual = _balloon_actual_bytes(vm)
                    available = _available_kb(vm)
                    if (
                        actual is not None
                        and actual <= 800 * 1024 * 1024
                        and available < before - 128 * 1024
                    ):
                        break
                    time.sleep(2)
                else:
                    raise AssertionError(
                        f"balloon did not reclaim guest memory: "
                        f"actual={actual}, available={available}, before={before}"
                    )
            finally:
                _set_balloon(vm, 1024)

            deadline = time.monotonic() + 60
            while time.monotonic() < deadline:
                actual = _balloon_actual_bytes(vm)
                available = _available_kb(vm)
                if (
                    actual is not None
                    and actual >= 1000 * 1024 * 1024
                    and available > before - 64 * 1024
                ):
                    break
                time.sleep(2)
            else:
                raise AssertionError(
                    f"balloon did not restore guest memory: "
                    f"actual={actual}, available={available}, before={before}"
                )
