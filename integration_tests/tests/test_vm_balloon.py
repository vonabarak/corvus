"""Verify that the VirtIO balloon changes guest memory and reported VM stats."""

from __future__ import annotations

import time

import pytest
from corvus_client import (
    BalloonDeviceNotEnabled,
    BalloonDriverNotReady,
    InvalidBalloonTarget,
)
from corvus_test_harness import SingleNodeCase, VmSsh


def _available_kb(vm: VmSsh) -> int:
    return int(vm.run("awk '/^MemAvailable:/{print $2}' /proc/meminfo").stdout.strip())


def _balloon_actual_bytes(vm: VmSsh) -> int | None:
    stats = vm.cap.show().stats
    return stats.balloon_actual_bytes if stats is not None else None


class _VmWithoutBalloon(VmSsh):
    balloon = False


class TestVmBalloon(SingleNodeCase):
    @pytest.fixture(scope="class", autouse=True)
    @classmethod
    def _install_client_certs(cls, _class_topology: object) -> None:
        cls().install_node_client_certs()

    def _assert_cli_error(self, vm: VmSsh, code: str, target: str = "768M") -> None:
        import json

        result = self.node.run(
            f"/opt/corvus/bin/crv -o json vm balloon {vm.cap.show().id} {target}",
            check=False,
        )
        assert result.returncode != 0
        payload = json.loads(result.stdout)
        assert payload["code"] == code, payload

    def test_balloon_rejects_vm_without_device(self) -> None:
        with _VmWithoutBalloon(self) as vm:
            assert not vm.cap.show().balloon
            assert (
                vm.run(
                    "grep -l '^0x0005$' /sys/bus/virtio/devices/*/device", check=False
                ).exit_code
                == 1
            )
            with pytest.raises(BalloonDeviceNotEnabled):
                vm.cap.set_balloon(target_bytes=768 * 1024**2)
            self._assert_cli_error(vm, "balloon_device_not_enabled")
            assert vm.cap.show().status == "running"
            assert _available_kb(vm) > 0

    def test_balloon_rejects_vm_without_guest_driver(self) -> None:
        with VmSsh(self) as vm:
            assert vm.cap.show().balloon
            vm.run("grep -l '^0x0005$' /sys/bus/virtio/devices/*/device")
            # Remove the driver entirely, rather than merely stopping a
            # userspace daemon: ballooning is implemented by the kernel.
            result = vm.cap.guest_exec("modprobe -r virtio_balloon")
            assert result.exit_code == 0, result.stderr
            try:
                vm.run("test ! -d /sys/bus/virtio/drivers/virtio_balloon")
                before = _available_kb(vm)
                with pytest.raises(BalloonDriverNotReady):
                    vm.cap.set_balloon(target_bytes=768 * 1024**2)
                self._assert_cli_error(vm, "balloon_driver_not_ready")
                assert vm.cap.show().status == "running"
                assert _available_kb(vm) > before - 128 * 1024
            finally:
                result = vm.cap.guest_exec("modprobe virtio_balloon")
                assert result.exit_code == 0, result.stderr
            vm.run("test -d /sys/bus/virtio/drivers/virtio_balloon")
            # The same capability must remain usable after the error.
            vm.cap.set_balloon(target_bytes=1024 * 1024**2)

    def test_balloon_reclaims_and_restores_guest_memory(self) -> None:
        with VmSsh(self) as vm:
            details = vm.cap.show()
            assert details.balloon
            before = _available_kb(vm)
            with pytest.raises(InvalidBalloonTarget):
                vm.cap.set_balloon(target_bytes=1025 * 1024**2)
            self._assert_cli_error(vm, "invalid_balloon_target", "1025M")
            assert vm.cap.show().ram == 1073741824

            try:
                result = self.node.run(
                    f"/opt/corvus/bin/crv vm balloon {details.id} 768M", check=False
                )
                assert result.returncode == 0, result.stderr.decode(errors="replace")
                vm.cap.set_balloon(target_bytes=768 * 1024**2)
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
                vm.cap.set_balloon(target_bytes=1024 * 1024 * 1024)

            tasks = self.client.tasks.list(subsystem="vm", result="success", limit=50)
            assert any(
                t.command == "balloon" and t.entity and t.entity.id == details.id
                for t in tasks
            )
            assert vm.cap.show().ram == details.ram

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
