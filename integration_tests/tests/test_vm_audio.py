"""Boot each audio backend and verify its PCI card inside the guest."""

from __future__ import annotations

import secrets
from collections.abc import Iterator
from contextlib import contextmanager, suppress

import pytest
from corvus_client._sync.vm import SyncVm
from corvus_test_harness import SingleNodeCase


class TestVmAudio(SingleNodeCase):
    @contextmanager
    def _audio_server(self, backend: str) -> Iterator[None]:
        if backend == "spice":
            yield
            return

        pids: list[int] = []
        try:
            for command, socket, log in (
                ("pipewire", "/run/corvus/pipewire-0", "/tmp/corvus-it-pipewire.log"),
                (
                    "pipewire-pulse",
                    "/run/corvus/pulse/native",
                    "/tmp/corvus-it-pipewire-pulse.log",
                ),
            ):
                result = self.node.run(
                    f"nohup env XDG_RUNTIME_DIR=/run/corvus {command} "
                    f">{log} 2>&1 </dev/null & echo $!"
                )
                pids.append(int(result.stdout.decode().strip()))
                ready = self.node.run(
                    f"for i in $(seq 1 50); do test -S {socket} && exit 0; "
                    f"sleep 0.1; done; cat {log}; exit 1",
                    check=False,
                )
                assert ready.returncode == 0, (
                    f"{command} did not create {socket}:\n"
                    f"{ready.stdout.decode()}\n{ready.stderr.decode()}"
                )
            yield
        finally:
            for pid in reversed(pids):
                self.node.run(f"kill {pid}", check=False)

    @pytest.mark.parametrize(
        ("backend", "model", "pci_id"),
        [
            ("pipewire", "virtio-sound", "1af4:1059"),
            ("pulse", "virtio-sound", "1af4:1059"),
            ("spice", "virtio-sound", "1af4:1059"),
            ("spice", "ich9-intel-hda", "8086:293e"),
        ],
    )
    def test_audio_card_is_visible_in_guest(
        self, backend: str, model: str, pci_id: str
    ) -> None:
        qemu_backend = "pa" if backend == "pulse" else backend
        available = self.node.run("qemu-system-x86_64 -audiodev help").stdout.decode()
        assert qemu_backend in available.splitlines(), (
            f"QEMU lacks the {qemu_backend} audio driver; rebuild the test node "
            "with `make image-rebuild IMAGE=node`"
        )

        with self._audio_server(backend):
            images = self.register_base_images()
            name = f"corvus-it-audio-{backend}-{model}-{secrets.token_hex(3)}"
            self.client.disks.create_overlay(name, images["alpine"], ephemeral=True)
            vm: SyncVm | None = None
            try:
                vm = self.client.vms.create(
                    name,
                    cpu_count=2,
                    ram_mb=1024,
                    headless=backend != "spice",
                    guest_agent=True,
                    audio_devices=[(backend, "", model)],
                )
                vm.attach_disk(name, interface="virtio")
                vm.start(wait=True)

                result = vm.guest_exec("/usr/bin/lspci -n")
                assert result.exit_code == 0, result.stderr
                assert any(pci_id in line for line in result.stdout.splitlines()), (
                    f"{model} card missing from guest lspci:\n{result.stdout}"
                )
            finally:
                if vm is not None:
                    with suppress(Exception):
                        vm.reset()
                    with suppress(Exception):
                        vm.delete()
                with suppress(Exception):
                    self.client.disks.get(name).delete()
