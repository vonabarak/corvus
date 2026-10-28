"""Capture real QEMU displays locally and through a remote nodeagent."""

from pathlib import Path

import pytest
from corvus_client import VmHeadless, VmNotRunning
from corvus_test_harness import OneDaemonTwoNodesCase, TestNode

from .test_multi_node import _poll_until, _qemu_count, _retry_start


class TestScreenshot(OneDaemonTwoNodesCase):
    def test_png_on_both_nodes(self, tmp_path: Path) -> None:
        client = self.client_alpha
        remote = client.nodes.create(
            self.node_beta.short_name,
            self.node_beta.outer_ip,
            node_agent_port=9878,
            net_agent_port=9877,
        )
        try:
            for node in (self.node_alpha, self.node_beta):
                vm = client.vms.create(
                    f"screenshot-{node.short_name}",
                    node=node.short_name,
                    cpu_count=1,
                    ram=134217728,
                    headless=False,
                    guest_agent=False,
                    cloud_init=False,
                )
                try:
                    path = tmp_path / f"{node.short_name}.png"
                    with pytest.raises(VmNotRunning):
                        vm.screenshot_to_file(path)
                    assert not path.exists()
                    _retry_start(vm)

                    def started(node: TestNode = node) -> bool:
                        return _qemu_count(node, f"screenshot-{node.short_name}") == 1

                    _poll_until(
                        started,
                        timeout_sec=15,
                        msg="QEMU did not start",
                    )
                    count = vm.screenshot_to_file(path)
                    assert path.read_bytes().startswith(b"\x89PNG\r\n\x1a\n")
                    assert count == path.stat().st_size
                    vm.pause()
                    paused = tmp_path / f"{node.short_name}-paused.png"
                    assert vm.screenshot_to_file(paused) == paused.stat().st_size
                    with pytest.raises(FileExistsError):
                        vm.screenshot_to_file(path)
                finally:
                    vm.reset()
                    vm.delete()
            vm = client.vms.create("screenshot-headless", headless=True)
            try:
                with pytest.raises(VmHeadless):
                    vm.screenshot_to_file(tmp_path / "headless.png")
            finally:
                vm.delete()
        finally:
            remote.delete()
