"""Operator-facing surfaces for VM-start failures.

When a VM fails to start (e.g. QEMU OOMs because the requested RAM
exceeds the host's), three things must hold:

* the caller's `vm.start(...)` RPC fails fast — well under the 90 s
  wait-for-first-QGA budget;
* the failure reason ends up in `vm.show().error_message` so
  `crv vm show` shows the operator what happened without making
  them hunt task history;
* `vm.show().last_error_at` is set, so the operator can correlate
  with logs.

Implementation hook for the test: we make QEMU's `-m <MiB>` request
exceed the test-node's total memory. The kernel rejects the
allocation, QEMU exits within ~1 s, and the nodeagent's
`waitForFirstQgaPing` sees `vlsLastExitCode` populated and surfaces
"QEMU exited with code N before first guest-agent ping" instead of
the misleading 90 s "QGA ping timeout".
"""

from __future__ import annotations

import secrets
import time

import pytest
from corvus_client.exceptions import CorvusError
from corvus_test_harness import SingleNodeCase

pytestmark = [pytest.mark.slow, pytest.mark.timeout(300)]


class TestVmStartFailures(SingleNodeCase):
    def test_vm_oversized_ram_surfaces_in_vm_show(self):
        """Requesting more RAM than the node has surfaces a real
        error on `vm.show()`, not a misleading QGA-timeout."""
        # Discover the node's total RAM so the test is independent
        # of how the test-node is sized today (4 GiB) vs. tomorrow
        # (16 GiB).
        r = self.node.run("awk '/MemTotal/{print int($2/1024)}' /proc/meminfo")
        node_ram_mb = int(r.stdout.decode().strip())
        # 2 GiB beyond what the kernel can possibly grant. Margin
        # avoids racing against the dev VM's own working set.
        oversized_mb = node_ram_mb + 2048

        token = secrets.token_hex(4)
        vm_name = f"corvus-it-oversize-{token}"
        overlay_name = f"{vm_name}-overlay"
        images = self.register_base_images()
        base_disk = images["alpine"]

        self.client.disks.create_overlay(overlay_name, base_disk)
        vm = self.client.vms.create(
            vm_name,
            cpu_count=1,
            ram_mb=oversized_mb,
            headless=True,
            guest_agent=True,
            cloud_init=False,
        )
        try:
            vm.attach_disk(disk_ref=overlay_name, interface="virtio")
            start = time.monotonic()
            with pytest.raises(CorvusError) as exc_info:
                vm.start(wait=True)
            elapsed = time.monotonic() - start

            # Fast-fail: must NOT eat the 90 s wait-for-first-ping
            # budget. 30 s allows generous margin for QEMU init,
            # the reaper picking up the exit, and the RPC roundtrip
            # back through the daemon.
            assert elapsed < 30, f"vm.start hung for {elapsed:.1f}s — should be <30s"

            # The error string (lands in task.message) must blame
            # QEMU's early exit, not a guest-agent timeout. With
            # the forked first-ping watcher, the wording the
            # daemon surfaces comes from the agent's push-channel
            # exit code (just "QEMU exited with code N"); the
            # original verbose phrase lived on the synchronous
            # vmStart return path that no longer exists. We
            # therefore assert (a) the message mentions the QEMU
            # exit, and (b) it does NOT misattribute the failure
            # to a guest-agent timeout.
            msg = str(exc_info.value).lower()
            assert "exited" in msg, msg
            assert "qga ping timeout" not in msg, msg
            assert "guest agent did not respond" not in msg, msg

            # `crv vm show` surface: the VM is in error with a
            # populated reason and timestamp.
            details = vm.show()
            assert details.status == "error", details.status
            assert details.error_message is not None
            assert "exited" in details.error_message.lower()
            assert details.last_error_at is not None
        finally:
            try:
                vm.reset()
            except Exception:
                pass
            try:
                vm.delete()
            except Exception:
                pass
