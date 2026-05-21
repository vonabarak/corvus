"""CloudInit set / get / delete."""
from __future__ import annotations

from ._helpers import with_client


SAMPLE_USERDATA = "#cloud-config\nhostname: py-test\n"


def test_cloudinit_set_get_delete(daemon_socket):
    run = with_client(daemon_socket)

    async def go(c):
        vm = await c.vms.create(
            "py-ci-vm",
            cpu_count=1,
            ram_mb=256,
            headless=True,
            cloud_init=True,
        )
        await c.cloud_init.set(vm_ref="py-ci-vm", user_data=SAMPLE_USERDATA)
        info = await c.cloud_init.get(vm_ref="py-ci-vm")
        assert info.user_data == SAMPLE_USERDATA
        await c.cloud_init.delete(vm_ref="py-ci-vm")
        info2 = await c.cloud_init.get(vm_ref="py-ci-vm")
        # After delete, cloud-init config is empty/default; the daemon
        # may still return a config record (cloudInit on the VM stays on).
        assert info2.user_data is None or info2.user_data == ""
        await vm.delete()

    run(go)
