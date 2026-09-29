"""SSH-key lifecycle and VM association views."""

from __future__ import annotations

import secrets

import pytest
from corvus_client import SshKeyInUse
from corvus_test_harness import SingleNodeCase

TEST_PUB_KEY = (
    "ssh-ed25519 "
    "AAAAC3NzaC1lZDI1NTE5AAAAIH3OAFlPq8wAYIKL3kZx0sMo2krfh1g+OmRkLD1OvBnK "
    "corvus-it-test"
)


class TestSshKeyLifecycle(SingleNodeCase):
    def test_create_attach_detach_and_delete_key(self) -> None:
        token = secrets.token_hex(4)
        key_name = f"ssh-lifecycle-{token}"
        vm_name = f"ssh-lifecycle-vm-{token}"
        vm = self.client.vms.create(
            vm_name,
            cpu_count=1,
            ram_mb=64,
            headless=True,
            cloud_init=True,
        )
        key = self.client.ssh_keys.create(key_name, TEST_PUB_KEY)
        attached = False
        try:
            created = key.show()
            assert created.name == key_name
            assert created.public_key == TEST_PUB_KEY
            assert (
                self.client.ssh_keys.get(key_name, by_name=True).show().id == created.id
            )
            assert any(item.id == created.id for item in self.client.ssh_keys.list())

            vm.attach_ssh_key(key_name)
            attached = True
            vm_keys = vm.list_ssh_keys()
            assert [(item.id, item.name) for item in vm_keys] == [
                (created.id, key_name)
            ]
            attached_vms = key.show().attached_vms
            assert [(item.vm.id, item.vm.name) for item in attached_vms] == [
                (vm.show().id, vm_name)
            ]

            with pytest.raises(SshKeyInUse):
                key.delete()

            vm.detach_ssh_key(key_name)
            attached = False
            assert vm.list_ssh_keys() == []
            assert key.show().attached_vms == []
            key.delete()
        finally:
            if attached:
                try:
                    vm.detach_ssh_key(key_name)
                except Exception:
                    pass
            try:
                self.client.ssh_keys.get(key_name, by_name=True).delete()
            except Exception:
                pass
            vm.delete()
