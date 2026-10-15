"""Native QGA key injection without cloud-init, through API and CLI."""

from __future__ import annotations

import json
import secrets

import pytest
from corvus_client.exceptions import GuestAgentError, ServerError
from corvus_test_harness import SingleNodeCase, Vm

from .test_ssh_key_lifecycle import TEST_PUB_KEY


class TestSshKeyInjection(SingleNodeCase):
    @pytest.fixture(scope="class", autouse=True)
    @classmethod
    def _install_client_certs(cls, _class_topology: object) -> None:
        cls().install_node_client_certs()

    def test_append_reset_and_repeat(self) -> None:
        token = secrets.token_hex(4)
        first = TEST_PUB_KEY + "-first"
        second = TEST_PUB_KEY + "-second"
        unrelated = TEST_PUB_KEY + "-unrelated"
        keys = [
            self.client.ssh_keys.create(f"inject-first-{token}", first),
            self.client.ssh_keys.create(f"inject-second-{token}", second),
        ]
        try:
            with Vm(self) as vm:
                vm_id = vm.cap.show().id
                assert not vm.cap.show().cloud_init
                # Use a dedicated account so reset never alters the harness login.
                result = vm.cap.guest_exec(
                    "adduser -D keytest && mkdir -p /home/keytest/.ssh && "
                    f"printf '%s\\n' '{unrelated}' > /home/keytest/.ssh/authorized_keys && "
                    "chown -R keytest:keytest /home/keytest/.ssh"
                )
                assert result.exit_code == 0, result
                for key in keys:
                    vm.cap.attach_ssh_key(key.show().id)

                def installed() -> list[str]:
                    reply = vm.cap.guest_exec("cat /home/keytest/.ssh/authorized_keys")
                    assert reply.exit_code == 0, reply
                    return [line for line in reply.stdout.splitlines() if line]

                assert installed() == [unrelated]
                self.client.ssh_keys.inject(vm_id, "keytest")
                assert installed() == [unrelated, first, second]
                self.client.ssh_keys.inject(vm.name, "keytest", by_name=True)
                assert installed() == [unrelated, first, second]
                reply = self.node.run(
                    f"/opt/corvus/bin/crv --output json ssh-key inject {vm_id} keytest --reset",
                    check=False,
                )
                assert reply.returncode == 0, reply.stderr
                assert json.loads(reply.stdout)["status"] == "ok"
                assert installed() == [first, second]
                with pytest.raises(GuestAgentError):
                    self.client.ssh_keys.inject(vm_id, "missing-key-user")
                assert installed() == [first, second]
                for key in keys:
                    vm.cap.detach_ssh_key(key.show().id)
                assert installed() == [first, second]
                with pytest.raises(ServerError, match="no assigned SSH keys"):
                    self.client.ssh_keys.inject(vm_id, "keytest", reset=True)
                assert installed() == [first, second]
                tasks = self.client.tasks.list(
                    subsystem="vm", entity_id=vm_id, limit=50
                )
                injections = [
                    task for task in tasks if task.command == "inject-ssh-keys"
                ]
                assert (
                    len([task for task in injections if task.result == "success"]) == 3
                )
                assert len([task for task in injections if task.result == "error"]) == 2
        finally:
            for key in keys:
                key.delete()
