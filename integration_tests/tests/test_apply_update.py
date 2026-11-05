"""Apply update preserves identical resources and replaces declarative drift."""

from __future__ import annotations

import secrets
import shlex
from contextlib import suppress

import pytest
from corvus_client.exceptions import CorvusError
from corvus_client.types import BuildPipelineEnd
from corvus_test_harness import SingleNodeCase

import yaml


class TestApplyUpdate(SingleNodeCase):
    def test_build_apply_step_defaults_to_update(self) -> None:
        name = f"corvus-it-pipeline-update-{secrets.token_hex(4)}"
        template = {
            "name": name,
            "cpuCount": 1,
            "ram": "128M",
            "cloudInit": False,
            "drives": [],
        }

        def build() -> int:
            body = yaml.safe_dump({"pipeline": [{"apply": {"templates": [template]}}]})
            events = list(self.client.build_stream_text(body))
            assert any(isinstance(event, BuildPipelineEnd) for event in events)
            return self.client.templates.get(name, by_name=True).show().id

        try:
            original = build()
            assert build() == original
            template["cpuCount"] = 2
            replacement = build()
            assert replacement != original
            assert build() == replacement
        finally:
            with suppress(CorvusError):
                self.client.templates.get(name, by_name=True).delete()

    @pytest.mark.parametrize(
        ("section", "fields", "changed"),
        [
            (
                "sshKeys",
                {"publicKey": "ssh-ed25519 AAAA"},
                {"publicKey": "ssh-ed25519 BBBB"},
            ),
            ("networks", {"subnet": ""}, {"autostart": True}),
            (
                "templates",
                {"cpuCount": 1, "ram": "128M", "cloudInit": False, "drives": []},
                {"cpuCount": 2},
            ),
            (
                "vms",
                {"cpuCount": 1, "ram": "128M", "cloudInit": False, "vsock": False},
                {"cpuCount": 2},
            ),
            ("disks", {"format": "qcow2", "size": "4M"}, {"size": "8M"}),
        ],
    )
    def test_default_update_identity_and_policies(
        self, section: str, fields: dict[str, object], changed: dict[str, object]
    ) -> None:
        name = f"corvus-it-update-{secrets.token_hex(4)}"
        resource = {"name": name, **fields}
        collection = getattr(
            self.client, "ssh_keys" if section == "sshKeys" else section
        )
        ids: set[int] = set()

        def apply(policy: str | None = None) -> int:
            config: dict[str, object] = {section: [resource]}
            if policy is not None:
                config["ifExists"] = policy
            result, _ = self.client.apply(yaml.safe_dump(config), wait=True)
            items = getattr(result, "ssh_keys" if section == "sshKeys" else section)
            # Existing VMs are deliberately absent from the creation summary.
            value = (
                items[0].id if items else collection.get(name, by_name=True).show().id
            )
            ids.add(value)
            return int(value)

        try:
            first = apply()
            assert apply() == first
            assert apply("update") == first
            with pytest.raises(CorvusError):
                apply("error")
            resource.update(changed)
            assert apply("skip") == first
            second = apply()
            assert second != first
            assert apply() == second
            assert apply("overwrite") != second
        finally:
            for resource_id in sorted(ids, reverse=True):
                with suppress(CorvusError):
                    collection.get(resource_id).delete()

    @pytest.mark.parametrize("strategy", ["clone", "overlay", "import", "register"])
    def test_disk_strategies_are_idempotent(self, strategy: str) -> None:
        token = secrets.token_hex(4)
        source = self.client.disks.create(
            f"corvus-it-source-{token}", size=4194304, format="qcow2"
        )
        target = f"corvus-it-derived-{token}"
        info = source.show()
        resource: dict[str, object] = {"name": target}
        if strategy in ("clone", "overlay"):
            resource[strategy] = str(info.id)
        else:
            # Register an independent file so deleting it cannot damage source.
            path = f"/tmp/corvus-it-update-{token}.qcow2"
            self.node.run(shlex.join(["cp", info.placements[0].file_path, path]))
            resource[strategy] = path
        body = yaml.safe_dump({"disks": [resource]})
        created: set[int] = set()
        try:
            first, _ = self.client.apply(body, wait=True)
            created.add(first.disks[0].id)
            second, _ = self.client.apply(body, wait=True)
            created.add(second.disks[0].id)
            assert first.disks[0].id == second.disks[0].id
            if strategy == "import":
                # Source byte changes must invalidate a local import recipe.
                self.node.run(shlex.join(["qemu-img", "resize", path, "8M"]))
                third, _ = self.client.apply(body, wait=True)
                created.add(third.disks[0].id)
                assert third.disks[0].id != first.disks[0].id
                self.node.run(shlex.join(["rm", path]))
                with pytest.raises(CorvusError):
                    self.client.apply(body, wait=True)
                assert (
                    self.client.disks.get(target, by_name=True).show().id
                    == third.disks[0].id
                )
        finally:
            for resource_id in sorted(created, reverse=True):
                with suppress(CorvusError):
                    self.client.disks.get(resource_id).delete()
            source.delete()
            if strategy in ("import", "register"):
                self.node.run(shlex.join(["rm", "-f", path]), check=False)

    def test_vm_cloud_init_and_ephemeral_data_survive_update(self) -> None:
        token = secrets.token_hex(4)
        name = f"corvus-it-composed-{token}"
        disk_name = f"{name}-data"
        vm: dict[str, object] = {
            "name": name,
            "cpuCount": 1,
            "ram": "128M",
            "vsock": False,
            "guestAgent": False,
            "drives": [{"disk": disk_name, "interface": "virtio"}],
            "networkInterfaces": [{"type": "user"}],
            "cloudInitConfig": {"userData": "#cloud-config\nhostname: update-test"},
        }
        config = {
            "disks": [
                {"name": disk_name, "format": "qcow2", "size": "4M", "ephemeral": True}
            ],
            "vms": [vm],
        }
        vm_ids: set[int] = set()
        disk_ids: set[int] = set()
        try:
            first, _ = self.client.apply(yaml.safe_dump(config), wait=True)
            vm_ids.add(first.vms[0].id)
            disk_ids.add(first.disks[0].id)
            original = self.client.vms.get(first.vms[0].id).show()
            disk = self.client.disks.get(first.disks[0].id).show()
            self.node.run(
                shlex.join(
                    [
                        "qemu-io",
                        "-f",
                        "qcow2",
                        "-c",
                        "write -P 0x5a 0 512",
                        disk.placements[0].file_path,
                    ]
                )
            )
            self.client.vms.get(original.id).start(wait=True)
            second, _ = self.client.apply(yaml.safe_dump(config), wait=True)
            assert second.vms == []
            assert second.disks[0].id == disk.id
            assert self.client.vms.get(original.id).show().net_ifs == original.net_ifs
            vm["cpuCount"] = 2
            with pytest.raises(CorvusError):
                self.client.apply(yaml.safe_dump(config), wait=True)
            assert self.client.vms.get(original.id).show().id == original.id
            self.client.vms.get(original.id).reset()
            third, _ = self.client.apply(yaml.safe_dump(config), wait=True)
            vm_ids.add(third.vms[0].id)
            assert third.vms[0].id != original.id
            assert third.disks[0].id == disk.id
            self.node.run(
                shlex.join(
                    [
                        "qemu-io",
                        "-f",
                        "qcow2",
                        "-c",
                        "read -P 0x5a 0 512",
                        disk.placements[0].file_path,
                    ]
                )
            )
            fourth, _ = self.client.apply(yaml.safe_dump(config), wait=True)
            assert fourth.vms == []
        finally:
            for vm_id in vm_ids:
                with suppress(CorvusError):
                    self.client.vms.get(vm_id).reset()
                    self.client.vms.get(vm_id).delete()
            for disk_id in disk_ids:
                with suppress(CorvusError):
                    self.client.disks.get(disk_id).delete()

    def test_local_import_tracks_backing_file_content(self) -> None:
        token = secrets.token_hex(4)
        base = f"/tmp/corvus-it-base-{token}.qcow2"
        overlay = f"/tmp/corvus-it-overlay-{token}.qcow2"
        name = f"corvus-it-chain-{token}"
        created: set[int] = set()
        self.node.run(shlex.join(["qemu-img", "create", "-f", "qcow2", base, "4M"]))
        self.node.run(
            shlex.join(
                [
                    "qemu-img",
                    "create",
                    "-f",
                    "qcow2",
                    "-F",
                    "qcow2",
                    "-b",
                    base,
                    overlay,
                ]
            )
        )
        body = yaml.safe_dump({"disks": [{"name": name, "import": overlay}]})
        try:
            first, _ = self.client.apply(body, wait=True)
            created.add(first.disks[0].id)
            self.node.run(
                shlex.join(
                    ["qemu-io", "-f", "qcow2", "-c", "write -P 0x5a 0 512", base]
                )
            )
            second, _ = self.client.apply(body, wait=True)
            created.add(second.disks[0].id)
            assert second.disks[0].id != first.disks[0].id
            third, _ = self.client.apply(body, wait=True)
            assert third.disks[0].id == second.disks[0].id
        finally:
            for disk_id in created:
                with suppress(CorvusError):
                    self.client.disks.get(disk_id).delete()
            self.node.run(shlex.join(["rm", "-f", overlay, base]), check=False)
