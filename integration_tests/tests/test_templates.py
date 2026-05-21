"""Template CRUD + instantiate + sub-resource propagation.

Ports `TemplateIntegrationSpec` (see
`doc/integration-tests-pre-capnp.md:540-575`): create from YAML,
instantiate, boot, run `whoami` via QGA, and verify cloud-init
config round-trips into the instantiated VM.

Adds gap coverage absent from the pre-refactor spec:
  * Sub-resource propagation — `networkInterfaces` + `sshKeys` on a
    template flow through to the instantiated VM at finishInstantiation
    time (`src/Corvus/Handlers/Template.hs:372-381`).
  * Atomic `update(yaml)` replace + rollback on collision.
  * Duplicate-name rejection at create time.
  * Missing-disk-reference rejection at create time (validation
    should fail before any disk-overlay work).
"""

from __future__ import annotations

import secrets
import textwrap

import pytest

from corvus_client.exceptions import CorvusError
from corvus_test_harness import SingleNodeCase


pytestmark = pytest.mark.slow


# Dummy ed25519 public key for templates that reference an SSH key.
# The daemon stores the text as-is — it never verifies the key is
# cryptographically valid, so a static literal is fine for tests.
TEST_PUB_KEY = (
    "ssh-ed25519 "
    "AAAAC3NzaC1lZDI1NTE5AAAAIH3OAFlPq8wAYIKL3kZx0sMo2krfh1g+OmRkLD1OvBnK "
    "corvus-it-test"
)


class TestTemplates(SingleNodeCase):
    def test_create_show_instantiate_boot(self):
        """Ports both pre-refactor TemplateIntegrationSpec tests.

        One Alpine boot covers: create from YAML, show, list,
        cloud-init config round-trip (userData + injectSshKeys),
        instantiate, VM field propagation, boot to QGA-ready,
        `whoami` via guest_exec, clean stop, delete.
        """
        token = secrets.token_hex(4)
        images = self.register_base_images()
        base_disk = images["alpine"]
        tpl_name = f"corvus-it-tpl-{token}"
        vm_name = f"corvus-it-tpl-vm-{token}"
        body = textwrap.dedent(f"""
            name: {tpl_name}
            description: A test template
            cpuCount: 2
            ramMb: 1024
            headless: true
            guestAgent: true
            cloudInit: true
            cloudInitConfig:
              userData:
                users:
                  - name: custom-user
                    sudo: "ALL=(ALL) NOPASSWD:ALL"
                packages:
                  - curl
              injectSshKeys: false
            drives:
              - diskImageName: {base_disk}
                interface: virtio
                strategy: overlay
                sizeMb: 1024
        """).strip()

        tpl = self.client.templates.create(body)
        try:
            # ── show: fields land verbatim ──────────────────────────
            details = tpl.show()
            assert details.cpu_count == 2
            assert details.ram_mb == 1024
            assert details.description == "A test template"
            assert details.cloud_init is True
            assert details.guest_agent is True
            assert len(details.drives) == 1
            assert details.drives[0].disk_image_name == base_disk
            assert details.drives[0].clone_strategy == "overlay"

            ci = details.cloud_init_config
            assert ci is not None
            assert ci.inject_ssh_keys is False
            assert "custom-user" in (ci.user_data or "")
            assert "curl" in (ci.user_data or "")

            # ── list: template appears in templateList ─────────────
            assert tpl_name in [t.name for t in self.client.templates.list()]

            # ── instantiate + field propagation ────────────────────
            vm = tpl.instantiate(vm_name)
            try:
                info = vm.show()
                assert info.name == vm_name
                assert info.cpu_count == 2
                assert info.ram_mb == 1024
                assert info.description == "A test template"
                assert info.cloud_init is True
                assert info.guest_agent is True

                # ── boot + QGA exec ────────────────────────────────
                vm.start(wait=True)
                r = vm.guest_exec("/usr/bin/whoami")
                assert r.exit_code == 0
                assert r.stdout.strip() == "root"
                vm.stop(wait=True)
            finally:
                vm.reset()  # idempotent hard-stop
                vm.delete(delete_disks=True)
        finally:
            tpl.delete()

    def test_subresources_propagate(self):
        """`networkInterfaces` + `sshKeys` flow into the instance.

        Pre-refactor spec missed both fields entirely. Verify
        `finishInstantiation` (`src/Corvus/Handlers/Template.hs:372`)
        attaches one VmNetIf per template net-if (with a fresh MAC)
        and one VmSshKey per template ssh-key reference. No drives,
        no boot — purely a wiring test (~3 s).
        """
        token = secrets.token_hex(4)
        key_name = f"corvus-it-tpl-key-{token}"
        tpl_name = f"corvus-it-tpl-{token}"
        vm_name = f"corvus-it-tpl-vm-{token}"
        # `cloudInit: true` is required by the daemon's invariant —
        # SSH keys imply cloud-init injection (see
        # `Handlers/Template.hs`'s validation). Instantiation will
        # auto-create a cloud-init ISO and attach it as a drive; we
        # don't boot, just observe the wiring.
        body = textwrap.dedent(f"""
            name: {tpl_name}
            cpuCount: 1
            ramMb: 512
            headless: true
            cloudInit: true
            networkInterfaces:
              - type: user
            sshKeys:
              - name: {key_name}
            drives: []
        """).strip()

        key = self.client.ssh_keys.create(key_name, TEST_PUB_KEY)
        try:
            tpl = self.client.templates.create(body)
            try:
                details = tpl.show()
                assert len(details.net_ifs) == 1
                assert details.net_ifs[0].type == "user"
                assert len(details.ssh_keys) == 1
                assert details.ssh_keys[0].name == key_name

                vm = tpl.instantiate(vm_name)
                try:
                    net_ifs = vm.list_net_ifs()
                    assert len(net_ifs) == 1
                    assert net_ifs[0].type == "user"
                    # Fresh MAC populated by generateMacAddress.
                    assert net_ifs[0].mac_address
                    keys = vm.list_ssh_keys()
                    assert len(keys) == 1
                    assert keys[0].name == key_name
                finally:
                    vm.delete(delete_disks=True)
            finally:
                tpl.delete()
        finally:
            key.delete()

    def test_update_atomic_replace(self):
        """`template.update(yaml)` replaces the row atomically.

        Happy path: cpu/ram/description swap lands. Rollback path:
        renaming to a name that collides with another existing
        template fails and leaves the original row untouched.
        """
        token = secrets.token_hex(4)
        name = f"corvus-it-tpl-{token}"
        name_other = f"corvus-it-tpl-other-{token}"

        def body(template_name, *, cpu=1, ram=512, desc=None):
            lines = [
                f"name: {template_name}",
                f"cpuCount: {cpu}",
                f"ramMb: {ram}",
                "headless: true",
                "drives: []",
            ]
            if desc is not None:
                lines.insert(3, f'description: "{desc}"')
            return "\n".join(lines)

        tpl = self.client.templates.create(body(name))
        other = self.client.templates.create(body(name_other))
        try:
            # Happy path. `update` is atomic-replace: the old row is
            # deleted and a new one is inserted, so the existing cap
            # holds a stale id. Re-resolve by name to inspect.
            tpl.update(body(name, cpu=4, ram=2048, desc="Updated"))
            tpl = self.client.templates.get(name, by_name=True)
            d = tpl.show()
            assert d.cpu_count == 4
            assert d.ram_mb == 2048
            assert d.description == "Updated"

            # Rollback: renaming to a colliding name must fail and
            # leave the renamed-to-self row untouched.
            colliding = body(name_other, cpu=4, ram=2048, desc="Updated")
            with pytest.raises(CorvusError):
                tpl.update(colliding)
            # After a failed update, the cap is still valid (rollback
            # left the row in place with its original id).
            d = tpl.show()
            assert d.cpu_count == 4
            assert d.ram_mb == 2048
        finally:
            tpl.delete()
            other.delete()

    def test_rejects_duplicate_name(self):
        """A second `create()` with the same name must fail."""
        token = secrets.token_hex(4)
        name = f"corvus-it-tpl-{token}"
        body = textwrap.dedent(f"""
            name: {name}
            cpuCount: 1
            ramMb: 512
            headless: true
            drives: []
        """).strip()
        tpl = self.client.templates.create(body)
        try:
            with pytest.raises(CorvusError):
                self.client.templates.create(body)
        finally:
            tpl.delete()

    def test_rejects_missing_disk(self):
        """Template referencing a nonexistent disk fails at create.

        Validation lives in `Handlers/Template.hs` before any
        instantiation work — referencing a disk that doesn't exist
        must produce a clean rejection, not a deferred error at
        instantiate time.
        """
        token = secrets.token_hex(4)
        ghost = f"corvus-it-does-not-exist-{token}"
        body = textwrap.dedent(f"""
            name: corvus-it-tpl-{token}
            cpuCount: 1
            ramMb: 512
            headless: true
            drives:
              - diskImageName: {ghost}
                interface: virtio
                strategy: overlay
                sizeMb: 1024
        """).strip()
        with pytest.raises(CorvusError) as excinfo:
            self.client.templates.create(body)
        msg = str(excinfo.value)
        assert ("not found" in msg) or ("does not exist" in msg), msg
