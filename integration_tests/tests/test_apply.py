"""End-to-end YAML apply against the inner daemon.

Ports `ApplyIntegrationSpec` (see
`doc/integration-tests-pre-capnp.md:60-99`):
  * two VMs on a managed network can ping each other after static
    IPs are assigned via QGA;
  * cloud-init injection of an SSH key from the YAML's `sshKeys:`
    section lands in `/home/corvus/.ssh/authorized_keys`;
  * custom `cloudInitConfig.userData` creates a `deployer` user
    with the deployed key in its authorized_keys.

Plus two gap tests absent from the pre-refactor spec:
  * `skip_existing=True` idempotency — re-apply returns the same
    resource IDs and doesn't error;
  * a VM that references a non-existent disk is rejected at
    validation, before any partial state lands.

All five tests drive the inner daemon's `apply` cap directly (the
harness's `Vm` class bypasses `apply`; this is the first apply
coverage at the cap level).
"""

from __future__ import annotations

import secrets
import textwrap
import time

import pytest
from corvus_client.exceptions import CorvusError
from corvus_client.types import (
    ApplyEnd,
    ApplyEntityEnd,
    ApplyEntityStart,
    ApplyPhaseStart,
)
from corvus_test_harness import SingleNodeCase

pytestmark = pytest.mark.timeout(1200)


# Module-level helper: raises with a useful message on non-zero exit.
def _qga(vm, cmd: str) -> None:
    r = vm.guest_exec(cmd)
    assert r.exit_code == 0, (
        f"{cmd!r} failed: exit={r.exit_code} stdout={r.stdout!r} stderr={r.stderr!r}"
    )


# Static ed25519 public key — the daemon stores the text verbatim
# and doesn't crypto-verify, so a literal works for tests.
_TEST_PUB_KEY_BLOB = (
    "AAAAC3NzaC1lZDI1NTE5AAAAIH3OAFlPq8wAYIKL3kZx0sMo2krfh1g+OmRkLD1OvBnK"
)


class TestApply(SingleNodeCase):
    def test_two_vms_managed_network_can_ping(self):
        """Two VMs on a managed network can ping each other.

        First integration test to exercise the rootful managed-network
        code path on the inner daemon. Configures static IPs via QGA
        (the managed network has empty subnet → no DHCP), then pings
        bidirectionally.
        """
        token = secrets.token_hex(4)
        images = self.register_base_images()
        base_disk = images["alpine"]
        net_name = f"corvus-it-apply-net-{token}"
        vm1_name = f"corvus-it-apply-vm1-{token}"
        vm2_name = f"corvus-it-apply-vm2-{token}"
        root1 = f"corvus-it-apply-root1-{token}"
        root2 = f"corvus-it-apply-root2-{token}"
        yaml_body = textwrap.dedent(f"""
            disks:
              - name: {root1}
                overlay: {base_disk}
                ephemeral: true
              - name: {root2}
                overlay: {base_disk}
                ephemeral: true
            networks:
              - name: {net_name}
                subnet: ""
            vms:
              - name: {vm1_name}
                cpuCount: 2
                ramMb: 512
                headless: true
                guestAgent: true
                drives:
                  - disk: {root1}
                    interface: virtio
                networkInterfaces:
                  - type: user
                  - type: managed
                    network: {net_name}
              - name: {vm2_name}
                cpuCount: 2
                ramMb: 512
                headless: true
                guestAgent: true
                drives:
                  - disk: {root2}
                    interface: virtio
                networkInterfaces:
                  - type: user
                  - type: managed
                    network: {net_name}
        """).strip()

        result, _ = self.client.apply(yaml_body, wait=True)
        try:
            assert len(result.disks) == 2
            assert len(result.networks) == 1
            assert len(result.vms) == 2

            net = self.client.networks.get(net_name, by_name=True)
            net.start()
            try:
                v1 = self.client.vms.get(vm1_name, by_name=True)
                v2 = self.client.vms.get(vm2_name, by_name=True)
                v1.start(wait=True)
                v2.start(wait=True)
                try:
                    # eth1 is the managed NIC (eth0 = user NIC).
                    # Assign static IPs via QGA (no DHCP on empty subnet).
                    _qga(v1, "/sbin/ip link set eth1 up")
                    _qga(v1, "/sbin/ip addr add 10.0.0.1/24 dev eth1")
                    _qga(v2, "/sbin/ip link set eth1 up")
                    _qga(v2, "/sbin/ip addr add 10.0.0.2/24 dev eth1")
                    # Bidirectional ping over the managed bridge.
                    _qga(v1, "/bin/ping -c 3 -W 5 10.0.0.2")
                    _qga(v2, "/bin/ping -c 3 -W 5 10.0.0.1")
                finally:
                    v1.stop(wait=True)
                    v2.stop(wait=True)
            finally:
                net.stop()
        finally:
            # Order matters: VMs → network → (disks deleted via VM).
            for name in (vm1_name, vm2_name):
                try:
                    v = self.client.vms.get(name, by_name=True)
                    v.reset()
                    v.delete()
                except Exception:
                    pass
            try:
                self.client.networks.get(net_name, by_name=True).delete()
            except Exception:
                pass

    def test_deploys_ssh_key_via_cloud_init(self):
        """SSH key declared in YAML lands in authorized_keys.

        Apply's contract: cloud-init injects the key referenced in
        the VM's `sshKeys:` section. Verified by reading
        /home/corvus/.ssh/authorized_keys via QGA — not by SSH'ing
        in (which would require building a VSOCK SSH path for
        apply-managed VMs).
        """
        token = secrets.token_hex(4)
        images = self.register_base_images()
        # Gentoo-headless-cloudinit image: ships both cloud-init AND
        # QGA. The plain gentoo-base-headless variant strips
        # cloud-init out (it's the bake host for non-cloud-init
        # VMs like corvus-test-node), so key injection would
        # silently no-op there. The Alpine cloud image has
        # cloud-init but no QGA → daemon `vm.start(wait=True)`
        # hangs.
        base_disk = images["gentoo-base-headless-cloudinit"]
        key_name = f"corvus-it-apply-key-{token}"
        vm_name = f"corvus-it-apply-vm-{token}"
        root_name = f"corvus-it-apply-root-{token}"
        pubkey = f"ssh-ed25519 {_TEST_PUB_KEY_BLOB} corvus-it-test-{token}"
        yaml_body = textwrap.dedent(f"""
            sshKeys:
              - name: {key_name}
                publicKey: "{pubkey}"
            disks:
              - name: {root_name}
                overlay: {base_disk}
                sizeMb: 2048
                ephemeral: true
            vms:
              - name: {vm_name}
                cpuCount: 1
                ramMb: 512
                headless: true
                guestAgent: true
                cloudInit: true
                drives:
                  - disk: {root_name}
                    interface: virtio
                networkInterfaces:
                  - type: user
                sshKeys:
                  - {key_name}
        """).strip()

        result, _ = self.client.apply(yaml_body, wait=True)
        try:
            assert len(result.ssh_keys) == 1
            assert len(result.disks) == 1
            assert len(result.vms) == 1

            vm = self.client.vms.get(vm_name, by_name=True)
            vm.start(wait=True)
            try:
                # Cloud-init runs at first boot. Poll up to ~60 s.
                last = None
                for _ in range(30):
                    last = vm.guest_exec("/bin/cat /home/corvus/.ssh/authorized_keys")
                    if last.exit_code == 0 and _TEST_PUB_KEY_BLOB in last.stdout:
                        break
                    time.sleep(2)
                else:
                    raise AssertionError(f"ssh key not injected; last={last!r}")
            finally:
                vm.stop(wait=True)
        finally:
            try:
                v = self.client.vms.get(vm_name, by_name=True)
                v.reset()
                v.delete()
            except Exception:
                pass
            try:
                self.client.ssh_keys.get(key_name, by_name=True).delete()
            except Exception:
                pass

    def test_custom_cloud_init_creates_deployer_user(self):
        """`cloudInitConfig.userData` creates a deployer user + injects key.

        Verifies the apply layer carries the userData through to the
        cloud-init ISO and that injectSshKeys: true causes the
        referenced key to land in deployer's authorized_keys.
        """
        token = secrets.token_hex(4)
        images = self.register_base_images()
        # gentoo-base-headless-cloudinit — see
        # test_deploys_ssh_key_via_cloud_init for the variant rationale.
        base_disk = images["gentoo-base-headless-cloudinit"]
        key_name = f"corvus-it-apply-key-{token}"
        vm_name = f"corvus-it-apply-vm-{token}"
        root_name = f"corvus-it-apply-root-{token}"
        pubkey = f"ssh-ed25519 {_TEST_PUB_KEY_BLOB} corvus-it-test-{token}"
        yaml_body = textwrap.dedent(f"""
            sshKeys:
              - name: {key_name}
                publicKey: "{pubkey}"
            disks:
              - name: {root_name}
                overlay: {base_disk}
                sizeMb: 2048
                ephemeral: true
            vms:
              - name: {vm_name}
                cpuCount: 1
                ramMb: 512
                headless: true
                guestAgent: true
                cloudInit: true
                cloudInitConfig:
                  userData:
                    users:
                      - name: deployer
                        sudo: "ALL=(ALL) NOPASSWD:ALL"
                        shell: /bin/sh
                        lock_passwd: false
                    ssh_pwauth: true
                  injectSshKeys: true
                drives:
                  - disk: {root_name}
                    interface: virtio
                networkInterfaces:
                  - type: user
                sshKeys:
                  - {key_name}
        """).strip()

        self.client.apply(yaml_body, wait=True)
        try:
            vm = self.client.vms.get(vm_name, by_name=True)
            vm.start(wait=True)
            try:
                seen_user = False
                seen_key = False
                last_user = None
                last_key = None
                for _ in range(60):
                    if not seen_user:
                        last_user = vm.guest_exec("/usr/bin/id deployer")
                        if last_user.exit_code == 0 and "deployer" in last_user.stdout:
                            seen_user = True
                    if seen_user and not seen_key:
                        last_key = vm.guest_exec(
                            "/bin/cat /home/deployer/.ssh/authorized_keys"
                        )
                        if (
                            last_key.exit_code == 0
                            and _TEST_PUB_KEY_BLOB in last_key.stdout
                        ):
                            seen_key = True
                    if seen_user and seen_key:
                        break
                    time.sleep(2)
                assert seen_user, f"deployer user never appeared; last={last_user!r}"
                assert seen_key, (
                    f"key not in deployer authorized_keys; last={last_key!r}"
                )
            finally:
                vm.stop(wait=True)
        finally:
            try:
                v = self.client.vms.get(vm_name, by_name=True)
                v.reset()
                v.delete()
            except Exception:
                pass
            try:
                self.client.ssh_keys.get(key_name, by_name=True).delete()
            except Exception:
                pass

    def test_skip_existing_idempotent(self):
        """Re-applying the same YAML with skip_existing=True is a no-op.

        Same IDs round-trip on the second apply — proves the daemon
        resolved names to existing resources rather than erroring or
        re-creating.
        """
        token = secrets.token_hex(4)
        images = self.register_base_images()
        base_disk = images["alpine"]
        root_name = f"corvus-it-apply-root-{token}"
        net_name = f"corvus-it-apply-net-{token}"
        vm_name = f"corvus-it-apply-vm-{token}"
        yaml_body = textwrap.dedent(f"""
            disks:
              - name: {root_name}
                overlay: {base_disk}
                sizeMb: 1024
                ephemeral: true
            networks:
              - name: {net_name}
                subnet: ""
            vms:
              - name: {vm_name}
                cpuCount: 1
                ramMb: 256
                headless: true
                guestAgent: false
                drives:
                  - disk: {root_name}
                    interface: virtio
                networkInterfaces:
                  - type: user
        """).strip()

        r1, _ = self.client.apply(yaml_body, wait=True, skip_existing=True)
        r2, _ = self.client.apply(yaml_body, wait=True, skip_existing=True)
        try:
            # Disks, networks (and ssh-keys, templates) return the
            # existing id on the second apply.
            assert r1.disks[0].id == r2.disks[0].id
            assert r1.networks[0].id == r2.networks[0].id
            # VM creation has an asymmetry in the daemon
            # (`runSequentialVms` at `Handlers/Apply.hs:287-289` skips
            # appending an existing VM to the result). The VM exists,
            # but doesn't appear in `r2.vms`.
            assert len(r1.vms) == 1
            assert r2.vms == []
            # Verify the VM is still there (by name resolution).
            self.client.vms.get(vm_name, by_name=True)
        finally:
            try:
                self.client.vms.get(vm_name, by_name=True).delete()
            except Exception:
                pass
            try:
                self.client.networks.get(net_name, by_name=True).delete()
            except Exception:
                pass

    def test_rejects_missing_disk_reference(self):
        """A VM referencing a nonexistent disk fails the apply.

        Resource creation order is sshKeys → disks → networks → VMs;
        for each VM the daemon creates the row first and then resolves
        drives/netifs (`Handlers/Apply.hs`). A nonexistent disk
        reference therefore surfaces at drive-attach time, with the
        partial VM left behind — that's accepted current behaviour;
        the test cleans the orphan up. The apply call itself must
        raise a typed error mentioning the missing disk.
        """
        token = secrets.token_hex(4)
        ghost = f"corvus-it-does-not-exist-{token}"
        vm_name = f"corvus-it-apply-vm-{token}"
        yaml_body = textwrap.dedent(f"""
            vms:
              - name: {vm_name}
                cpuCount: 1
                ramMb: 256
                headless: true
                drives:
                  - disk: {ghost}
                    interface: virtio
                networkInterfaces:
                  - type: user
        """).strip()
        try:
            with pytest.raises(CorvusError) as excinfo:
                self.client.apply(yaml_body, wait=True)
            msg = str(excinfo.value)
            assert ("not found" in msg) or ("does not exist" in msg), msg
        finally:
            try:
                v = self.client.vms.get(vm_name, by_name=True)
                v.reset()
                v.delete()
            except Exception:
                pass

    def test_md5_mismatch_fails_after_retries(self):
        """A URL-imported disk whose ``md5`` field is set to the
        wrong digest must fail the apply — the importer retries up
        to 3 times (per ``Handlers/Disk/Import.hs:317-320``) and
        then deletes the partial download and reports the
        mismatch. The test serves a real qcow2 over a one-shot
        HTTP server on the node, then claims a deliberately-bogus
        md5 in the apply YAML; the apply must raise."""
        token = secrets.token_hex(4)
        src_name = f"corvus-it-md5-src-{token}"
        target_name = f"corvus-it-md5-bad-{token}"

        # Create a source qcow2 and serve it via Python's
        # http.server. Mirrors the helper in test_disk.py's
        # test_import_from_http_url.
        src = self.client.disks.create(src_name, size_mb=4, format="qcow2")
        try:
            src_path = src.show().placements[0].file_path
            srv_dir = f"/tmp/md5-srv-{token}"
            self.node.run(f"mkdir -p {srv_dir}")
            self.node.run(f"cp {src_path} {srv_dir}/payload.qcow2")
            port = 30000 + secrets.randbelow(20000)
            self.node.run(
                f"nohup python3 -m http.server {port} --bind 127.0.0.1 "
                f"--directory {srv_dir} > /tmp/md5-srv-{token}.log 2>&1 &"
            )
            try:
                time.sleep(0.5)
                yaml_body = textwrap.dedent(f"""
                    disks:
                      - name: {target_name}
                        import: http://127.0.0.1:{port}/payload.qcow2
                        format: qcow2
                        md5: deadbeefdeadbeefdeadbeefdeadbeef
                """).strip()
                with pytest.raises(CorvusError) as excinfo:
                    self.client.apply(yaml_body, wait=True)
                msg = str(excinfo.value).lower()
                assert "md5" in msg, (
                    f"expected an md5-mismatch diagnostic; got: {msg!r}"
                )
            finally:
                self.node.run(f"pkill -f 'http.server {port}'", check=False)
                self.node.run(f"rm -rf {srv_dir}", check=False)
        finally:
            try:
                src.delete()
            except Exception:
                pass
            try:
                self.client.disks.get(target_name, by_name=True).delete()
            except Exception:
                pass

    def test_yaml_anchors_and_merge_keys_apply(self):
        """The daemon's YAML parser must honour anchors (``&name``)
        and merge keys (``<<: *name``) — without them, operators
        can't deduplicate common stanzas across many VMs and the
        apply YAMLs balloon. Defines a single anchor for the
        common per-VM fields, then references it from two VMs.
        Both VMs must materialise with the merged fields."""
        token = secrets.token_hex(4)
        net_name = f"corvus-it-anchor-net-{token}"
        vm_a = f"corvus-it-anchor-a-{token}"
        vm_b = f"corvus-it-anchor-b-{token}"
        # YAML 1.1 merge-key spec; libyaml (which Haskell's
        # ``Data.Yaml`` wraps) supports both anchors and merge
        # keys natively. We do NOT call ``textwrap.dedent`` because
        # YAML's whitespace + anchors are sensitive to indentation.
        yaml_body = (
            "_template: &vmcommon\n"
            "  cpuCount: 1\n"
            "  ramMb: 128\n"
            "  headless: true\n"
            "  guestAgent: false\n"
            f"networks:\n"
            f"  - name: {net_name}\n"
            f'    subnet: ""\n'
            "vms:\n"
            f"  - <<: *vmcommon\n"
            f"    name: {vm_a}\n"
            f"  - <<: *vmcommon\n"
            f"    name: {vm_b}\n"
        )
        try:
            self.client.apply(yaml_body, wait=True)
            a = self.client.vms.get(vm_a, by_name=True).show()
            b = self.client.vms.get(vm_b, by_name=True).show()
            for v in (a, b):
                assert v.cpu_count == 1, v
                assert v.ram_mb == 128, v
                assert v.headless is True, v
                assert v.guest_agent is False, v
        finally:
            for n in (vm_a, vm_b):
                try:
                    self.client.vms.get(n, by_name=True).delete()
                except Exception:
                    pass
            try:
                self.client.networks.get(net_name, by_name=True).delete()
            except Exception:
                pass

    def test_validation_rejects_duplicate_disk_name(self):
        """Two disk entries with the same ``name`` in one YAML are
        a validation error — the daemon checks for duplicates
        before any side-effects (``Handlers/Apply.hs`` ``validateConfig``)."""
        token = secrets.token_hex(4)
        dup_name = f"corvus-it-dup-{token}"
        yaml_body = textwrap.dedent(f"""
            disks:
              - name: {dup_name}
                sizeMb: 8
                format: qcow2
              - name: {dup_name}
                sizeMb: 16
                format: qcow2
        """).strip()
        try:
            with pytest.raises(CorvusError) as excinfo:
                self.client.apply(yaml_body, wait=True)
            msg = str(excinfo.value).lower()
            assert "duplicate" in msg or dup_name.lower() in msg, msg
        finally:
            # Best-effort cleanup in case the daemon partially
            # created the first one before noticing the duplicate.
            try:
                self.client.disks.get(dup_name, by_name=True).delete()
            except Exception:
                pass

    def test_if_exists_overwrite_replaces_disk(self):
        """``ifExists: overwrite`` in apply YAML deletes the matching
        existing entity before re-creating it. (The Schema/Apply.hs:70
        comment claims this is rejected at parse time, but the
        dispatcher at Handlers/Apply.hs:494-496 actually implements
        the delete + recreate path — comment is outdated.)

        We exercise the disk overwrite path: create a disk, then
        re-apply YAML carrying the same name but a different size
        and ``ifExists: overwrite``. The disk's id changes (new
        row) and the size reflects the new YAML."""
        token = secrets.token_hex(4)
        disk_name = f"corvus-it-overwrite-{token}"

        try:
            yaml_first = textwrap.dedent(f"""
                disks:
                  - name: {disk_name}
                    sizeMb: 8
                    format: qcow2
            """).strip()
            self.client.apply(yaml_first, wait=True)
            first = self.client.disks.get(disk_name, by_name=True).show()
            assert first.size_mb == 8

            yaml_second = textwrap.dedent(f"""
                ifExists: overwrite
                disks:
                  - name: {disk_name}
                    sizeMb: 32
                    format: qcow2
            """).strip()
            self.client.apply(yaml_second, wait=True)
            second = self.client.disks.get(disk_name, by_name=True).show()
            # Overwrite = delete + recreate, so id changes and the
            # new size from the second YAML is in effect.
            assert second.id != first.id, (
                f"overwrite did not delete + recreate the disk row "
                f"(id stayed at {first.id}); the dispatcher's "
                f"IfExistsOverwrite branch must call delete first"
            )
            assert second.size_mb == 32, second
        finally:
            try:
                self.client.disks.get(disk_name, by_name=True).delete()
            except Exception:
                pass

    def test_stream_apply_phase_and_entity_events(self):
        """`apply_stream` emits per-phase / per-entity events in order.

        Drives a trivial config (one SSH key + one register-only disk)
        through the streaming RPC and asserts:

          * a `ApplyPhaseStart` arrives for every non-empty phase, in
            dependency order;
          * each entity is bracketed by a matching
            `ApplyEntityStart` / `ApplyEntityEnd`;
          * the stream terminates with exactly one `ApplyEnd` whose
            `result == "success"`;
          * `task_id` round-trips as the final tuple yielded by the
            generator.
        """
        token = secrets.token_hex(4)
        images = self.register_base_images()
        # `register_base_images` lands a few well-known disks already;
        # use one of those for a register-only apply (no qemu-img).
        existing_disk_name = images["alpine"]
        del existing_disk_name  # only the SSH key is created here
        key_name = f"corvus-it-stream-key-{token}"
        yaml_body = textwrap.dedent(f"""
            sshKeys:
              - name: {key_name}
                publicKey: ssh-ed25519 {_TEST_PUB_KEY_BLOB} {key_name}
        """).strip()

        try:
            events: list = []
            task_id = None
            for item in self.client.apply_stream(yaml_body):
                if isinstance(item, tuple):
                    kind, payload = item
                    assert kind == "task_id"
                    task_id = payload
                else:
                    events.append(item)

            assert task_id is not None and task_id > 0, task_id

            phases = [e.phase for e in events if isinstance(e, ApplyPhaseStart)]
            assert "sshKeys" in phases, phases

            starts = [
                (e.phase, e.name) for e in events if isinstance(e, ApplyEntityStart)
            ]
            ends = [
                (e.phase, e.name, e.result)
                for e in events
                if isinstance(e, ApplyEntityEnd)
            ]
            assert ("sshKeys", key_name) in starts, starts
            assert any(
                p == "sshKeys" and n == key_name and r == "success" for p, n, r in ends
            ), ends

            apply_ends = [e for e in events if isinstance(e, ApplyEnd)]
            assert len(apply_ends) == 1, apply_ends
            assert apply_ends[0].result == "success", apply_ends[0]
            assert apply_ends[0].task_id == task_id, (apply_ends[0], task_id)
        finally:
            try:
                self.client.ssh_keys.get(key_name, by_name=True).delete()
            except Exception:
                pass
