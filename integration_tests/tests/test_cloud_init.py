"""Cloud-init NoCloud SSH-key injection across the multi-OS matrix.

Ports `CloudInitIntegrationSpec` from
[`doc/integration-tests-pre-capnp.md`](../../doc/integration-tests-pre-capnp.md);
the BIOS/UEFI matrix is collapsed to BIOS-only because cloud-init
behaves identically on either firmware.

Each VM-booting test:

  1. Generates an ed25519 keypair in a tempdir.
  2. Registers it with the inner daemon and attaches it to a fresh
     VM whose disk is an overlay on an upstream cloud image.
  3. Calls `client.cloud_init.set(vm_id, inject_ssh_keys=True)` so the
     NoCloud datasource places the pubkey in the guest's default user's
     `authorized_keys`.
  4. Boots, waits for cloud-init + sshd, SSHes in as the distro's
     default cloud-init user (`ubuntu`, `debian`, `alpine`, …), and
     asserts the pubkey landed where it should.

`test_cloud_init_crud` is the cheap one — no VM start; just the
daemon's set/get/delete round-trip.

`test_multiple_ssh_keys` and `test_custom_user_data_with_ssh_injection`
exercise the same machinery with the Gentoo `gentoo-base-headless`
image: the first attaches a second keypair on top of the primary;
the second supplies fully-custom user-data that creates a
`testadmin` user with sudo and an explicit `ssh_authorized_keys`.
"""

from __future__ import annotations

import secrets
import shutil
import subprocess
import tempfile
import textwrap
from pathlib import Path

import pytest
from corvus_test_harness import SingleNodeCase, VmCloudInit

pytestmark = [pytest.mark.slow]


def _uniq(stem: str) -> str:
    return f"{stem}-{secrets.token_hex(3)}"


class _CIBase(VmCloudInit):
    """Bigger defaults than the bare-Alpine baseline — cloud images
    do real package work at first boot (qemu-guest-agent, sshd
    configuration, …). 2 GiB is enough headroom for every distro in
    the matrix without slowing down the fast ones."""

    cpu_count = 2
    ram_mb = 2048


class TestCloudInit(SingleNodeCase):
    """Multi-distro cloud-init key injection + CRUD + custom user-data.

    All methods share one inner daemon and create + delete their own
    VMs under unique names so the methods are mutually independent.
    """

    # ---- verification helpers ----------------------------------------------

    def _verify_marker(self, vm, *, marker_path: str, marker_label: str) -> None:
        """Poll for a NoCloud-completion marker file inside the guest.

        Poll for up to ~20 min at 2 s intervals; cloud-init/nuageinit
        write their respective marker as soon as their final stage
        succeeds, so under normal conditions this returns within a
        second or two of SSH coming up.
        """
        poll = (
            f"i=0; "
            f"while [ $i -lt 60 ]; do "
            f"  test -f {marker_path} && exit 0; "
            f"  sleep 2; i=$((i+1)); "
            f"done; "
            f"exit 1"
        )
        bf = vm.run(poll, check=False, timeout_sec=180.0)
        assert bf.exit_code == 0, (
            f"{marker_label} never reached a terminal state — "
            f"{marker_path} did not appear within ~180 s of SSH coming up"
        )

    def _verify_authorized_key(self, vm) -> None:
        """The registered pubkey is in the default user's
        authorized_keys. Strip the trailing comment off the public
        key — `authorized_keys` may or may not preserve it depending
        on the cloud-init/nuageinit implementation."""
        pub_body = vm.public_key.split()[1]
        ak = vm.run("cat ~/.ssh/authorized_keys").stdout
        assert pub_body in ak, f"injected pubkey not found in authorized_keys:\n{ak!r}"

    def _verify_login_linux(self, vm) -> None:
        """Verify a cloud-init-based Linux distro (Alpine, AlmaLinux,
        Ubuntu, Debian, Gentoo): SSH'd in as `vm.ssh_user`, cloud-init
        wrote `/var/lib/cloud/instance/boot-finished` from its
        `cloud-final` stage, and the registered pubkey landed in
        ~/.ssh/authorized_keys."""
        r = vm.run("whoami")
        assert r.stdout.strip() == vm.ssh_user, r
        self._verify_marker(
            vm,
            marker_path="/var/lib/cloud/instance/boot-finished",
            marker_label="cloud-init",
        )
        self._verify_authorized_key(vm)

    def _verify_login_freebsd(self, vm) -> None:
        """Verify a FreeBSD vm. FreeBSD stock cloud images don't ship
        cloud-init proper — they use `nuageinit`, an in-base,
        C-implemented NoCloud processor enabled via
        `nuageinit_enable=YES` in `/etc/rc.conf`. nuageinit runs as a
        `firstboot` rc.d script with `BEFORE: NETWORKING`, writing its
        log to `/var/log/nuageinit.log` via `tee`; by the time SSH is
        reachable the file already exists, so its presence is
        equivalent to cloud-init's `boot-finished` marker."""
        r = vm.run("whoami")
        assert r.stdout.strip() == vm.ssh_user, r
        self._verify_marker(
            vm,
            marker_path="/var/log/nuageinit.log",
            marker_label="nuageinit",
        )
        self._verify_authorized_key(vm)

    # ---- per-distro key-injection tests ------------------------------------

    def test_alpine(self):
        class _AlpineCI(_CIBase):
            base_image_key = "alpine-3-21-base"

        with _AlpineCI(self) as vm:
            self._verify_login_linux(vm)

    def test_almalinux(self):
        class _AlmaLinuxCI(_CIBase):
            base_image_key = "almalinux-10-base"

        with _AlmaLinuxCI(self) as vm:
            self._verify_login_linux(vm)

    def test_ubuntu(self):
        class _UbuntuCI(_CIBase):
            base_image_key = "ubuntu-24-04-server-base"

        with _UbuntuCI(self) as vm:
            self._verify_login_linux(vm)

    def test_debian(self):
        class _DebianCI(_CIBase):
            base_image_key = "debian-12-generic-base"

        with _DebianCI(self) as vm:
            self._verify_login_linux(vm)

    def test_freebsd(self):
        class _FreeBsdCI(_CIBase):
            base_image_key = "freebsd-14-base"

        with _FreeBsdCI(self) as vm:
            self._verify_login_freebsd(vm)

    def test_gentoo(self):
        # gentoo-base-headless has cloud-init STRIPPED OUT (it's the
        # bake host for corvus-test-node which doesn't want cloud-init
        # at apply time). Use the cloudinit variant so cloud-init's
        # key-injection module actually runs on first boot.
        class _GentooCI(_CIBase):
            base_image_key = "gentoo-base-headless-cloudinit"

        with _GentooCI(self) as vm:
            self._verify_login_linux(vm)

    # ---- multiple SSH keys -------------------------------------------------

    def test_multiple_ssh_keys(self):
        """Attach two distinct keypairs; both end up in the guest's
        authorized_keys and both can independently authenticate over
        SSH."""

        class _GentooMulti(_CIBase):
            # cloudinit variant — see test_gentoo for the rationale.
            base_image_key = "gentoo-base-headless-cloudinit"

        key2_dir = Path(tempfile.mkdtemp(prefix="corvus-ci-key2-"))
        key2_priv = key2_dir / "id_ed25519"
        try:
            subprocess.run(
                [
                    "ssh-keygen",
                    "-t",
                    "ed25519",
                    "-f",
                    str(key2_priv),
                    "-N",
                    "",
                    "-C",
                    "corvus-ci-key2",
                ],
                check=True,
                stdout=subprocess.DEVNULL,
                stderr=subprocess.PIPE,
            )
            key2_priv.chmod(0o600)
            key2_pub = key2_priv.with_suffix(".pub").read_text().strip()

            ctx = _GentooMulti(self)
            ctx.extra_keys.append((_uniq("key2"), key2_pub))
            with ctx as vm:
                # Both pubkey bodies show up in authorized_keys.
                ak = vm.run("cat ~/.ssh/authorized_keys").stdout
                assert vm.public_key.split()[1] in ak, (
                    f"primary key missing from authorized_keys:\n{ak!r}"
                )
                assert key2_pub.split()[1] in ak, (
                    f"secondary key missing from authorized_keys:\n{ak!r}"
                )

                # The secondary private key also authenticates over
                # the same node-side hostfwd as the primary shell.
                with self.vm_shell(
                    vm.cap,
                    host_key_path=key2_priv,
                    user=vm.ssh_user,
                    vm_tcp_port=vm._tcp_port,
                ) as sh2:
                    sh2.wait_ready(timeout_sec=60)
                    r = sh2.run("echo key2-ok")
                    assert r.stdout.strip() == "key2-ok", r
        finally:
            shutil.rmtree(key2_dir, ignore_errors=True)

    # ---- cloud-init config CRUD (no boot) ----------------------------------

    def test_cloud_init_crud(self):
        """Pure daemon-side CRUD; no VM start.

        Creates a VM with `cloud_init=True`, asserts the initial
        config is empty, sets `user_data` + `network_config`, re-reads
        and confirms both fields, deletes and confirms reset.
        """
        name = _uniq("ci-crud")
        vm = self.client.vms.create(
            name,
            cpu_count=1,
            ram_mb=256,
            headless=True,
            cloud_init=True,
        )
        try:
            vm_id = vm.show().id

            info = self.client.cloud_init.get(vm_id)
            assert info.user_data is None
            assert info.network_config is None
            assert info.inject_ssh_keys is False

            user_data = "#cloud-config\nhostname: crud-test\n"
            network_config = "version: 2\nethernets:\n  eth0:\n    dhcp4: true\n"
            self.client.cloud_init.set(
                vm_id,
                user_data=user_data,
                network_config=network_config,
                inject_ssh_keys=False,
            )

            info = self.client.cloud_init.get(vm_id)
            assert info.user_data == user_data, info
            assert info.network_config == network_config, info
            assert info.inject_ssh_keys is False, info

            self.client.cloud_init.delete(vm_id)

            info = self.client.cloud_init.get(vm_id)
            assert info.user_data is None, info
            assert info.network_config is None, info
            assert info.inject_ssh_keys is False, info
        finally:
            try:
                vm.delete()
            except Exception:
                pass

    # ---- custom user-data + key injection ----------------------------------

    def test_custom_user_data_with_ssh_injection(self):
        """Supply fully-custom user-data that creates a `testadmin`
        user with sudo and an explicit `ssh_authorized_keys` entry.
        After boot, SSH in as `testadmin` and verify the user is in
        place + has passwordless sudo."""

        # Override `_cloud_init_config()` so the daemon writes our
        # `user_data` verbatim into the NoCloud ISO, replacing the
        # default `corvus`-user template. The `testadmin` user gets
        # the harness-generated pubkey directly under its own
        # `ssh_authorized_keys`, so authentication doesn't depend on
        # the daemon's `inject_ssh_keys` path.
        class _CustomCI(_CIBase):
            # cloudinit variant — see test_gentoo for the rationale.
            base_image_key = "gentoo-base-headless-cloudinit"
            ssh_user = "testadmin"

            def _cloud_init_config(_self):
                user_data = textwrap.dedent(f"""\
                    #cloud-config
                    users:
                      - name: testadmin
                        sudo: ALL=(ALL) NOPASSWD:ALL
                        plain_text_passwd: testpass
                        lock_passwd: false
                        shell: /bin/bash
                        ssh_authorized_keys:
                          - {_self.public_key}
                    ssh_pwauth: true
                """)
                return {"user_data": user_data, "inject_ssh_keys": False}

        with _CustomCI(self) as vm:
            r = vm.run("whoami")
            assert r.stdout.strip() == "testadmin", r

            # Passwordless sudo from the user-data took effect.
            r = vm.run("sudo -n true", check=False)
            assert r.exit_code == 0, (
                f"sudo -n failed for testadmin: stdout={r.stdout!r} stderr={r.stderr!r}"
            )
