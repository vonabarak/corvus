"""Cloud-init CLI verbs + networkConfig coverage.

Closes a gap: `test_cloud_init.py` covers the typed
``client.cloud_init.set/get/delete`` round-trip and SSH-key
injection at boot, but the user-facing CLI verbs
(``crv cloud-init set/show/edit/delete/generate``) and the
``networkConfig`` payload have no direct test.

All tests here drive the inner daemon via SSH-to-test-node
(``self.node.run("/opt/corvus/bin/crv …")``) for the CLI surface
and the typed RPC client for the assertions.  No VM boots — every
test creates a stopped cloud-init VM, exercises the config
machinery, and tears the VM down.  The only test that needs the
on-disk ISO greps through it via ``grep -ao`` over the
already-registered ``<vm>-cloud-init`` disk image.
"""

from __future__ import annotations

import base64
import secrets
import shlex
import textwrap

import pytest
from corvus_client.exceptions import CorvusError
from corvus_test_harness import SingleNodeCase

_NET_MARKER_IP = "192.0.2.42"
_USER_DATA_MARKER = "corvus-it-userdata-marker"


def _crv(node, args: str, *, check: bool = False):
    """Run `crv <args>` on the test-node and return CompletedProcess.

    Defaults to ``check=False`` so callers can assert on
    ``returncode`` with stdout/stderr in the diagnostic message —
    otherwise SSH-piped failures surface only as an opaque
    ``CalledProcessError`` with no visible output."""
    return node.run(f"/opt/corvus/bin/crv {args}", check=check)


def _assert_ok(cp, *, what: str = "crv") -> None:
    """Assert a CompletedProcess exit 0; include captured streams
    in the diagnostic so SSH-piped failures are debuggable."""
    assert cp.returncode == 0, (
        f"{what} exited {cp.returncode}\n"
        f"stdout={cp.stdout.decode(errors='replace')!r}\n"
        f"stderr={cp.stderr.decode(errors='replace')!r}"
    )


def _write_file(node, path: str, content: str) -> None:
    """Stage ``content`` at ``path`` on the test-node.

    Base64-pipes the payload so embedded quotes, newlines and
    backticks in YAML bodies don't collide with ssh + ``sh -c``
    quoting layers."""
    b64 = base64.b64encode(content.encode("utf-8")).decode("ascii")
    node.run(f"sh -c {shlex.quote(f'printf %s {b64} | base64 -d > {path}')}")


class TestCloudInitCli(SingleNodeCase):
    """Direct coverage of `crv cloud-init` verbs + networkConfig."""

    @pytest.fixture(scope="class", autouse=True)
    def _install_client_certs(self, _class_topology):
        """The inner ``/opt/corvus/bin/crv`` calls below dial the
        daemon over mTLS; install the client cert trio under the
        corvus user's XDG config dir before any test runs."""
        self.install_node_client_certs()

    def _make_ci_vm(self, name: str):
        """Create a stopped cloud-init VM. Caller is responsible
        for deleting it via the returned cap."""
        return self.client.vms.create(
            name,
            cpu_count=1,
            ram_mb=256,
            headless=True,
            cloud_init=True,
        )

    def test_crv_cloud_init_set_and_show(self):
        """`crv cloud-init set <vm> <file.yml>` writes the config;
        `crv cloud-init show <vm>` prints it back.

        Round-trips both userData and injectSshKeys through the CLI,
        not just the typed RPC."""
        name = f"corvus-it-ci-set-{secrets.token_hex(3)}"
        vm = self._make_ci_vm(name)
        yaml_path = f"/tmp/{name}.yml"
        try:
            yaml_body = textwrap.dedent(f"""
                injectSshKeys: false
                userData: |
                  #cloud-config
                  hostname: {_USER_DATA_MARKER}
            """).strip()
            _write_file(self.node, yaml_path, yaml_body)
            _assert_ok(_crv(self.node, f"cloud-init set {name} {yaml_path}"))

            # Typed RPC sees the same record.
            info = self.client.cloud_init.get(vm.show().id)
            assert info.inject_ssh_keys is False, info
            assert info.user_data is not None
            assert _USER_DATA_MARKER in info.user_data, info.user_data

            # `crv cloud-init show` prints the user-data verbatim.
            show_cp = _crv(self.node, f"cloud-init show {name}")
            _assert_ok(show_cp)
            shown = show_cp.stdout.decode()
            assert _USER_DATA_MARKER in shown, shown
            assert "Inject SSH Keys: False" in shown, shown
        finally:
            self.node.run(f"rm -f {yaml_path}", check=False)
            try:
                vm.delete()
            except Exception:
                pass

    def test_crv_cloud_init_delete_reverts_to_default(self):
        """`crv cloud-init delete` strips the custom config; a
        second delete is a no-op (idempotent — the daemon doesn't
        error on "delete what isn't there")."""
        name = f"corvus-it-ci-del-{secrets.token_hex(3)}"
        vm = self._make_ci_vm(name)
        try:
            vm_id = vm.show().id
            self.client.cloud_init.set(
                vm_id,
                user_data="#cloud-config\nhostname: to-be-deleted\n",
                inject_ssh_keys=True,
            )
            assert self.client.cloud_init.get(vm_id).user_data is not None

            cp = _crv(self.node, f"cloud-init delete {name}")
            _assert_ok(cp)
            assert "deleted" in cp.stdout.decode().lower(), cp.stdout

            info = self.client.cloud_init.get(vm_id)
            assert info.user_data is None, info
            assert info.network_config is None, info
            assert info.inject_ssh_keys is False, info

            # Second delete: daemon may or may not error; the test
            # is just that the typed view still shows "default".
            _crv(self.node, f"cloud-init delete {name}")
            info = self.client.cloud_init.get(vm_id)
            assert info.user_data is None, info
        finally:
            try:
                vm.delete()
            except Exception:
                pass

    def test_show_default_message_when_no_custom_config(self):
        """`crv cloud-init show` on a VM that has no custom config
        prints the sentinel `Using default cloud-init configuration.`
        documented in `src/Corvus/Client/Commands/CloudInit.hs`."""
        name = f"corvus-it-ci-default-{secrets.token_hex(3)}"
        vm = self._make_ci_vm(name)
        try:
            cp = _crv(self.node, f"cloud-init show {name}")
            _assert_ok(cp)
            text = cp.stdout.decode()
            assert "default" in text.lower(), text
        finally:
            try:
                vm.delete()
            except Exception:
                pass

    def test_generate_registers_iso_disk(self):
        """`crv cloud-init generate` produces the NoCloud ISO and
        the daemon registers it as the ``<vm>-cloud-init`` disk —
        the same disk the VM start path would normally produce.

        Also verifies a custom networkConfig payload lands in the
        ISO bytes: the daemon writes it as a `network-config`
        file inside the iso9660 image, so a literal marker string
        survives in the raw bytes."""
        name = f"corvus-it-ci-gen-{secrets.token_hex(3)}"
        vm = self._make_ci_vm(name)
        try:
            vm_id = vm.show().id
            network_config = textwrap.dedent(f"""
                version: 2
                ethernets:
                  eth0:
                    addresses:
                      - {_NET_MARKER_IP}/24
            """).strip()
            self.client.cloud_init.set(
                vm_id,
                user_data=(f"#cloud-config\nhostname: {_USER_DATA_MARKER}\n"),
                network_config=network_config,
                inject_ssh_keys=False,
            )

            # Driving via the CLI verb (not the typed regenerate
            # path) so this test catches a CLI-only regression
            # if `handleCloudInitGenerate` stops calling
            # `vm.cloudInit`.
            _assert_ok(_crv(self.node, f"cloud-init generate {name}"))

            iso_disk_name = f"{name}-cloud-init"
            iso_disk = self.client.disks.get(iso_disk_name, by_name=True).show()
            # Daemon side: ISO disk registered, format raw,
            # ephemeral, placement on the self node.
            assert iso_disk.format == "raw", iso_disk
            assert iso_disk.ephemeral is True, iso_disk
            assert iso_disk.placements, iso_disk
            iso_path = iso_disk.placements[0].file_path
            # Read the ISO file on the test-node and grep for the
            # two markers. iso9660 doesn't compress, so literal
            # ASCII payloads survive in the raw bytes.
            grep_cmd = (
                f"grep -ao -e {shlex.quote(_USER_DATA_MARKER)} "
                f"-e {shlex.quote(_NET_MARKER_IP)} {shlex.quote(iso_path)}"
            )
            cp = self.node.run(grep_cmd, check=False)
            out = cp.stdout.decode()
            assert _USER_DATA_MARKER in out, (cp.returncode, out, cp.stderr.decode())
            assert _NET_MARKER_IP in out, (cp.returncode, out, cp.stderr.decode())
        finally:
            try:
                vm.delete()
            except Exception:
                pass

    def test_set_rejected_on_non_cloud_init_vm(self):
        """`crv cloud-init set` against a VM with cloudInit=false
        is refused at the handler — surfaces as a non-zero exit
        from the CLI and a typed `CorvusError` from the RPC."""
        name = f"corvus-it-ci-noci-{secrets.token_hex(3)}"
        vm = self.client.vms.create(
            name,
            cpu_count=1,
            ram_mb=256,
            headless=True,
            cloud_init=False,
        )
        yaml_path = f"/tmp/{name}.yml"
        try:
            # Direct typed call surfaces the daemon's structured
            # error.
            with pytest.raises(CorvusError) as excinfo:
                self.client.cloud_init.set(
                    vm.show().id,
                    user_data="#cloud-config\nhostname: nope\n",
                    inject_ssh_keys=False,
                )
            msg = str(excinfo.value).lower()
            assert "cloud-init" in msg or "cloudinit" in msg, msg

            # CLI invocation must reach the same failure path —
            # non-zero exit, no daemon side-effect.
            _write_file(self.node, yaml_path, "injectSshKeys: false\n")
            cp = _crv(self.node, f"cloud-init set {name} {yaml_path}", check=False)
            assert cp.returncode != 0, (cp.stdout, cp.stderr)
        finally:
            self.node.run(f"rm -f {yaml_path}", check=False)
            try:
                vm.delete()
            except Exception:
                pass
