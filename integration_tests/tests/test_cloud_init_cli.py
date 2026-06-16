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
import subprocess
import tempfile
import textwrap
from pathlib import Path

import pytest
from corvus_client.exceptions import CorvusError
from corvus_test_harness import SingleNodeCase


def _generate_ed25519_pubkey(comment: str) -> str:
    """Mint a fresh ed25519 keypair via ``ssh-keygen`` and return the
    public key as the single-line ``ssh-ed25519 AAAA… <comment>``
    string. cloud-init's user-data builder runs basic syntactic
    checks on attached SSH keys; using a real, well-formed key
    sidesteps the validator's silent-drop path so the test can
    assert the key actually lands in the ISO."""
    with tempfile.TemporaryDirectory(prefix="corvus-ci-key-") as d:
        priv = Path(d) / "id_ed25519"
        subprocess.run(
            ["ssh-keygen", "-t", "ed25519", "-f", str(priv), "-N", "", "-C", comment],
            check=True,
            stdout=subprocess.DEVNULL,
            stderr=subprocess.PIPE,
        )
        return (priv.with_suffix(".pub")).read_text().strip()


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

    def test_attach_ssh_key_regenerates_iso(self):
        """Attaching an SSH key to a cloud-init VM is supposed to
        regenerate the NoCloud ISO so the next boot picks up the
        new authorized_keys. Without an explicit regenerate after
        the attach, the key would only show up on whatever next
        action triggers an ISO build — surprising operator UX.

        Generate an initial ISO (no keys), capture its mtime, then
        attach an SSH key; the ISO file must be replaced (newer
        mtime) and the new file must contain the public key as
        plain ASCII inside iso9660."""
        name = f"corvus-it-ci-key-{secrets.token_hex(3)}"
        key_name = f"corvus-it-ci-key-{secrets.token_hex(3)}"
        public_key = _generate_ed25519_pubkey(f"ci-it-key-{secrets.token_hex(4)}")
        vm = self._make_ci_vm(name)
        try:
            vm_id = vm.show().id
            # Initial ISO with no key injection.
            # NOTE: user_data MUST be structured YAML without the
            # ``#cloud-config`` header — ``isRawUserDataScript`` at
            # src/Corvus/CloudInit.hs:131 treats ANY ``#``-prefixed
            # user-data as a "raw script" and skips key injection,
            # so providing ``#cloud-config`` would defeat the very
            # path we're testing. The daemon prepends its own
            # header before writing the ISO.
            self.client.cloud_init.set(
                vm_id,
                user_data="hostname: base\n",
                inject_ssh_keys=True,
            )
            _assert_ok(_crv(self.node, f"cloud-init generate {name}"))
            iso = self.client.disks.get(f"{name}-cloud-init", by_name=True).show()
            iso_path = iso.placements[0].file_path
            mtime_before = (
                self.node.run(f"stat -c %Y {shlex.quote(iso_path)}")
                .stdout.decode()
                .strip()
            )

            # Sleep BEFORE the attach so the regenerate's
            # write-mtime lands in a different second than the
            # initial generate. ``stat -c %Y`` resolution is one
            # second; without this gap the test flakes when both
            # generates happen in the same wall-clock second.
            import time

            time.sleep(1.2)

            # Attach an SSH key. The daemon's `attach_ssh_key`
            # handler at src/Corvus/Handlers/SshKey.hs:189-197
            # synchronously runs ``RegenerateCloudInit`` after
            # persisting the association, so the on-disk ISO
            # must be replaced before this call returns.
            self.client.ssh_keys.create(key_name, public_key)
            try:
                vm.attach_ssh_key(key_name)
                mtime_after = (
                    self.node.run(f"stat -c %Y {shlex.quote(iso_path)}")
                    .stdout.decode()
                    .strip()
                )
                assert int(mtime_after) > int(mtime_before), (
                    f"ISO mtime did not advance across attach_ssh_key "
                    f"({mtime_before} → {mtime_after}); the daemon "
                    f"didn't regenerate the NoCloud ISO"
                )

                # ASCII fragment of the public key survives in the
                # iso9660 bytes (no compression).
                key_fragment = public_key.split()[1][:32]
                cp = self.node.run(
                    f"grep -ao {shlex.quote(key_fragment)} {shlex.quote(iso_path)}",
                    check=False,
                )
                assert key_fragment in cp.stdout.decode(), (
                    f"public key not found in regenerated ISO; "
                    f"grep stdout={cp.stdout!r}, stderr={cp.stderr!r}"
                )
            finally:
                try:
                    vm.detach_ssh_key(key_name)
                except Exception:
                    pass
                try:
                    self.client.ssh_keys.get(key_name).delete()
                except Exception:
                    pass
        finally:
            try:
                vm.delete()
            except Exception:
                pass

    def test_inject_ssh_keys_false_does_not_inject(self):
        """``injectSshKeys: false`` MUST keep attached keys out of
        the generated ISO. An operator that opts out should never
        find their pubkeys baked into the user-data. Catches a
        regression that would silently bypass the flag."""
        name = f"corvus-it-ci-noinj-{secrets.token_hex(3)}"
        key_name = f"corvus-it-ci-noinj-{secrets.token_hex(3)}"
        public_key = _generate_ed25519_pubkey(f"ci-it-noinj-{secrets.token_hex(4)}")
        # Match a base64 fragment that's stable per-key — the comment
        # part may get stripped by the user-data normalizer.
        marker = public_key.split()[1][-32:]
        vm = self._make_ci_vm(name)
        try:
            vm_id = vm.show().id
            # Structured YAML (no ``#`` prefix) so the daemon's
            # ``injectSshKeysIntoYaml`` path runs at all — the
            # ``inject_ssh_keys=False`` flag is what we're
            # asserting it respects.
            self.client.cloud_init.set(
                vm_id,
                user_data="hostname: no-inject\n",
                inject_ssh_keys=False,
            )
            self.client.ssh_keys.create(key_name, public_key)
            try:
                vm.attach_ssh_key(key_name)
                _assert_ok(_crv(self.node, f"cloud-init generate {name}"))
                iso = self.client.disks.get(f"{name}-cloud-init", by_name=True).show()
                iso_path = iso.placements[0].file_path
                cp = self.node.run(
                    f"grep -ao {shlex.quote(marker)} {shlex.quote(iso_path)}",
                    check=False,
                )
                assert marker not in cp.stdout.decode(), (
                    f"key fragment {marker!r} present in ISO despite "
                    f"injectSshKeys=false: grep={cp.stdout!r}"
                )
            finally:
                try:
                    vm.detach_ssh_key(key_name)
                except Exception:
                    pass
                try:
                    self.client.ssh_keys.get(key_name).delete()
                except Exception:
                    pass
        finally:
            try:
                vm.delete()
            except Exception:
                pass

    def test_raw_script_user_data_skips_key_injection(self):
        """When ``userData`` starts with ``#!`` (a raw script
        body, not a ``#cloud-config`` document), the daemon's
        cloud-init builder can't safely splice ``users:`` into it,
        so attached SSH keys must NOT be injected — operator's raw
        script wins. Documented in ``doc/cloud-init.md`` (the
        "Raw script user-data" subsection) and pinned here so a
        refactor of the user-data classifier flags."""
        name = f"corvus-it-ci-raw-{secrets.token_hex(3)}"
        key_name = f"corvus-it-ci-raw-{secrets.token_hex(3)}"
        public_key = _generate_ed25519_pubkey(f"ci-it-raw-{secrets.token_hex(4)}")
        marker = public_key.split()[1][-32:]
        vm = self._make_ci_vm(name)
        try:
            vm_id = vm.show().id
            # ``#!`` line is the canonical raw-script marker.
            self.client.cloud_init.set(
                vm_id,
                user_data="#!/bin/sh\necho hello\n",
                inject_ssh_keys=True,  # injection allowed in config…
            )
            self.client.ssh_keys.create(key_name, public_key)
            try:
                vm.attach_ssh_key(key_name)
                _assert_ok(_crv(self.node, f"cloud-init generate {name}"))
                iso = self.client.disks.get(f"{name}-cloud-init", by_name=True).show()
                iso_path = iso.placements[0].file_path
                cp = self.node.run(
                    f"grep -ao {shlex.quote(marker)} {shlex.quote(iso_path)}",
                    check=False,
                )
                # …but the raw-script form blocks the injection.
                assert marker not in cp.stdout.decode(), (
                    f"key fragment {marker!r} present in ISO despite "
                    f"raw-script user-data; grep={cp.stdout!r}"
                )
            finally:
                try:
                    vm.detach_ssh_key(key_name)
                except Exception:
                    pass
                try:
                    self.client.ssh_keys.get(key_name).delete()
                except Exception:
                    pass
        finally:
            try:
                vm.delete()
            except Exception:
                pass

    def test_generate_is_idempotent_at_disk_record_level(self):
        """Re-running ``crv cloud-init generate`` doesn't break the
        existing ``<vm>-cloud-init`` disk record — same id, same
        placement path, no second disk row spawned. The on-disk
        bytes ARE allowed to differ (genisoimage embeds a wall-
        clock creation timestamp into iso9660 volume metadata), so
        this test asserts the operator-facing invariant (no
        double-registration / no ghost rows) rather than byte
        equality.

        A byte-identical re-generate would be nicer but needs the
        generator to use a fixed creation-time; flagged in the
        docstring above for a future enhancement."""
        name = f"corvus-it-ci-idemp-{secrets.token_hex(3)}"
        vm = self._make_ci_vm(name)
        try:
            self.client.cloud_init.set(
                vm.show().id,
                user_data="hostname: idemp\n",
                inject_ssh_keys=False,
            )
            _assert_ok(_crv(self.node, f"cloud-init generate {name}"))
            iso = self.client.disks.get(f"{name}-cloud-init", by_name=True).show()
            iso_id_a = iso.id
            iso_path_a = iso.placements[0].file_path

            _assert_ok(_crv(self.node, f"cloud-init generate {name}"))
            iso2 = self.client.disks.get(f"{name}-cloud-init", by_name=True).show()
            assert iso2.id == iso_id_a, (
                f"second generate spawned a new disk row "
                f"({iso_id_a} → {iso2.id}); the daemon should reuse "
                f"the existing ``<vm>-cloud-init`` record"
            )
            assert iso2.placements[0].file_path == iso_path_a, (
                f"second generate moved the on-disk file "
                f"({iso_path_a} → {iso2.placements[0].file_path})"
            )
            # Exactly one cloud-init disk per VM — no ghost rows.
            cloud_init_disks = [
                d for d in self.client.disks.list() if d.name == f"{name}-cloud-init"
            ]
            assert len(cloud_init_disks) == 1, cloud_init_disks
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
