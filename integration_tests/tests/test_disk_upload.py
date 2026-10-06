"""Client-local disk uploads traverse daemon and nodeagent intact."""

from __future__ import annotations

import hashlib
import secrets
import shlex
from pathlib import Path

from corvus_test_harness import SingleNodeCase


def _uniq(stem: str) -> str:
    return f"{stem}-{secrets.token_hex(4)}"


class TestDiskUpload(SingleNodeCase):
    """Exercise the public streaming upload API, including replacement."""

    def _assert_remote_digest(self, path: str, expected: str) -> None:
        result = self.node.run(f"sha256sum {shlex.quote(path)}")
        actual = result.stdout.decode().split()[0]
        assert actual == expected, (
            f"upload at {path!r} has digest {actual}, not {expected}"
        )

    def test_upload_from_file_places_exact_bytes_on_node(self, tmp_path: Path) -> None:
        name = _uniq("upload")
        source = tmp_path / "payload.raw"
        payload = secrets.token_bytes(1024 * 1024 + 137)
        source.write_bytes(payload)
        expected_digest = hashlib.sha256(payload).hexdigest()

        disk = self.client.disks.upload_from_file(name, source, format="raw")
        try:
            info = disk.show()
            assert info.name == name
            assert info.format == "raw"
            assert len(info.placements) == 1
            assert info.placements[0].node.name == self.node.short_name
            self._assert_remote_digest(info.placements[0].file_path, expected_digest)
        finally:
            disk.delete()

    def test_upload_publishes_versions_and_preserves_old_bytes(
        self, tmp_path: Path
    ) -> None:
        name = _uniq("upload-version")
        first = tmp_path / "first.raw"
        second = tmp_path / "second.raw"
        first_payload = secrets.token_bytes(256 * 1024 + 11)
        second_payload = secrets.token_bytes(256 * 1024 + 29)
        first.write_bytes(first_payload)
        second.write_bytes(second_payload)
        old = self.client.disks.upload_from_file(name + ":v1", first, format="raw")
        new = None
        try:
            new = self.client.disks.upload_from_file(name + ":v2", second, format="raw")
            old_info, new_info = old.show(), new.show()
            assert (
                Path(old_info.placements[0].file_path).name
                == f"{old_info.id}-{name}:v1.raw"
            )
            assert (
                Path(new_info.placements[0].file_path).name
                == f"{new_info.id}-{name}:v2.raw"
            )
            assert old_info.id != new_info.id
            assert old_info.tags == ["v1"]
            assert new_info.tags == ["latest", "v2"]
            assert self.client.disks.get(name).show().id == new_info.id
            self._assert_remote_digest(
                old_info.placements[0].file_path,
                hashlib.sha256(first_payload).hexdigest(),
            )
            self._assert_remote_digest(
                new_info.placements[0].file_path,
                hashlib.sha256(second_payload).hexdigest(),
            )
            new.tag("stable")
            old.tag("stable")
            assert self.client.disks.get(name + ":stable").show().id == old_info.id
            new.delete()
            new = None
            assert self.client.disks.get(name).show().id == old_info.id
        finally:
            if new is not None:
                new.delete()
            old.delete()

    def test_template_selectors_follow_tags_and_pin_ids(self) -> None:
        name = _uniq("tag-template-source")
        old = self.client.disks.create(name + ":v1", size=1048576)
        old_id = old.show().id
        templates = []
        vms = []
        new = None
        try:
            for label, selector in [
                ("latest", name),
                ("v1", name + ":v1"),
                ("id", old_id),
            ]:
                templates.append(
                    self.client.templates.create(
                        f"name: {_uniq(label)}\ncpuCount: 1\nram: 128M\ndrives:\n"
                        f"  - diskImage: {selector}\n    strategy: overlay\n    interface: virtio\n"
                    )
                )
            before = templates[0].instantiate(_uniq("before-publish"))
            vms.append(before)
            new = self.client.disks.create(name + ":v2", size=2097152)
            for template, expected in zip(
                templates, [new.show().id, old_id, old_id], strict=True
            ):
                vm = template.instantiate(_uniq("from-template"))
                vms.append(vm)
                source = vm.show().drives[0].disk_image
                assert source is not None
                backing = self.client.disks.get(source.id).show().backing_image
                assert backing is not None and backing.id == expected
            source = before.show().drives[0].disk_image
            assert source is not None
            backing = self.client.disks.get(source.id).show().backing_image
            assert backing is not None and backing.id == old_id
        finally:
            for vm in reversed(vms):
                vm.delete()
            for template in templates:
                template.delete()
            if new is not None:
                new.delete()
            old.delete()
