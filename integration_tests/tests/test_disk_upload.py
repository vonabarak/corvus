"""Client-local image uploads against the real daemon and nodeagent."""

from __future__ import annotations

import hashlib
import json
import os
import secrets
import shlex
from pathlib import Path

import pytest
from corvus_client import DiskNotFound
from corvus_client._schema import disk as schema
from corvus_client.exceptions import CorvusError
from corvus_test_harness import SingleNodeCase, SqliteDatabase


def _uniq(stem: str) -> str:
    return f"{stem}-{secrets.token_hex(4)}"


class TestDiskUpload(SingleNodeCase):
    def _bytes(self, path: str) -> bytes:
        return self.node.run(shlex.join(["cat", "--", path])).stdout

    def test_conditional_upload_policies_and_metadata(self, tmp_path: Path) -> None:
        """Verified bytes, rather than path/size/mtime, drive conditional reuse.

        Exercise the selected publication tag independently of latest, metadata
        persistence, old uploads without metadata, and immutable older files.
        Error and skip must decide before touching the requested destination.
        """
        name = f"upload-{secrets.token_hex(4)}"
        source = tmp_path / "first.raw"
        source.write_bytes(b"a" * 4096)
        original_stat = source.stat()
        first = self.client.disks.upload_from_file(
            name + ":stable", source, format="raw"
        )
        info = first.show()
        destination = info.placements[0].file_path
        with SqliteDatabase(self.node) as database:
            assert database.query(
                "SELECT digest, source_path FROM disk_image_upload_identity WHERE disk_image_id = ?",
                (info.id,),
            ) == [
                [hashlib.sha256(source.read_bytes()).hexdigest(), str(source.resolve())]
            ]
            renamed = tmp_path / "renamed.raw"
            renamed.write_bytes(source.read_bytes())
            reused = self.client.disks.upload_from_file(
                name + ":stable",
                renamed,
                format="raw",
                if_exists="update",
                path="/unwritable/answer.raw",
                node="missing-node",
                ephemeral=True,
            )
            assert reused.show().id == info.id
            assert reused.show().ephemeral is False
            with pytest.raises(CorvusError, match="already exists"):
                self.client.disks.upload_from_file(
                    name + ":stable",
                    source,
                    format="raw",
                    if_exists="error",
                    path=destination,
                )
            skipped = self.client.disks.upload_from_file(
                name + ":stable",
                tmp_path / "absent",
                format="qcow2",
                if_exists="skip",
                path=destination,
            )
            assert skipped.show().id == info.id
            # The CLI also lets skip reuse an image with no local source file.
            self.install_node_client_certs()
            cli = self.node.run(
                shlex.join(
                    [
                        "/opt/corvus/bin/crv",
                        "--output",
                        "json",
                        "disk",
                        "upload",
                        name + ":stable",
                        "/absent-upload-source.raw",
                        "--format",
                        "raw",
                        "--if-exists",
                        "skip",
                    ]
                ),
                check=False,
            )
            assert cli.returncode == 0, (cli.stdout, cli.stderr)
            assert json.loads(cli.stdout)["id"] == info.id
            assert self._bytes(destination) == b"a" * 4096
            source.write_bytes(b"b" * 4096)
            os.utime(source, ns=(original_stat.st_atime_ns, original_stat.st_mtime_ns))
            second = self.client.disks.upload_from_file(
                name + ":other", source, format="raw", if_exists="update"
            )
            assert second.show().id != info.id
            # Same stable-tag payload still reuses stable, without moving latest.
            stable = self.client.disks.upload_from_file(
                name + ":stable", renamed, format="raw", if_exists="update"
            )
            assert stable.show().id == info.id
            assert self.client.disks.get(name).show().id == second.show().id
            changed = self.client.disks.upload_from_file(
                name + ":stable", source, format="raw", if_exists="update"
            )
            assert changed.show().id != second.show().id
            assert self._bytes(destination) == b"a" * 4096
            assert (
                self.client.disks.upload_from_file(
                    name, source, format="raw", if_exists="update"
                )
                .show()
                .id
                == changed.show().id
            )
            # A pre-metadata image is republished once, then conditional reuse works.
            database.execute(
                "DELETE FROM disk_image_upload_identity WHERE disk_image_id = ?",
                (changed.show().id,),
            )
            refreshed = self.client.disks.upload_from_file(
                name, source, format="raw", if_exists="update"
            )
            assert refreshed.show().id != changed.show().id
            assert (
                self.client.disks.upload_from_file(
                    name, source, format="raw", if_exists="update"
                )
                .show()
                .id
                == refreshed.show().id
            )
            for disk in self.client.disks.list():
                if disk.name == name:
                    self.client.disks.get(disk.id).delete()
            assert database.query(
                "SELECT COUNT(*) FROM disk_image_upload_identity WHERE disk_image_id = ?",
                (info.id,),
            ) == [[0]]

    def test_collision_and_format_failure_preserve_publication(
        self, tmp_path: Path
    ) -> None:
        name = f"upload-failure-{secrets.token_hex(4)}"
        source = tmp_path / "source.raw"
        source.write_bytes(b"first version" * 1024)
        old = self.client.disks.upload_from_file(name + ":v1", source, format="raw")
        info = old.show()
        destination = info.placements[0].file_path
        try:
            source.write_bytes(b"replacement" * 1024)
            with pytest.raises(CorvusError):
                self.client.disks.upload_from_file(
                    name + ":v2", source, format="raw", path=destination
                )
            assert self._bytes(destination) == b"first version" * 1024
            assert self.client.disks.get(name).show().id == info.id
            with pytest.raises(DiskNotFound):
                self.client.disks.get(name + ":v2")
            with pytest.raises(CorvusError, match="format differs"):
                self.client.disks.upload_from_file(
                    name, source, format="qcow2", if_exists="update"
                )
            assert self.client.disks.get(name).show().id == info.id
        finally:
            old.delete()

    def test_digest_mismatch_and_abort_do_not_publish(self) -> None:
        name = f"upload-digest-{secrets.token_hex(4)}"

        async def exercise() -> None:
            manager = await self.client._a.disks._ensure()
            params = schema.DiskUploadParams.new_message()
            params.name = name
            params.format = "raw"
            params.expectedSha256 = hashlib.sha256(b"expected").hexdigest()
            response = await manager.beginUpload(params=params)
            upload = response.result.upload
            await upload.write(chunk=b"actual")
            with pytest.raises(Exception, match="expectedSha256"):
                await upload.finish()
            with pytest.raises(Exception, match="closed"):
                await upload.write(chunk=b"late")
            await upload.abort()
            response = await manager.beginUpload(params=params)
            await response.result.upload.write(chunk=b"expected")
            await response.result.upload.abort()
            with pytest.raises(Exception, match="closed"):
                await response.result.upload.finish()

        self.client._rl.run(exercise())
        with pytest.raises(DiskNotFound):
            self.client.disks.get(name)
        with SqliteDatabase(self.node) as database:
            assert database.query(
                "SELECT COUNT(*) FROM disk_image WHERE name = ?", (name,)
            ) == [[0]]

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
