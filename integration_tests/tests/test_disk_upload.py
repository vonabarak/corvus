"""Client-local disk uploads traverse daemon and nodeagent intact."""

from __future__ import annotations

import hashlib
import secrets
import shlex

import pytest
from corvus_client.exceptions import CorvusError
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

    def test_upload_from_file_places_exact_bytes_on_node(self, tmp_path):
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

    def test_upload_rejects_duplicate_unless_overwrite_then_replaces_bytes(
        self, tmp_path
    ):
        name = _uniq("upload-overwrite")
        first = tmp_path / "first.raw"
        second = tmp_path / "second.raw"
        first_payload = secrets.token_bytes(256 * 1024 + 11)
        second_payload = secrets.token_bytes(256 * 1024 + 29)
        first.write_bytes(first_payload)
        second.write_bytes(second_payload)

        disk = self.client.disks.upload_from_file(name, first, format="raw")
        try:
            first_info = disk.show()
            path = first_info.placements[0].file_path
            self._assert_remote_digest(path, hashlib.sha256(first_payload).hexdigest())

            with pytest.raises(CorvusError):
                self.client.disks.upload_from_file(name, second, format="raw")
            self._assert_remote_digest(path, hashlib.sha256(first_payload).hexdigest())

            replacement = self.client.disks.upload_from_file(
                name, second, format="raw", overwrite=True
            )
            info = replacement.show()
            assert info.id == first_info.id
            assert (
                len([item for item in self.client.disks.list() if item.name == name])
                == 1
            )
            assert len(info.placements) == 1
            self._assert_remote_digest(
                info.placements[0].file_path, hashlib.sha256(second_payload).hexdigest()
            )
        finally:
            disk.delete()
