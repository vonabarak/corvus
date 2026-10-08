"""Disk imports against the real daemon and nodeagent in a nested test node."""

from __future__ import annotations

import hashlib
import lzma
import secrets
import shlex
from http.server import BaseHTTPRequestHandler, HTTPServer
from pathlib import Path
from threading import Lock, Thread
from types import TracebackType

import pytest
from corvus_client.exceptions import CorvusError
from corvus_client.types import (
    ApplyEnd,
    ApplyEntityStart,
    ApplyStreamItem,
    BuildPipelineEnd,
    BuildStepStart,
    BuildStreamItem,
)
from corvus_test_harness import SingleNodeCase, SqliteDatabase, TestNode
from corvus_test_harness.runner import NodeShellRunner
from corvus_test_harness.ssh import HOST_ALPINE_KEY_PATH, NodeShell

import yaml


def _uniq(stem: str) -> str:
    return f"{stem}-{secrets.token_hex(3)}"


def _node_bytes(node: TestNode, path: str) -> bytes:
    """Read an inner-node placement without interpreting its path on the host."""
    return node.run(shlex.join(["cat", "--", path])).stdout


class _ImportServer:
    """Host-side HTTP source specific to these disk-import scenarios."""

    def __init__(self, node: TestNode, *, suffix: str = ".qcow2") -> None:
        self.host = node.host_ip
        self.suffix = suffix
        self.url = ""
        self._body = b""
        self._requests: list[str] = []
        self._lock = Lock()
        self._server: HTTPServer | None = None
        self._thread: Thread | None = None

    def __enter__(self) -> _ImportServer:
        source = self

        class Handler(BaseHTTPRequestHandler):
            def setup(self) -> None:
                super().setup()
                self.connection.settimeout(5)

            def do_GET(self) -> None:
                with source._lock:
                    source._requests.append(self.path)
                    body = source._body
                if self.path.startswith("/redirect/"):
                    self.send_response(302)
                    self.send_header("Location", "/artifact" + source.suffix)
                    self.send_header("Content-Length", "0")
                    self.end_headers()
                    return
                self.send_response(200)
                self.send_header("Content-Length", str(len(body)))
                self.end_headers()
                self.wfile.write(body)

            def log_message(self, format: str, *args: object) -> None:
                pass

        # HTTPServer binds and listens during construction, so no remote
        # readiness script or request that affects the GET count is needed.
        server = HTTPServer((self.host, 0), Handler)
        thread = Thread(target=server.serve_forever, daemon=True)
        try:
            thread.start()
        except BaseException:
            server.server_close()
            raise
        self._server = server
        self._thread = thread
        self.url = f"http://{self.host}:{server.server_port}"
        return self

    def __exit__(
        self,
        exc_type: type[BaseException] | None,
        exc_value: BaseException | None,
        traceback: TracebackType | None,
    ) -> None:
        if self._server is not None:
            try:
                self._server.shutdown()
            finally:
                self._server.server_close()
        if self._thread is not None:
            self._thread.join(timeout=5)
            if self._thread.is_alive():
                raise RuntimeError("Import HTTP server did not stop")

    def set_payload(self, body: bytes) -> None:
        """Atomically replace the served release between import attempts."""
        with self._lock:
            self._body = body

    def request_count(self) -> int:
        with self._lock:
            return len(self._requests)


class TestDiskImport(SingleNodeCase):
    """Import, version publication, decompression, and conditional updates."""

    def _delete_versions_silent(self, *names: str) -> None:
        """Clean up superseded versions as well as the currently selected one."""
        logical_names = {name.split(":", 1)[0] for name in names}
        try:
            images = self.client.disks.list()
        except Exception:
            return
        for image in images:
            if image.name in logical_names:
                try:
                    self.client.disks.get(image.id).delete()
                except Exception:
                    pass

    @staticmethod
    def _import_url(database: SqliteDatabase, image_id: int) -> str:
        rows = database.query(
            "SELECT import_url FROM disk_image_import_identity WHERE disk_image_id = ?",
            (image_id,),
        )
        url = rows[0][0] if rows else None
        assert isinstance(url, str), f"no import URL recorded for image {image_id}"
        return url

    def _build_with_url(self, disk: dict[str, object], url: str) -> None:
        """Run a CLI build with an expanded URL in an independently owned directory."""
        directory = (
            self.node.run("mktemp -d /tmp/corvus-it-build.XXXXXX")
            .stdout.decode()
            .strip()
        )
        runner = NodeShellRunner(
            NodeShell(cid=self.node.cid, user="corvus", key_path=HOST_ALPINE_KEY_PATH)
        )
        try:
            pipeline = f"{directory}/imports.yml"
            runner.copy_bytes(
                yaml.safe_dump(
                    {
                        "vars": {"image_url": None},
                        "pipeline": [
                            {
                                "apply": {
                                    "disks": [{**disk, "import": "{{ image_url }}"}]
                                }
                            }
                        ],
                    }
                ).encode(),
                pipeline,
                mode=0o600,
            )
            result = self.node.run(
                shlex.join(
                    [
                        "/opt/corvus/bin/crv",
                        "build",
                        pipeline,
                        "--var",
                        f"image_url={url}",
                        "--wait",
                    ]
                ),
                check=False,
                timeout_sec=30,
            )
            assert result.returncode == 0, (result.stdout + result.stderr).decode(
                errors="replace"
            )
        finally:
            self.node.run(shlex.join(["rm", "-rf", directory]), check=False)

    def test_import_local_file_copies(self) -> None:
        """Register a daemon-owned file, then `import_` it under a new
        name pointing at the registered file's on-disk path. The import
        copies (canonicalised dest != src), and we get a fresh disk
        record with a distinct file."""
        src_name = _uniq("import-src")
        copy_name = _uniq("import-copy")
        src = self.client.disks.create(src_name, size=4194304, format="qcow2")
        try:
            # Phase 3: file_path lives on a per-node placement now.
            # The harness's single-node topology has exactly one
            # placement; grab its file_path.
            src_path = src.show().placements[0].file_path
            task_id = self.client.disks.import_(copy_name, src_path, format="qcow2")
            self.wait_for_task(self.client, task_id, timeout_sec=60.0)
            info = self.client.disks.get(copy_name).show()
            assert info.name == copy_name
            assert info.format == "qcow2"
            # New disk lives at a different path than the source.
            copy_paths = [p.file_path for p in info.placements]
            assert src_path not in copy_paths, (
                f"import landed at the source path {src_path!r}; "
                f"copy placements: {copy_paths!r}"
            )
        finally:
            self._delete_versions_silent(copy_name, src_name)

    def test_import_same_name_publishes_a_separate_version(self) -> None:
        """Importing a registered file publishes a version at a fresh path."""
        name = _uniq("import-version")
        old = self.client.disks.create(name, size=4194304, format="qcow2")
        try:
            old_info = old.show()
            src_path = old_info.placements[0].file_path
            task_id = self.client.disks.import_(name, src_path, format="qcow2")
            self.wait_for_task(self.client, task_id, timeout_sec=60.0)
            new = self.client.disks.get(name)
            new_info = new.show()
            assert new_info.id != old_info.id
            assert new_info.placements[0].file_path != src_path
            assert old.show().id == old_info.id
            assert old.show().tags == []
            assert new_info.tags == ["latest"]
        finally:
            self._delete_versions_silent(name)

    def test_import_from_http_url(self) -> None:
        """The agent downloads a host-side HTTP source, places the file,
        and the daemon registers a fresh disk row."""
        src_name = _uniq("url-src")
        url_name = _uniq("url-import")
        src = self.client.disks.create(src_name, size=4194304, format="qcow2")
        try:
            with _ImportServer(self.node) as server:
                source_path = src.show().placements[0].file_path
                source_bytes = _node_bytes(self.node, source_path)
                server.set_payload(source_bytes)
                task_id = self.client.disks.import_url(
                    url_name, f"{server.url}/payload.qcow2", format="qcow2"
                )
                self.wait_for_task(self.client, task_id, timeout_sec=60.0)
                info = self.client.disks.get(url_name).show()
                assert info.name == url_name
                assert info.format == "qcow2"
                imported_paths = [p.file_path for p in info.placements]
                assert imported_paths and source_path not in imported_paths
                assert all(
                    _node_bytes(self.node, path) == source_bytes
                    for path in imported_paths
                )
        finally:
            self._delete_versions_silent(url_name, src_name)

    def test_import_xz_auto_decompress(self) -> None:
        """An .xz URL is automatically decompressed on the nodeagent,
        and the imported file has the image format and no .xz suffix."""
        src_name = _uniq("xz-src")
        xz_name = _uniq("xz-import")
        src = self.client.disks.create(src_name, size=4194304, format="qcow2")
        try:
            with _ImportServer(self.node, suffix=".qcow2.xz") as server:
                source_bytes = _node_bytes(
                    self.node, src.show().placements[0].file_path
                )
                server.set_payload(lzma.compress(source_bytes))
                task_id = self.client.disks.import_url(
                    xz_name, f"{server.url}/payload.qcow2.xz", format="qcow2"
                )
                self.wait_for_task(self.client, task_id, timeout_sec=60.0)
                info = self.client.disks.get(xz_name).show()
                assert info.format == "qcow2"
                paths = [p.file_path for p in info.placements]
                assert paths
                assert all(
                    _node_bytes(self.node, path) == source_bytes for path in paths
                )
                assert all(not p.endswith(".xz") for p in paths), (
                    f"imported file path retains .xz suffix; decompress "
                    f"didn't run: {paths!r}"
                )
        finally:
            self._delete_versions_silent(xz_name, src_name)

    @pytest.mark.parametrize(
        "compressed,target", [(False, "download"), (True, "download"), (True, "final")]
    )
    def test_import_update(self, compressed: bool, target: str) -> None:
        """Verify conditional HTTP imports and source URL metadata end to end.

        A host-side HTTP server serves two successive releases to the real
        daemon and nodeagent. Its request log lets the test distinguish reusing
        an image from downloading it again. The parameterized cases cover raw
        downloads and xz archives, with checksums over either the archive or the decompressed image.

        The test checks that:
        - An unchanged checksum reuses the image ID without a download, even when
          the digest's case or the source URL changes, and emits a skip event.
        - Explicit disk policies override the top-level skip policy and CLI flag;
          error and skip avoid downloading, while update imports a changed digest.
        - A checksum failure preserves the selected image, its bytes and its URL;
          a successful update publishes a new ID and keeps the old version intact.
        - Inline build apply steps also skip an unchanged import. URLs supplied
          through crv build --var are stored after expansion, including query
          strings, and redirects do not replace them with the destination URL.
        - Skipped imports retain their original URL metadata, and each newly
          imported version records its own supplied URL.
        - Explicit destination paths cannot overwrite a previous version, and an
          image without a verified identity is imported once before updates skip it.
        """
        self.install_node_client_certs()
        name = _uniq(f"conditional-{compressed}-{target}") + ":stable"
        cli_name = _uniq(f"build-url-{compressed}-{target}")
        legacy_name = _uniq(f"legacy-{compressed}-{target}")
        first_payload = b"first release" * 1024
        second_payload = b"second release" * 1024
        first_download = lzma.compress(first_payload) if compressed else first_payload
        second_download = (
            lzma.compress(second_payload) if compressed else second_payload
        )
        first_checksum = hashlib.sha256(
            first_download if target == "download" else first_payload
        ).hexdigest()
        second_checksum = hashlib.sha256(
            second_download if target == "download" else second_payload
        ).hexdigest()
        suffix = ".raw.xz" if compressed else ".raw"
        checksum_spec: dict[str, str] = {"algorithm": "sha256", "target": target}
        with (
            SqliteDatabase(self.node) as database,
            _ImportServer(self.node, suffix=suffix) as server,
        ):
            server.set_payload(first_download)
            disk: dict[str, object] = {
                "name": name,
                "import": f"{server.url}/image{suffix}?release=First",
                "ifExists": "update",
                "checksum": checksum_spec,
            }

            try:

                def apply(
                    expected: str = "success", *, skip_existing: bool = False
                ) -> list[ApplyStreamItem]:
                    if expected == "error":
                        with pytest.raises(CorvusError):
                            self.client.apply(
                                yaml.safe_dump({"ifExists": "skip", "disks": [disk]}),
                                wait=True,
                                skip_existing=skip_existing,
                            )
                        return []
                    events = list(
                        self.client.apply_stream(
                            yaml.safe_dump({"ifExists": "skip", "disks": [disk]}),
                            skip_existing=skip_existing,
                        )
                    )
                    ends = [event for event in events if isinstance(event, ApplyEnd)]
                    assert len(ends) == 1 and ends[0].result == expected, events
                    return events

                checksum_spec["value"] = first_checksum
                disk["ifExists"] = "overwrite"
                apply(skip_existing=True)
                disk["ifExists"] = "update"
                old = self.client.disks.get(str(disk["name"]))
                old_info = old.show()
                assert server.request_count() == 1
                original_url = str(disk["import"])
                assert self._import_url(database, old_info.id) == original_url
                checksum_spec["value"] = first_checksum.upper()
                # Changing the URL doesn't change verified identity.
                disk["import"] = str(disk["import"]).replace("/image", "/other")
                events = apply()
                assert any(
                    isinstance(event, ApplyEntityStart) and event.kind == "skip"
                    for event in events
                )
                assert server.request_count() == 1
                assert self.client.disks.get(str(disk["name"])).show().id == old_info.id
                assert self._import_url(database, old_info.id) == original_url

                server.set_payload(second_download)
                disk["ifExists"] = "error"
                before = server.request_count()
                apply("error", skip_existing=True)
                assert server.request_count() == before
                disk["ifExists"] = "skip"
                before = server.request_count()
                apply()
                assert server.request_count() == before
                disk["ifExists"] = "update"
                checksum_spec["value"] = "0" * 64
                apply("error")
                assert self.client.disks.get(str(disk["name"])).show().id == old_info.id
                assert self._import_url(database, old_info.id) == original_url
                assert (
                    _node_bytes(self.node, old_info.placements[0].file_path)
                    == first_payload
                )

                checksum_spec["value"] = second_checksum
                before = server.request_count()
                apply(skip_existing=True)
                new = self.client.disks.get(str(disk["name"]))
                new_info = new.show()
                assert new_info.id != old_info.id
                assert self._import_url(database, new_info.id) == disk["import"]
                assert self._import_url(database, old_info.id) == original_url
                assert server.request_count() == before + 1
                assert (
                    _node_bytes(self.node, new_info.placements[0].file_path)
                    == second_payload
                )
                assert Path(new_info.placements[0].file_path).name.startswith(
                    f"{new_info.id}-"
                )
                apply()
                assert server.request_count() == before + 1
                build_events = list(
                    self.client.build_stream_text(
                        yaml.safe_dump({"pipeline": [{"apply": {"disks": [disk]}}]})
                    )
                )
                ends = [
                    event
                    for event in build_events
                    if isinstance(event, BuildPipelineEnd)
                ]
                assert len(ends) == 1 and all(
                    build.error_message is None for build in ends[0].builds
                )
                assert server.request_count() == before + 1
                # CLI build variables are expanded before publication; redirects do
                # not replace the supplied URL in the identity's debugging metadata.
                cli_url = (
                    f"{server.url}/redirect/image{suffix}?origin=CLI&release=Second"
                )
                self._build_with_url({**disk, "name": cli_name}, cli_url)
                cli_image = self.client.disks.get(cli_name)
                assert self._import_url(database, cli_image.show().id) == cli_url
                # Explicit destination files cannot clobber a previous version.
                disk["path"] = old_info.placements[0].file_path
                checksum_spec["value"] = "0" * 64
                apply("error")
                assert self.client.disks.get(str(disk["name"])).show().id == new_info.id
                # A legacy/non-imported image acquires an identity on its first update.
                disk.pop("path")
                disk["name"] = legacy_name
                legacy = self.client.disks.create(
                    str(disk["name"]), format="raw", size=1024
                )
                checksum_spec["value"] = second_checksum
                before = server.request_count()
                apply()
                assert server.request_count() == before + 1
                upgraded = self.client.disks.get(str(disk["name"]))
                assert upgraded.show().id != legacy.show().id
                apply()
                assert server.request_count() == before + 1
            finally:
                self._delete_versions_silent(name, cli_name, legacy_name)

    @pytest.mark.slow
    @pytest.mark.timeout(1800)
    def test_unchanged_import_skips_derived_build(self) -> None:
        """A verified release download gates the derived build. Equal checksums
        cause neither an HTTP GET nor a provisioner run; a new verified release
        publishes a new source version and rebuilds the artifact.
        """
        base = self.register_base_images()["alpine"]
        original = _node_bytes(
            self.node, self.client.disks.get(base).show().placements[0].file_path
        )
        source, artifact, template = [
            _uniq(stem) for stem in ("release-source", "release-build", "release-tpl")
        ]
        with _ImportServer(self.node) as server:
            server.set_payload(original)
            checksum = {
                "algorithm": "sha256",
                "target": "download",
                "value": hashlib.sha256(original).hexdigest(),
            }
            pipeline = {
                "pipeline": [
                    {
                        "apply": {
                            "ifExists": "skip",
                            "disks": [
                                {
                                    "name": source,
                                    "import": server.url + "/image.qcow2",
                                    "ifExists": "update",
                                    "checksum": checksum,
                                }
                            ],
                            "templates": [
                                {
                                    "name": template,
                                    "cpuCount": 2,
                                    "ram": "1024M",
                                    "headless": True,
                                    "guestAgent": True,
                                    "drives": [
                                        {
                                            "diskImage": source,
                                            "strategy": "overlay",
                                            "interface": "virtio",
                                            "size": "2G",
                                        }
                                    ],
                                }
                            ],
                        }
                    },
                    {
                        "build": {
                            "name": artifact,
                            "template": template,
                            "target": {
                                "ifExists": "update",
                                "size": "2G",
                                "format": "qcow2",
                            },
                            "vm": {"cpuCount": 2, "ram": "1024M"},
                            "provisioners": [
                                {"shell": "echo verified-release > /tmp/release"}
                            ],
                            "cleanup": "always",
                        }
                    },
                ]
            }

            def run() -> tuple[int, list[BuildStreamItem]]:
                events = list(self.client.build_stream_text(yaml.safe_dump(pipeline)))
                end = next(
                    event for event in events if isinstance(event, BuildPipelineEnd)
                )
                assert not [result for result in end.builds if result.error_message], (
                    end
                )
                image_id = end.builds[-1].artifact_disk_id
                assert image_id is not None
                return image_id, events

            try:
                first, _ = run()
                source_id = self.client.disks.get(source).show().id
                assert server.request_count() == 1
                same, events = run()
                assert same == first
                assert self.client.disks.get(source).show().id == source_id
                assert server.request_count() == 1
                assert not any(isinstance(event, BuildStepStart) for event in events)
                # Trailing unused bytes leave qcow2 bootable while changing the
                # downloaded release identity, just as a republished image does.
                released = original + bytes(512)
                server.set_payload(released)
                checksum["value"] = hashlib.sha256(released).hexdigest()
                updated, events = run()
                assert updated != first
                assert self.client.disks.get(source).show().id != source_id
                assert server.request_count() == 2
                assert any(isinstance(event, BuildStepStart) for event in events)
            finally:
                try:
                    self.client.templates.get(template).delete()
                finally:
                    self._delete_versions_silent(artifact, source)
