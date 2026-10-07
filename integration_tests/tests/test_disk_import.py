"""Disk imports against the real daemon and nodeagent in a nested test node."""

from __future__ import annotations

import base64
import hashlib
import json
import lzma
import secrets
import shlex
from pathlib import Path
from types import TracebackType

import pytest
from corvus_client.exceptions import CorvusError
from corvus_client.types import ApplyEnd, ApplyEntityStart, BuildPipelineEnd
from corvus_test_harness import SingleNodeCase, TestNode
from corvus_test_harness.runner import NodeShellRunner
from corvus_test_harness.ssh import HOST_ALPINE_KEY_PATH, NodeShell

import yaml

_SERVER_SCRIPT = """\
import sys
from http.server import BaseHTTPRequestHandler, HTTPServer
from pathlib import Path

root = Path(sys.argv[1])
suffix = sys.argv[2]

class Handler(BaseHTTPRequestHandler):
    def do_GET(self):
        with (root / "requests").open("a") as requests:
            requests.write(self.path + "\\n")
        if self.path.startswith("/redirect/"):
            self.send_response(302)
            self.send_header("Location", "/artifact" + suffix)
            self.end_headers()
            return
        body = (root / "body").read_bytes()
        self.send_response(200)
        self.send_header("Content-Length", str(len(body)))
        self.end_headers()
        self.wfile.write(body)

    def log_message(self, format, *args):
        pass

server = HTTPServer(("127.0.0.1", 0), Handler)
(root / "port").write_text(str(server.server_port))
server.serve_forever()
"""


def _uniq(stem: str) -> str:
    return f"{stem}-{secrets.token_hex(3)}"


def _node_bytes(node: TestNode, path: str) -> bytes:
    """Read an inner-node placement without interpreting its path on the host."""
    return base64.b64decode(node.run(f"base64 < {shlex.quote(path)}").stdout)


class _ImportServer:
    """Node-local HTTP source with mutable bytes, redirects, and a GET log."""

    def __init__(self, node: TestNode, *, suffix: str = ".qcow2") -> None:
        self.node = node
        self.suffix = suffix
        self.directory = ""
        self.url = ""
        self.runner = NodeShellRunner(
            NodeShell(cid=node.cid, user="corvus", key_path=HOST_ALPINE_KEY_PATH)
        )

    def __enter__(self) -> _ImportServer:
        self.directory = (
            self.node.run("mktemp -d /tmp/corvus-it-import.XXXXXX")
            .stdout.decode()
            .strip()
        )
        try:
            self.runner.copy_bytes(
                _SERVER_SCRIPT.encode(), f"{self.directory}/server.py", mode=0o600
            )
            self.runner.copy_bytes(b"", f"{self.directory}/requests", mode=0o600)
            self.set_payload(b"")
            command = shlex.join(
                [
                    "nohup",
                    "python3",
                    f"{self.directory}/server.py",
                    self.directory,
                    self.suffix,
                ]
            )
            self.node.run(
                f"{command} > {shlex.quote(self.directory + '/server.log')} 2>&1 "
                f"< /dev/null & echo $! > {shlex.quote(self.directory + '/pid')}"
            )
            # A TCP probe does not add a GET to the request log. Keep the
            # startup deadline inside one SSH call so a busy node cannot
            # multiply it by the number of readiness probes.
            probe = """\
import socket
import sys
import time
from pathlib import Path

port_file = Path(sys.argv[1]) / "port"
deadline = time.monotonic() + 10
while time.monotonic() < deadline:
    try:
        port = int(port_file.read_text())
        with socket.create_connection(("127.0.0.1", port), timeout=0.2):
            print(port)
            break
    except (OSError, ValueError):
        time.sleep(0.1)
else:
    raise RuntimeError("HTTP source did not become ready within 10 seconds")
"""
            result = self.node.run(
                shlex.join(["python3", "-c", probe, self.directory]),
                check=False,
                timeout_sec=15,
            )
            log = self.node.run(
                f"cat {shlex.quote(self.directory + '/server.log')}", check=False
            )
            assert result.returncode == 0, result.stderr.decode() + log.stdout.decode()
            port = int(result.stdout)
            self.url = f"http://127.0.0.1:{port}"
            return self
        except Exception:
            self._close()
            raise

    def __exit__(
        self,
        exc_type: type[BaseException] | None,
        exc_value: BaseException | None,
        traceback: TracebackType | None,
    ) -> None:
        self._close()

    def _close(self) -> None:
        # Kill only this server, even if another test runs on the same host.
        # Cleanup must not hide a failure in the import assertions.
        for command in (
            f"test ! -f {shlex.quote(self.directory + '/pid')} || "
            f"kill $(cat {shlex.quote(self.directory + '/pid')})",
            shlex.join(["rm", "-rf", self.directory]),
        ):
            try:
                self.node.run(command, check=False)
            except Exception:
                pass

    def set_payload(self, body: bytes) -> None:
        """Atomically replace the served release between import attempts."""
        self.runner.copy_bytes(body, f"{self.directory}/body.next", mode=0o600)
        self.node.run(
            shlex.join(["mv", f"{self.directory}/body.next", f"{self.directory}/body"])
        )

    def serve_file(self, path: str, *, compressed: bool = False) -> None:
        if compressed:
            self.node.run(
                f"xz -c {shlex.quote(path)} > {shlex.quote(self.directory + '/body')}"
            )
        else:
            self.node.run(shlex.join(["cp", path, f"{self.directory}/body"]))

    def request_count(self) -> int:
        return int(
            self.node.run(f"wc -l < {shlex.quote(self.directory + '/requests')}").stdout
        )


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

    def _import_url(self, image_id: int) -> str:
        script = """\
import json
import sqlite3
import sys
from contextlib import closing

with closing(sqlite3.connect("file:/var/lib/corvus/corvus.db?mode=ro", uri=True)) as db:
    row = db.execute(
        "SELECT import_url FROM disk_image_import_identity WHERE disk_image_id = ?",
        (int(sys.argv[1]),),
    ).fetchone()
print(json.dumps(row[0] if row is not None else None))
"""
        result = self.node.run(shlex.join(["python3", "-c", script, str(image_id)]))
        url = json.loads(result.stdout)
        assert isinstance(url, str), f"no import URL recorded for image {image_id}"
        return url

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
        """The agent downloads a node-local HTTP source, places the file,
        and the daemon registers a fresh disk row."""
        src_name = _uniq("url-src")
        url_name = _uniq("url-import")
        src = self.client.disks.create(src_name, size=4194304, format="qcow2")
        try:
            with _ImportServer(self.node) as server:
                server.serve_file(src.show().placements[0].file_path)
                task_id = self.client.disks.import_url(
                    url_name, f"{server.url}/payload.qcow2", format="qcow2"
                )
                self.wait_for_task(self.client, task_id, timeout_sec=60.0)
                info = self.client.disks.get(url_name).show()
                assert info.name == url_name
                assert info.format == "qcow2"
                imported_paths = [p.file_path for p in info.placements]
                assert not any(server.directory in p for p in imported_paths), (
                    f"imported disk left at the staging dir: {imported_paths!r}"
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
                server.serve_file(src.show().placements[0].file_path, compressed=True)
                task_id = self.client.disks.import_url(
                    xz_name, f"{server.url}/payload.qcow2.xz", format="qcow2"
                )
                self.wait_for_task(self.client, task_id, timeout_sec=60.0)
                info = self.client.disks.get(xz_name).show()
                assert info.format == "qcow2"
                paths = [p.file_path for p in info.placements]
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

        A node-local HTTP server serves two successive releases to the real
        daemon and nodeagent. Its request log lets the test distinguish reusing
        an image from downloading it again. The parameterized cases cover raw downloads and xz
        archives, with checksums over either the archive or the decompressed image.

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
        payload = b"first release" * 1024
        suffix = ".raw.xz" if compressed else ".raw"
        checksum_spec: dict[str, str] = {"algorithm": "sha256", "target": target}
        with _ImportServer(self.node, suffix=suffix) as server:
            server.set_payload(lzma.compress(payload) if compressed else payload)
            disk: dict[str, object] = {
                "name": name,
                "import": f"{server.url}/image{suffix}?release=First",
                "ifExists": "update",
                "checksum": checksum_spec,
            }

            def checksum() -> str:
                body = (
                    lzma.compress(payload)
                    if compressed and target == "download"
                    else payload
                )
                return hashlib.sha256(body).hexdigest()

            try:

                def apply(
                    expected: str = "success", *, skip_existing: bool = False
                ) -> list[object]:
                    if expected == "error":
                        with pytest.raises(CorvusError):
                            self.client.apply(
                                yaml.safe_dump({"ifExists": "skip", "disks": [disk]}),
                                wait=True,
                                skip_existing=skip_existing,
                            )
                        return []
                    events: list[object] = [
                        event
                        for event in self.client.apply_stream(
                            yaml.safe_dump({"ifExists": "skip", "disks": [disk]}),
                            skip_existing=skip_existing,
                        )
                    ]
                    ends = [event for event in events if isinstance(event, ApplyEnd)]
                    assert len(ends) == 1 and ends[0].result == expected, events
                    return events

                checksum_spec["value"] = checksum()
                disk["ifExists"] = "overwrite"
                apply(skip_existing=True)
                disk["ifExists"] = "update"
                old = self.client.disks.get(str(disk["name"]))
                old_info = old.show()
                assert server.request_count() == 1
                original_url = str(disk["import"])
                assert self._import_url(old_info.id) == original_url
                checksum_spec["value"] = checksum().upper()
                # Changing the URL doesn't change verified identity.
                disk["import"] = str(disk["import"]).replace("/image", "/other")
                events = apply()
                assert any(
                    isinstance(event, ApplyEntityStart) and event.kind == "skip"
                    for event in events
                )
                assert server.request_count() == 1
                assert self.client.disks.get(str(disk["name"])).show().id == old_info.id
                assert self._import_url(old_info.id) == original_url

                payload = b"second release" * 1024
                server.set_payload(lzma.compress(payload) if compressed else payload)
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
                assert self._import_url(old_info.id) == original_url
                assert (
                    _node_bytes(self.node, old_info.placements[0].file_path)
                    == b"first release" * 1024
                )

                checksum_spec["value"] = checksum()
                before = server.request_count()
                apply(skip_existing=True)
                new = self.client.disks.get(str(disk["name"]))
                new_info = new.show()
                assert new_info.id != old_info.id
                assert self._import_url(new_info.id) == disk["import"]
                assert self._import_url(old_info.id) == original_url
                assert server.request_count() == before + 1
                assert (
                    _node_bytes(self.node, new_info.placements[0].file_path) == payload
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
                pipeline = f"{server.directory}/imports.yml"
                server.runner.copy_bytes(
                    yaml.safe_dump(
                        {
                            "vars": {"image_url": None},
                            "pipeline": [
                                {
                                    "apply": {
                                        "disks": [
                                            {
                                                **disk,
                                                "name": cli_name,
                                                "import": "{{ image_url }}",
                                            }
                                        ]
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
                            f"image_url={cli_url}",
                            "--wait",
                        ]
                    ),
                    check=False,
                    timeout_sec=30,
                )
                assert result.returncode == 0, (result.stdout + result.stderr).decode(
                    errors="replace"
                )
                cli_image = self.client.disks.get(cli_name)
                assert self._import_url(cli_image.show().id) == cli_url
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
                checksum_spec["value"] = checksum()
                before = server.request_count()
                apply()
                assert server.request_count() == before + 1
                upgraded = self.client.disks.get(str(disk["name"]))
                assert upgraded.show().id != legacy.show().id
                apply()
                assert server.request_count() == before + 1
            finally:
                self._delete_versions_silent(name, cli_name, legacy_name)
