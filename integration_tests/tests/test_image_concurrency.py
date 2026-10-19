"""Image leases allow independent tasks and bound conflicting waits."""

from __future__ import annotations

import asyncio
import secrets
import shlex
import subprocess
import textwrap
import time
from collections.abc import AsyncIterator, Generator
from contextlib import asynccontextmanager, contextmanager

import pytest
from corvus_client import AsyncClient
from corvus_client._schema import disk as schema
from corvus_client.exceptions import CorvusError
from corvus_client.types import BuildPipelineEnd
from corvus_test_harness import SingleNodeCase, SqliteDatabase
from corvus_test_harness.ssh import HOST_ALPINE_KEY_PATH, VmShell

import yaml

pytestmark = [pytest.mark.slow, pytest.mark.timeout(1800)]


class TestImageConcurrency(SingleNodeCase):
    @contextmanager
    def _resources(self) -> Generator[str]:
        prefix = f"leases-{secrets.token_hex(4)}"
        try:
            yield prefix
        finally:
            # Cancel daemon workers before deleting their leased images. Closing
            # the Python stream alone does not cancel a build task.
            for task in self.client.tasks.list(result="running"):
                if task.command == "build" and task.parent_id is None:
                    self.client.tasks.cancel(task.id)
            deadline = time.monotonic() + 30
            while any(
                task.command == "build" and task.parent_id is None
                for task in self.client.tasks.list(result="running")
            ):
                assert time.monotonic() < deadline, "Build cancellation did not finish"
                time.sleep(0.2)
            for vm in self.client.vms.list():
                if prefix in vm.name:
                    cap = self.client.vms.get(vm.id)
                    cap.reset()
                    cap.delete()
            for template in self.client.templates.list():
                if prefix in template.name:
                    self.client.templates.get(template.id).delete()
            pending = {
                disk.id: disk
                for disk in self.client.disks.list()
                if prefix in disk.name
            }
            while pending:
                parents = {
                    disk.backing_image.id
                    for disk in pending.values()
                    if disk.backing_image is not None
                }
                leaves = set(pending) - parents
                assert leaves, "Image dependency cycle"
                for image_id in leaves:
                    self.client.disks.get(image_id).delete()
                    del pending[image_id]

    def _template(self, name: str, source: str) -> None:
        self.client.templates.create(
            yaml.safe_dump(
                {
                    "name": name,
                    "cpuCount": 1,
                    "ram": "512M",
                    "headless": True,
                    "guestAgent": True,
                    "drives": [
                        {
                            "diskImage": source,
                            "interface": "virtio",
                            "strategy": "overlay",
                            "size": "2G",
                        }
                    ],
                }
            )
        )

    def _pipeline(self, name: str, template: str) -> str:
        return yaml.safe_dump(
            {
                "pipeline": [
                    {
                        "build": {
                            "name": name,
                            "template": template,
                            "strategy": "overlay",
                            "target": {"format": "qcow2", "ifExists": "overwrite"},
                            "provisioners": [
                                {
                                    "shell": "touch /tmp/lease-ready; while [ ! -e /tmp/lease-release ]; do sleep 0.2; done; echo lease-complete"
                                }
                            ],
                            "cleanup": "always",
                        }
                    }
                ]
            }
        )

    async def _build(self, pipeline: str) -> BuildPipelineEnd:
        end = None
        async for event in self.client._a.build_stream_text(pipeline):
            if isinstance(event, BuildPipelineEnd):
                end = event
        assert end is not None
        return end

    async def _ready_vm(self, name: str) -> VmShell:
        deadline = time.monotonic() + 600
        while time.monotonic() < deadline:
            for vm in await self.client._a.vms.list():
                if name in vm.name and vm.name.startswith("__build_"):
                    details = await (await self.client._a.vms.get(vm.id)).show()
                    if details.vsock_cid is None:
                        continue
                    shell = VmShell(
                        node_cid=self.node.cid,
                        host_key_path=HOST_ALPINE_KEY_PATH,
                        vm_cid=details.vsock_cid,
                    )
                    try:
                        result = await asyncio.to_thread(
                            shell.run,
                            "test -e /tmp/lease-ready",
                            timeout_sec=5,
                            check=False,
                        )
                        if result.exit_code == 0:
                            return shell
                    except (OSError, subprocess.TimeoutExpired):
                        pass
                    shell.close()
            await asyncio.sleep(0.2)
        raise AssertionError("Build did not reach its provisioner barrier")

    @asynccontextmanager
    async def _held_build(
        self, name: str, template: str
    ) -> AsyncIterator[asyncio.Task[BuildPipelineEnd]]:
        existing = {
            entry.id for entry in await self.client._a.tasks.list(result="running")
        }
        task = asyncio.create_task(self._build(self._pipeline(name, template)))
        vm = None
        try:
            vm = await self._ready_vm(name)
            assert not task.done(), "build finished before its provisioner barrier"
            yield task
            await asyncio.to_thread(vm.run, "touch /tmp/lease-release", timeout_sec=10)
            await asyncio.wait_for(asyncio.shield(task), timeout=60)
        finally:
            if vm is not None:
                vm.close()
            if not task.done():
                for entry in await self.client._a.tasks.list(result="running"):
                    if (
                        entry.command == "build"
                        and entry.parent_id is None
                        and entry.id not in existing
                    ):
                        await self.client._a.tasks.cancel(entry.id)
                await asyncio.wait_for(task, timeout=30)

    @pytest.mark.parametrize("shared_source", [True, False])
    def test_two_builds_overlap(self, shared_source: bool) -> None:
        base = self.register_base_images()["corvus-test-vm"]
        with self._resources() as prefix:
            second = base
            if not shared_source:
                second = f"{prefix}-second-base"
                self.client.disks.clone(base, second)
            self._template(f"{prefix}-template-a", base)
            self._template(f"{prefix}-template-b", second)

            async def exercise() -> None:
                async with self._held_build(
                    f"{prefix}-a", f"{prefix}-template-a"
                ) as first:
                    async with self._held_build(
                        f"{prefix}-b", f"{prefix}-template-b"
                    ) as other:
                        assert not first.done() and not other.done()
                    assert not first.done(), (
                        "second build must finish while first is held"
                    )
                for task in (first, other):
                    result = task.result().builds[0]
                    assert not result.error_message, result
                    assert result.artifact_disk_id
                    info = await (
                        await self.client._a.disks.get(result.artifact_disk_id)
                    ).show()
                    assert info.placements and info.format == "qcow2"

            self.client._rl.run(exercise())

    def test_build_upload_download_and_cleanup_overlap(self) -> None:
        base = self.register_base_images()["corvus-test-vm"]
        with self._resources() as prefix:
            source_name = f"{prefix}-source"
            source = self.client.disks.clone(base, source_name).show()
            self._template(f"{prefix}-template", source_name)
            obsolete = self.client.disks.create(f"{prefix}-prune", 1048576).show()
            self.client.disks.create(f"{prefix}-prune", 1048576)
            directory = f"/tmp/{prefix}"
            port = 30000 + secrets.randbelow(20000)
            server = textwrap.dedent(f"""
                import http.server, pathlib, time
                root = pathlib.Path({directory!r})
                class Handler(http.server.BaseHTTPRequestHandler):
                    def do_GET(self):
                        if self.path == '/health':
                            self.send_response(200); self.end_headers(); return
                        (root / 'ready').touch()
                        while not (root / 'release').exists(): time.sleep(0.05)
                        self.send_response(200); self.send_header('Content-Length', '4096'); self.end_headers()
                        self.wfile.write(b'd' * 4096)
                http.server.ThreadingHTTPServer(('127.0.0.1', {port}), Handler).serve_forever()
            """)
            self.node.run(f"mkdir -p {directory}")
            self.node.run(
                f"setsid -f python3 -c {shlex.quote(server)} </dev/null >{directory}/server.log 2>&1"
            )
            try:

                async def exercise() -> None:
                    async with self._held_build(
                        f"{prefix}-output", f"{prefix}-template"
                    ) as build:
                        mgr = await self.client._a.disks._ensure()
                        for name in (f"{prefix}-unrelated", source_name):
                            params = schema.DiskUploadParams.new_message()
                            params.name = name
                            params.format = "raw"
                            params.ifExists = "overwrite"
                            upload = (
                                await mgr.beginUpload(params=params)
                            ).result.upload
                            await upload.write(chunk=b"u" * 4096)
                            await upload.finish()
                        assert not build.done()
                        # The new latest version must not replace the pinned source.
                        old = await (await self.client._a.disks.get(source.id)).show()
                        assert old.placements == source.placements
                        report = await self.client._a.disks.cleanup(f"{prefix}-prune")
                        assert any(
                            v.disk_image.id == obsolete.id and v.status == "removed"
                            for v in report.versions
                        )
                        import_id = await self.client._a.disks.import_url(
                            f"{prefix}-download",
                            f"http://127.0.0.1:{port}/image.raw",
                            format="raw",
                        )
                        deadline = time.monotonic() + 30
                        while self.node.run(
                            f"test -e {directory}/ready", check=False
                        ).returncode:
                            assert time.monotonic() < deadline, (
                                "URL import did not reach barrier"
                            )
                            await asyncio.sleep(0.1)
                        assert not build.done(), (
                            "import reached its barrier while build was held"
                        )
                        self.node.run(f"touch {directory}/release")
                        deadline = time.monotonic() + 60
                        while True:
                            assert time.monotonic() < deadline, (
                                "URL import did not finish"
                            )
                            info = await (
                                await self.client._a.tasks.get(import_id)
                            ).show()
                            if info.result != "running":
                                assert info.result == "success", info
                                break
                            await asyncio.sleep(0.1)
                    assert not build.result().builds[0].error_message

                self.client._rl.run(exercise())
            finally:
                self.node.run(
                    f"touch {directory}/release; fuser -k {port}/tcp", check=False
                )

    def test_conflicts_timeout_and_release_before_deadline(self) -> None:
        base = self.register_base_images()["corvus-test-vm"]
        with self._resources() as prefix, SqliteDatabase(self.node) as database:
            source = self.client.disks.clone(base, f"{prefix}-source").show()
            self._template(f"{prefix}-template", f"{prefix}-source")

            async def exercise() -> None:
                async with self._held_build(
                    f"{prefix}-output", f"{prefix}-template"
                ) as build:
                    cursor = (
                        self.node.run(
                            "sudo journalctl -u corvus.service -n 0 --show-cursor --no-pager"
                        )
                        .stdout.decode()
                        .split("-- cursor: ", 1)[1]
                        .strip()
                    )
                    baseline = database.query(
                        "SELECT node_agent_healthcheck FROM node WHERE name = ?",
                        (self.node.short_name,),
                    )[0][0]
                    started = time.monotonic()
                    # The same family is reserved even when the publisher selects
                    # a different tag. Other calls through this manager stay live.
                    blocked = asyncio.create_task(
                        self.client._a.disks.create(f"{prefix}-output:other", 1048576)
                    )
                    source_cap = await self.client._a.disks.get(source.id)
                    writer = asyncio.create_task(source_cap.resize(3 * 1024**3))
                    await asyncio.wait_for(self.client._a.disks.list(), timeout=5)
                    with pytest.raises(
                        CorvusError,
                        match=r"Timed out after 60 seconds.*image family.*held by task",
                    ):
                        await asyncio.wait_for(blocked, timeout=75)
                    with pytest.raises(
                        CorvusError,
                        match=r"Timed out after 60 seconds.*image version.*held by task",
                    ):
                        await asyncio.wait_for(writer, timeout=5)
                    errors = await self.client._a.tasks.list(result="error")
                    assert any(
                        t.message and "Timed out after 60 seconds" in t.message
                        for t in errors
                    )
                    assert 59 <= time.monotonic() - started < 75
                    healthcheck = database.query(
                        "SELECT node_agent_healthcheck FROM node WHERE name = ?",
                        (self.node.short_name,),
                    )[0][0]
                    assert healthcheck is not None and healthcheck != baseline, (
                        "Long provisioner blocked node status snapshots"
                    )
                    journal = self.node.run(
                        "sudo journalctl -u corvus.service --no-pager "
                        f"--after-cursor={shlex.quote(cursor)}"
                    ).stdout.decode()
                    assert "no processed VM status snapshot" not in journal, (
                        "Busy guest agent triggered a status subscription reconnect"
                    )
                    assert not build.done()
                    assert not any(
                        d.name == f"{prefix}-output"
                        for d in await self.client._a.disks.list()
                    )
                assert not build.result().builds[0].error_message
                mgr = await self.client._a.disks._ensure()
                params = schema.DiskUploadParams.new_message()
                params.name = f"{prefix}-abort"
                params.format = "raw"
                params.ifExists = "error"
                upload = (await mgr.beginUpload(params=params)).result.upload
                waiter = asyncio.create_task(
                    self.client._a.disks.create(params.name, 1048576)
                )
                await upload.abort()
                await asyncio.wait_for(waiter, timeout=15)

            self.client._rl.run(exercise())

    def test_cancelled_build_releases_waiting_publisher(self) -> None:
        base = self.register_base_images()["corvus-test-vm"]
        with self._resources() as prefix:
            self._template(f"{prefix}-template", base)

            async def exercise() -> None:
                build = asyncio.create_task(
                    self._build(
                        self._pipeline(f"{prefix}-output", f"{prefix}-template")
                    )
                )
                shell = await self._ready_vm(f"{prefix}-output")
                try:
                    roots = [
                        task
                        for task in await self.client._a.tasks.list(result="running")
                        if task.command == "build" and task.parent_id is None
                    ]
                    assert len(roots) == 1
                    publisher = asyncio.create_task(
                        self.client._a.disks.create(f"{prefix}-output", 1048576)
                    )
                    await self.client._a.tasks.cancel(roots[0].id)
                    await asyncio.wait_for(build, timeout=30)
                    await asyncio.wait_for(publisher, timeout=15)
                    status = await (await self.client._a.tasks.get(roots[0].id)).show()
                    assert status.result == "cancelled", status
                    assert not any(
                        prefix in vm.name for vm in await self.client._a.vms.list()
                    )
                finally:
                    if not build.done():
                        await asyncio.to_thread(
                            shell.run, "touch /tmp/lease-release", check=False
                        )
                        await asyncio.wait_for(build, timeout=60)
                    shell.close()

            self.client._rl.run(exercise())

    @pytest.mark.parametrize("release", ["cancel", "disconnect"])
    def test_abandoned_upload_releases_waiting_publisher(self, release: str) -> None:
        with self._resources() as prefix:

            async def exercise() -> None:
                host, port = self.node.relay.endpoint
                async with AsyncClient(
                    host=host, port=port, cert_dir=self.node._client_cert_dir
                ) as uploader:
                    manager = await uploader.disks._ensure()
                    params = schema.DiskUploadParams.new_message()
                    params.name = prefix
                    params.format = "raw"
                    params.ifExists = "error"
                    upload = (await manager.beginUpload(params=params)).result.upload
                    await upload.write(chunk=b"unfinished")
                    waiter = asyncio.create_task(
                        self.client._a.disks.create(prefix, 1048576)
                    )
                    if release == "disconnect":
                        await uploader.close()
                    else:
                        tasks = await self.client._a.tasks.list(result="running")
                        active = [
                            t
                            for t in tasks
                            if t.command == "upload" and t.parent_id is None
                        ]
                        assert len(active) == 1
                        await self.client._a.tasks.cancel(active[0].id)
                    await asyncio.wait_for(waiter, timeout=15)
                    images = [
                        d for d in await self.client._a.disks.list() if d.name == prefix
                    ]
                    assert len(images) == 1
                    assert images[0].size == 1048576

            self.client._rl.run(exercise())
