"""Cancellation of an asynchronous apply stops future work without rollback."""

from __future__ import annotations

import json
import secrets
import shlex
import textwrap
import time

import pytest
from corvus_client import VmNotFound
from corvus_client.types import TaskProgressEvent
from corvus_test_harness import SingleNodeCase, WebGateway
from websockets.exceptions import ConnectionClosedOK
from websockets.sync.client import connect as ws_connect

pytestmark = pytest.mark.timeout(180)


class TestTaskCancellation(SingleNodeCase):
    def test_cancel_apply_cancels_active_import_and_keeps_prior_disk(self) -> None:
        token = secrets.token_hex(4)
        keep_name = f"cancel-keep-{token}"
        import_name = f"cancel-import-{token}"
        future_vm = f"cancel-future-{token}"
        server_dir = f"/tmp/cancel-http-{token}"
        port = 30000 + secrets.randbelow(20000)

        # A throttled local server makes the import deterministically long
        # enough to subscribe and cancel. Random bytes avoid sparse-file and
        # compression shortcuts in the transfer path.
        server = textwrap.dedent(
            f"""
            import http.server, time
            class Slow(http.server.SimpleHTTPRequestHandler):
                def copyfile(self, source, output):
                    while chunk := source.read(65536):
                        output.write(chunk); output.flush(); time.sleep(0.03)
            http.server.ThreadingHTTPServer(('127.0.0.1', {port}), Slow).serve_forever()
            """
        ).strip()
        quoted_server = shlex.quote(server)
        self.node.run(
            f"mkdir -p {server_dir}; dd if=/dev/urandom of={server_dir}/payload.raw bs=1M count=32"
        )
        self.node.run(
            # setsid -f forks before returning and all descriptors are
            # redirected, so the SSH command cannot wait on the server.
            f"cd {server_dir} && setsid -f python3 -c {quoted_server} "
            f"</dev/null > {server_dir}/server.log 2>&1"
        )
        try:
            url = f"http://127.0.0.1:{port}/payload.raw"
            deadline = time.monotonic() + 10
            while self.node.run(
                f"curl --fail --silent -o /dev/null {url}", check=False
            ).returncode:
                if time.monotonic() >= deadline:
                    raise AssertionError("throttled HTTP server did not start")
                time.sleep(0.1)

            yaml_body = textwrap.dedent(
                f"""
                disks:
                  - name: {keep_name}
                    sizeMb: 8
                    format: qcow2
                  - name: {import_name}
                    import: {url}
                    format: raw
                vms:
                  - name: {future_vm}
                    cpuCount: 1
                    ramMb: 64
                    headless: true
                """
            ).strip()
            # Starting asynchronously gives us the task id before any child
            # work begins.  In particular, do not run apply_stream from a
            # second synchronous Client thread: it races the Cap'n Proto
            # event loop with the polling below and can return before the
            # apply action has been scheduled.
            _, parent_id = self.client.apply(yaml_body, wait=False)
            assert parent_id is not None
            events: list[TaskProgressEvent] = []
            sub = self.client.tasks.subscribe(parent_id, events.append)
            child = None
            try:
                # The gateway must relay the same lifecycle subscription as
                # the Python client and close normally after the terminal
                # event. The throttled import keeps the parent live while it
                # starts and the socket connects.
                with (
                    WebGateway(self.node) as web,
                    ws_connect(
                        web.ws_url(f"/api/tasks/{parent_id}/ws"), open_timeout=10.0
                    ) as ws,
                ):
                    started = json.loads(ws.recv(timeout=10.0))
                    assert started["type"] == "started"
                    assert started["task_id"] == parent_id

                    deadline = time.monotonic() + 30
                    while time.monotonic() < deadline:
                        children = self.client.tasks.list_children(parent_id)
                        child = next(
                            (task for task in children if task.result == "running"),
                            None,
                        )
                        if child is not None:
                            break
                        time.sleep(0.2)
                    assert child is not None, "apply import never became active"

                    self.client.tasks.cancel(parent_id)
                    # Cancellation is a request: the manager returns after
                    # signalling the workers, before their finalizers publish
                    # terminal records.  Poll both task records for that state.
                    deadline = time.monotonic() + 30
                    while time.monotonic() < deadline:
                        final_parent = self.client.tasks.get(parent_id).show()
                        final_child = self.client.tasks.get(child.id).show()
                        if (
                            final_parent.result == "cancelled"
                            and final_child.result == "cancelled"
                        ):
                            break
                        time.sleep(0.1)
                    assert final_parent.result == "cancelled", final_parent
                    assert final_child.result == "cancelled", final_child

                    ws_events = [started]
                    deadline = time.monotonic() + 10
                    while time.monotonic() < deadline:
                        try:
                            message = ws.recv(timeout=deadline - time.monotonic())
                        except TimeoutError:
                            break
                        ws_events.append(json.loads(message))
                        if ws_events[-1]["type"] == "finished":
                            break
                    finished = ws_events[-1]
                    assert finished["type"] == "finished"
                    assert finished["task_id"] == parent_id
                    assert finished["result"] == final_parent.result
                    with pytest.raises(ConnectionClosedOK):
                        ws.recv(timeout=5.0)

                    # The direct subscription was live before cancellation
                    # and receives the parent terminal event too.
                    deadline = time.monotonic() + 5
                    while time.monotonic() < deadline:
                        if any(
                            getattr(event, "result", None) == "cancelled"
                            for event in events
                        ):
                            break
                        time.sleep(0.05)
                    assert any(
                        getattr(event, "result", None) == "cancelled"
                        for event in events
                    ), events
                    assert (
                        self.client.disks.get(keep_name, by_name=True).show().name
                        == keep_name
                    )
                    with pytest.raises(VmNotFound):
                        self.client.vms.get(future_vm, by_name=True)
            finally:
                sub.close()
        finally:
            self.node.run(f"pkill -f 'python3 -c.*{port}'", check=False)
            self.node.run(f"rm -rf {server_dir}", check=False)
            for name in (import_name, keep_name):
                try:
                    self.client.disks.get(name, by_name=True).delete()
                except Exception:
                    pass
