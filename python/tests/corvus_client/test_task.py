"""Task manager: list + filter."""

from __future__ import annotations

from ._helpers import with_client


def test_task_list_after_disk_create(daemon_socket):
    """Every mutating call (here `disks.create`) creates a Task row.

    We verify the manager can list/get/filter without asserting on
    exact counts (the daemon's startup task is also recorded). Also
    verify that the Unix-socket client name is recorded as "local".
    """
    run = with_client(daemon_socket)

    async def go(c):
        disk = await c.disks.create("py-task-disk", size_mb=32)
        await disk.show()
        tasks = await c.tasks.list(subsystem="disk")
        assert tasks, "expected at least one disk task after create"
        # Look up one task by id via the resource cap.
        first = tasks[0]
        same = await (await c.tasks.get(first.id)).show()
        assert same.id == first.id
        # Unix-socket connections are recorded as "local".
        assert first.client_name == "local", (
            f"expected client_name='local' on unix-socket task, got {first.client_name!r}"
        )
        await disk.delete()

    run(go)
