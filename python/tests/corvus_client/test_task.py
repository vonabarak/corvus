"""Task manager: list + filter."""

from __future__ import annotations

from pathlib import Path

from corvus_client import AsyncClient

from ._helpers import with_client


def test_task_list_after_disk_create(daemon_socket: Path) -> None:
    """Every mutating call (here `disks.create`) creates a Task row.

    We verify the manager can list/get/filter without asserting on
    exact counts (the daemon's startup task is also recorded). Also
    verify that the Unix-socket client name is recorded as "local".
    """
    run = with_client(daemon_socket)

    async def go(c: AsyncClient) -> None:
        disk = await c.disks.create("py-task-disk", size_mb=32)
        disk_info = await disk.show()
        tasks = await c.tasks.list(subsystem="disk")
        assert tasks, "expected at least one disk task after create"
        entity_tasks = await c.tasks.list(entity_id=disk_info.id)
        assert entity_tasks, "expected task rows for the created disk"
        assert all(t.entity and t.entity.id == disk_info.id for t in entity_tasks)
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


def test_task_cancel_dispatches(daemon_socket: Path) -> None:
    """`tasks.cancel` dispatches even when the target has already finished."""
    run = with_client(daemon_socket)

    async def go(c: AsyncClient) -> None:
        disk = await c.disks.create("py-task-cancel", size_mb=32)
        info = await disk.show()
        task = next(
            t for t in await c.tasks.list(entity_id=info.id) if t.command == "create"
        )
        await c.tasks.cancel(task.id)  # completed tasks are a daemon-side no-op
        await disk.delete()

    run(go)
