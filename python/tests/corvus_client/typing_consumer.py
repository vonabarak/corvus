"""Static consumer contract, checked by the repository's mypy lint target."""

from __future__ import annotations

from collections.abc import Iterator
from typing import TYPE_CHECKING

from corvus_client import AsyncClient, Client
from corvus_client.types import (
    ApplyStreamItem,
    BuildStreamItem,
    GuestAgentStatus,
    StatusInfo,
    TaskProgressEvent,
    VmDetails,
    VmInfo,
)

if TYPE_CHECKING:

    def check_sync(client: Client) -> None:
        status: StatusInfo = client.status()
        vms: list[VmInfo] = client.vms.list()
        details: VmDetails = client.vms.get("web-1").show()
        build: Iterator[BuildStreamItem] = client.build_stream("build.yml")
        apply: Iterator[ApplyStreamItem] = client.apply_stream("vms.yml")
        client.vms.create("worker", ram_mb=2048)
        client.vms.create("worker", ram_mb="large")  # type: ignore[arg-type]
        client.vms.get("worker").edit(cpu_count="four")  # type: ignore[arg-type]
        _ = (status, vms, details, build, apply)

    async def check_async(client: AsyncClient) -> None:
        status: StatusInfo = await client.status()
        vms: list[VmInfo] = await client.vms.list()
        vm = await client.vms.get("web-1")
        details: VmDetails = await vm.show()

        async def on_guest_agent(event: GuestAgentStatus) -> None:
            _ = event.reachable

        async def on_progress(event: TaskProgressEvent) -> None:
            _ = event.task_id

        await vm.subscribe_guest_agent(on_guest_agent)
        await client.tasks.subscribe(42, on_progress)
        _ = (status, vms, details)
