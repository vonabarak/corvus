"""Worker-thread bridge between Qt's GUI loop and ``AsyncClient``.

Threading contract
==================

There are exactly two threads:

* the Qt **GUI thread** (where ``QApplication.exec()`` runs);
* one dedicated **worker thread** (``corvus-desktop-bridge``).

The worker thread is the **only** thread that touches pycapnp objects.
``AsyncClient`` and every cap-bearing wrapper it returns (``AsyncVm``,
``AsyncTask``, ``ByteStream``, subscriptions) live and die there. Only
frozen dataclasses from ``corvus_client.types`` (``StatusInfo``,
``VmInfo``, ``TaskInfo``, ``DiskImageInfo``, …) and plain ``bytes``
ever cross the boundary as signal payloads.

The worker runs its own asyncio loop wrapped in ``capnp.kj_loop()``.
The lifespan mirrors :mod:`corvus_web.app`'s FastAPI lifespan:

* ``AsyncExitStack`` enters ``capnp.kj_loop()`` first;
* then enters ``AsyncClient`` (which performs the TLS handshake);
* on ``connected``, sits in ``await stop_event.wait()`` until the GUI
  thread requests shutdown via ``call_soon_threadsafe``.

Signals are emitted from the worker thread. PySide6 marshals them to
the GUI thread automatically via ``Qt.QueuedConnection`` when the
receiver lives there (which is the case for any widget). Slots in the
opposite direction (GUI → worker) push coroutines onto the worker
loop via ``asyncio.run_coroutine_threadsafe``.

This Phase 1 surface is intentionally narrow — only ``request_status``
is implemented. Later phases extend with ``request_vm_list``,
``start_vm``, ``open_serial``, ``subscribe_task``, etc., all following
the same template: a slot that enqueues a coroutine, a coroutine that
calls into ``AsyncClient`` and emits a signal with the result.
"""

from __future__ import annotations

import asyncio
import logging
import threading
from collections.abc import Coroutine
from contextlib import AsyncExitStack
from typing import Any

import capnp
from corvus_client._async.client import AsyncClient
from corvus_client.exceptions import CorvusError
from PySide6.QtCore import QObject, Signal

from .cli import DesktopConfig

logger = logging.getLogger("corvus_desktop.bridge")


def _friendly_error(err: BaseException) -> str:
    """Translate raw daemon / pycapnp errors into messages the GUI can
    show without burying the operator in stack-trace text.

    The most useful one is the Cap'n Proto schema-mismatch case: the
    running daemon binary was compiled before a schema change the
    client just made (or vice-versa). ``corvus_client.exceptions``
    surfaces it as a ``ServerError`` whose message contains
    ``"Schema mismatch"``; we rewrite that to point the user at the
    fix instead of the raw KJ exception text.
    """
    msg = str(err)
    if "Schema mismatch" in msg:
        return "daemon schema is older than the client — rebuild and restart the daemon"
    return msg


class CorvusBridge(QObject):
    """Owns the worker thread that runs ``AsyncClient`` for the GUI."""

    # Worker → GUI signals. PySide6 marshals across threads via
    # Qt.QueuedConnection automatically when sender and receiver live
    # on different threads (the bridge is constructed on the GUI thread
    # so all widgets that connect() get queued delivery for free).
    connected = Signal()
    connection_failed = Signal(str)
    status_ready = Signal(object)  # corvus_client.types.StatusInfo
    operation_failed = Signal(str, str)  # (op_name, message)

    # Tasks (Phase 2). task_event carries (task_id, TaskProgressStarted
    # | TaskProgressProgress | TaskProgressFinished) — three different
    # dataclasses share the channel; receivers dispatch on isinstance.
    task_list_ready = Signal(object)  # list[TaskInfo]
    task_event = Signal(int, object)
    task_detail_ready = Signal(object)  # TaskInfo — static record

    # VMs (Phase 3).
    vm_list_ready = Signal(object)  # list[VmInfo]
    vm_detail_ready = Signal(object)  # VmDetails
    # action: one of "start"/"stop"/"pause"/"reset"/"save"/"send_ctrl_alt_del"
    vm_action_completed = Signal(int, str, str)  # (vm_id, action, new_status)
    serial_opened = Signal(int)  # vm_id
    serial_data = Signal(int, bytes)  # (vm_id, chunk)
    serial_closed = Signal(int, str)  # (vm_id, reason)

    # Disks (Phase 4).
    disk_list_ready = Signal(object)  # list[DiskImageInfo]
    disk_detail_ready = Signal(object)  # DiskImageInfo
    snapshot_list_ready = Signal(int, object)  # (disk_id, list[SnapshotInfo])
    # disk action ∈ "delete" | "resize"
    disk_action_completed = Signal(int, str)  # (disk_id, action)
    # snapshot action ∈ "create" | "delete" | "rollback" | "merge"
    snapshot_action_completed = Signal(int, str, str)  # (disk_id, action, snap_name)

    # SSH keys (Phase 6).
    ssh_key_list_ready = Signal(object)  # list[SshKeyInfo]
    # action ∈ "create" | "delete"; key_id is 0 on create until refresh
    ssh_key_action_completed = Signal(int, str)  # (key_id, action)

    # Networks (Phase 6).
    network_list_ready = Signal(object)  # list[NetworkInfo]
    network_detail_ready = Signal(object)  # NetworkInfo
    # action ∈ "create" | "delete" | "start" | "stop" | "edit"
    network_action_completed = Signal(int, str)  # (network_id, action)

    # Templates / cloud-init / apply (Phase 7).
    template_list_ready = Signal(object)  # list[TemplateVmInfo]
    template_detail_ready = Signal(object)  # TemplateDetails
    # action ∈ "create" | "update" | "delete" | "instantiate"
    template_action_completed = Signal(int, str)  # (template_id, action)
    cloud_init_ready = Signal(int, object)  # (vm_id, CloudInitInfo)
    cloud_init_action_completed = Signal(int, str)  # (vm_id, action)
    apply_completed = Signal(object, int)  # (ApplyResult, task_id)

    # VM editing surface (Phase 8).
    # action ∈ "create" | "edit" | "delete" | "attach_disk" | "detach_disk"
    #   | "add_net_if" | "remove_net_if" | "attach_ssh_key" | "detach_ssh_key"
    #   | "add_shared_dir" | "remove_shared_dir"
    vm_edit_completed = Signal(int, str)  # (vm_id, action) — id=0 on create
    vm_shared_dirs_ready = Signal(int, object)  # (vm_id, list[SharedDirInfo])
    guest_exec_result = Signal(int, object)  # (vm_id, GuestExecResult)

    # Disk creation flows (Phase 9). Same disk_action_completed channel
    # the Phase 4 mutations use (action ∈ "create"|"register"|"overlay"|
    # "clone"|"import_url"|"import_file"|"rebase"|"flatten"|"copy"|"move").
    # disk_import_started carries the task id so the GUI can subscribe.
    disk_import_started = Signal(int)  # task_id

    # Nodes / multi-node ops (Phase 10).
    node_list_ready = Signal(object)  # list[NodeInfo]
    node_detail_ready = Signal(object)  # NodeDetails
    # action ∈ "create" | "edit" | "drain" | "delete"
    node_action_completed = Signal(int, str)  # (node_id, action) — id=0 on create
    # vm_migrate_started carries the task id for the migration.
    vm_migrate_started = Signal(int, int)  # (vm_id, task_id)

    # Build pipeline streaming (Phase 11).
    # Each build event (BuildLogLine / BuildStepStart / BuildStepOutput /
    # BuildStepEnd / BuildBuildEnd / BuildPipelineEnd) crosses as a single
    # build_event payload. The final ("task_id", N) tuple becomes a
    # build_started emission with that id, then build_finished closes
    # the session.
    build_started = Signal(int)  # task_id
    build_event = Signal(object)  # one of the BuildBuild* dataclasses
    build_finished = Signal(str)  # "" on success, otherwise error message

    # Phase 12 — polish.
    vm_stats_event = Signal(int, object)  # (vm_id, VmStats)
    vm_stats_history_ready = Signal(int, object)  # (vm_id, list[VmStats])
    guest_agent_event = Signal(int, object)  # (vm_id, GuestAgentStatus)
    view_grant_ready = Signal(int, object)  # (vm_id, ViewGrant)
    daemon_shutdown_completed = Signal()
    hmp_data = Signal(int, bytes)
    hmp_closed = Signal(int, str)
    # VM-attached SSH keys (list-vm).
    vm_ssh_keys_ready = Signal(int, object)  # (vm_id, list[SshKeyInfo])

    def __init__(self, config: DesktopConfig, parent: QObject | None = None) -> None:
        super().__init__(parent)
        self._config = config
        self._thread: threading.Thread | None = None
        # _loop and _stop_event are populated by the worker on startup
        # and read from the GUI thread. The worker writes them BEFORE
        # emitting `connected`, so any GUI code that waits for that
        # signal is guaranteed to see populated values.
        self._loop: asyncio.AbstractEventLoop | None = None
        self._stop_event: asyncio.Event | None = None
        self._client: AsyncClient | None = None
        # Task subscriptions, keyed by task id. Owned exclusively by
        # the worker thread; mutated only from inside coroutines that
        # run on the worker loop. The GUI never reads or writes this.
        self._task_subs: dict[int, Any] = {}
        # Serial sessions: (ByteStream, pump_task). Same ownership rule.
        self._serial_streams: dict[int, tuple[Any, asyncio.Task[Any]]] = {}
        # HMP monitor sessions (same pattern as serial).
        self._hmp_streams: dict[int, tuple[Any, asyncio.Task[Any]]] = {}
        # VM stats / guest-agent subscriptions keyed by vm id.
        self._stats_subs: dict[int, Any] = {}
        self._guest_agent_subs: dict[int, Any] = {}
        # Task ids the GUI wants restored on resume (set when the app
        # is paused via :meth:`pause_subscriptions`). Read & written
        # only from the worker thread inside the pause/resume coros.
        self._paused_task_subs: list[int] = []

    # ------------------------------------------------------------ lifecycle

    def start(self) -> None:
        """Start the worker thread. Returns immediately.

        Subscribe to :attr:`connected` / :attr:`connection_failed`
        before calling this — the worker may emit either signal as
        soon as ``AsyncClient.__aenter__`` resolves (or raises).
        """
        if self._thread is not None:
            raise RuntimeError("bridge already started")
        self._thread = threading.Thread(
            target=self._serve,
            name="corvus-desktop-bridge",
            daemon=True,
        )
        self._thread.start()

    def shutdown(self, timeout: float = 10.0) -> None:
        """Request graceful shutdown and join the worker thread.

        Safe to call from the GUI thread. After this returns, no
        further signals will fire. Idempotent.
        """
        loop, stop = self._loop, self._stop_event
        if loop is not None and stop is not None:
            # The worker's `_main` awaits stop_event; set it from
            # whichever thread is calling, via the loop's own
            # thread-safe scheduling primitive.
            try:
                loop.call_soon_threadsafe(stop.set)
            except RuntimeError:
                # Loop already closed — the worker exited on its own
                # (e.g. connection_failed). Nothing to do.
                pass
        if self._thread is not None and self._thread.is_alive():
            self._thread.join(timeout=timeout)

    # --------------------------------------------------------- public slots

    def request_status(self) -> None:
        """Ask the daemon for a ``StatusInfo``; emit :attr:`status_ready`."""
        self._enqueue(self._do_status())

    def request_task_list(
        self,
        limit: int | None = 50,
        subsystem: str | None = None,
        result: str | None = None,
    ) -> None:
        """List recent tasks; emit :attr:`task_list_ready`."""
        self._enqueue(self._do_task_list(limit, subsystem, result))

    def request_task_detail(self, task_id: int) -> None:
        """Fetch the static :class:`TaskInfo` record; emit
        :attr:`task_detail_ready`. Use alongside :meth:`subscribe_task`
        — the subscription only fires for new progress events, so
        already-finished tasks need the static row to populate fields."""
        self._enqueue(self._do_task_detail(task_id))

    def subscribe_task(self, task_id: int) -> None:
        """Subscribe to live progress for ``task_id``. Each event fires
        :attr:`task_event`; the final ``TaskProgressFinished`` is also
        delivered through the same channel. Safe to call again — the
        bridge collapses duplicate subscriptions per task id."""
        self._enqueue(self._do_subscribe_task(task_id))

    def unsubscribe_task(self, task_id: int) -> None:
        """Tear down a subscription. Idempotent: unknown ids are a no-op."""
        self._enqueue(self._do_unsubscribe_task(task_id))

    def request_vm_list(self) -> None:
        """List all VMs; emit :attr:`vm_list_ready`."""
        self._enqueue(self._do_vm_list())

    def request_vm_detail(self, vm_id: int) -> None:
        """Fetch a single VM's full :class:`VmDetails`."""
        self._enqueue(self._do_vm_detail(vm_id))

    def vm_action(self, vm_id: int, action: str) -> None:
        """Drive a VM lifecycle transition.

        ``action`` ∈ ``{"start", "stop", "pause", "reset", "save",
        "send_ctrl_alt_del"}``. The bridge emits
        :attr:`vm_action_completed` on success and
        :attr:`operation_failed` on a daemon-side error or invalid
        state transition.
        """
        self._enqueue(self._do_vm_action(vm_id, action))

    def open_serial(self, vm_id: int) -> None:
        """Open a bidirectional serial console to ``vm_id``. The
        worker pumps chunks via :attr:`serial_data`; the close is
        signalled by :attr:`serial_closed`."""
        self._enqueue(self._do_open_serial(vm_id))

    def send_serial(self, vm_id: int, chunk: bytes) -> None:
        """Write bytes from the GUI's terminal widget to the daemon."""
        self._enqueue(self._do_send_serial(vm_id, chunk))

    def close_serial(self, vm_id: int) -> None:
        """Close the serial session. Idempotent — unknown ids no-op."""
        self._enqueue(self._do_close_serial(vm_id))

    def request_disk_list(self) -> None:
        """List all disks; emit :attr:`disk_list_ready`."""
        self._enqueue(self._do_disk_list())

    def request_disk_detail(self, disk_id: int) -> None:
        """Fetch a single disk's :class:`DiskImageInfo`."""
        self._enqueue(self._do_disk_detail(disk_id))

    def request_snapshot_list(self, disk_id: int) -> None:
        """List snapshots on ``disk_id``; emit :attr:`snapshot_list_ready`."""
        self._enqueue(self._do_snapshot_list(disk_id))

    def disk_resize(self, disk_id: int, new_size_mb: int) -> None:
        """Grow a qcow2 disk to ``new_size_mb``."""
        self._enqueue(self._do_disk_resize(disk_id, new_size_mb))

    def disk_delete(self, disk_id: int) -> None:
        """Delete a disk (must not be attached to any VM)."""
        self._enqueue(self._do_disk_delete(disk_id))

    def snapshot_create(self, disk_id: int, name: str) -> None:
        """Create a new named snapshot on ``disk_id``."""
        self._enqueue(self._do_snapshot_create(disk_id, name))

    def snapshot_action(self, disk_id: int, snap_name: str, action: str) -> None:
        """Drive a per-snapshot action: ``"delete"`` | ``"rollback"`` | ``"merge"``."""
        self._enqueue(self._do_snapshot_action(disk_id, snap_name, action))

    def request_ssh_key_list(self) -> None:
        """List all SSH keys; emit :attr:`ssh_key_list_ready`."""
        self._enqueue(self._do_ssh_key_list())

    def ssh_key_create(self, name: str, public_key: str) -> None:
        """Create an SSH key entry. Emits :attr:`ssh_key_action_completed`
        with ``key_id=0`` (the GUI refreshes the list to pick up the real id)."""
        self._enqueue(self._do_ssh_key_create(name, public_key))

    def ssh_key_delete(self, key_id: int) -> None:
        """Delete an SSH key by id."""
        self._enqueue(self._do_ssh_key_delete(key_id))

    def request_network_list(self) -> None:
        """List all networks; emit :attr:`network_list_ready`."""
        self._enqueue(self._do_network_list())

    def request_network_detail(self, network_id: int) -> None:
        """Fetch a single network's :class:`NetworkInfo`."""
        self._enqueue(self._do_network_detail(network_id))

    def network_create(
        self,
        name: str,
        subnet: str,
        *,
        node: str | None = None,
        dhcp: bool = False,
        nat: bool = False,
        autostart: bool = False,
    ) -> None:
        """Create a virtual network."""
        self._enqueue(self._do_network_create(name, subnet, node, dhcp, nat, autostart))

    def network_delete(self, network_id: int) -> None:
        self._enqueue(self._do_network_delete(network_id))

    def network_start(self, network_id: int) -> None:
        self._enqueue(self._do_network_start(network_id))

    def network_stop(self, network_id: int, *, force: bool = False) -> None:
        self._enqueue(self._do_network_stop(network_id, force))

    def network_edit(
        self,
        network_id: int,
        *,
        subnet: str | None = None,
        dhcp: bool | None = None,
        nat: bool | None = None,
        autostart: bool | None = None,
    ) -> None:
        """Patch an existing network. Only non-``None`` fields are sent."""
        self._enqueue(self._do_network_edit(network_id, subnet, dhcp, nat, autostart))

    def request_template_list(self) -> None:
        self._enqueue(self._do_template_list())

    def request_template_detail(self, template_id: int) -> None:
        self._enqueue(self._do_template_detail(template_id))

    def template_create(self, yaml_text: str) -> None:
        self._enqueue(self._do_template_create(yaml_text))

    def template_update(self, template_id: int, yaml_text: str) -> None:
        self._enqueue(self._do_template_update(template_id, yaml_text))

    def template_delete(self, template_id: int) -> None:
        self._enqueue(self._do_template_delete(template_id))

    def template_instantiate(
        self, template_id: int, vm_name: str, node: str | None = None
    ) -> None:
        self._enqueue(self._do_template_instantiate(template_id, vm_name, node))

    def request_cloud_init(self, vm_id: int) -> None:
        self._enqueue(self._do_cloud_init_get(vm_id))

    def cloud_init_set(
        self,
        vm_id: int,
        *,
        user_data: str | None = None,
        network_config: str | None = None,
        inject_ssh_keys: bool = False,
    ) -> None:
        self._enqueue(
            self._do_cloud_init_set(vm_id, user_data, network_config, inject_ssh_keys)
        )

    def cloud_init_delete(self, vm_id: int) -> None:
        self._enqueue(self._do_cloud_init_delete(vm_id))

    def apply_yaml(self, yaml_text: str, *, skip_existing: bool = False) -> None:
        self._enqueue(self._do_apply(yaml_text, skip_existing))

    # ---------------------------------------------------- VM editing slots

    def vm_create(self, **kwargs: Any) -> None:
        """Create a VM. Accepts the AsyncVmManager.create kwarg surface."""
        self._enqueue(self._do_vm_create(kwargs))

    def vm_edit(self, vm_id: int, **kwargs: Any) -> None:
        """Patch a VM. Only non-``None`` kwargs are sent."""
        self._enqueue(self._do_vm_edit(vm_id, kwargs))

    def vm_delete(self, vm_id: int, *, keep_disks: bool = False) -> None:
        self._enqueue(self._do_vm_delete(vm_id, keep_disks))

    def vm_attach_disk(
        self,
        vm_id: int,
        disk_ref: int | str,
        *,
        interface: str | None = None,
        media: str | None = None,
        read_only: bool = False,
        cache_type: str | None = None,
        discard: bool = False,
    ) -> None:
        self._enqueue(
            self._do_vm_attach_disk(
                vm_id, disk_ref, interface, media, read_only, cache_type, discard
            )
        )

    def vm_detach_disk(self, vm_id: int, drive_id: int) -> None:
        self._enqueue(self._do_vm_detach_disk(vm_id, drive_id))

    def vm_add_net_if(
        self,
        vm_id: int,
        *,
        type: str | None = None,
        host_device: str | None = None,
        mac_address: str | None = None,
        network_ref: int | str | None = None,
    ) -> None:
        self._enqueue(
            self._do_vm_add_net_if(vm_id, type, host_device, mac_address, network_ref)
        )

    def vm_remove_net_if(self, vm_id: int, net_if_id: int) -> None:
        self._enqueue(self._do_vm_remove_net_if(vm_id, net_if_id))

    def vm_attach_ssh_key(self, vm_id: int, key_ref: int | str) -> None:
        self._enqueue(self._do_vm_attach_ssh_key(vm_id, key_ref))

    def vm_detach_ssh_key(self, vm_id: int, key_ref: int | str) -> None:
        self._enqueue(self._do_vm_detach_ssh_key(vm_id, key_ref))

    def vm_add_shared_dir(
        self,
        vm_id: int,
        path: str,
        tag: str,
        *,
        cache: str | None = None,
        read_only: bool = False,
    ) -> None:
        self._enqueue(self._do_vm_add_shared_dir(vm_id, path, tag, cache, read_only))

    def vm_remove_shared_dir(self, vm_id: int, shared_dir_id: int) -> None:
        self._enqueue(self._do_vm_remove_shared_dir(vm_id, shared_dir_id))

    def request_vm_shared_dirs(self, vm_id: int) -> None:
        self._enqueue(self._do_vm_shared_dirs(vm_id))

    def vm_guest_exec(self, vm_id: int, command: str) -> None:
        self._enqueue(self._do_vm_guest_exec(vm_id, command))

    # ---------------------------------------------------- disk creation slots

    def disk_create(
        self,
        name: str,
        size_mb: int,
        *,
        format: str | None = None,
        ephemeral: bool = False,
        node: int | str | None = None,
    ) -> None:
        self._enqueue(self._do_disk_create(name, size_mb, format, ephemeral, node))

    def disk_register(
        self,
        name: str,
        file_path: str,
        *,
        format: str | None = None,
        ephemeral: bool = False,
        node: int | str | None = None,
    ) -> None:
        self._enqueue(self._do_disk_register(name, file_path, format, ephemeral, node))

    def disk_overlay(
        self,
        name: str,
        backing_disk_ref: int | str,
        *,
        ephemeral: bool = False,
    ) -> None:
        self._enqueue(self._do_disk_overlay(name, backing_disk_ref, ephemeral))

    def disk_clone(
        self,
        source_ref: int | str,
        new_name: str,
        *,
        path: str | None = None,
        ephemeral: bool = False,
    ) -> None:
        self._enqueue(self._do_disk_clone(source_ref, new_name, path, ephemeral))

    def disk_import_url(
        self,
        name: str,
        url: str,
        *,
        format: str | None = None,
        size_mb: int | None = None,
        ephemeral: bool = False,
        node: int | str | None = None,
    ) -> None:
        self._enqueue(
            self._do_disk_import_url(name, url, format, size_mb, ephemeral, node)
        )

    def disk_rebase(self, disk_id: int, new_backing_disk_ref: int | str) -> None:
        self._enqueue(self._do_disk_rebase(disk_id, new_backing_disk_ref))

    def disk_flatten(self, disk_id: int) -> None:
        self._enqueue(self._do_disk_flatten(disk_id))

    def disk_copy(
        self,
        disk_id: int,
        to_node_ref: int | str,
        *,
        to_path: str | None = None,
        with_backing_chain: bool = False,
    ) -> None:
        self._enqueue(
            self._do_disk_copy(disk_id, to_node_ref, to_path, with_backing_chain)
        )

    def disk_move(
        self,
        disk_id: int,
        to_node_ref: int | str,
        *,
        to_path: str | None = None,
        with_backing_chain: bool = False,
    ) -> None:
        self._enqueue(
            self._do_disk_move(disk_id, to_node_ref, to_path, with_backing_chain)
        )

    # ---------------------------------------------------- node / multi-node

    def request_node_list(self) -> None:
        self._enqueue(self._do_node_list())

    def request_node_detail(self, node_id: int) -> None:
        self._enqueue(self._do_node_detail(node_id))

    def node_create(
        self,
        name: str,
        host: str,
        *,
        node_agent_port: int = 9878,
        net_agent_port: int = 9877,
        base_path: str = "/home/corvus/VMs",
        description: str | None = None,
        admin_state: str = "online",
        netd_disabled: bool = False,
    ) -> None:
        self._enqueue(
            self._do_node_create(
                name,
                host,
                node_agent_port,
                net_agent_port,
                base_path,
                description,
                admin_state,
                netd_disabled,
            )
        )

    def node_edit(self, node_id: int, **kwargs: Any) -> None:
        self._enqueue(self._do_node_edit(node_id, kwargs))

    def node_drain(self, node_id: int) -> None:
        self._enqueue(self._do_node_drain(node_id))

    def node_delete(self, node_id: int) -> None:
        self._enqueue(self._do_node_delete(node_id))

    def vm_migrate(self, vm_id: int, to_node_ref: int | str) -> None:
        self._enqueue(self._do_vm_migrate(vm_id, to_node_ref))

    def network_attach_node(self, network_id: int, node_ref: int | str) -> None:
        self._enqueue(self._do_network_attach_node(network_id, node_ref))

    def network_detach_node(self, network_id: int, node_ref: int | str) -> None:
        self._enqueue(self._do_network_detach_node(network_id, node_ref))

    def build_run(self, yaml_text: str) -> None:
        """Stream daemon build events for ``yaml_text``.

        Emits one :attr:`build_event` per event, then
        :attr:`build_started` once the task id is known, then
        :attr:`build_finished` on stream close (empty payload on
        success, error string otherwise).
        """
        self._enqueue(self._do_build_run(yaml_text))

    # ---------------------------------------------------- Phase 12 slots

    def subscribe_vm_stats(self, vm_id: int) -> None:
        self._enqueue(self._do_subscribe_vm_stats(vm_id))

    def unsubscribe_vm_stats(self, vm_id: int) -> None:
        self._enqueue(self._do_unsubscribe_vm_stats(vm_id))

    def request_vm_stats_history(self, vm_id: int) -> None:
        """Seed the GUI's chart from the daemon's recent-samples buffer."""
        self._enqueue(self._do_vm_stats_history(vm_id))

    def request_vm_ssh_keys(self, vm_id: int) -> None:
        """List the SSH keys attached to a VM; emit :attr:`vm_ssh_keys_ready`."""
        self._enqueue(self._do_vm_ssh_keys(vm_id))

    def subscribe_guest_agent(self, vm_id: int) -> None:
        self._enqueue(self._do_subscribe_guest_agent(vm_id))

    def unsubscribe_guest_agent(self, vm_id: int) -> None:
        self._enqueue(self._do_unsubscribe_guest_agent(vm_id))

    def request_view_grant(self, vm_id: int) -> None:
        """Mint a SPICE view grant. The GUI uses ``remote-viewer`` to
        present it (see :class:`corvus_desktop.widgets.spice_launcher`)."""
        self._enqueue(self._do_view_grant(vm_id))

    def daemon_shutdown(self) -> None:
        self._enqueue(self._do_daemon_shutdown())

    def open_hmp(self, vm_id: int) -> None:
        self._enqueue(self._do_open_hmp(vm_id))

    def send_hmp(self, vm_id: int, chunk: bytes) -> None:
        self._enqueue(self._do_send_hmp(vm_id, chunk))

    def close_hmp(self, vm_id: int) -> None:
        self._enqueue(self._do_close_hmp(vm_id))

    def pause_subscriptions(self) -> None:
        """Drop every live subscription. The set of task ids is
        remembered so :meth:`resume_subscriptions` can restore them.
        Called when the application moves to the background — keeps
        the daemon from pushing events to a window the user can't see."""
        self._enqueue(self._do_pause_subscriptions())

    def resume_subscriptions(self) -> None:
        """Re-subscribe to every task that was active when
        :meth:`pause_subscriptions` was last called."""
        self._enqueue(self._do_resume_subscriptions())

    # ----------------------------------------------------------- internals

    def _enqueue(self, coro: Coroutine[Any, Any, None]) -> None:
        """Push a coroutine onto the worker loop from any thread.

        If the bridge isn't connected yet (or has shut down), surface
        the failure via :attr:`operation_failed` rather than raising —
        the GUI shouldn't have to None-check before every slot call.
        """
        loop = self._loop
        if loop is None or not loop.is_running():
            coro.close()
            self.operation_failed.emit(
                "bridge", "bridge not connected; request dropped"
            )
            return
        asyncio.run_coroutine_threadsafe(coro, loop)

    def _serve(self) -> None:
        """Worker-thread entry point. Owns the asyncio loop."""
        loop = asyncio.new_event_loop()
        asyncio.set_event_loop(loop)
        self._loop = loop
        try:
            loop.run_until_complete(self._main())
        finally:
            # Drop references on the worker thread (where pycapnp
            # finalisers belong) before the thread exits.
            self._task_subs.clear()
            self._serial_streams.clear()
            self._hmp_streams.clear()
            self._stats_subs.clear()
            self._guest_agent_subs.clear()
            self._client = None
            self._stop_event = None
            loop.close()
            self._loop = None

    async def _main(self) -> None:
        """Lifespan: kj_loop → AsyncClient → wait for stop."""
        self._stop_event = asyncio.Event()
        try:
            async with AsyncExitStack() as stack:
                await stack.enter_async_context(capnp.kj_loop())
                client = await stack.enter_async_context(
                    AsyncClient(**self._client_kwargs())
                )
                self._client = client
                logger.info("bridge: connected to daemon")
                self.connected.emit()
                await self._stop_event.wait()
                logger.info("bridge: shutdown requested, unwinding")
        except CorvusError as e:
            logger.warning("bridge: connection failed: %s", e)
            self.connection_failed.emit(_friendly_error(e))
        except Exception as e:
            # AsyncClient.__aenter__ can raise OSError (refused socket,
            # bad cert path); pycapnp raises capnp.lib.capnp.KjException
            # on protocol errors. Surface them all via the same channel
            # so the GUI has one place to dispatch.
            logger.exception("bridge: unexpected error during lifespan")
            self.connection_failed.emit(f"{type(e).__name__}: {e}")

    def _client_kwargs(self) -> dict[str, Any]:
        """Translate :class:`DesktopConfig` into AsyncClient kwargs."""
        c = self._config
        kwargs: dict[str, Any] = {}
        if c.daemon_unix_socket is not None:
            kwargs["unix_socket"] = c.daemon_unix_socket
        else:
            # cli._resolve_config guarantees host is set when unix is None.
            assert c.daemon_host is not None
            kwargs["host"] = c.daemon_host
            kwargs["port"] = c.daemon_port
        if c.daemon_tls is not None:
            kwargs["tls"] = c.daemon_tls
        if c.daemon_cert_dir is not None:
            kwargs["cert_dir"] = c.daemon_cert_dir
        return kwargs

    async def _do_status(self) -> None:
        client = self._client
        if client is None:
            self.operation_failed.emit("status", "not connected")
            return
        try:
            info = await client.status()
        except CorvusError as e:
            self.operation_failed.emit("status", _friendly_error(e))
            return
        self.status_ready.emit(info)

    async def _do_task_list(
        self,
        limit: int | None,
        subsystem: str | None,
        result: str | None,
    ) -> None:
        client = self._client
        if client is None:
            self.operation_failed.emit("task_list", "not connected")
            return
        try:
            tasks = await client.tasks.list(
                limit=limit, subsystem=subsystem, result=result
            )
        except CorvusError as e:
            self.operation_failed.emit("task_list", _friendly_error(e))
            return
        self.task_list_ready.emit(tasks)

    async def _do_task_detail(self, task_id: int) -> None:
        client = self._client
        if client is None:
            self.operation_failed.emit("task_detail", "not connected")
            return
        try:
            task = await client.tasks.get(task_id)
            info = await task.show()
        except CorvusError as e:
            self.operation_failed.emit("task_detail", _friendly_error(e))
            return
        self.task_detail_ready.emit(info)

    async def _do_subscribe_task(self, task_id: int) -> None:
        client = self._client
        if client is None:
            self.operation_failed.emit("task_subscribe", "not connected")
            return
        if task_id in self._task_subs:
            # Already subscribed — the daemon would happily accept a
            # second subscription on the same task, but the GUI gets
            # duplicate events. Quietly collapse.
            return

        async def _emit(event: Any) -> None:
            # Runs on the worker thread; the signal hop marshals to
            # the GUI thread automatically.
            self.task_event.emit(task_id, event)

        try:
            sub = await client.tasks.subscribe(task_id, _emit)
        except CorvusError as e:
            self.operation_failed.emit("task_subscribe", _friendly_error(e))
            return
        self._task_subs[task_id] = sub

    async def _do_unsubscribe_task(self, task_id: int) -> None:
        sub = self._task_subs.pop(task_id, None)
        if sub is not None:
            await sub.close()

    async def _do_vm_list(self) -> None:
        client = self._client
        if client is None:
            self.operation_failed.emit("vm_list", "not connected")
            return
        try:
            vms = await client.vms.list()
        except CorvusError as e:
            self.operation_failed.emit("vm_list", _friendly_error(e))
            return
        self.vm_list_ready.emit(vms)

    async def _do_vm_detail(self, vm_id: int) -> None:
        client = self._client
        if client is None:
            self.operation_failed.emit("vm_detail", "not connected")
            return
        try:
            vm = await client.vms.get(vm_id)
            details = await vm.show()
        except CorvusError as e:
            self.operation_failed.emit("vm_detail", _friendly_error(e))
            return
        self.vm_detail_ready.emit(details)

    async def _do_vm_action(self, vm_id: int, action: str) -> None:
        client = self._client
        if client is None:
            self.operation_failed.emit(f"vm_{action}", "not connected")
            return
        op_label = f"vm_{action}"
        try:
            vm = await client.vms.get(vm_id)
            new_status = ""
            if action == "start":
                new_status = await vm.start()
            elif action == "stop":
                new_status = await vm.stop()
            elif action == "pause":
                new_status = await vm.pause()
            elif action == "reset":
                new_status = await vm.reset()
            elif action == "save":
                new_status = await vm.save()
            elif action == "send_ctrl_alt_del":
                await vm.send_ctrl_alt_del()
            else:
                self.operation_failed.emit(op_label, f"unknown action {action!r}")
                return
        except CorvusError as e:
            self.operation_failed.emit(op_label, _friendly_error(e))
            return
        self.vm_action_completed.emit(vm_id, action, new_status)

    async def _do_open_serial(self, vm_id: int) -> None:
        client = self._client
        if client is None:
            self.operation_failed.emit("serial", "not connected")
            return
        if vm_id in self._serial_streams:
            return  # already open
        try:
            vm = await client.vms.get(vm_id)
            stream = await vm.serial_console()
        except CorvusError as e:
            self.operation_failed.emit("serial", _friendly_error(e))
            return
        task = asyncio.create_task(self._serial_pump(vm_id, stream))
        self._serial_streams[vm_id] = (stream, task)
        self.serial_opened.emit(vm_id)

    async def _do_send_serial(self, vm_id: int, chunk: bytes) -> None:
        entry = self._serial_streams.get(vm_id)
        if entry is None:
            self.operation_failed.emit("serial", "no active session")
            return
        stream, _ = entry
        try:
            await stream.write(chunk)
        except CorvusError as e:
            self.operation_failed.emit("serial", _friendly_error(e))

    async def _do_close_serial(self, vm_id: int) -> None:
        entry = self._serial_streams.pop(vm_id, None)
        if entry is None:
            return
        stream, task = entry
        task.cancel()
        try:
            await stream.close()
        except CorvusError:
            pass
        self.serial_closed.emit(vm_id, "closed")

    async def _do_disk_list(self) -> None:
        client = self._client
        if client is None:
            self.operation_failed.emit("disk_list", "not connected")
            return
        try:
            disks = await client.disks.list()
        except CorvusError as e:
            self.operation_failed.emit("disk_list", _friendly_error(e))
            return
        self.disk_list_ready.emit(disks)

    async def _do_disk_detail(self, disk_id: int) -> None:
        client = self._client
        if client is None:
            self.operation_failed.emit("disk_detail", "not connected")
            return
        try:
            disk = await client.disks.get(disk_id)
            info = await disk.show()
        except CorvusError as e:
            self.operation_failed.emit("disk_detail", _friendly_error(e))
            return
        self.disk_detail_ready.emit(info)

    async def _do_snapshot_list(self, disk_id: int) -> None:
        client = self._client
        if client is None:
            self.operation_failed.emit("snapshot_list", "not connected")
            return
        try:
            disk = await client.disks.get(disk_id)
            snaps = await disk.snapshot_list()
        except CorvusError as e:
            self.operation_failed.emit("snapshot_list", _friendly_error(e))
            return
        self.snapshot_list_ready.emit(disk_id, snaps)

    async def _do_disk_resize(self, disk_id: int, new_size_mb: int) -> None:
        client = self._client
        if client is None:
            self.operation_failed.emit("disk_resize", "not connected")
            return
        try:
            disk = await client.disks.get(disk_id)
            await disk.resize(new_size_mb)
        except CorvusError as e:
            self.operation_failed.emit("disk_resize", _friendly_error(e))
            return
        self.disk_action_completed.emit(disk_id, "resize")

    async def _do_disk_delete(self, disk_id: int) -> None:
        client = self._client
        if client is None:
            self.operation_failed.emit("disk_delete", "not connected")
            return
        try:
            disk = await client.disks.get(disk_id)
            await disk.delete()
        except CorvusError as e:
            self.operation_failed.emit("disk_delete", _friendly_error(e))
            return
        self.disk_action_completed.emit(disk_id, "delete")

    async def _do_snapshot_create(self, disk_id: int, name: str) -> None:
        client = self._client
        if client is None:
            self.operation_failed.emit("snapshot_create", "not connected")
            return
        try:
            disk = await client.disks.get(disk_id)
            await disk.snapshot_create(name)
        except CorvusError as e:
            self.operation_failed.emit("snapshot_create", _friendly_error(e))
            return
        self.snapshot_action_completed.emit(disk_id, "create", name)

    async def _do_pause_subscriptions(self) -> None:
        # Snapshot the current set before we close anything — the
        # close calls await, and a concurrent unsubscribe coroutine
        # could mutate _task_subs in between.
        ids = list(self._task_subs.keys())
        self._paused_task_subs = ids
        for task_id in ids:
            sub = self._task_subs.pop(task_id, None)
            if sub is not None:
                try:
                    await sub.close()
                except CorvusError:
                    pass

    async def _do_resume_subscriptions(self) -> None:
        ids, self._paused_task_subs = self._paused_task_subs, []
        for task_id in ids:
            await self._do_subscribe_task(task_id)

    async def _do_snapshot_action(
        self, disk_id: int, snap_name: str, action: str
    ) -> None:
        client = self._client
        if client is None:
            self.operation_failed.emit(f"snapshot_{action}", "not connected")
            return
        op_label = f"snapshot_{action}"
        try:
            disk = await client.disks.get(disk_id)
            snap = await disk.snapshot_get(snap_name, by_name=True)
            if action == "delete":
                await snap.delete()
            elif action == "rollback":
                await snap.rollback()
            elif action == "merge":
                await snap.merge()
            else:
                self.operation_failed.emit(op_label, f"unknown action {action!r}")
                return
        except CorvusError as e:
            self.operation_failed.emit(op_label, _friendly_error(e))
            return
        self.snapshot_action_completed.emit(disk_id, action, snap_name)

    # ------------------------------------------------------------ ssh keys

    async def _do_ssh_key_list(self) -> None:
        client = self._client
        if client is None:
            self.operation_failed.emit("ssh_key_list", "not connected")
            return
        try:
            keys = await client.ssh_keys.list()
        except CorvusError as e:
            self.operation_failed.emit("ssh_key_list", _friendly_error(e))
            return
        self.ssh_key_list_ready.emit(keys)

    async def _do_ssh_key_create(self, name: str, public_key: str) -> None:
        client = self._client
        if client is None:
            self.operation_failed.emit("ssh_key_create", "not connected")
            return
        try:
            await client.ssh_keys.create(name, public_key)
        except CorvusError as e:
            self.operation_failed.emit("ssh_key_create", _friendly_error(e))
            return
        # ``key_id=0`` is the agreed sentinel for "new — refresh to get id".
        self.ssh_key_action_completed.emit(0, "create")

    async def _do_ssh_key_delete(self, key_id: int) -> None:
        client = self._client
        if client is None:
            self.operation_failed.emit("ssh_key_delete", "not connected")
            return
        try:
            key = await client.ssh_keys.get(key_id)
            await key.delete()
        except CorvusError as e:
            self.operation_failed.emit("ssh_key_delete", _friendly_error(e))
            return
        self.ssh_key_action_completed.emit(key_id, "delete")

    # ------------------------------------------------------------ networks

    async def _do_network_list(self) -> None:
        client = self._client
        if client is None:
            self.operation_failed.emit("network_list", "not connected")
            return
        try:
            networks = await client.networks.list()
        except CorvusError as e:
            self.operation_failed.emit("network_list", _friendly_error(e))
            return
        self.network_list_ready.emit(networks)

    async def _do_network_detail(self, network_id: int) -> None:
        client = self._client
        if client is None:
            self.operation_failed.emit("network_detail", "not connected")
            return
        try:
            net = await client.networks.get(network_id)
            info = await net.show()
        except CorvusError as e:
            self.operation_failed.emit("network_detail", _friendly_error(e))
            return
        self.network_detail_ready.emit(info)

    async def _do_network_create(
        self,
        name: str,
        subnet: str,
        node: str | None,
        dhcp: bool,
        nat: bool,
        autostart: bool,
    ) -> None:
        client = self._client
        if client is None:
            self.operation_failed.emit("network_create", "not connected")
            return
        try:
            await client.networks.create(
                name,
                subnet,
                node=node,
                dhcp=dhcp,
                nat=nat,
                autostart=autostart,
            )
        except CorvusError as e:
            self.operation_failed.emit("network_create", _friendly_error(e))
            return
        self.network_action_completed.emit(0, "create")

    async def _do_network_delete(self, network_id: int) -> None:
        client = self._client
        if client is None:
            self.operation_failed.emit("network_delete", "not connected")
            return
        try:
            net = await client.networks.get(network_id)
            await net.delete()
        except CorvusError as e:
            self.operation_failed.emit("network_delete", _friendly_error(e))
            return
        self.network_action_completed.emit(network_id, "delete")

    async def _do_network_start(self, network_id: int) -> None:
        client = self._client
        if client is None:
            self.operation_failed.emit("network_start", "not connected")
            return
        try:
            net = await client.networks.get(network_id)
            await net.start()
        except CorvusError as e:
            self.operation_failed.emit("network_start", _friendly_error(e))
            return
        self.network_action_completed.emit(network_id, "start")

    async def _do_network_stop(self, network_id: int, force: bool) -> None:
        client = self._client
        if client is None:
            self.operation_failed.emit("network_stop", "not connected")
            return
        try:
            net = await client.networks.get(network_id)
            await net.stop(force=force)
        except CorvusError as e:
            self.operation_failed.emit("network_stop", _friendly_error(e))
            return
        self.network_action_completed.emit(network_id, "stop")

    async def _do_network_edit(
        self,
        network_id: int,
        subnet: str | None,
        dhcp: bool | None,
        nat: bool | None,
        autostart: bool | None,
    ) -> None:
        client = self._client
        if client is None:
            self.operation_failed.emit("network_edit", "not connected")
            return
        try:
            net = await client.networks.get(network_id)
            await net.edit(subnet=subnet, dhcp=dhcp, nat=nat, autostart=autostart)
        except CorvusError as e:
            self.operation_failed.emit("network_edit", _friendly_error(e))
            return
        self.network_action_completed.emit(network_id, "edit")

    # ------------------------------------------------------------ templates

    async def _do_template_list(self) -> None:
        client = self._client
        if client is None:
            self.operation_failed.emit("template_list", "not connected")
            return
        try:
            templates = await client.templates.list()
        except CorvusError as e:
            self.operation_failed.emit("template_list", _friendly_error(e))
            return
        self.template_list_ready.emit(templates)

    async def _do_template_detail(self, template_id: int) -> None:
        client = self._client
        if client is None:
            self.operation_failed.emit("template_detail", "not connected")
            return
        try:
            tpl = await client.templates.get(template_id)
            details = await tpl.show()
        except CorvusError as e:
            self.operation_failed.emit("template_detail", _friendly_error(e))
            return
        self.template_detail_ready.emit(details)

    async def _do_template_create(self, yaml_text: str) -> None:
        client = self._client
        if client is None:
            self.operation_failed.emit("template_create", "not connected")
            return
        try:
            await client.templates.create(yaml_text)
        except CorvusError as e:
            self.operation_failed.emit("template_create", _friendly_error(e))
            return
        self.template_action_completed.emit(0, "create")

    async def _do_template_update(self, template_id: int, yaml_text: str) -> None:
        client = self._client
        if client is None:
            self.operation_failed.emit("template_update", "not connected")
            return
        try:
            tpl = await client.templates.get(template_id)
            await tpl.update(yaml_text)
        except CorvusError as e:
            self.operation_failed.emit("template_update", _friendly_error(e))
            return
        self.template_action_completed.emit(template_id, "update")

    async def _do_template_delete(self, template_id: int) -> None:
        client = self._client
        if client is None:
            self.operation_failed.emit("template_delete", "not connected")
            return
        try:
            tpl = await client.templates.get(template_id)
            await tpl.delete()
        except CorvusError as e:
            self.operation_failed.emit("template_delete", _friendly_error(e))
            return
        self.template_action_completed.emit(template_id, "delete")

    async def _do_template_instantiate(
        self, template_id: int, vm_name: str, node: str | None
    ) -> None:
        client = self._client
        if client is None:
            self.operation_failed.emit("template_instantiate", "not connected")
            return
        try:
            tpl = await client.templates.get(template_id)
            await tpl.instantiate(vm_name, node=node)
        except CorvusError as e:
            self.operation_failed.emit("template_instantiate", _friendly_error(e))
            return
        self.template_action_completed.emit(template_id, "instantiate")

    # ------------------------------------------------------------ cloud-init

    async def _do_cloud_init_get(self, vm_id: int) -> None:
        client = self._client
        if client is None:
            self.operation_failed.emit("cloud_init_get", "not connected")
            return
        try:
            info = await client.cloud_init.get(vm_id)
        except CorvusError as e:
            self.operation_failed.emit("cloud_init_get", _friendly_error(e))
            return
        self.cloud_init_ready.emit(vm_id, info)

    async def _do_cloud_init_set(
        self,
        vm_id: int,
        user_data: str | None,
        network_config: str | None,
        inject_ssh_keys: bool,
    ) -> None:
        client = self._client
        if client is None:
            self.operation_failed.emit("cloud_init_set", "not connected")
            return
        try:
            await client.cloud_init.set(
                vm_id,
                user_data=user_data,
                network_config=network_config,
                inject_ssh_keys=inject_ssh_keys,
            )
        except CorvusError as e:
            self.operation_failed.emit("cloud_init_set", _friendly_error(e))
            return
        self.cloud_init_action_completed.emit(vm_id, "set")

    async def _do_cloud_init_delete(self, vm_id: int) -> None:
        client = self._client
        if client is None:
            self.operation_failed.emit("cloud_init_delete", "not connected")
            return
        try:
            await client.cloud_init.delete(vm_id)
        except CorvusError as e:
            self.operation_failed.emit("cloud_init_delete", _friendly_error(e))
            return
        self.cloud_init_action_completed.emit(vm_id, "delete")

    # ------------------------------------------------------------ apply

    async def _do_apply(self, yaml_text: str, skip_existing: bool) -> None:
        client = self._client
        if client is None:
            self.operation_failed.emit("apply", "not connected")
            return
        try:
            result, task_id = await client.apply(yaml_text, skip_existing=skip_existing)
        except CorvusError as e:
            self.operation_failed.emit("apply", _friendly_error(e))
            return
        self.apply_completed.emit(result, task_id)

    # ------------------------------------------------------------ VM editing

    async def _do_vm_create(self, kwargs: dict[str, Any]) -> None:
        client = self._client
        if client is None:
            self.operation_failed.emit("vm_create", "not connected")
            return
        try:
            await client.vms.create(**kwargs)
        except CorvusError as e:
            self.operation_failed.emit("vm_create", _friendly_error(e))
            return
        self.vm_edit_completed.emit(0, "create")

    async def _do_vm_edit(self, vm_id: int, kwargs: dict[str, Any]) -> None:
        client = self._client
        if client is None:
            self.operation_failed.emit("vm_edit", "not connected")
            return
        try:
            vm = await client.vms.get(vm_id)
            await vm.edit(**kwargs)
        except CorvusError as e:
            self.operation_failed.emit("vm_edit", _friendly_error(e))
            return
        self.vm_edit_completed.emit(vm_id, "edit")

    async def _do_vm_delete(self, vm_id: int, keep_disks: bool) -> None:
        client = self._client
        if client is None:
            self.operation_failed.emit("vm_delete", "not connected")
            return
        try:
            vm = await client.vms.get(vm_id)
            await vm.delete(keep_disks=keep_disks)
        except CorvusError as e:
            self.operation_failed.emit("vm_delete", _friendly_error(e))
            return
        self.vm_edit_completed.emit(vm_id, "delete")

    async def _do_vm_attach_disk(
        self,
        vm_id: int,
        disk_ref: int | str,
        interface: str | None,
        media: str | None,
        read_only: bool,
        cache_type: str | None,
        discard: bool,
    ) -> None:
        client = self._client
        if client is None:
            self.operation_failed.emit("vm_attach_disk", "not connected")
            return
        try:
            vm = await client.vms.get(vm_id)
            await vm.attach_disk(
                disk_ref,
                interface=interface,
                media=media,
                read_only=read_only,
                cache_type=cache_type,
                discard=discard,
            )
        except CorvusError as e:
            self.operation_failed.emit("vm_attach_disk", _friendly_error(e))
            return
        self.vm_edit_completed.emit(vm_id, "attach_disk")

    async def _do_vm_detach_disk(self, vm_id: int, drive_id: int) -> None:
        client = self._client
        if client is None:
            self.operation_failed.emit("vm_detach_disk", "not connected")
            return
        try:
            vm = await client.vms.get(vm_id)
            await vm.detach_disk(drive_id)
        except CorvusError as e:
            self.operation_failed.emit("vm_detach_disk", _friendly_error(e))
            return
        self.vm_edit_completed.emit(vm_id, "detach_disk")

    async def _do_vm_add_net_if(
        self,
        vm_id: int,
        type: str | None,
        host_device: str | None,
        mac_address: str | None,
        network_ref: int | str | None,
    ) -> None:
        client = self._client
        if client is None:
            self.operation_failed.emit("vm_add_net_if", "not connected")
            return
        try:
            vm = await client.vms.get(vm_id)
            await vm.add_net_if(
                type=type,
                host_device=host_device,
                mac_address=mac_address,
                network_ref=network_ref,
            )
        except CorvusError as e:
            self.operation_failed.emit("vm_add_net_if", _friendly_error(e))
            return
        self.vm_edit_completed.emit(vm_id, "add_net_if")

    async def _do_vm_remove_net_if(self, vm_id: int, net_if_id: int) -> None:
        client = self._client
        if client is None:
            self.operation_failed.emit("vm_remove_net_if", "not connected")
            return
        try:
            vm = await client.vms.get(vm_id)
            await vm.remove_net_if(net_if_id)
        except CorvusError as e:
            self.operation_failed.emit("vm_remove_net_if", _friendly_error(e))
            return
        self.vm_edit_completed.emit(vm_id, "remove_net_if")

    async def _do_vm_attach_ssh_key(self, vm_id: int, key_ref: int | str) -> None:
        client = self._client
        if client is None:
            self.operation_failed.emit("vm_attach_ssh_key", "not connected")
            return
        try:
            vm = await client.vms.get(vm_id)
            await vm.attach_ssh_key(key_ref)
        except CorvusError as e:
            self.operation_failed.emit("vm_attach_ssh_key", _friendly_error(e))
            return
        self.vm_edit_completed.emit(vm_id, "attach_ssh_key")

    async def _do_vm_detach_ssh_key(self, vm_id: int, key_ref: int | str) -> None:
        client = self._client
        if client is None:
            self.operation_failed.emit("vm_detach_ssh_key", "not connected")
            return
        try:
            vm = await client.vms.get(vm_id)
            await vm.detach_ssh_key(key_ref)
        except CorvusError as e:
            self.operation_failed.emit("vm_detach_ssh_key", _friendly_error(e))
            return
        self.vm_edit_completed.emit(vm_id, "detach_ssh_key")

    async def _do_vm_add_shared_dir(
        self,
        vm_id: int,
        path: str,
        tag: str,
        cache: str | None,
        read_only: bool,
    ) -> None:
        client = self._client
        if client is None:
            self.operation_failed.emit("vm_add_shared_dir", "not connected")
            return
        try:
            vm = await client.vms.get(vm_id)
            await vm.add_shared_dir(path, tag, cache=cache, read_only=read_only)
        except CorvusError as e:
            self.operation_failed.emit("vm_add_shared_dir", _friendly_error(e))
            return
        self.vm_edit_completed.emit(vm_id, "add_shared_dir")

    async def _do_vm_remove_shared_dir(self, vm_id: int, shared_dir_id: int) -> None:
        client = self._client
        if client is None:
            self.operation_failed.emit("vm_remove_shared_dir", "not connected")
            return
        try:
            vm = await client.vms.get(vm_id)
            await vm.remove_shared_dir(shared_dir_id)
        except CorvusError as e:
            self.operation_failed.emit("vm_remove_shared_dir", _friendly_error(e))
            return
        self.vm_edit_completed.emit(vm_id, "remove_shared_dir")

    async def _do_vm_shared_dirs(self, vm_id: int) -> None:
        client = self._client
        if client is None:
            self.operation_failed.emit("vm_shared_dirs", "not connected")
            return
        try:
            vm = await client.vms.get(vm_id)
            dirs = await vm.list_shared_dirs()
        except CorvusError as e:
            self.operation_failed.emit("vm_shared_dirs", _friendly_error(e))
            return
        self.vm_shared_dirs_ready.emit(vm_id, dirs)

    async def _do_vm_guest_exec(self, vm_id: int, command: str) -> None:
        client = self._client
        if client is None:
            self.operation_failed.emit("vm_guest_exec", "not connected")
            return
        try:
            vm = await client.vms.get(vm_id)
            result = await vm.guest_exec(command)
        except CorvusError as e:
            self.operation_failed.emit("vm_guest_exec", _friendly_error(e))
            return
        self.guest_exec_result.emit(vm_id, result)

    # ------------------------------------------------------------ disk create

    async def _do_disk_create(
        self,
        name: str,
        size_mb: int,
        format: str | None,
        ephemeral: bool,
        node: int | str | None,
    ) -> None:
        client = self._client
        if client is None:
            self.operation_failed.emit("disk_create", "not connected")
            return
        try:
            await client.disks.create(
                name, size_mb, format=format, ephemeral=ephemeral, node=node
            )
        except CorvusError as e:
            self.operation_failed.emit("disk_create", _friendly_error(e))
            return
        self.disk_action_completed.emit(0, "create")

    async def _do_disk_register(
        self,
        name: str,
        file_path: str,
        format: str | None,
        ephemeral: bool,
        node: int | str | None,
    ) -> None:
        client = self._client
        if client is None:
            self.operation_failed.emit("disk_register", "not connected")
            return
        try:
            await client.disks.register(
                name, file_path, format=format, ephemeral=ephemeral, node=node
            )
        except CorvusError as e:
            self.operation_failed.emit("disk_register", _friendly_error(e))
            return
        self.disk_action_completed.emit(0, "register")

    async def _do_disk_overlay(
        self, name: str, backing_disk_ref: int | str, ephemeral: bool
    ) -> None:
        client = self._client
        if client is None:
            self.operation_failed.emit("disk_overlay", "not connected")
            return
        try:
            await client.disks.create_overlay(
                name, backing_disk_ref, ephemeral=ephemeral
            )
        except CorvusError as e:
            self.operation_failed.emit("disk_overlay", _friendly_error(e))
            return
        self.disk_action_completed.emit(0, "overlay")

    async def _do_disk_clone(
        self,
        source_ref: int | str,
        new_name: str,
        path: str | None,
        ephemeral: bool,
    ) -> None:
        client = self._client
        if client is None:
            self.operation_failed.emit("disk_clone", "not connected")
            return
        try:
            await client.disks.clone(
                source_ref, new_name, path=path, ephemeral=ephemeral
            )
        except CorvusError as e:
            self.operation_failed.emit("disk_clone", _friendly_error(e))
            return
        self.disk_action_completed.emit(0, "clone")

    async def _do_disk_import_url(
        self,
        name: str,
        url: str,
        format: str | None,
        size_mb: int | None,
        ephemeral: bool,
        node: int | str | None,
    ) -> None:
        client = self._client
        if client is None:
            self.operation_failed.emit("disk_import_url", "not connected")
            return
        try:
            task_id = await client.disks.import_url(
                name,
                url,
                format=format,
                size_mb=size_mb,
                ephemeral=ephemeral,
                node=node,
            )
        except CorvusError as e:
            self.operation_failed.emit("disk_import_url", _friendly_error(e))
            return
        # Re-use the existing task-detail subscription pipeline.
        self.disk_import_started.emit(task_id)
        self.disk_action_completed.emit(0, "import_url")

    async def _do_disk_rebase(
        self, disk_id: int, new_backing_disk_ref: int | str
    ) -> None:
        client = self._client
        if client is None:
            self.operation_failed.emit("disk_rebase", "not connected")
            return
        try:
            await client.disks.rebase(disk_id, new_backing_disk_ref)
        except CorvusError as e:
            self.operation_failed.emit("disk_rebase", _friendly_error(e))
            return
        self.disk_action_completed.emit(disk_id, "rebase")

    async def _do_disk_flatten(self, disk_id: int) -> None:
        client = self._client
        if client is None:
            self.operation_failed.emit("disk_flatten", "not connected")
            return
        try:
            await client.disks.flatten(disk_id)
        except CorvusError as e:
            self.operation_failed.emit("disk_flatten", _friendly_error(e))
            return
        self.disk_action_completed.emit(disk_id, "flatten")

    async def _do_disk_copy(
        self,
        disk_id: int,
        to_node_ref: int | str,
        to_path: str | None,
        with_backing_chain: bool,
    ) -> None:
        client = self._client
        if client is None:
            self.operation_failed.emit("disk_copy", "not connected")
            return
        try:
            task_id = await client.disks.copy(
                disk_id,
                to_node_ref,
                to_path=to_path,
                with_backing_chain=with_backing_chain,
            )
        except CorvusError as e:
            self.operation_failed.emit("disk_copy", _friendly_error(e))
            return
        self.disk_import_started.emit(task_id)
        self.disk_action_completed.emit(disk_id, "copy")

    async def _do_disk_move(
        self,
        disk_id: int,
        to_node_ref: int | str,
        to_path: str | None,
        with_backing_chain: bool,
    ) -> None:
        client = self._client
        if client is None:
            self.operation_failed.emit("disk_move", "not connected")
            return
        try:
            task_id = await client.disks.move(
                disk_id,
                to_node_ref,
                to_path=to_path,
                with_backing_chain=with_backing_chain,
            )
        except CorvusError as e:
            self.operation_failed.emit("disk_move", _friendly_error(e))
            return
        self.disk_import_started.emit(task_id)
        self.disk_action_completed.emit(disk_id, "move")

    # ------------------------------------------------------------ nodes

    async def _do_node_list(self) -> None:
        client = self._client
        if client is None:
            self.operation_failed.emit("node_list", "not connected")
            return
        try:
            nodes = await client.nodes.list()
        except CorvusError as e:
            self.operation_failed.emit("node_list", _friendly_error(e))
            return
        self.node_list_ready.emit(nodes)

    async def _do_node_detail(self, node_id: int) -> None:
        client = self._client
        if client is None:
            self.operation_failed.emit("node_detail", "not connected")
            return
        try:
            node = await client.nodes.get(node_id)
            details = await node.show()
        except CorvusError as e:
            self.operation_failed.emit("node_detail", _friendly_error(e))
            return
        self.node_detail_ready.emit(details)

    async def _do_node_create(
        self,
        name: str,
        host: str,
        node_agent_port: int,
        net_agent_port: int,
        base_path: str,
        description: str | None,
        admin_state: str,
        netd_disabled: bool,
    ) -> None:
        client = self._client
        if client is None:
            self.operation_failed.emit("node_create", "not connected")
            return
        try:
            await client.nodes.create(
                name,
                host,
                node_agent_port=node_agent_port,
                net_agent_port=net_agent_port,
                base_path=base_path,
                description=description,
                admin_state=admin_state,
                netd_disabled=netd_disabled,
            )
        except CorvusError as e:
            self.operation_failed.emit("node_create", _friendly_error(e))
            return
        self.node_action_completed.emit(0, "create")

    async def _do_node_edit(self, node_id: int, kwargs: dict[str, Any]) -> None:
        client = self._client
        if client is None:
            self.operation_failed.emit("node_edit", "not connected")
            return
        try:
            node = await client.nodes.get(node_id)
            await node.edit(**kwargs)
        except CorvusError as e:
            self.operation_failed.emit("node_edit", _friendly_error(e))
            return
        self.node_action_completed.emit(node_id, "edit")

    async def _do_node_drain(self, node_id: int) -> None:
        client = self._client
        if client is None:
            self.operation_failed.emit("node_drain", "not connected")
            return
        try:
            node = await client.nodes.get(node_id)
            await node.drain()
        except CorvusError as e:
            self.operation_failed.emit("node_drain", _friendly_error(e))
            return
        self.node_action_completed.emit(node_id, "drain")

    async def _do_node_delete(self, node_id: int) -> None:
        client = self._client
        if client is None:
            self.operation_failed.emit("node_delete", "not connected")
            return
        try:
            node = await client.nodes.get(node_id)
            await node.delete()
        except CorvusError as e:
            self.operation_failed.emit("node_delete", _friendly_error(e))
            return
        self.node_action_completed.emit(node_id, "delete")

    async def _do_vm_migrate(self, vm_id: int, to_node_ref: int | str) -> None:
        client = self._client
        if client is None:
            self.operation_failed.emit("vm_migrate", "not connected")
            return
        try:
            vm = await client.vms.get(vm_id)
            task_id = await vm.migrate(to_node_ref)
        except CorvusError as e:
            self.operation_failed.emit("vm_migrate", _friendly_error(e))
            return
        self.vm_migrate_started.emit(vm_id, task_id)

    async def _do_network_attach_node(
        self, network_id: int, node_ref: int | str
    ) -> None:
        client = self._client
        if client is None:
            self.operation_failed.emit("network_attach_node", "not connected")
            return
        try:
            net = await client.networks.get(network_id)
            await net.attach_node(node_ref)
        except CorvusError as e:
            self.operation_failed.emit("network_attach_node", _friendly_error(e))
            return
        self.network_action_completed.emit(network_id, "edit")

    async def _do_network_detach_node(
        self, network_id: int, node_ref: int | str
    ) -> None:
        client = self._client
        if client is None:
            self.operation_failed.emit("network_detach_node", "not connected")
            return
        try:
            net = await client.networks.get(network_id)
            await net.detach_node(node_ref)
        except CorvusError as e:
            self.operation_failed.emit("network_detach_node", _friendly_error(e))
            return
        self.network_action_completed.emit(network_id, "edit")

    # ------------------------------------------------------------ build

    async def _do_build_run(self, yaml_text: str) -> None:
        client = self._client
        if client is None:
            self.operation_failed.emit("build", "not connected")
            self.build_finished.emit("not connected")
            return
        try:
            async for item in client.build_stream_text(yaml_text):
                if isinstance(item, tuple) and len(item) == 2 and item[0] == "task_id":
                    self.build_started.emit(int(item[1]))
                else:
                    self.build_event.emit(item)
        except CorvusError as e:
            self.operation_failed.emit("build", _friendly_error(e))
            self.build_finished.emit(_friendly_error(e))
            return
        except Exception as e:
            logger.exception("bridge: build stream crashed")
            self.operation_failed.emit("build", f"{type(e).__name__}: {e}")
            self.build_finished.emit(str(e))
            return
        self.build_finished.emit("")

    # ------------------------------------------------------------ Phase 12

    async def _do_subscribe_vm_stats(self, vm_id: int) -> None:
        client = self._client
        if client is None:
            self.operation_failed.emit("vm_stats", "not connected")
            return
        if vm_id in self._stats_subs:
            return

        async def _emit(sample: Any) -> None:
            self.vm_stats_event.emit(vm_id, sample)

        try:
            vm = await client.vms.get(vm_id)
            sub = await vm.subscribe_stats(_emit)
        except CorvusError as e:
            self.operation_failed.emit("vm_stats", _friendly_error(e))
            return
        self._stats_subs[vm_id] = sub

    async def _do_unsubscribe_vm_stats(self, vm_id: int) -> None:
        sub = self._stats_subs.pop(vm_id, None)
        if sub is not None:
            try:
                await sub.close()
            except CorvusError:
                pass

    async def _do_vm_stats_history(self, vm_id: int) -> None:
        client = self._client
        if client is None:
            self.operation_failed.emit("vm_stats_history", "not connected")
            return
        try:
            vm = await client.vms.get(vm_id)
            history = await vm.get_stats_history()
        except CorvusError as e:
            self.operation_failed.emit("vm_stats_history", _friendly_error(e))
            return
        self.vm_stats_history_ready.emit(vm_id, history)

    async def _do_vm_ssh_keys(self, vm_id: int) -> None:
        client = self._client
        if client is None:
            self.operation_failed.emit("vm_ssh_keys", "not connected")
            return
        try:
            vm = await client.vms.get(vm_id)
            keys = await vm.list_ssh_keys()
        except CorvusError as e:
            self.operation_failed.emit("vm_ssh_keys", _friendly_error(e))
            return
        self.vm_ssh_keys_ready.emit(vm_id, keys)

    async def _do_subscribe_guest_agent(self, vm_id: int) -> None:
        client = self._client
        if client is None:
            self.operation_failed.emit("guest_agent_status", "not connected")
            return
        if vm_id in self._guest_agent_subs:
            return

        async def _emit(status: Any) -> None:
            self.guest_agent_event.emit(vm_id, status)

        try:
            vm = await client.vms.get(vm_id)
            sub = await vm.subscribe_guest_agent(_emit)
        except CorvusError as e:
            self.operation_failed.emit("guest_agent_status", _friendly_error(e))
            return
        self._guest_agent_subs[vm_id] = sub

    async def _do_unsubscribe_guest_agent(self, vm_id: int) -> None:
        sub = self._guest_agent_subs.pop(vm_id, None)
        if sub is not None:
            try:
                await sub.close()
            except CorvusError:
                pass

    async def _do_view_grant(self, vm_id: int) -> None:
        client = self._client
        if client is None:
            self.operation_failed.emit("view_grant", "not connected")
            return
        try:
            vm = await client.vms.get(vm_id)
            grant = await vm.view_grant()
        except CorvusError as e:
            self.operation_failed.emit("view_grant", _friendly_error(e))
            return
        self.view_grant_ready.emit(vm_id, grant)

    async def _do_daemon_shutdown(self) -> None:
        client = self._client
        if client is None:
            self.operation_failed.emit("daemon_shutdown", "not connected")
            return
        try:
            await client.shutdown()
        except CorvusError as e:
            self.operation_failed.emit("daemon_shutdown", _friendly_error(e))
            return
        self.daemon_shutdown_completed.emit()

    async def _do_open_hmp(self, vm_id: int) -> None:
        client = self._client
        if client is None:
            self.operation_failed.emit("hmp", "not connected")
            return
        if vm_id in self._hmp_streams:
            return
        try:
            vm = await client.vms.get(vm_id)
            stream = await vm.hmp_monitor()
        except CorvusError as e:
            self.operation_failed.emit("hmp", _friendly_error(e))
            return
        task = asyncio.create_task(self._hmp_pump(vm_id, stream))
        self._hmp_streams[vm_id] = (stream, task)

    async def _do_send_hmp(self, vm_id: int, chunk: bytes) -> None:
        entry = self._hmp_streams.get(vm_id)
        if entry is None:
            self.operation_failed.emit("hmp", "no active monitor")
            return
        stream, _ = entry
        try:
            await stream.write(chunk)
        except CorvusError as e:
            self.operation_failed.emit("hmp", _friendly_error(e))

    async def _do_close_hmp(self, vm_id: int) -> None:
        entry = self._hmp_streams.pop(vm_id, None)
        if entry is None:
            return
        stream, task = entry
        task.cancel()
        try:
            await stream.close()
        except CorvusError:
            pass
        self.hmp_closed.emit(vm_id, "closed")

    async def _hmp_pump(self, vm_id: int, stream: Any) -> None:
        try:
            while True:
                chunk = await stream.read()
                if chunk is None:
                    self._hmp_streams.pop(vm_id, None)
                    self.hmp_closed.emit(vm_id, "eof")
                    return
                self.hmp_data.emit(vm_id, chunk)
        except asyncio.CancelledError:
            raise
        except Exception as e:
            logger.exception("bridge: hmp pump for vm %d crashed", vm_id)
            self._hmp_streams.pop(vm_id, None)
            self.hmp_closed.emit(vm_id, f"error: {e}")

    async def _serial_pump(self, vm_id: int, stream: Any) -> None:
        """Read from the daemon and emit chunks to the GUI.

        Coalesces small reads up to 4 KiB or 16 ms before emitting so
        a noisy guest (``yes``-style flood) doesn't enqueue thousands
        of Qt events per second.
        """
        COALESCE_BYTES = 4096
        COALESCE_MS = 16
        buf = bytearray()
        deadline: float | None = None
        loop = asyncio.get_running_loop()
        try:
            while True:
                # Wait for a chunk with a deadline if we have buffered
                # data — otherwise block indefinitely on the next read.
                if buf and deadline is not None:
                    timeout = max(0.0, deadline - loop.time())
                    try:
                        chunk = await asyncio.wait_for(stream.read(), timeout=timeout)
                    except asyncio.TimeoutError:
                        chunk = None
                        # Drain buffer below; reset deadline.
                        self.serial_data.emit(vm_id, bytes(buf))
                        buf.clear()
                        deadline = None
                        continue
                else:
                    chunk = await stream.read()
                if chunk is None:
                    # Daemon closed the stream. Flush whatever's left.
                    if buf:
                        self.serial_data.emit(vm_id, bytes(buf))
                        buf.clear()
                    self._serial_streams.pop(vm_id, None)
                    self.serial_closed.emit(vm_id, "eof")
                    return
                buf.extend(chunk)
                if deadline is None:
                    deadline = loop.time() + COALESCE_MS / 1000.0
                if len(buf) >= COALESCE_BYTES:
                    self.serial_data.emit(vm_id, bytes(buf))
                    buf.clear()
                    deadline = None
        except asyncio.CancelledError:
            if buf:
                self.serial_data.emit(vm_id, bytes(buf))
            raise
        except Exception as e:
            logger.exception("bridge: serial pump for vm %d crashed", vm_id)
            self._serial_streams.pop(vm_id, None)
            self.serial_closed.emit(vm_id, f"error: {e}")
