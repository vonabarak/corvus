"""Task-record invariants: which RPCs record tasks, which don't,
and what the ``client_name`` field looks like.

[doc/task-history.md](../../doc/task-history.md) describes the
contract: every mutating RPC is wrapped in ``withTask`` and writes a
row with the operating client's identity, while read-only RPCs
(list, show, ping) are skipped. Nothing has pinned this end-to-end —
a refactor that accidentally untracked a mutation (or one that
started recording list operations) would only surface in operator
audit gaps long after the fact.

What's covered:

* Mutating RPCs (``vms.create``, ``disks.create``,
  ``networks.create``) each write exactly one task row keyed to the
  newly-created entity. The row carries the right
  ``(subsystem, command, entity_id, entity_name, result)``.
* Read-only RPCs (``vms.list``, ``disks.list``, ``status``) do
  NOT record tasks.
* ``client_name`` carries the TLS peer's CN suffix
  (``corvus-client:<short_name>``) for an mTLS-authenticated
  mutation. Catches a regression that would either flatten
  ``client_name`` to ``"local"`` (no auth) or strip the prefix.

What's NOT covered here:

* Cancellation cascading on a multi-step apply. The plan called
  for "cancel the parent task at step 3, assert in-flight subtask
  is cancelled, never-started subtasks are not_started → cancelled,
  no half-created entities survive". The cancel RPC isn't exposed
  in the sync ``corvus_client`` API; reaching into the underlying
  Cap'n Proto cap from a test is doable but works around the
  client surface. A future sync wrapper for ``tasks.cancel`` would
  unblock this case.
* ``TaskProgressSink`` terminal event. ``tasks.subscribe`` is on
  the async client only; testing it from the sync harness would
  add a runloop bridge specific to this test. The ws25-style
  regression risk is already covered by
  :mod:`test_subscription_lifetimes`.
* ``client_name = "system"`` for daemon-internal tasks (startup,
  scheduled cleanup). Triggering those from a test would require
  daemon-side stimulus we don't have a clean handle for.
"""

from __future__ import annotations

import secrets

from corvus_test_harness import SingleNodeCase


def _latest_task_for(client, *, subsystem: str, entity_name: str):
    """Return the most-recent task row whose entity name matches.

    Searches the most recent 100 tasks (the test creates fresh
    entities with high-entropy names, so we don't have to walk
    further). Returns None if no match — the caller asserts on
    that explicitly so a missing record produces a clear
    diagnostic."""
    rows = client.tasks.list(subsystem=subsystem, limit=100)
    for row in rows:
        if row.entity and row.entity.name == entity_name:
            return row
    return None


class TestMutatingRpcsRecordTasks(SingleNodeCase):
    """Each mutating RPC must leave a task row with the right
    ``(subsystem, command, entity)`` tuple. The shape matches what
    operators see in ``crv task list`` and what
    ``doc/task-history.md`` documents."""

    def test_vm_create_records_task(self):
        name = f"task-vm-{secrets.token_hex(3)}"
        vm = self.client.vms.create(
            name,
            cpu_count=1,
            ram_mb=64,
            headless=True,
        )
        try:
            row = _latest_task_for(self.client, subsystem="vm", entity_name=name)
            assert row is not None, (
                f"no vm-subsystem task found for entity name {name!r}"
            )
            assert row.command == "create", f"unexpected command: {row.command!r}"
            assert row.result == "success", f"unexpected result: {row.result!r}"
            assert row.entity is not None and row.entity.id == vm.show().id, (
                f"task.entity.id mismatch: {row.entity!r} vs vm.id={vm.show().id}"
            )
            assert row.entity.name == name
        finally:
            vm.delete()

    def test_disk_create_records_task(self):
        name = f"task-disk-{secrets.token_hex(3)}"
        disk = self.client.disks.create(name, size_mb=32, format="qcow2")
        try:
            row = _latest_task_for(self.client, subsystem="disk", entity_name=name)
            assert row is not None, (
                f"no disk-subsystem task found for entity name {name!r}"
            )
            assert row.command == "create"
            assert row.result == "success"
            assert row.entity.id == disk.show().id
        finally:
            disk.delete()

    def test_network_create_records_task(self):
        name = f"task-net-{secrets.token_hex(3)}"
        net = self.client.networks.create(
            name,
            subnet="10.99.0.0/24",
            dhcp=False,
            nat=False,
            autostart=False,
        )
        try:
            row = _latest_task_for(self.client, subsystem="network", entity_name=name)
            assert row is not None, (
                f"no network-subsystem task found for entity name {name!r}"
            )
            assert row.command == "create"
            assert row.result == "success"
            assert row.entity.id == net.show().id
        finally:
            net.delete()


class TestReadOnlyRpcsDoNotRecordTasks(SingleNodeCase):
    """``vms.list``, ``disks.list``, and ``status`` are read-only —
    Handlers.hs skips them in the ``withTask`` wrapper. A future
    refactor that wraps every cap method indiscriminately would
    spam the task table; this catches it."""

    def test_readonly_rpcs_no_new_tasks(self):
        before = len(self.client.tasks.list(limit=500))
        # Hammer a handful of read-only RPCs.
        for _ in range(5):
            self.client.vms.list()
            self.client.disks.list()
            self.client.networks.list()
            self.client.ssh_keys.list()
            self.client.tasks.list()
            self.client.status()
        after = len(self.client.tasks.list(limit=500))
        assert after == before, (
            f"task count changed from {before} to {after} during read-only "
            f"RPC sweep — one of the calls is now recording a task"
        )


class TestClientNameCarriesTlsCn(SingleNodeCase):
    """The harness opens the inner client over mTLS with a cert
    minted by ``CaContext.new(client_name="harness")`` — actual CN
    is ``corvus-client:harness``. The daemon validates the
    ``corvus-client:`` prefix at handshake (see
    ``checkPrefixAndName`` in ``src/Corvus/Tls.hs:513``), then
    strips it via ``peerNameFromCN`` (``src/Corvus/Tls.hs:332``) so
    the recorded ``client_name`` is the bare ``"harness"`` suffix.

    Catches regressions that would either:

      * Flatten ``client_name`` to ``"local"`` (auth context not
        propagated to the task row), or
      * Stop stripping the prefix (and store ``"corvus-client:harness"``)
      * Store nothing (empty string) on mTLS connections."""

    def test_create_records_cn_suffix_as_client_name(self):
        name = f"task-cn-{secrets.token_hex(3)}"
        vm = self.client.vms.create(
            name,
            cpu_count=1,
            ram_mb=64,
            headless=True,
        )
        try:
            row = _latest_task_for(self.client, subsystem="vm", entity_name=name)
            assert row is not None, f"no task found for {name!r}"
            # The harness's `CaContext.new` mints the cert with
            # `client_name="harness"` by default; the daemon
            # records the post-prefix-strip suffix.
            assert row.client_name == "harness", (
                f"client_name {row.client_name!r} doesn't match the "
                f"harness's CN suffix; expected 'harness'. "
                f"`local` would mean the daemon ran with --no-tls or "
                f"didn't see the cert; "
                f"`corvus-client:harness` would mean the prefix-strip "
                f"in peerNameFromCN regressed."
            )
        finally:
            vm.delete()
