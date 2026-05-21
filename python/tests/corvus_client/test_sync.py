"""Sync API tests — same coverage as the async tests but via Client.

Verifies the sync wrapper threads correctly and the background runloop
is reentrant: multiple calls in the same Client instance share a loop.
"""
from __future__ import annotations

import pytest

from corvus_client import Client, DiskNotFound


def test_sync_status_and_ping(daemon_socket):
    with Client(unix_socket=str(daemon_socket)) as c:
        info = c.status()
        assert info.version
        assert info.uptime_seconds >= 0
        c.ping()


def test_sync_disk_lifecycle(daemon_socket):
    with Client(unix_socket=str(daemon_socket)) as c:
        d = c.disks.create("sync-disk", size_mb=64)
        info = d.show()
        assert info.name == "sync-disk"
        assert info.size_mb == 64
        d.delete()
        with pytest.raises(DiskNotFound):
            c.disks.get("sync-disk")


def test_sync_vm_create_edit_delete(daemon_socket):
    with Client(unix_socket=str(daemon_socket)) as c:
        v = c.vms.create("sync-vm", cpu_count=1, ram_mb=256, headless=True)
        details = v.show()
        assert details.name == "sync-vm"
        v.edit(ram_mb=512)
        assert v.show().ram_mb == 512
        v.delete()


def test_sync_multiple_clients_isolated(daemon_socket):
    """Two Client instances must not share state, even on the same daemon."""
    with Client(unix_socket=str(daemon_socket)) as c1, Client(
        unix_socket=str(daemon_socket)
    ) as c2:
        c1.ping()
        c2.ping()
        # Manager caps fetched independently per client.
        d1 = c1.disks.create("sync-iso-1", size_mb=32)
        d2 = c2.disks.create("sync-iso-2", size_mb=32)
        names = [info.name for info in c1.disks.list()]
        assert "sync-iso-1" in names
        assert "sync-iso-2" in names
        d1.delete()
        d2.delete()


def test_sync_close_is_idempotent(daemon_socket):
    c = Client(unix_socket=str(daemon_socket))
    c.ping()
    c.close()
    c.close()  # second close is a no-op
