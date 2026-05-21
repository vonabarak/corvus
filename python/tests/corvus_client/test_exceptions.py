"""Translation of daemon KjException messages to typed Python exceptions."""

from __future__ import annotations

import capnp
import pytest

from corvus_client.exceptions import (
    CorvusError,
    DiskInUse,
    DiskNotFound,
    NetworkNotFound,
    NodeInUse,
    NodeNotFound,
    ServerError,
    SnapshotNotFound,
    SshKeyNotFound,
    TaskNotFound,
    TemplateNotFound,
    VmNotFound,
    translate_kj_exception,
)


def _fake_kj(description: str) -> capnp.KjException:
    """Build a KjException carrying the given .description for unit tests."""
    return capnp.KjException(description)


@pytest.mark.parametrize(
    "raw,expected",
    [
        # Plain static messages from cap dispatch.
        ("(remote):0: failed: remote exception: VM not found", VmNotFound),
        ("(remote):0: failed: remote exception: Disk not found", DiskNotFound),
        ("(remote):0: failed: remote exception: Disk in use", DiskInUse),
        ("(remote):0: failed: remote exception: Snapshot not found", SnapshotNotFound),
        ("(remote):0: failed: remote exception: Network not found", NetworkNotFound),
        ("(remote):0: failed: remote exception: SSH key not found", SshKeyNotFound),
        ("(remote):0: failed: remote exception: Template not found", TemplateNotFound),
        ("(remote):0: failed: remote exception: Task not found", TaskNotFound),
        # Decorated messages from Corvus.Handlers.Resolve.
        ("(remote):0: failed: remote exception: VM 'web-1' not found", VmNotFound),
        ("(remote):0: failed: remote exception: VM #42 not found", VmNotFound),
        ("(remote):0: failed: remote exception: Disk 'root' not found", DiskNotFound),
        (
            "(remote):0: failed: remote exception: Snapshot #5 not found",
            SnapshotNotFound,
        ),
        (
            "(remote):0: failed: remote exception: Network 'br0' not found",
            NetworkNotFound,
        ),
        # Multi-node Node translations.
        ("(remote):0: failed: remote exception: Node not found", NodeNotFound),
        ("(remote):0: failed: remote exception: Node 'alpha' not found", NodeNotFound),
        (
            "(remote):0: failed: remote exception: Node 'alpha' is still referenced: "
            "2 VM(s), 0 network(s), 0 disk placement(s). Delete them first.",
            NodeInUse,
        ),
        # Unknown message → ServerError (the catch-all branch).
        ("(remote):0: failed: remote exception: something weird", ServerError),
    ],
)
def test_translate_known_messages(raw, expected):
    exc = _fake_kj(raw)
    out = translate_kj_exception(exc)
    assert isinstance(out, expected)
    assert isinstance(out, CorvusError)


def test_translate_strips_remote_envelope():
    exc = _fake_kj("(remote):0: failed: remote exception: VM 'web-1' not found")
    out = translate_kj_exception(exc)
    # The exception's str() should be the bare daemon message, not the
    # whole pycapnp envelope.
    assert str(out) == "VM 'web-1' not found"
    assert isinstance(out, VmNotFound)
