"""Week-2 tests for the Haskell-backed Corvus Python extension.

Covers the universal `call(bytes) -> bytes` FFI entry and the JSON
envelope contract. Each test uses a mock daemon pre-loaded with exactly
the binary response bytes the Haskell client should see.
"""
import pytest

import corvus_client
from corvus_client import (
    Client,
    ConnectError,
    InvalidTransition,
    ServerError,
    VmNotFound,
)
from tests.conftest import (
    VM_RUNNING,
    resp_disk_created,
    resp_error,
    resp_invalid_transition,
    resp_network_not_found,
    resp_pong,
    resp_ssh_key_in_use,
    resp_status,
    resp_unknown_tag,
    resp_vm_created,
    resp_vm_not_found,
)


def test_ping_ok(mock_factory):
    d = mock_factory(resp_pong())
    Client(unix_socket=d.path).ping()


def test_ping_server_error(mock_factory):
    d = mock_factory(resp_error("simulated server error"))
    with pytest.raises(ServerError) as ei:
        Client(unix_socket=d.path).ping()
    assert "simulated server error" in str(ei.value)


def test_ping_vm_not_found_response(mock_factory):
    # RespVmNotFound is translated from its Response tag uniformly —
    # the client raises VmNotFound regardless of which op produced it.
    # (The bridge's generic dispatch no longer distinguishes "unexpected
    # variant for this op" from "legitimate-but-different variant".)
    d = mock_factory(resp_vm_not_found())
    with pytest.raises(VmNotFound):
        Client(unix_socket=d.path).ping()


def test_ping_connect_error_missing_socket(tmp_path):
    missing = tmp_path / "nope.sock"
    with pytest.raises(ConnectError):
        Client(unix_socket=str(missing)).ping()


def test_status_full_fields(mock_factory):
    d = mock_factory(resp_status(
        uptime=42,
        connections=3,
        version="0.8.0.0-mock",
        protocol_version=29,
        namespace_pid=1234,
    ))
    # Auto-generated client returns a dataclass keyed by the "tag"
    # discriminator. Both the outer response and nested payloads
    # (StatusInfo etc.) are typed dataclasses, so field access is by
    # attribute through the whole tree.
    payload = Client(unix_socket=d.path).status()
    assert payload.tag == "status"
    info = payload.info
    assert info.uptime == 42
    assert info.connections == 3
    assert info.version == "0.8.0.0-mock"
    assert info.protocol_version == 29
    assert info.namespace_pid == 1234


def test_status_nothing_for_namespace_pid(mock_factory):
    d = mock_factory(resp_status(
        uptime=0, connections=0, version="v",
        protocol_version=29, namespace_pid=None,
    ))
    info = Client(unix_socket=d.path).status().info
    # namespace_pid Nothing → omitted from JSON (omitNothingFields) → None on
    # the Optional dataclass field.
    assert info.namespace_pid is None


def test_vm_show_not_found(mock_factory):
    d = mock_factory(resp_vm_not_found())
    with pytest.raises(VmNotFound):
        Client(unix_socket=d.path).vm_show(ref="ghost")


def test_vm_start_invalid_transition(mock_factory):
    d = mock_factory(resp_invalid_transition(VM_RUNNING, "already running"))
    with pytest.raises(InvalidTransition) as ei:
        Client(unix_socket=d.path).vm_start(ref="some-vm", wait=False)
    assert ei.value.status == "running"
    assert ei.value.reason == "already running"


def test_unknown_op_raises_bad_envelope(mock_factory):
    # The generic bridge decodes Request directly; an unknown op is a
    # FromJSON parse failure on the Haskell side, surfaced as BadEnvelope.
    from corvus_client import BadEnvelope
    d = mock_factory(resp_pong())
    client = Client(unix_socket=d.path)
    with pytest.raises(BadEnvelope):
        client._call("nope_not_a_real_op")


def test_protocol_error_on_bad_tag(mock_factory):
    from corvus_client import ProtocolError
    d = mock_factory(resp_unknown_tag())
    with pytest.raises(ProtocolError):
        Client(unix_socket=d.path).ping()


def test_native_module_exposes_call():
    assert callable(corvus_client._corvus.call)
    assert callable(corvus_client._corvus._shutdown)


def test_vm_create_returns_id(mock_factory):
    d = mock_factory(resp_vm_created(42))
    payload = Client(unix_socket=d.path).vm_create(
        name="test", cpu_count=2, ram_mb=1024,
        headless=True, guest_agent=False, cloud_init=False, autostart=False,
    )
    assert payload.tag == "vm_created"
    assert payload.id == 42


def test_disk_create_returns_id(mock_factory):
    d = mock_factory(resp_disk_created(7))
    payload = Client(unix_socket=d.path).disk_create(
        name="root", format="qcow2", size_mb=10_240,
    )
    assert payload.id == 7


def test_network_start_not_found(mock_factory):
    from corvus_client import NetworkNotFound
    d = mock_factory(resp_network_not_found())
    with pytest.raises(NetworkNotFound):
        Client(unix_socket=d.path).network_start(ref="ghost-net")


def test_ssh_key_delete_in_use(mock_factory):
    from corvus_client import SshKeyInUse
    d = mock_factory(resp_ssh_key_in_use([(1, "vm-a"), (2, "vm-b")]))
    with pytest.raises(SshKeyInUse) as ei:
        Client(unix_socket=d.path).ssh_key_delete(ref="my-key")
    # Details carry the list of (id, name) pairs that aeson renders as
    # JSON arrays (tuples round-trip as 2-element arrays).
    assert ei.value.details == [[1, "vm-a"], [2, "vm-b"]]


def test_list_disks_forwards_list(mock_factory):
    # RespDiskList with an empty list: tag + Int64(0) for list length.
    import struct
    from tests.conftest import RESP_DISK_LIST, PROTOCOL_VERSION
    payload = bytes([RESP_DISK_LIST]) + struct.pack(">q", 0)
    frame = bytes([PROTOCOL_VERSION]) + struct.pack(">Q", len(payload)) + payload
    d = mock_factory(frame)
    result = Client(unix_socket=d.path).disk_list()
    assert result.tag == "disk_list"
    assert result.disks == []  # list[DiskImageInfo], empty


# -------- Persistent-connection behaviour ----------------------------------


def test_multiple_pings_use_one_connection(mock_factory):
    """Client should reuse the socket across RPCs."""
    d = mock_factory(resp_pong())
    client = Client(unix_socket=d.path)
    for _ in range(5):
        client.ping()
    # The mock counts every request served. A non-persistent client would
    # also register 5 — but a persistent client reaches 5 over a single
    # accept(), which the mock thread handles inside _handle()'s loop.
    assert d.request_count == 5
    client.close()


def test_context_manager_closes(mock_factory):
    d = mock_factory(resp_pong())
    with Client(unix_socket=d.path) as client:
        client.ping()
        assert not client._closed
    assert client._closed
    # After close, further calls must fail fast.
    with pytest.raises(RuntimeError):
        client.ping()


def test_concurrent_pings_are_serialised(mock_factory):
    """Two threads sharing a Client don't interleave on the wire."""
    import threading
    d = mock_factory(resp_pong())
    client = Client(unix_socket=d.path)
    errors: list[BaseException] = []

    def worker():
        try:
            for _ in range(20):
                client.ping()
        except BaseException as e:  # noqa: BLE001
            errors.append(e)

    threads = [threading.Thread(target=worker) for _ in range(4)]
    for t in threads:
        t.start()
    for t in threads:
        t.join()
    client.close()
    assert errors == []
    assert d.request_count == 80


def test_mixed_responses_across_rpcs(mock_factory):
    """Each RPC receives the right response when the mock cycles a list."""
    d = mock_factory([
        resp_pong(),
        resp_vm_created(100),
        resp_disk_created(200),
    ])
    with Client(unix_socket=d.path) as client:
        client.ping()
        vm = client.vm_create(
            name="x", cpu_count=1, ram_mb=512,
            headless=False, guest_agent=False, cloud_init=False, autostart=False,
        )
        disk = client.disk_create(name="d", format="qcow2", size_mb=100)
    assert vm.id == 100
    assert disk.id == 200
