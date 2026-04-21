"""Test fixtures — a mock Corvus daemon speaking the binary protocol.

The mock is deliberately dumb: it accepts a connection, drains one request
frame, and replies with pre-configured bytes. Each test passes the exact
response payload it wants the daemon to emit via the `response` parameter
of the `mock_daemon` fixture.

Binary encoders in this file mirror the Data.Binary instances on the
Haskell side (see src/Corvus/Protocol.hs and src/Corvus/Model.hs). Tag
numbers match the 0-based declaration order of the sum type constructors.
"""
from __future__ import annotations

import os
import socket
import struct
import tempfile
import threading
from typing import Callable, Optional

import pytest

PROTOCOL_VERSION = 33

# Response constructor tags (0-based declaration order in Protocol.hs).
# Source of truth: src/Corvus/Protocol.hs — keep in sync when the order
# of constructors changes.
RESP_PONG = 0
RESP_STATUS = 1
RESP_ERROR = 3
RESP_VM_NOT_FOUND = 6
RESP_VM_CREATED = 7
RESP_INVALID_TRANSITION = 11
RESP_DISK_LIST = 12
RESP_DISK_CREATED = 14
RESP_SSH_KEY_IN_USE = 39
RESP_NETWORK_NOT_FOUND = 53

# VmStatus tags (declaration order in Model.hs).
VM_STOPPED = 0
VM_STARTING = 1
VM_RUNNING = 2
VM_STOPPING = 3
VM_PAUSED = 4
VM_ERROR = 5


# --- Binary primitives ------------------------------------------------------


def enc_bool(b: bool) -> bytes:
    return b"\x01" if b else b"\x00"


def enc_word8(n: int) -> bytes:
    return bytes([n])


def enc_int(n: int) -> bytes:
    # Data.Binary encodes Haskell `Int` as Int64 big-endian signed.
    return struct.pack(">q", n)


def enc_int64(n: int) -> bytes:
    return struct.pack(">q", n)


def enc_text(s: str) -> bytes:
    data = s.encode("utf-8")
    return struct.pack(">q", len(data)) + data


def enc_maybe(inner: Optional[bytes]) -> bytes:
    return b"\x00" if inner is None else b"\x01" + inner


def frame(payload: bytes) -> bytes:
    return struct.pack(">BQ", PROTOCOL_VERSION, len(payload)) + payload


# --- Response builders ------------------------------------------------------


def resp_pong() -> bytes:
    return frame(bytes([RESP_PONG]))


def resp_error(message: str) -> bytes:
    return frame(bytes([RESP_ERROR]) + enc_text(message))


def resp_vm_not_found() -> bytes:
    return frame(bytes([RESP_VM_NOT_FOUND]))


def resp_invalid_transition(status_tag: int, reason: str) -> bytes:
    return frame(bytes([RESP_INVALID_TRANSITION]) + enc_word8(status_tag) + enc_text(reason))


def resp_status(
    uptime: int,
    connections: int,
    version: str,
    protocol_version: int,
    namespace_pid: Optional[int],
) -> bytes:
    # RespStatus fields in declaration order: uptime, connections, version,
    # protocolVersion (Word8), namespacePid (Maybe Int).
    body = bytes([RESP_STATUS])
    body += enc_int(uptime)
    body += enc_int(connections)
    body += enc_text(version)
    body += enc_word8(protocol_version)
    body += enc_maybe(enc_int(namespace_pid) if namespace_pid is not None else None)
    return frame(body)


def resp_vm_created(vm_id: int) -> bytes:
    return frame(bytes([RESP_VM_CREATED]) + enc_int64(vm_id))


def resp_disk_created(disk_id: int) -> bytes:
    return frame(bytes([RESP_DISK_CREATED]) + enc_int64(disk_id))


def resp_network_not_found() -> bytes:
    return frame(bytes([RESP_NETWORK_NOT_FOUND]))


def resp_ssh_key_in_use(vms: list[tuple[int, str]]) -> bytes:
    body = bytes([RESP_SSH_KEY_IN_USE]) + enc_int64(len(vms))
    for vm_id, name in vms:
        body += enc_int64(vm_id) + enc_text(name)
    return frame(body)


def resp_unknown_tag() -> bytes:
    """Emit a response with a tag no Haskell decoder recognises."""
    return frame(bytes([250]))


def resp_raw(payload: bytes) -> bytes:
    """Frame an arbitrary payload (escape hatch for ad-hoc tests)."""
    return frame(payload)


# --- Mock daemon -----------------------------------------------------------


class MockDaemon:
    """Mock Corvus daemon.

    The `response` argument can be either:
    - a single `bytes` value: replies with the same bytes to every request
      on a single connection (loops until the client disconnects);
    - a list of `bytes`: replies with the N-th element for the N-th
      request; cycles if the client sends more than len(list) requests.

    Designed around the persistent-connection client: one accept(),
    many requests served on that socket.
    """

    def __init__(self, response) -> None:
        if isinstance(response, (bytes, bytearray)):
            self.responses = [bytes(response)]
        else:
            self.responses = [bytes(r) for r in response]
        self.request_count = 0
        self._sockdir = tempfile.mkdtemp(prefix="corvus-mock-")
        self.path = os.path.join(self._sockdir, "s")
        self._sock = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
        self._sock.bind(self.path)
        self._sock.listen(4)
        self._stop = False
        self._thread = threading.Thread(target=self._run, daemon=True)
        self._thread.start()

    def _run(self) -> None:
        self._sock.settimeout(0.5)
        while not self._stop:
            try:
                conn, _ = self._sock.accept()
            except socket.timeout:
                continue
            except OSError:
                return
            try:
                self._handle(conn)
            finally:
                try:
                    conn.close()
                except OSError:
                    pass

    def _handle(self, conn: socket.socket) -> None:
        while True:
            hdr = _recv_exact(conn, 9)
            if hdr is None:
                return
            _, length = struct.unpack(">BQ", hdr)
            _ = _recv_exact(conn, length)  # drain request body
            resp = self.responses[self.request_count % len(self.responses)]
            self.request_count += 1
            try:
                conn.sendall(resp)
            except OSError:
                return

    def close(self) -> None:
        self._stop = True
        try:
            self._sock.close()
        except OSError:
            pass
        self._thread.join(timeout=2)
        try:
            os.unlink(self.path)
        except OSError:
            pass
        try:
            os.rmdir(self._sockdir)
        except OSError:
            pass


def _recv_exact(conn: socket.socket, n: int) -> Optional[bytes]:
    buf = bytearray()
    while len(buf) < n:
        chunk = conn.recv(n - len(buf))
        if not chunk:
            return None
        buf.extend(chunk)
    return bytes(buf)


@pytest.fixture
def mock_factory() -> Callable[[bytes], MockDaemon]:
    """Factory fixture; creates a daemon and ensures cleanup."""
    daemons: list[MockDaemon] = []

    def _mk(response: bytes) -> MockDaemon:
        d = MockDaemon(response)
        daemons.append(d)
        return d

    yield _mk
    for d in daemons:
        d.close()
