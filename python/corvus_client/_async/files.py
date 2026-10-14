"""Bounded local file callbacks for QGA transfers."""

from __future__ import annotations

from typing import BinaryIO

from .. import _schema

CHUNK_SIZE = 32768


class FileSource(_schema.streams.ByteSource.Server):  # type: ignore[misc,name-defined]
    def __init__(self, handle: BinaryIO) -> None:
        self.handle = handle
        self.active = True
        self.count = 0

    async def read(self, maxBytes: int, _context: object) -> tuple[bytes, bool]:
        if not self.active or self.handle.closed:
            raise ValueError("file source closed")
        if not 0 < maxBytes <= CHUNK_SIZE:
            raise ValueError("invalid file chunk size")
        chunk = self.handle.read(maxBytes)
        self.count += len(chunk)
        eof = len(chunk) < maxBytes
        if eof:
            self.active = False
        return chunk, eof


class FileSink(_schema.streams.ByteSink.Server):  # type: ignore[misc,name-defined]
    def __init__(self, handle: BinaryIO) -> None:
        self.handle = handle
        self.active = True
        self.ended = False
        self.count = 0

    async def write(self, chunk: bytes, _context: object) -> None:
        if not self.active or self.handle.closed or len(chunk) > CHUNK_SIZE:
            raise ValueError("file sink closed or oversized chunk")
        data = bytes(chunk)
        if self.handle.write(data) != len(data):
            raise OSError("short local file write")
        self.count += len(data)

    async def end(self, _context: object) -> None:
        if not self.active:
            raise ValueError("file sink closed")
        self.active = False
        self.ended = True

    async def abort(self, _context: object) -> None:
        self.active = False
        self.ended = False
