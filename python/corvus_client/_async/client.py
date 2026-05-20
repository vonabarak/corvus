"""Async Corvus client.

Manages a single Cap'n Proto session over a Unix or TCP transport.
Use as an async context manager:

    async with capnp.kj_loop():
        async with AsyncClient(unix_socket="/run/.../corvus.sock") as c:
            info = await c.status()
            vm = await c.vms.get("web-1")
            await vm.start(wait=True)

The lazy `vms`/`disks`/... properties cache subsystem manager wrappers
so they survive across calls.
"""
from __future__ import annotations

from pathlib import Path
from typing import TYPE_CHECKING, Optional

import capnp

from .. import _schema, _tls
from ..exceptions import translate_errors
from . import _convert as conv

if TYPE_CHECKING:  # avoid circular imports at runtime
    from .cloudinit import AsyncCloudInitManager
    from .disk import AsyncDiskManager
    from .network import AsyncNetworkManager
    from .node import AsyncNodeManager
    from .sshkey import AsyncSshKeyManager
    from .task import AsyncTaskManager
    from .template import AsyncTemplateManager
    from .vm import AsyncVmManager


@translate_errors
class AsyncClient:
    def __init__(
        self,
        *,
        unix_socket: Optional[str] = None,
        host: Optional[str] = None,
        port: int = 9876,
        cert_dir: Optional[str | Path] = None,
        tls: Optional[bool] = None,
    ) -> None:
        """Connect to the daemon over either a Unix socket or TCP.

        :param cert_dir: when set, force the client to load its
            cert / key / CA from this directory and skip the
            usual search path. Ignored for Unix-socket
            connections.
        :param tls: explicit override. ``None`` (the default)
            means *auto*: TLS on for TCP, off for Unix. ``True``
            forces TLS on (errors on Unix since pycapnp's
            ``create_unix_connection`` doesn't take ``ssl=``);
            ``False`` disables TLS even for TCP — useful when
            the daemon was started with ``--no-tls`` for dev.
        """

        if unix_socket and host:
            raise ValueError("specify exactly one of unix_socket or host")
        if unix_socket is None and host is None:
            raise ValueError("specify unix_socket or host")
        self._unix = unix_socket
        self._host = host
        self._port = port
        self._cert_dir: Optional[Path] = (
            Path(cert_dir) if cert_dir is not None else None
        )
        # Resolve effective TLS mode now so we can fail-fast in
        # the constructor rather than mid-handshake.
        if tls is None:
            self._tls_enabled = host is not None
        else:
            if tls and unix_socket is not None:
                raise ValueError(
                    "tls=True requested but unix_socket was given; "
                    "Unix sockets rely on filesystem permissions and "
                    "do not wrap with TLS"
                )
            self._tls_enabled = bool(tls)
        self._tls_bundle: Optional[_tls.TlsBundle] = None
        self._twoparty: Optional[capnp.TwoPartyClient] = None
        self._daemon = None
        self._stream: Optional[capnp.AsyncIoStream] = None
        self._vms: Optional["AsyncVmManager"] = None
        self._disks: Optional["AsyncDiskManager"] = None
        self._networks: Optional["AsyncNetworkManager"] = None
        self._ssh_keys: Optional["AsyncSshKeyManager"] = None
        self._templates: Optional["AsyncTemplateManager"] = None
        self._tasks: Optional["AsyncTaskManager"] = None
        self._cloud_init: Optional["AsyncCloudInitManager"] = None
        self._nodes: Optional["AsyncNodeManager"] = None

    async def __aenter__(self) -> "AsyncClient":
        if self._unix:
            stream = await capnp.AsyncIoStream.create_unix_connection(self._unix)
        else:
            kwargs = {}
            if self._tls_enabled:
                # Build the bundle once on the connection path so
                # the constructor stays cheap and so we can pull
                # the validated cert dir into log lines.
                self._tls_bundle = _tls.build_client_bundle(
                    cert_dir=self._cert_dir,
                )
                kwargs["ssl"] = self._tls_bundle.context
                # `server_hostname=None` disables SNI; the daemon
                # doesn't dispatch by SNI and the cert SAN may not
                # match the dialed address. Hostname matching is
                # disabled in the SSLContext too.
                kwargs["server_hostname"] = None
            stream = await capnp.AsyncIoStream.create_connection(
                self._host, self._port, **kwargs
            )
            if self._tls_enabled:
                self._validate_peer_cn(stream)
        self._stream = stream
        self._twoparty = capnp.TwoPartyClient(stream)
        self._daemon = self._twoparty.bootstrap().cast_as(_schema.corvus.Daemon)
        return self

    def _validate_peer_cn(self, stream: "capnp.AsyncIoStream") -> None:
        """Pull the peer cert off the underlying asyncio transport
        and check the CN against the bundle's expectations. Called
        once per connection, immediately after the handshake."""

        bundle = self._tls_bundle
        if bundle is None:
            return
        try:
            transport = stream.protocol.transport
        except AttributeError as e:
            raise _tls.CertificateError(
                "pycapnp stream has no protocol.transport; cannot read peer cert"
            ) from e
        peercert = transport.get_extra_info("peercert")
        _tls.validate_peer_cn(peercert, bundle)

    async def __aexit__(self, exc_type, exc, tb) -> None:
        await self.close()

    async def close(self) -> None:
        self._daemon = None
        if self._twoparty is not None:
            try:
                self._twoparty.close()
            except Exception:
                pass
            self._twoparty = None
        if self._stream is not None:
            try:
                self._stream.close()
                await self._stream.wait_closed()
            except Exception:
                pass
            self._stream = None

    @property
    def daemon(self):
        """Raw Daemon cap. Library users may invoke un-wrapped methods through this."""
        if self._daemon is None:
            raise RuntimeError("AsyncClient: not connected (use as async context manager)")
        return self._daemon

    # ---- top-level Daemon methods ----------------------------------------

    async def ping(self) -> None:
        await self.daemon.ping()

    async def status(self):
        info = (await self.daemon.status()).info
        return conv.status_info(info)

    async def shutdown(self) -> None:
        await self.daemon.shutdown()

    async def apply(self, yaml: str, *, skip_existing: bool = False, wait: bool = False):
        """Run `apply` against a YAML pipeline. Returns (ApplyResult, task_id)."""
        resp = await self.daemon.apply(
            yaml=yaml, skipExisting=skip_existing, wait=wait
        )
        return conv.apply_result(resp.result), resp.taskId

    def build_stream(self, yaml_path: str):
        """Stream `Daemon.build` events for a YAML pipeline file.

        Returns an async generator. The YAML is preprocessed client-side
        (shell.script/file.from/floppy.from are inlined) before being
        sent. Yields BuildEvent dataclasses; the final item is a
        `('task_id', N)` tuple.
        """
        from .build import stream_build_from_file

        return stream_build_from_file(self.daemon, yaml_path)

    def build_stream_text(self, yaml_text: str):
        """Like `build_stream`, but accepts already-preprocessed YAML text.

        Useful for callers that read/transform the YAML themselves.
        """
        from .streams import stream_build_events

        return stream_build_events(self.daemon, yaml_text)

    # ---- subsystem managers (lazy) ---------------------------------------

    @property
    def vms(self) -> "AsyncVmManager":
        if self._vms is None:
            from .vm import AsyncVmManager
            self._vms = AsyncVmManager(self.daemon)
        return self._vms

    @property
    def disks(self) -> "AsyncDiskManager":
        if self._disks is None:
            from .disk import AsyncDiskManager
            self._disks = AsyncDiskManager(self.daemon)
        return self._disks

    @property
    def networks(self) -> "AsyncNetworkManager":
        if self._networks is None:
            from .network import AsyncNetworkManager
            self._networks = AsyncNetworkManager(self.daemon)
        return self._networks

    @property
    def ssh_keys(self) -> "AsyncSshKeyManager":
        if self._ssh_keys is None:
            from .sshkey import AsyncSshKeyManager
            self._ssh_keys = AsyncSshKeyManager(self.daemon)
        return self._ssh_keys

    @property
    def templates(self) -> "AsyncTemplateManager":
        if self._templates is None:
            from .template import AsyncTemplateManager
            self._templates = AsyncTemplateManager(self.daemon)
        return self._templates

    @property
    def tasks(self) -> "AsyncTaskManager":
        if self._tasks is None:
            from .task import AsyncTaskManager
            self._tasks = AsyncTaskManager(self.daemon)
        return self._tasks

    @property
    def cloud_init(self) -> "AsyncCloudInitManager":
        if self._cloud_init is None:
            from .cloudinit import AsyncCloudInitManager
            self._cloud_init = AsyncCloudInitManager(self.daemon)
        return self._cloud_init

    @property
    def nodes(self) -> "AsyncNodeManager":
        if self._nodes is None:
            from .node import AsyncNodeManager
            self._nodes = AsyncNodeManager(self.daemon)
        return self._nodes
