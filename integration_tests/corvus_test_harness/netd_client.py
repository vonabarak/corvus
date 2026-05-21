"""Sync wrapper around the `corvus-netd` Cap'n Proto agent API.

Mirrors the pattern in `corvus_client._sync._resource`: pycapnp's
native API is async, but tests read better as sync code. This module
hides the pycapnp KJ event-loop plumbing behind a plain
`NetdClient` whose methods block until the agent responds.

Reuses the existing pycapnp runloop from `TestNode.client()._rl`
(the same loop the harness uses for the inner daemon's client), so
agent calls and inner-daemon calls coexist on one event loop.

All return values are decoded to native Python types (dataclasses /
lists / strings) inside the coroutine, so callers never touch
pycapnp struct readers — those readers are unsafe to access once
their parent response has been GC'd off the loop thread.

Usage:

    from corvus_test_harness import NetdClient

    @pytest.fixture
    def agent(node, netd_endpoint):
        host, port = netd_endpoint
        with NetdClient.connect(host, port, node.client()._rl) as a:
            yield a

    def test_apply_network(self, agent):
        agent.apply_network({"name": "corvus-br-x", "cidr": "10.0.0.1/24", ...})
        agent.delete_network("corvus-br-x")
"""

from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path

import capnp
from corvus_client._schema import netagent as NETAGENT_SCHEMA


@dataclass(frozen=True)
class VersionInfo:
    semver: str
    capabilities: tuple[str, ...]


@dataclass(frozen=True)
class NetworkInfo:
    name: str
    cidr: str
    mtu: int
    nat_enabled: bool
    dhcp_enabled: bool
    up_state: str
    dnsmasq_pid: int


@dataclass(frozen=True)
class TapInfo:
    name: str
    bridge: str
    uid: int
    gid: int
    up_state: str


def _decode_network_info(info) -> NetworkInfo:
    spec = info.spec
    return NetworkInfo(
        name=spec.name,
        cidr=spec.cidr,
        mtu=spec.mtu,
        nat_enabled=spec.nat.enabled,
        dhcp_enabled=spec.dhcp.enabled,
        up_state=info.upState,
        dnsmasq_pid=info.dnsmasqPid,
    )


def _decode_tap_info(info) -> TapInfo:
    spec = info.spec
    return TapInfo(
        name=spec.name,
        bridge=spec.bridge,
        uid=spec.uid,
        gid=spec.gid,
        up_state=info.upState,
    )


class NetdClient:
    """Connection to a `corvus-netd` agent with a pre-opened Session.

    Exposes both top-level NetAgent methods (`ping`, `version`) and
    Session methods (`apply_network`, `delete_network`, …) on the
    same object — tests rarely need more than one session per
    connection.
    """

    def __init__(self, rl, stream, agent_cap, session_cap):
        self._rl = rl
        self._stream = stream
        self._agent = agent_cap
        self._sess = session_cap

    @classmethod
    def connect(
        cls,
        host: str,
        port: int,
        rl,
        *,
        owner: str = "test",
        cert_dir: Path | None = None,
    ) -> NetdClient:
        """Open a TCP connection to the agent, bootstrap, open a
        Session.

        When *cert_dir* is supplied, the dial wraps in TLS using
        the trio at ``<cert_dir>/{ca.crt,corvus-daemon.{crt,key}}``
        — netd's CN-prefix check requires the peer to present
        ``corvus-daemon:…`` (the daemon is its only legitimate
        caller in production). The harness builds such a dir
        per-test via :meth:`CaContext.mint_host_cert_dir`. With
        *cert_dir* unset the dial is plaintext, which only works
        against an agent started with ``--no-tls``.
        """

        async def _open():
            kwargs = {}
            if cert_dir is not None:
                # Local import to avoid a hard dep on
                # corvus_client._tls at module import time; the
                # plaintext path stays usable when corvus_client
                # is older than the TLS plumbing.
                from corvus_client import _tls

                bundle = _tls.build_client_bundle(
                    cert_dir=cert_dir,
                    role=_tls.ROLE_DAEMON,
                    expected_peer_prefix=_tls.ROLE_NETD,
                )
                kwargs["ssl"] = bundle.context
                kwargs["server_hostname"] = None
            stream = await capnp.AsyncIoStream.create_connection(
                host=host, port=port, **kwargs
            )
            two_party = capnp.TwoPartyClient(stream)
            agent_cap = two_party.bootstrap().cast_as(NETAGENT_SCHEMA.NetAgent)
            sess_cap = (await agent_cap.session(owner=owner)).session
            return stream, agent_cap, sess_cap

        stream, agent_cap, sess_cap = rl.run(_open())
        return cls(rl, stream, agent_cap, sess_cap)

    # Context manager: hand the pycapnp caps off to the runloop
    # thread for release. Dropping the last reference on the main
    # thread aborts kj with `expected loop != nullptr` — the same
    # issue handled by `LoopBoundResource` in
    # `corvus_client._sync._resource.py`. `schedule_drop` is a
    # GIL-atomic deque append; the runloop drains it on its own
    # thread where the kj loop is alive.
    def __enter__(self) -> NetdClient:
        return self

    def __exit__(self, exc_type, exc, tb) -> None:
        self.close()

    def close(self) -> None:
        rl = self.__dict__.pop("_rl", None)
        for key in ("_sess", "_agent", "_stream"):
            obj = self.__dict__.pop(key, None)
            if obj is not None and rl is not None:
                try:
                    rl.schedule_drop(obj)
                except Exception:
                    pass

    def __del__(self):
        # Mirror of close(); fires if the context manager wasn't used.
        try:
            self.close()
        except Exception:
            pass

    # ---- internals --------------------------------------------------------

    # pycapnp auto-generated method calls (e.g. `agent.ping()`) need
    # the runloop's asyncio loop to be the *running* loop at the
    # moment they're called, because pycapnp inspects
    # `asyncio.get_running_loop()` to attach the promise. Constructing
    # the coroutine inside an `async def` ensures the pycapnp call
    # happens on the loop thread.
    def _call(self, _async_fn):
        return self._rl.run(_async_fn())

    # ---- NetAgent (top-level) --------------------------------------------

    def ping(self) -> None:
        async def go():
            await self._agent.ping()

        self._call(go)

    def version(self) -> VersionInfo:
        async def go():
            info = (await self._agent.version()).info
            return VersionInfo(
                semver=info.semver,
                capabilities=tuple(info.capabilities),
            )

        return self._call(go)

    # ---- Session: networks ------------------------------------------------

    def apply_network(self, spec) -> NetworkInfo:
        async def go():
            return _decode_network_info((await self._sess.applyNetwork(spec=spec)).info)

        return self._call(go)

    def delete_network(self, name: str) -> None:
        # Positional: pycapnp clashes if `name=` is passed as kwarg
        # (`name` is internal to the call descriptor).
        async def go():
            await self._sess.deleteNetwork(name)

        self._call(go)

    def list_networks(self) -> list[NetworkInfo]:
        async def go():
            return [
                _decode_network_info(n)
                for n in (await self._sess.listNetworks()).networks
            ]

        return self._call(go)

    # ---- Session: TAPs ----------------------------------------------------

    def apply_tap(self, spec) -> TapInfo:
        async def go():
            return _decode_tap_info((await self._sess.applyTap(spec=spec)).info)

        return self._call(go)

    def delete_tap(self, name: str) -> None:
        async def go():
            await self._sess.deleteTap(name)

        self._call(go)

    def list_taps(self) -> list[TapInfo]:
        async def go():
            return [_decode_tap_info(t) for t in (await self._sess.listTaps()).taps]

        return self._call(go)

    # ---- Session: kernel knobs -------------------------------------------

    def set_ip_forwarding(self, enabled: bool, family: str) -> None:
        async def go():
            await self._sess.setIpForwarding(enabled=enabled, family=family)

        self._call(go)
