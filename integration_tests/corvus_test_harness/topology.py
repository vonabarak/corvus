"""Declarative multi-node scenarios.

A `Topology` is a context manager that owns:
  - one or more orchestrator nodes (Gentoo VMs) created via the
    outer Corvus daemon
  - the per-node virtiofs shared-dirs (host binary; optionally source tree)
  - the per-node VSOCK-to-TCP relays the harness uses to reach the
    inner daemons
  - the per-node pycapnp `Client` instances

On entry the topology creates each node by writing a small `apply` YAML
to a temp file and running `crv apply` on it. The YAML declares an
overlay disk on top of the integration-test base image plus a VM that
references that overlay AND the per-node sharedDirs (host stack-install
bin → /opt/corvus/bin). Doing it in one `apply` ensures the node is
born with the share attached — no post-creation `shared-dir add`,
no template (Corvus templates currently don't carry sharedDirs).

On exit, the topology tears everything down in reverse: close clients
→ stop relays → stop + delete nodes (the per-node overlays are
created as ephemeral, so `vm delete` reaps them along with the VM).

Tests get a `TestNode` handle per node and call `.client()` to reach
the inner daemon running on it.
"""

from __future__ import annotations

import enum
import secrets
import shutil
import subprocess
import sys
import tempfile
from contextlib import ExitStack
from dataclasses import dataclass
from pathlib import Path

from corvus_client import Client

import yaml as _yaml

from . import component_deploy
from .base_images import BASE_IMAGES_TAG, HOST_BASE_IMAGES_DIR
from .component_deploy import CaContext
from .host_binary import REPO_ROOT, HostBinary
from .images import ImageReady
from .inner import open_client
from .outer import Crv, CrvError
from .ssh import HOST_ALPINE_KEY_PATH, NodeShell
from .transport import VsockTcpRelay

# Prefix for every test-owned node-side resource. Makes orphan
# cleanup a trivial `crv vm delete corvus-it-*`.
RESOURCE_PREFIX = "corvus-it"


class NodeRole(enum.Enum):
    """What software stack runs on a given test-node.

    ``FULL_STACK``: corvus daemon + corvus-nodeagent + corvus-netd.
        Has a `client()` that dials the daemon's TCP listener.
    ``AGENTS_ONLY``: corvus-nodeagent + corvus-netd only.
        Used for the beta node in `OneDaemonTwoNodesCase` —
        alpha's daemon dials beta over mTLS. `client()` raises
        :class:`NoDaemonOnNodeError`.
    """

    FULL_STACK = "full_stack"
    AGENTS_ONLY = "agents_only"


class NoDaemonOnNodeError(RuntimeError):
    """Raised by :meth:`TestNode.client` when the node only runs
    the agents (no daemon → nothing to dial)."""


@dataclass
class TestNode:
    """One orchestrator node, paired with its host-side relay + the
    pycapnp client to the node's inner daemon."""

    name: str
    short_name: str
    cid: int
    relay: VsockTcpRelay
    # The stack the harness will deploy onto this VM. The default
    # matches the old single-node-everything-on shape; multi-node
    # cases pick per-node via `Topology.add(role=…)`.
    role: NodeRole = NodeRole.FULL_STACK
    # Key into the topology's `_ca_contexts` map. Default is the
    # shared `"shared"` CA; `TwoDaemonsCase` sets a per-node key.
    # Stashed on the TestNode (rather than passing a `CaContext`
    # directly) so the case fixture can build all CAs up front and
    # then call `Topology.deploy_certs()` once.
    ca_key: str = "shared"
    _client: Client | None = None
    # Populated by `Topology.deploy_certs()` with the host-side
    # path holding {ca.crt, corvus-client.{crt,key}} the pycapnp
    # client uses to dial this node's daemon over mTLS.
    _client_cert_dir: Path | None = None
    # IP address inside the VM that the deployer baked into the
    # cert SAN (and that any *other* node would dial this node
    # by). Captured by `Topology.deploy_certs()`; available to
    # tests via `TestNode.outer_ip`.
    _outer_ip: str | None = None

    @property
    def host_endpoint(self) -> tuple[str, int]:
        return self.relay.endpoint

    @property
    def outer_ip(self) -> str:
        """The address baked into this node's cert IP SAN. Tests
        that bring up cross-node connections (e.g. registering
        beta as a node on alpha's daemon) use this directly."""

        if self._outer_ip is None:
            raise RuntimeError(
                f"TestNode {self.short_name!r}: outer_ip not resolved yet — "
                "Topology.deploy_certs() hasn't run"
            )
        return self._outer_ip

    def client(self) -> Client:
        """Lazily open (or return) the pycapnp client to this node's
        inner daemon.

        First call blocks for up to 3 min while the inner daemon
        finishes its first boot; subsequent calls return immediately.
        Raises :class:`NoDaemonOnNodeError` on `AGENTS_ONLY` nodes —
        there's nothing to dial.
        """
        if self.role is NodeRole.AGENTS_ONLY:
            raise NoDaemonOnNodeError(
                f"node {self.short_name!r} runs corvus-nodeagent + corvus-netd "
                "only; no daemon to dial"
            )
        if self._client is None:
            # cert_dir/tls land here after deploy_certs(); without
            # them open_client would fall back to its (mTLS-on)
            # default and fail to validate the server cert.
            if self._client_cert_dir is None:
                raise RuntimeError(
                    f"node {self.short_name!r}: cert dir not set — "
                    "Topology.deploy_certs() hasn't run"
                )
            # Register self-node under `short_name`, not the
            # default "self". The daemon's per-node supervisor
            # dials the agents over mTLS and expects the peer's
            # CN to match `corvus-node:<Node.name>` — the
            # nodeagent's cert was minted with `corvus-node:<short_name>`
            # in deploy_certs(), so the row name has to agree.
            # Register self with the routable outer_ip rather than
            # 127.0.0.1: the daemon's own dial still works because
            # the agent listens on 0.0.0.0, AND other nodeagents
            # (during the inter-agent disk-transfer flow) can dial
            # this node by the same address the daemon recorded.
            self._client = open_client(
                self.relay,
                tls=True,
                cert_dir=self._client_cert_dir,
                self_node_name=self.short_name,
                self_node_host=self.outer_ip,
            )
        return self._client

    def run(
        self,
        command: str,
        *,
        timeout_sec: float = 60.0,
        check: bool = True,
        user: str = "corvus",
    ) -> subprocess.CompletedProcess:
        """Run `command` on this node via SSH-over-VSOCK.

        Returns the raw `subprocess.CompletedProcess`; stdout/stderr
        are bytes (decode with `.stdout.decode()`). Set `check=False`
        to let non-zero exits return normally instead of raising.
        """
        if not HOST_ALPINE_KEY_PATH.exists():
            raise RuntimeError(
                f"SSH private key not found at {HOST_ALPINE_KEY_PATH} — "
                "run `make test-image-key` to generate it"
            )
        return NodeShell(cid=self.cid, user=user, key_path=HOST_ALPINE_KEY_PATH).run(
            command, timeout_sec=timeout_sec, check=check
        )


class Topology:
    """Per-test fixture that owns a set of `TestNode`s + their lifecycle."""

    def __init__(
        self,
        crv: Crv,
        image: ImageReady,
        host_binary: HostBinary,
        *,
        class_name: str,
        network_name: str,
        run_id: str | None = None,
        attach_source: bool = False,
    ) -> None:
        self.crv = crv
        self.image = image
        self.host_binary = host_binary
        # The managed network all test-nodes attach to for their
        # outer NIC. Created session-wide by the
        # `session_test_network` fixture in conftest.py and shared
        # across every Topology in this pytest invocation.
        self.network_name = network_name
        # Test class name (e.g. "TestVmLifecycle") embedded into every
        # node name so `crv vm list` on the outer daemon shows which
        # test class owns which node.
        self.class_name = class_name
        # 8-char hex run id keeps node names short. Tests can also
        # pass an explicit `run_id` to make logs grep-friendly.
        self.run_id = run_id or secrets.token_hex(4)
        self.attach_source = attach_source
        self._nodes: list[TestNode] = []
        self._stack: ExitStack = ExitStack()
        # Per-class cert tree. CA stores live under `_cert_root/ca/<key>/`;
        # per-node host-side client cert dirs (handed to `Client(cert_dir=…)`)
        # live under `_cert_root/host/<short_name>/`. Cleaned up by finalize()
        # unless we're leaking on failure.
        #
        # The dir name embeds `run_id` (same run_id baked into every VM
        # name) so debug tools — e.g. `integration_tests/scripts/crv-it` —
        # can map a leaked outer-daemon VM name back to its cert dir
        # without guessing among multiple leaked trees.
        self._cert_root = (
            Path(tempfile.gettempdir())
            / f"corvus-it-pki-{self.class_name}-{self.run_id}"
        )
        self._cert_root.mkdir(parents=True, exist_ok=False)
        self._ca_contexts: dict[str, CaContext] = {}
        self._certs_deployed = False

    # ---- lifecycle -------------------------------------------------------

    def __enter__(self) -> Topology:
        self._stack.__enter__()
        return self

    def __exit__(self, exc_type, exc, tb) -> None:
        # Treat any exception inside the with-block as "tests failed,
        # leak the nodes for inspection". Callers that need to distinguish
        # "with-block raised" from "test method later failed" should
        # construct a Topology manually and drive `finalize` themselves
        # (see corvus_test_harness.cases.IntegrationTestCase).
        self.finalize(leak_on_failure=exc_type is not None)

    def finalize(self, *, leak_on_failure: bool) -> None:
        """Tear down clients, relays, and nodes.

        On `leak_on_failure=True` the nodes are left running and a
        cleanup-by-hand hint is printed to stderr — useful when the
        caller (a test method, a class fixture) wants to inspect a
        failed run rather than lose the state to teardown.

        Idempotent: subsequent calls are no-ops.
        """
        if getattr(self, "_finalized", False):
            return
        self._finalized = True

        # Tear down in reverse: clients first (cap drops), then relays,
        # then nodes. ExitStack handles the relays; we close clients and
        # delete nodes manually since both depend on Topology state.
        for node in self._nodes:
            if node._client is not None:
                try:
                    node._client.close()
                except Exception:
                    pass
        # Drop relays (popped by stack.__exit__).
        self._stack.__exit__(None, None, None)

        if leak_on_failure:
            if self._nodes:
                names = " ".join(node.name for node in self._nodes)
                print(
                    f"[harness] leaving {len(self._nodes)} node(s) "
                    f"alive for inspection: {names}\n"
                    f"[harness] clean up manually with: "
                    f"`make integration-tests-clean` "
                    "or "
                    f"`for n in {names}; do crv vm delete $n; done`"
                )
            return

        # `crv vm delete` refuses to delete a running VM (the daemon
        # returns RespVmRunning), so we have to stop first. There's no
        # `--force` for vm stop — only `--timeout`, after which the
        # daemon's watchdog SIGKILLs QEMU. A short timeout keeps test
        # cleanup tight; we don't care about an orderly guest shutdown
        # for a doomed test node.
        cleanup_errors: list[str] = []
        for node in self._nodes:
            try:
                self.crv.vm_stop(node.name, wait=True, timeout_sec=10)
            except CrvError as e:
                # Already-stopped is fine; anything else is unexpected.
                if "not running" not in str(e).lower():
                    cleanup_errors.append(f"stop {node.name}: {e}")
            try:
                self.crv.vm_delete(node.name)
            except CrvError as e:
                cleanup_errors.append(f"delete {node.name}: {e}")
        if cleanup_errors:
            raise RuntimeError(
                "Topology teardown failed for one or more nodes:\n  - "
                + "\n  - ".join(cleanup_errors)
            )

        # CA stores + per-node host cert dirs are intermediate
        # build products of the deploy step; nothing else holds
        # references to them after the clients close. Wipe.
        try:
            shutil.rmtree(self._cert_root, ignore_errors=True)
        except Exception:
            pass

    # ---- CA + cert deploy ----------------------------------------------------

    def init_cas(self, keys: tuple[str, ...]) -> None:
        """Build one :class:`CaContext` per key in *keys*.

        Called by the case fixture before any ``add()``; the
        keys must match whatever ``add(ca_key=…)`` will reference.
        Typical shapes:

        * ``("shared",)`` — SingleNodeCase, OneDaemonTwoNodesCase.
          Every node points at the one CA.
        * ``("alpha", "beta")`` — TwoDaemonsCase. Each node has
          its own CA so beta's daemon won't validate alpha's
          cert and vice-versa.
        """

        if self._ca_contexts:
            raise RuntimeError("init_cas may only be called once per Topology")
        for key in keys:
            root = self._cert_root / "ca" / key
            self._ca_contexts[key] = CaContext.new(root, client_name="harness")

    def deploy_certs(self) -> None:
        """Push cert trios into every registered node and start
        the inner services.

        Walks every :class:`TestNode` added to this topology, in
        order. For each:

        1. Resolve the node's outer IP (the address baked into
           the cert IP SAN — also what cross-node dials will
           use).
        2. Look up the CA context this node is keyed against.
        3. Either :func:`component_deploy.deploy_full_stack` or
           :func:`component_deploy.deploy_agents_only` depending
           on `node.role`.
        4. Stash the returned host-side cert dir (full-stack
           only) onto the TestNode so the lazy
           :meth:`TestNode.client` can dial over mTLS.

        Idempotent: calling twice is a no-op (the case fixture
        shouldn't, but a leaked-on-failure restart shouldn't
        crash either).
        """

        if self._certs_deployed:
            return
        if not self._ca_contexts:
            raise RuntimeError(
                "deploy_certs() requires init_cas() first; case fixture "
                "should call it before the first add()"
            )

        host_cert_root = self._cert_root / "host"
        host_cert_root.mkdir(parents=True, exist_ok=True)

        for node in self._nodes:
            shell = NodeShell(
                cid=node.cid, user="corvus", key_path=HOST_ALPINE_KEY_PATH
            )
            outer_ip = shell.outer_ip()
            node._outer_ip = outer_ip

            ca_ctx = self._ca_contexts.get(node.ca_key)
            if ca_ctx is None:
                raise RuntimeError(
                    f"node {node.short_name!r}: ca_key={node.ca_key!r} "
                    f"has no matching CA (init_cas keys: "
                    f"{tuple(self._ca_contexts)})"
                )
            sys.stderr.write(
                f"[harness] deploying certs to {node.short_name} "
                f"(role={node.role.value}, ip={outer_ip})\n"
            )
            sys.stderr.flush()

            if node.role is NodeRole.FULL_STACK:
                cert_dir = component_deploy.deploy_full_stack(
                    ca_ctx,
                    shell,
                    node_name=node.short_name,
                    node_ip=outer_ip,
                    host_cert_root=host_cert_root,
                )
                node._client_cert_dir = cert_dir
            else:
                component_deploy.deploy_agents_only(
                    ca_ctx,
                    shell,
                    node_name=node.short_name,
                    node_ip=outer_ip,
                )

        self._certs_deployed = True

    @property
    def ca(self) -> CaContext:
        """Convenience accessor for the single-CA case. Raises
        when there's more than one CA — TwoDaemonsCase callers
        should walk :attr:`ca_contexts` instead."""
        if len(self._ca_contexts) != 1:
            raise RuntimeError(
                f"Topology has {len(self._ca_contexts)} CAs; "
                "use .ca_contexts[<key>] explicitly"
            )
        return next(iter(self._ca_contexts.values()))

    @property
    def ca_contexts(self) -> dict[str, CaContext]:
        return dict(self._ca_contexts)

    # ---- node creation ----------------------------------------------------

    def add(
        self,
        short_name: str,
        *,
        cpu_count: int = 8,
        ram_mb: int = 8192,
        extra_shared_dirs: list[tuple[str, str, bool]] | None = None,
        role: NodeRole = NodeRole.FULL_STACK,
        ca_key: str = "shared",
    ) -> TestNode:
        """Add one node to the topology and return its handle.

        `extra_shared_dirs` is a list of `(host_path, tag, read_only)`
        tuples; each becomes an additional sharedDir on the node.
        """
        node_name = f"{RESOURCE_PREFIX}-{self.class_name}-{self.run_id}-{short_name}"
        overlay_name = f"{node_name}-rootfs"

        shared_dirs: list[dict] = [
            {
                "path": str(self.host_binary.bin_dir),
                "tag": "corvus_host",
                "cache": "auto",
                "readOnly": True,
            }
        ]
        # Always attach the host's BaseImages dir. The image's
        # home-corvus-VMs-BaseImages.mount auto-mounts it under
        # /home/corvus/VMs/BaseImages, so tests can register pre-baked
        # images without touching the node. Tolerate a missing host dir
        # (developers may not have run `make test-image-*` yet) — the
        # daemon doesn't fault if the share is absent, and the
        # `base_images` fixture surfaces a clear error per-test.
        if HOST_BASE_IMAGES_DIR.is_dir():
            shared_dirs.append(
                {
                    "path": str(HOST_BASE_IMAGES_DIR),
                    "tag": BASE_IMAGES_TAG,
                    "cache": "auto",
                    "readOnly": True,
                }
            )
        if self.attach_source:
            shared_dirs.append(
                {
                    "path": str(REPO_ROOT),
                    "tag": "corvus_src",
                    "cache": "auto",
                    "readOnly": True,
                }
            )
        for host_path, tag, read_only in extra_shared_dirs or []:
            shared_dirs.append(
                {
                    "path": host_path,
                    "tag": tag,
                    "cache": "auto",
                    "readOnly": read_only,
                }
            )

        # `crv apply <file>` parses the YAML root directly as an
        # ApplyConfig — root fields are sshKeys / disks / networks /
        # vms / templates / ifExists. The `apply:` wrapper is ONLY
        # used inside `pipeline:` documents consumed by `crv build`;
        # wrapping here silently succeeds with zero ops (every list
        # defaults to []) and then `crv vm start` fails with
        # "VM not found".
        doc = {
            "ifExists": "skip",
            "disks": [
                {
                    "name": overlay_name,
                    "overlay": self.image.disk_name,
                    # Reaped together with the test-node VM on
                    # topology teardown. Without this flag, every
                    # integration-test session would leave a
                    # `<prefix>-<class>-<run>-<node>-rootfs` qcow2
                    # behind under the operator's $HOME/VMs.
                    "ephemeral": True,
                }
            ],
            "vms": [
                {
                    "name": node_name,
                    "cpuCount": cpu_count,
                    "ramMb": ram_mb,
                    # The integration-test image bakes the harness's
                    # SSH key into /home/corvus/.ssh/authorized_keys at
                    # build time, so we don't need cloud-init at apply
                    # time. Disabling it saves a few seconds of boot
                    # per class (cloud-init would otherwise spin for a
                    # bit before deciding there's no datasource).
                    "cloudInit": False,
                    "guestAgent": True,
                    "headless": True,
                    "description": f"Corvus integration test node ({short_name})",
                    "drives": [
                        {
                            "disk": overlay_name,
                            "interface": "virtio",
                            "cacheType": "writeback",
                            "discard": True,
                        }
                    ],
                    "networkInterfaces": [
                        {
                            "type": "managed",
                            "network": self.network_name,
                        }
                    ],
                    "sharedDirs": shared_dirs,
                }
            ],
        }

        # Write the per-node apply YAML to a temp file and feed it to
        # `crv apply`. `--wait` blocks until the synchronous part of
        # apply (disk overlay + VM record creation) is done; the
        # actual VM `start` happens below.
        with tempfile.NamedTemporaryFile(
            mode="w", suffix=".yml", prefix=f"{node_name}-", delete=False
        ) as f:
            _yaml.safe_dump(doc, f, sort_keys=False)
            tmp_path = Path(f.name)
        try:
            self.crv.apply(tmp_path, skip_existing=True, wait=True, timeout_sec=300)
        except Exception:
            # Keep the temp YAML on failure so the developer can inspect
            # exactly what was fed to `crv apply`.
            print(f"[harness] apply YAML kept for inspection: {tmp_path}")
            raise
        else:
            try:
                tmp_path.unlink()
            except OSError:
                pass

        # Sanity-check that apply persisted what we asked for.
        # A silently-empty drives / net_ifs / sharedDirs list means the
        # YAML structure was wrong and apply succeeded vacuously; the
        # node would otherwise boot without a Corvus binary mount and
        # the systemd corvus.service `RequiresMountsFor` check would
        # fail without a hint. Fail loudly here instead.
        # NB: `crv vm show` deliberately omits sharedDirs (the
        # protocol-side VmDetails record doesn't carry them), so we
        # query `crv shared-dir list` separately.
        post_apply = self.crv.vm_show(node_name)
        post_apply_shared_dirs = self.crv.shared_dir_list(node_name)
        missing: list[str] = []
        if not post_apply.get("drives"):
            missing.append("drives")
        if not post_apply.get("net_ifs") and not post_apply.get("network_interfaces"):
            missing.append("net_ifs")
        if not post_apply_shared_dirs:
            missing.append("shared_dirs")
        if missing:
            raise RuntimeError(
                f"`crv apply` created node {node_name!r} but it has no "
                f"{', '.join(missing)}. Schema drift in `crv apply` YAML? "
                f"VM show: {post_apply!r} shared-dir list: "
                f"{post_apply_shared_dirs!r}"
            )

        # Start and wait for the guest-agent ping — that proves the
        # node is running, but NOT that the inner daemon is up; the
        # daemon readiness is checked inside `TestNode.client()`.
        self.crv.vm_start(node_name, wait=True)

        # Resolve the node's permanent VSOCK CID and spin the relay.
        details = self.crv.vm_show(node_name)
        cid = details.get("vsock_cid")
        if not isinstance(cid, int) or cid <= 2:
            raise RuntimeError(
                f"Node {node_name} has no usable VSOCK CID (got {cid!r}). "
                "Check the outer Corvus version supports vsock allocation."
            )
        relay = self._stack.enter_context(VsockTcpRelay.start(cid))
        test_node = TestNode(
            name=node_name,
            short_name=short_name,
            cid=cid,
            relay=relay,
            role=role,
            ca_key=ca_key,
        )
        self._nodes.append(test_node)
        return test_node

    # ---- convenience -----------------------------------------------------

    def wait_for_all_clients(self) -> None:
        """Force-open every node's client so first-boot waits happen up-front."""
        for node in self._nodes:
            node.client()

    @property
    def nodes(self) -> list[TestNode]:
        return list(self._nodes)
