"""Declarative multi-VM scenarios.

A `Topology` is a context manager that owns:
  - one or more inner test VMs created via the outer Corvus
  - the per-VM virtiofs shared-dirs (host binary; optionally source tree)
  - the per-VM VSOCK-to-TCP relays the harness uses to reach the daemons
  - the per-VM pycapnp `Client` instances

On entry the topology creates each VM by writing a small `apply` YAML
to a temp file and running `crv apply` on it. The YAML declares an
overlay disk on top of the integration-test base image plus a VM that
references that overlay AND the per-VM sharedDirs (host stack-install
bin → /opt/corvus/bin). Doing it in one `apply` ensures the VM is
born with the share attached — no post-creation `shared-dir add`,
no template (Corvus templates currently don't carry sharedDirs).

On exit, the topology tears everything down in reverse: close clients
→ stop relays → stop + delete VMs (`--delete-disks` removes the
per-VM overlays too).

Tests get a `TestVm` handle per VM and call `.client()` to reach the
inner daemon.
"""
from __future__ import annotations

import secrets
import tempfile
from contextlib import ExitStack
from dataclasses import dataclass
from pathlib import Path
from typing import Optional

import yaml as _yaml

from corvus_client import Client

from .host_binary import HostBinary, REPO_ROOT
from .images import ImageReady
from .inner import open_client
from .outer import Crv, CrvError
from .transport import VsockTcpRelay


# Prefix for every test-owned outer-side resource. Makes orphan
# cleanup a trivial `crv vm delete corvus-it-*`.
RESOURCE_PREFIX = "corvus-it"


@dataclass
class TestVm:
    """One inner test VM, paired with its host-side relay + client."""

    outer_name: str
    short_name: str
    cid: int
    relay: VsockTcpRelay
    _client: Optional[Client] = None

    @property
    def host_endpoint(self) -> tuple[str, int]:
        return self.relay.endpoint

    def client(self) -> Client:
        """Lazily open (or return) the pycapnp client to this VM's daemon.

        First call blocks for up to 3 min while the inner daemon
        finishes its first boot; subsequent calls return immediately.
        """
        if self._client is None:
            self._client = open_client(self.relay)
        return self._client


class Topology:
    """Per-test fixture that owns a set of `TestVm`s + their lifecycle."""

    def __init__(
        self,
        crv: Crv,
        image: ImageReady,
        host_binary: HostBinary,
        *,
        run_id: Optional[str] = None,
        attach_source: bool = False,
    ) -> None:
        self.crv = crv
        self.image = image
        self.host_binary = host_binary
        # 8-char hex run id keeps outer-side names short. Tests can
        # also pass an explicit `run_id` to make logs grep-friendly.
        self.run_id = run_id or secrets.token_hex(4)
        self.attach_source = attach_source
        self._vms: list[TestVm] = []
        self._stack: ExitStack = ExitStack()

    # ---- lifecycle -------------------------------------------------------

    def __enter__(self) -> "Topology":
        self._stack.__enter__()
        return self

    def __exit__(self, exc_type, exc, tb) -> None:
        # Tear down in reverse: clients first (cap drops), then relays,
        # then VMs. ExitStack handles the relays; we close clients and
        # delete VMs manually since both depend on Topology state.
        for vm in self._vms:
            if vm._client is not None:
                try:
                    vm._client.close()
                except Exception:
                    pass
        # Drop relays (popped by stack.__exit__).
        self._stack.__exit__(exc_type, exc, tb)

        # VM cleanup policy:
        #   - test passed → stop + delete every VM strictly; surface any
        #     cleanup failure so leaks don't accumulate silently.
        #   - test failed → leave VMs in place for inspection; print
        #     their names so the developer can poke at them, and how to
        #     clean up by hand.
        if exc_type is not None:
            if self._vms:
                names = " ".join(vm.outer_name for vm in self._vms)
                print(
                    f"[harness] test failed; leaving {len(self._vms)} VM(s) "
                    f"for inspection: {names}\n"
                    f"[harness] clean up manually with: "
                    f"for n in {names}; do crv vm delete --delete-disks $n; done"
                )
            return

        # `crv vm delete` refuses to delete a running VM (the daemon
        # returns RespVmRunning), so we have to stop first. There's no
        # `--force` for vm stop — only `--timeout`, after which the
        # daemon's watchdog SIGKILLs QEMU. A short timeout keeps test
        # cleanup tight; we don't care about an orderly guest shutdown
        # for a doomed test VM.
        cleanup_errors: list[str] = []
        for vm in self._vms:
            try:
                self.crv.vm_stop(vm.outer_name, wait=True, timeout_sec=10)
            except CrvError as e:
                # Already-stopped is fine; anything else is unexpected.
                if "not running" not in str(e).lower():
                    cleanup_errors.append(f"stop {vm.outer_name}: {e}")
            try:
                self.crv.vm_delete(vm.outer_name, delete_disks=True)
            except CrvError as e:
                cleanup_errors.append(f"delete {vm.outer_name}: {e}")
        if cleanup_errors:
            raise RuntimeError(
                "Topology teardown failed for one or more VMs:\n  - "
                + "\n  - ".join(cleanup_errors)
            )

    # ---- VM creation ------------------------------------------------------

    def add(
        self,
        short_name: str,
        *,
        cpu_count: int = 8,
        ram_mb: int = 8192,
        extra_shared_dirs: Optional[list[tuple[str, str, bool]]] = None,
    ) -> TestVm:
        """Add one VM to the topology and return its handle.

        `extra_shared_dirs` is a list of `(host_path, tag, read_only)`
        tuples; each becomes an additional sharedDir on the VM.
        """
        outer_name = f"{RESOURCE_PREFIX}-{self.run_id}-{short_name}"
        overlay_name = f"{outer_name}-rootfs"

        shared_dirs: list[dict] = [
            {
                "path": str(self.host_binary.bin_dir),
                "tag": "corvus_host",
                "cache": "auto",
                "readOnly": True,
            }
        ]
        if self.attach_source:
            shared_dirs.append(
                {
                    "path": str(REPO_ROOT),
                    "tag": "corvus_src",
                    "cache": "auto",
                    "readOnly": True,
                }
            )
        for host_path, tag, read_only in (extra_shared_dirs or []):
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
                }
            ],
            "vms": [
                {
                    "name": outer_name,
                    "cpuCount": cpu_count,
                    "ramMb": ram_mb,
                    "cloudInit": True,
                    "guestAgent": True,
                    "headless": True,
                    "description": f"Corvus integration test VM ({short_name})",
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
                            "type": "vde",
                            "hostDevice": "/run/vde2/switch.ctl",
                        }
                    ],
                    "sharedDirs": shared_dirs,
                    "sshKeys": ["corvus"],
                }
            ],
        }

        # Write the per-VM apply YAML to a temp file and feed it to
        # `crv apply`. `--wait` blocks until the synchronous part of
        # apply (disk overlay + VM record creation) is done; the
        # actual VM `start` happens below.
        with tempfile.NamedTemporaryFile(
            mode="w", suffix=".yml", prefix=f"{outer_name}-", delete=False
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
        # VM would otherwise boot without a Corvus binary mount and the
        # systemd corvus-test.service `RequiresMountsFor` check would
        # fail without a hint. Fail loudly here instead.
        # NB: `crv vm show` deliberately omits sharedDirs (the
        # protocol-side VmDetails record doesn't carry them), so we
        # query `crv shared-dir list` separately.
        post_apply = self.crv.vm_show(outer_name)
        post_apply_shared_dirs = self.crv.shared_dir_list(outer_name)
        missing: list[str] = []
        if not post_apply.get("drives"):
            missing.append("drives")
        if not post_apply.get("net_ifs") and not post_apply.get("network_interfaces"):
            missing.append("net_ifs")
        if not post_apply_shared_dirs:
            missing.append("shared_dirs")
        if missing:
            raise RuntimeError(
                f"`crv apply` created VM {outer_name!r} but it has no "
                f"{', '.join(missing)}. Schema drift in `crv apply` YAML? "
                f"VM show: {post_apply!r} shared-dir list: "
                f"{post_apply_shared_dirs!r}"
            )

        # Start and wait for the guest-agent ping — that proves the VM
        # is running, but NOT that the inner daemon is up; the daemon
        # readiness is checked inside `TestVm.client()`.
        self.crv.vm_start(outer_name, wait=True)

        # Resolve the VM's permanent VSOCK CID and spin the relay.
        details = self.crv.vm_show(outer_name)
        cid = details.get("vsock_cid")
        if not isinstance(cid, int) or cid <= 2:
            raise RuntimeError(
                f"VM {outer_name} has no usable VSOCK CID (got {cid!r}). "
                "Check the outer Corvus version supports vsock allocation."
            )
        relay = self._stack.enter_context(VsockTcpRelay.start(cid))
        test_vm = TestVm(outer_name=outer_name, short_name=short_name, cid=cid, relay=relay)
        self._vms.append(test_vm)
        return test_vm

    # ---- convenience -----------------------------------------------------

    def wait_for_all_clients(self) -> None:
        """Force-open every VM's client so first-boot waits happen up-front."""
        for vm in self._vms:
            vm.client()

    @property
    def vms(self) -> list[TestVm]:
        return list(self._vms)
