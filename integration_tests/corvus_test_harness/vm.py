"""Context managers that wrap the vm lifecycle for tests.

A test method that needs a booted VM otherwise has to chain
~25 lines of `register_base_images` lookup → `disks.create_overlay`
→ `vms.create` → `vm.attach_disk` (`vms.create` only creates the
bare VM record; drives, network interfaces, SSH keys, and
cloud-init are attached afterward) → `vm.start(wait=True)` → use
the VM → `vm.stop` → `vm.delete(delete_disks=True)` → belt-and-
suspenders disk cleanup. Multiplied across many tests that's a
lot of boilerplate.

Two classes live here:

* `Vm` — the lifecycle. Defaults to "Alpine, BIOS, headless,
  qemu-guest-agent, block on QGA first ping" and tears the VM down
  on exit. Knows nothing about SSH.
* `VmSsh` — `Vm` plus an open SSH session. Yields a `.run(cmd)`
  shortcut so the body of a `with` block can drive the guest with
  one-liner commands.

Subclasses adjust:

  * `base_image_key` — switch to "debian", "almalinux", etc.
  * `_drives()` — add OVMF pflash drives for UEFI.
  * `_prepare()` — register extra disks (e.g. OVMF) before VM
    create.
  * `_post_start()` — extra work after `vm.start` (this is where
    `VmSsh` opens its session).

The wrapper exposes the underlying pycapnp `SyncVm` capability as
`.cap` (not `.vm`, which would collide with the local variable
name in `with ... as vm:` bodies).
"""
from __future__ import annotations

import re
import secrets
import shutil
import subprocess
import tempfile
from pathlib import Path
from typing import TYPE_CHECKING, Optional

import pytest

if TYPE_CHECKING:
    from .cases import IntegrationTestCase
    from .ssh import SshResult, VmShell


class Vm:
    """Context manager: create + start + cleanup one VM.

    Usage:

        with Vm(self) as vm:
            # vm.cap is the pycapnp Vm capability; QGA first-ping
            # has already landed.
            details = vm.cap.show()
            assert details.cpu_count == 2

    Subclass to customise. The relevant overrides:

        class DebianVm(Vm):
            base_image_key = "debian"

        class UefiVm(Vm):
            def _prepare(self):
                # ovmf-code / ovmf-vars must be registered with the
                # inner daemon before we attach them as pflash. The
                # node has the qcow2s available at the standard
                # edk2 path.
                for name, path in [
                    ("ovmf-code", "/usr/share/edk2/OvmfX64/OVMF_CODE_4M.qcow2"),
                    ("ovmf-vars", "/usr/share/edk2/OvmfX64/OVMF_VARS_4M.qcow2"),
                ]:
                    try:
                        self.client.disks.get(name)
                    except Exception:
                        self.client.disks.register(name, path, format="qcow2")

            def _drives(self):
                return super()._drives() + [
                    {"disk_ref": "ovmf-code", "interface": "pflash", "read_only": True},
                    {"disk_ref": "ovmf-vars", "interface": "pflash"},
                ]
    """

    # ---- overrideable defaults ---------------------------------------------

    base_image_key: str = "alpine"
    cpu_count: int = 2
    ram_mb: int = 1024
    headless: bool = True
    guest_agent: bool = True
    cloud_init: bool = False
    # When True, `__enter__` blocks until the inner daemon reports the
    # first QGA ping. False = return as soon as `vm.start` is accepted
    # (the subclass's `_post_start` is then responsible for whatever
    # readiness gate the test needs).
    wait_for_qga: bool = True
    # Default user the SSH wrapper authenticates as. Subclasses override
    # for cloud-init images whose default user differs (Ubuntu → ubuntu,
    # Debian → debian, AlmaLinux → almalinux, …).
    ssh_user: str = "corvus"

    def __init__(
        self,
        case: "IntegrationTestCase",
        *,
        name: Optional[str] = None,
    ) -> None:
        self.case = case
        self.client = case.client
        self.name: str = name or self._derive_name()
        self.cap = None
        self._overlay_created = False
        # Names of SSH keys this VM registered with the inner daemon
        # (so cleanup can drop them on __exit__).
        self._registered_ssh_keys: list[str] = []

    # ---- public API --------------------------------------------------------

    def __enter__(self) -> "Vm":
        images = self.case.register_base_images()
        base_disk = images.get(self.base_image_key)
        if base_disk is None:
            pytest.skip(
                f"{self.base_image_key!r} base image not registered — "
                f"run `make test-image-{self.base_image_key}` to build it"
            )
        self._prepare()
        # Anything past this point owns resources in the inner daemon;
        # if a step (or the subclass's `_post_start`) raises — including
        # `pytest.skip` — run cleanup before re-raising so we don't leak
        # a half-built VM + overlay.
        try:
            self.client.disks.create_overlay(self.name, base_disk)
            self._overlay_created = True
            self.cap = self.client.vms.create(
                self.name,
                cpu_count=self.cpu_count,
                ram_mb=self.ram_mb,
                headless=self.headless,
                guest_agent=self.guest_agent,
                cloud_init=self.cloud_init,
            )
            # `vms.create` produces a bare VM record; drives, network
            # interfaces, SSH keys, cloud-init, and virtiofs shared
            # directories are attached afterward via the per-resource
            # cap methods.
            for drive in self._drives():
                self.cap.attach_disk(**drive)
            for net_if in self._net_ifs():
                self.cap.add_net_if(**net_if)
            for shared in self._shared_dirs():
                self.cap.add_shared_dir(**shared)
            for key_name, public_key in self._ssh_keys_to_attach():
                self.client.ssh_keys.create(key_name, public_key)
                self._registered_ssh_keys.append(key_name)
                self.cap.attach_ssh_key(key_name)
            ci_cfg = self._cloud_init_config()
            if ci_cfg is not None:
                self.client.cloud_init.set(self.cap.show().id, **ci_cfg)
            self.cap.start(wait=self.wait_for_qga)
            self._post_start()
        except BaseException:
            self.__exit__(None, None, None)
            raise
        return self

    def __exit__(self, exc_type, exc, tb) -> None:
        # Idempotent, best-effort cleanup. Mirror of
        # `Topology.finalize` (`topology.py:111`).
        if self.cap is not None:
            # Use `reset` rather than `stop` for teardown: it's a
            # hard kill (`Handlers/Vm.hs:handleVmReset` SIGTERMs
            # qemu and synchronously sets status=stopped) and works
            # from any state — paused, stopping, error, even an
            # already-stopped VM. `stop` would hang for up to 5 min
            # on a VM that ignored ACPI shutdown (e.g. one without
            # qemu-guest-agent that we tried to stop before its
            # acpid had loaded). Tests that want to exercise the
            # graceful shutdown path call `vm.stop` themselves; the
            # __exit__ just guarantees the VM goes away.
            try:
                self.cap.reset()
            except Exception:
                pass
            try:
                self.cap.delete(delete_disks=True)
            except Exception:
                pass
            self.cap = None
        if self._overlay_created:
            # If vm.delete ran with delete_disks=True the overlay is
            # gone; if it didn't (e.g. vm.create raised before we got
            # there), the overlay is still in the inner daemon's DB.
            try:
                self.client.disks.get(self.name).delete()
            except Exception:
                pass
            self._overlay_created = False
        for key_name in self._registered_ssh_keys:
            try:
                self.client.ssh_keys.get(key_name).delete()
            except Exception:
                pass
        self._registered_ssh_keys.clear()

    # ---- subclass hooks ----------------------------------------------------

    def _prepare(self) -> None:
        """Run before the overlay disk + VM are created. Default
        no-op. Subclasses use this to register additional disks with
        the inner daemon (e.g. OVMF for UEFI)."""

    def _drives(self) -> list[dict]:
        """Return the list of `vm.attach_disk` kwarg dicts to call
        after VM create. Default: one virtio drive on the overlay.
        UEFI subclass appends pflash entries for OVMF code+vars."""
        return [{"disk_ref": self.name, "interface": "virtio"}]

    def _net_ifs(self) -> list[dict]:
        """Return the list of `vm.add_net_if` kwarg dicts to call
        after drives are attached. Default empty (VM boots with no
        NIC; SSH happens over VSOCK). Cloud-init tests override to
        add a user-mode NIC with a host-port forward."""
        return []

    def _shared_dirs(self) -> list[dict]:
        """Return the list of `vm.add_shared_dir` kwarg dicts to call
        after the drives are attached and before `vm.start`. Default
        empty. Tests that need virtiofs override this — typically with
        an inline subclass that captures node-side temp paths via
        closure. Each dict accepts `path`, `tag`, plus optional
        `cache` and `read_only`."""
        return []

    def _post_start(self) -> None:
        """Run after `vm.start`. Default no-op (QGA first-ping has
        already landed if `wait_for_qga`)."""

    def _ssh_keys_to_attach(self) -> list[tuple[str, str]]:
        """Return `(key_name, public_key)` pairs to register with the
        inner daemon and attach to this VM. Default empty. Cloud-init
        subclasses override to inject a generated keypair (or several)
        so the daemon's NoCloud datasource places them in the guest's
        authorized_keys at first boot."""
        return []

    def _cloud_init_config(self) -> Optional[dict]:
        """Return a dict of kwargs for `client.cloud_init.set()`
        (`user_data`, `network_config`, `inject_ssh_keys`), or `None`
        to skip the call. Default `None`."""
        return None

    # ---- internals ---------------------------------------------------------

    def _derive_name(self) -> str:
        cls = type(self).__name__
        # CamelCase → camel-case, drop a leading dash. The random
        # suffix keeps the inner daemon's DB inspectable when a test
        # leaks ("vm-3f1a2c") without colliding across runs.
        slug = re.sub(r"(?<!^)([A-Z])", r"-\1", cls).lower()
        return f"{slug}-{secrets.token_hex(3)}"


class VmSsh(Vm):
    """`Vm` plus an open SSH session to the guest.

    On `__enter__`, after the VM boots, calls `case.vm_shell()` and
    blocks on `wait_ready()` so sshd is answering by the time the
    `with` body runs. The shell is accessible as `vm.shell`; the
    `vm.run(cmd)` shortcut forwards to it.

        with VmSsh(self) as vm:
            r = vm.run("uname -s")
            assert r.exit_code == 0 and r.stdout.strip() == "Linux"

    Skips cleanly (and tears the VM down) when the VM has no VSOCK
    CID or the host-side SSH private key isn't present —
    `vm_shell()` raises `RuntimeError` with a clear message in
    those cases.
    """

    # How long to wait for sshd to answer after QGA first-ping. The
    # Alpine test image typically opens its VSOCK SSH relay within a
    # second or two of QGA coming up, but boot can stretch on a busy
    # nested-KVM host.
    ssh_ready_timeout_sec: float = 90.0

    def __init__(
        self,
        case: "IntegrationTestCase",
        *,
        name: Optional[str] = None,
    ) -> None:
        super().__init__(case, name=name)
        self.shell: Optional["VmShell"] = None

    def _post_start(self) -> None:
        try:
            self.shell = self.case.vm_shell(
                self.cap,
                host_key_path=self._ssh_private_key_path(),
                user=self.ssh_user,
            )
        except RuntimeError as e:
            # Missing key / no vsock_cid — surface as a skip so the
            # test doesn't fail noisily on an unsupported host.
            pytest.skip(str(e))
        self.shell.wait_ready(timeout_sec=self.ssh_ready_timeout_sec)

    def _ssh_private_key_path(self) -> Path:
        """Path to the private SSH key the wrapper authenticates with.

        Default: the harness's baked test key
        (`integration_tests/keys/corvus-test-key`). Cloud-init
        subclasses override to point at a per-VM generated key.
        """
        from .ssh import HOST_ALPINE_KEY_PATH
        return HOST_ALPINE_KEY_PATH

    def __exit__(self, exc_type, exc, tb) -> None:
        # Drop the ControlMaster before tearing down the VM so the
        # vm-side sshd session closes cleanly; the base class then
        # stops + deletes the VM.
        if self.shell is not None:
            try:
                self.shell.close()
            except Exception:
                pass
            self.shell = None
        super().__exit__(exc_type, exc, tb)

    def run(self, command: str, **kw) -> "SshResult":
        """Run a shell command in the guest over the open SSH session."""
        return self.shell.run(command, **kw)


class VmUefi(VmSsh):
    """`VmSsh` that boots in UEFI mode via OVMF pflash drives.

    Each VM gets its own per-VM overlay on the registered OVMF
    `code` and `vars` disks. The shared `ovmf-code` / `ovmf-vars`
    registrations point at system files under `/usr/share/edk2/`
    and are **never** deleted — `disks.delete` would unlink the
    underlying file. The overlays are owned by the daemon (live
    under its `basePath`) and are reaped by `delete_disks=True`
    on VM teardown.

    The same daemon-side overlay-refcount protection that's already
    in `handleDiskDelete` (returns `RespDiskHasOverlays` when a
    disk has live overlays) means even an accidental
    `disks.delete("ovmf-code")` while a UEFI VM is running fails
    loudly instead of nuking the system file.
    """

    OVMF_CODE_PATH = "/usr/share/edk2/OvmfX64/OVMF_CODE_4M.qcow2"
    OVMF_VARS_PATH = "/usr/share/edk2/OvmfX64/OVMF_VARS_4M.qcow2"

    def _prepare(self) -> None:
        # Idempotent registration of the system OVMF files. Other
        # UEFI tests in the same class reuse these — no copy, just
        # a database row pointing at the existing qcow2.
        for name, path in (
            ("ovmf-code", self.OVMF_CODE_PATH),
            ("ovmf-vars", self.OVMF_VARS_PATH),
        ):
            try:
                self.client.disks.get(name)
            except Exception:
                self.client.disks.register(name, path, format="qcow2")

        self._code_overlay = f"{self.name}-ovmf-code"
        self._vars_overlay = f"{self.name}-ovmf-vars"
        self.client.disks.create_overlay(self._code_overlay, "ovmf-code")
        self.client.disks.create_overlay(self._vars_overlay, "ovmf-vars")

    def _drives(self) -> list[dict]:
        return super()._drives() + [
            {
                "disk_ref": self._code_overlay,
                "interface": "pflash",
                "read_only": True,
            },
            {"disk_ref": self._vars_overlay, "interface": "pflash"},
        ]


class VmWindows(Vm):
    """A Windows Server VM, driven via QGA `guest_exec` (no SSH).

    The harness's baked Windows image (`windows-server-2025-eval`)
    boots UEFI, runs qemu-guest-agent + cloudbase-init, and has the
    virtio-fs Windows driver + WinFSP installed at bake time (see
    `yaml/windows-server-2025/autounattend.xml`). Tests drive the
    guest via `vm.cap.guest_exec("cmd.exe /c …")` or
    `"powershell -Command …"`; the daemon auto-wraps non-prefixed
    commands in `cmd.exe /c`.

    Subclasses MAY override `_cloud_init_config()` to supply a custom
    `#ps1_sysnative` user-data script — the daemon writes it
    verbatim into the NoCloud ISO when the user-data starts with `#`
    (see `Corvus/CloudInit.hs::isRawUserDataScript`).
    """

    base_image_key = "windows-server-2025-eval"
    cpu_count = 4
    ram_mb = 4096
    headless = False
    guest_agent = True
    cloud_init = True
    # `start(wait=True)` blocks on the daemon's QGA first-ping.
    # Windows boots much slower than Linux under nested-KVM, but the
    # daemon has its own internal timeout for that phase — we trust
    # it here. If the daemon ever needs a callable timeout knob the
    # right fix is on the daemon side, not in the harness.
    wait_for_qga = True

    OVMF_CODE_PATH = "/usr/share/edk2/OvmfX64/OVMF_CODE_4M.qcow2"
    OVMF_VARS_PATH = "/usr/share/edk2/OvmfX64/OVMF_VARS_4M.qcow2"

    def _prepare(self) -> None:
        # Idempotent registration of the system OVMF files, mirroring
        # `VmUefi._prepare`. Kept inline rather than via inheritance
        # because `VmUefi` extends `VmSsh`, which would drag in the
        # SSH machinery we don't want for a QGA-only Windows VM.
        for name, path in (
            ("ovmf-code", self.OVMF_CODE_PATH),
            ("ovmf-vars", self.OVMF_VARS_PATH),
        ):
            try:
                self.client.disks.get(name)
            except Exception:
                self.client.disks.register(name, path, format="qcow2")

        self._code_overlay = f"{self.name}-ovmf-code"
        self._vars_overlay = f"{self.name}-ovmf-vars"
        self.client.disks.create_overlay(self._code_overlay, "ovmf-code")
        self.client.disks.create_overlay(self._vars_overlay, "ovmf-vars")

    def _drives(self) -> list[dict]:
        return super()._drives() + [
            {
                "disk_ref": self._code_overlay,
                "interface": "pflash",
                "read_only": True,
            },
            {"disk_ref": self._vars_overlay, "interface": "pflash"},
        ]


class VmCloudInit(VmSsh):
    """A VM whose SSH access is established only via cloud-init key
    injection (no baked authorized_keys).

    Reaches the guest over QEMU user-mode networking with a
    host-port forward — `-netdev user,hostfwd=tcp:127.0.0.1:<port>-:22`
    — rather than via AF_VSOCK, because upstream cloud images don't
    ship a VSOCK SSH listener and we don't want to bolt one on per
    distro. The node-side port forward is reachable from the host's
    second SSH leg (after the host→node VSOCK hop) as
    `socat - TCP:127.0.0.1:<port>`.

    Generates an ed25519 keypair in a per-VM tempdir, registers it
    with the inner daemon, attaches it to the VM, and calls
    `client.cloud_init.set(vm_id, inject_ssh_keys=True)` before
    `vm.start`. The daemon's default NoCloud user-data creates a
    `corvus` user with `sudo: ALL=(ALL) NOPASSWD:ALL` and seeds it
    with the attached SSH keys (see
    `Corvus/CloudInit.hs::defaultCloudInitConfig`), so all
    distro tests SSH in as `corvus`.

    Subclasses MUST set:

      * `base_image_key` — which disk in `register_base_images()` to
        overlay (e.g. `"alpine-3-21-base"`, `"gentoo-base-headless"`).

    Subclasses MAY override:

      * `_cloud_init_config()` — to supply custom `user_data` /
        `network_config` (replaces the daemon's default user-data).
      * `_ssh_keys_to_attach()` — to attach more than one keypair (see
        `test_multiple_ssh_keys`).
      * `ssh_user` — when the custom user-data creates a non-default
        user.
    """

    cloud_init = True
    # Daemon's `defaultCloudInitConfig.ciUser` is "corvus".
    ssh_user: str = "corvus"
    # Upstream cloud images vary wildly in whether qemu-guest-agent
    # is pre-installed and enabled (Debian/Ubuntu: yes; Alpine: no;
    # FreeBSD: no). Don't ask the daemon to gate on QGA — fall back
    # to "VM is ready when sshd answers", which works across all
    # distros uniformly.
    guest_agent: bool = False
    wait_for_qga: bool = False
    # Cloud-init at first boot is much slower than just booting an
    # already-baked image: it has to install packages, restart sshd,
    # etc. 5 min covers the slowest distros (FreeBSD).
    ssh_ready_timeout_sec: float = 300.0

    def __init__(
        self,
        case: "IntegrationTestCase",
        *,
        name: Optional[str] = None,
    ) -> None:
        super().__init__(case, name=name)
        self._key_dir = Path(tempfile.mkdtemp(prefix="corvus-ci-key-"))
        self._priv_key_path = self._key_dir / "id_ed25519"
        self._pub_key_path = self._key_dir / "id_ed25519.pub"
        self._generate_keypair()
        # Public-key text (single line, with comment) that the daemon
        # records and that the guest's ~/.ssh/authorized_keys should
        # end up containing.
        self.public_key = self._pub_key_path.read_text().strip()
        # Extra (name, pubkey) pairs the subclass wants attached in
        # addition to the primary key. `_ssh_keys_to_attach` returns
        # the primary key + these.
        self.extra_keys: list[tuple[str, str]] = []
        # Host-side port on the node for the SSH hostfwd. Random in a
        # high range so concurrent (or leaked, prior-run) hostfwds
        # don't collide.
        self._tcp_port = 30000 + secrets.randbelow(20000)

    def _generate_keypair(self) -> None:
        subprocess.run(
            [
                "ssh-keygen", "-t", "ed25519",
                "-f", str(self._priv_key_path),
                "-N", "",
                "-C", f"corvus-ci-{self.name}",
            ],
            check=True,
            stdout=subprocess.DEVNULL,
            stderr=subprocess.PIPE,
        )
        self._priv_key_path.chmod(0o600)

    def _ssh_private_key_path(self) -> Path:
        return self._priv_key_path

    def _primary_key_name(self) -> str:
        # Daemon-side name must be unique per active key; the VM name
        # already is, so reuse it as a stable per-VM identifier.
        return f"{self.name}-ed25519"

    def _ssh_keys_to_attach(self) -> list[tuple[str, str]]:
        return [(self._primary_key_name(), self.public_key)] + self.extra_keys

    def _net_ifs(self) -> list[dict]:
        # User-mode (SLIRP) network with a hostfwd that maps a unique
        # node-side TCP port to the guest's sshd. The daemon's
        # `Qemu/Command.hs::netdevArgs` accepts SLIRP options via
        # `hostDevice`, so `hostfwd=tcp:127.0.0.1:<port>-:22`
        # becomes `-netdev user,id=…,hostfwd=tcp:127.0.0.1:<port>-:22`.
        return [{
            "type": "user",
            "host_device": f"hostfwd=tcp:127.0.0.1:{self._tcp_port}-:22",
        }]

    def _cloud_init_config(self) -> dict:
        return {"inject_ssh_keys": True}

    def _post_start(self) -> None:
        try:
            self.shell = self.case.vm_shell(
                self.cap,
                host_key_path=self._ssh_private_key_path(),
                user=self.ssh_user,
                vm_tcp_port=self._tcp_port,
            )
        except RuntimeError as e:
            pytest.skip(str(e))
        self.shell.wait_ready(timeout_sec=self.ssh_ready_timeout_sec)

    def __exit__(self, exc_type, exc, tb) -> None:
        try:
            super().__exit__(exc_type, exc, tb)
        finally:
            shutil.rmtree(self._key_dir, ignore_errors=True)
