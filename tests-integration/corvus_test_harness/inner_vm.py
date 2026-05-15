"""Context managers that wrap the inner-VM lifecycle for tests.

A test method that needs a booted inner VM otherwise has to chain
~25 lines of `register_base_images` lookup → `disks.create_overlay`
→ `vms.create` → `vm.attach_disk` (the `drives=` kwarg on
`vms.create` has a pycapnp list-element copy quirk that leaves
QEMU without a `-drive`) → `vm.start(wait=True)` → use the VM →
`vm.stop` → `vm.delete(delete_disks=True)` → belt-and-suspenders
disk cleanup. Multiplied across many tests that's a lot of
boilerplate.

Two classes live here:

* `InnerVm` — the lifecycle. Defaults to "Alpine, BIOS, headless,
  qemu-guest-agent, block on QGA first ping" and tears the VM down
  on exit. Knows nothing about SSH.
* `InnerVmSsh` — `InnerVm` plus an open SSH session. Yields a
  `.run(cmd)` shortcut so the body of a `with` block can drive the
  guest with one-liner commands.

Subclasses adjust:

  * `base_image_key` — switch to "debian", "almalinux", etc.
  * `_drives()` — add OVMF pflash drives for UEFI.
  * `_prepare()` — register extra disks (e.g. OVMF) before VM
    create.
  * `_post_start()` — extra work after `vm.start` (this is where
    `InnerVmSsh` opens its session).
"""
from __future__ import annotations

import re
import secrets
from typing import TYPE_CHECKING, Optional

import pytest

if TYPE_CHECKING:
    from .cases import IntegrationTestCase
    from .ssh import InnerGuestShell, SshResult


class InnerVm:
    """Context manager: create + start + cleanup one inner VM.

    Usage:

        with InnerVm(self) as inner:
            # inner.vm is the pycapnp Vm cap; QGA first-ping has
            # already landed.
            details = inner.vm.show()
            assert details.cpu_count == 2

    Subclass to customise. The relevant overrides:

        class DebianInnerVm(InnerVm):
            base_image_key = "debian"

        class UefiInnerVm(InnerVm):
            def _prepare(self):
                # ovmf-code / ovmf-vars must be registered with the
                # inner daemon before we attach them as pflash. The
                # outer Gentoo VM has the qcow2s available at the
                # standard edk2 path.
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
    # When True, `__enter__` blocks until the inner daemon reports the
    # first QGA ping. False = return as soon as `vm.start` is accepted
    # (the subclass's `_post_start` is then responsible for whatever
    # readiness gate the test needs).
    wait_for_qga: bool = True

    def __init__(
        self,
        case: "IntegrationTestCase",
        *,
        name: Optional[str] = None,
    ) -> None:
        self.case = case
        self.client = case.client
        self.name: str = name or self._derive_name()
        self.vm = None
        self._overlay_created = False

    # ---- public API --------------------------------------------------------

    def __enter__(self) -> "InnerVm":
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
            self.vm = self.client.vms.create(
                self.name,
                cpu_count=self.cpu_count,
                ram_mb=self.ram_mb,
                headless=self.headless,
                guest_agent=self.guest_agent,
            )
            # Attach drives after create. `vms.create(drives=…)` exists
            # in the schema but a pycapnp list-element copy quirk in
            # `_build_drive_attach`
            # (`python/corvus_client/_async/vm.py:25`) leaves QEMU with
            # no `-drive`; `attach_disk` is the path the existing python
            # test suite covers.
            for drive in self._drives():
                self.vm.attach_disk(**drive)
            self.vm.start(wait=self.wait_for_qga)
            self._post_start()
        except BaseException:
            self.__exit__(None, None, None)
            raise
        return self

    def __exit__(self, exc_type, exc, tb) -> None:
        # Idempotent, best-effort cleanup. Mirror of
        # `Topology.finalize` (`topology.py:111`).
        if self.vm is not None:
            try:
                self.vm.stop(wait=True)
            except Exception:
                pass
            try:
                self.vm.delete(delete_disks=True)
            except Exception:
                pass
            self.vm = None
        if self._overlay_created:
            # If vm.delete ran with delete_disks=True the overlay is
            # gone; if it didn't (e.g. vm.create raised before we got
            # there), the overlay is still in the inner daemon's DB.
            try:
                self.client.disks.get(self.name).delete()
            except Exception:
                pass
            self._overlay_created = False

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

    def _post_start(self) -> None:
        """Run after `vm.start`. Default no-op (QGA first-ping has
        already landed if `wait_for_qga`)."""

    # ---- internals ---------------------------------------------------------

    def _derive_name(self) -> str:
        cls = type(self).__name__
        # CamelCase → camel-case, drop a leading dash. The random
        # suffix keeps the inner daemon's DB inspectable when a test
        # leaks ("inner-vm-3f1a2c") without colliding across runs.
        slug = re.sub(r"(?<!^)([A-Z])", r"-\1", cls).lower()
        return f"{slug}-{secrets.token_hex(3)}"


class InnerVmSsh(InnerVm):
    """`InnerVm` plus an open SSH session to the guest.

    On `__enter__`, after the VM boots, calls `case.inner_ssh()` and
    blocks on `wait_ready()` so sshd is answering by the time the
    `with` body runs. The shell is accessible as `inner.shell`; the
    `inner.run(cmd)` shortcut forwards to it.

        with InnerVmSsh(self) as inner:
            r = inner.run("uname -s")
            assert r.exit_code == 0 and r.stdout.strip() == "Linux"

    Skips cleanly (and tears the VM down) when the inner VM has no
    VSOCK CID or the host-side SSH private key isn't present —
    `inner_ssh()` raises `RuntimeError` with a clear message in
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
        self.shell: Optional["InnerGuestShell"] = None

    def _post_start(self) -> None:
        try:
            self.shell = self.case.inner_ssh(self.vm)
        except RuntimeError as e:
            # Missing key / no vsock_cid — surface as a skip so the
            # test doesn't fail noisily on an unsupported host.
            pytest.skip(str(e))
        self.shell.wait_ready(timeout_sec=self.ssh_ready_timeout_sec)

    def __exit__(self, exc_type, exc, tb) -> None:
        # Drop the ControlMaster before tearing down the VM so the
        # inner sshd session closes cleanly; the base class then
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
