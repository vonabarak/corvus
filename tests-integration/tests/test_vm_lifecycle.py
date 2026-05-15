"""End-to-end smoke test + VM lifecycle scenarios against the INNER daemon.

Each method in this class runs against the same Corvus daemon executing
inside a single nested VM (built from the freshly compiled binary on
the host). The outer Corvus only orchestrates the test VM; what we're
really testing is the inner daemon — which runs as root, has nested-KVM
access, and can exercise code paths the outer (unprivileged) deployment
cannot.

`test_inner_daemon_reachable` is the canary: if it passes, the whole
virtiofs + VSOCK transport chain is wired up correctly and the rest of
the methods in this class are meaningful. If it fails, the suite-wide
skip-on-first-failure machinery skips the rest of the class for a
single coherent reason instead of producing nine spurious failures.
"""
from __future__ import annotations

import time

import pytest

from corvus_client import VmNotFound
from corvus_test_harness import InnerVm, InnerVmSsh, InnerVmUefi, SingleVmCase


pytestmark = pytest.mark.slow


def _mem_total_kb(shell) -> int:
    """Parse /proc/meminfo's MemTotal in kB via the inner SSH shell."""
    out = shell.run("awk '/^MemTotal:/{print $2}' /proc/meminfo").stdout
    return int(out.strip())


class TestVmLifecycle(SingleVmCase):
    """All methods share one inner daemon. Each creates + deletes its
    own inner-side VMs under unique names, so the methods are mutually
    independent despite sharing the outer VM."""

    def _wait_status(
        self,
        vm,
        target: str,
        *,
        timeout_sec: float = 60.0,
        poll_sec: float = 0.5,
    ) -> None:
        """Poll vm.show().status until it matches `target` or the
        timeout elapses. Used by the async-start/stop and pause
        tests to gate on daemon-observable state transitions."""
        deadline = time.monotonic() + timeout_sec
        last = None
        while time.monotonic() < deadline:
            last = vm.show().status
            if last == target:
                return
            time.sleep(poll_sec)
        raise TimeoutError(
            f"VM stuck at {last!r}, expected {target!r} "
            f"(waited {timeout_sec}s)"
        )

    def test_inner_daemon_reachable(self):
        """Smoke test: the inner daemon answers `status()` after first boot.

        Implicitly verifies every layer of the harness:
          - outer Corvus created the VM from our template
          - virtiofs mounted the host stack-install dir at /opt/corvus/bin
          - systemd's corvus-test.service ExecStarted the inner daemon
          - corvus-tcp-relay.service forwarded VSOCK ↔ TCP
          - the host's socat relay bridged the host TCP socket
          - pycapnp completed the bootstrap handshake against the inner
        """
        info = self.client.status()
        assert info.version, "inner daemon returned an empty version string"
        assert info.uptime_seconds >= 0
        assert info.protocol_version > 0

    def test_two_status_calls(self):
        """Two `status()` calls in a row.

        Pinpoint test: if the first works and the second aborts (or
        raises), the bug is "any second RPC call on the Client breaks".
        If both work, any later abort is specific to cap-returning
        calls (e.g. `vms.create`).
        """
        info1 = self.client.status()
        info2 = self.client.status()
        assert info1.version == info2.version
        assert info2.uptime_seconds >= info1.uptime_seconds

    def test_vms_list(self):
        """`vms.list()` exercises two cap calls but neither passes a
        struct param nor returns a capability:

          - `daemon.vms()` — returns the VmManager cap (no params).
          - `mgr.list()` — returns a List(VmInfo).

        If this works, the abort in `vms.create` is specifically about
        one of: (a) passing a struct param via kwargs, (b) the server
        returning a *new* capability (Vm). If it aborts too, the issue
        is with capability traversal itself.
        """
        vms = self.client.vms.list()
        # Fresh VM, no inner VMs yet on first call. Subsequent methods
        # may leave their own VMs; the class fixture is per-CLASS, not
        # per-method, so the inner DB is shared. Every method below
        # creates + deletes its own under a unique name to avoid
        # interference.
        assert isinstance(vms, list)

    def test_create_inner_vm(self):
        """The inner daemon can create + list its own VMs.

        Sanity check that the inner Corvus's database (in-VM Postgres) is
        fully wired up and the daemon's mutation path works. We do not
        `start` the inner-of-inner VM here — that would exercise nested
        KVM, which has its own test module.
        """
        inner_vm = self.client.vms.create(
            "doubly-nested",
            cpu_count=1,
            ram_mb=128,
            headless=True,
        )
        details = inner_vm.show()
        assert details.name == "doubly-nested"
        assert details.cpu_count == 1
        assert details.ram_mb == 128
        inner_vm.delete()
        with pytest.raises(VmNotFound):
            self.client.vms.get("doubly-nested")

    def test_edit_noop(self):
        """vm.edit() with no fields set.

        Sends an empty VmEditParams (all `hasX` flags False). Pinpoints
        whether `edit` aborts at the cap-method-with-struct-params level
        regardless of payload content, or only when there's actual data.
        """
        vm = self.client.vms.create(
            "edit-target-noop",
            cpu_count=1,
            ram_mb=256,
            headless=True,
        )
        vm.edit()
        details = vm.show()
        # Nothing changed.
        assert details.cpu_count == 1
        assert details.ram_mb == 256
        vm.delete()

    def test_edit_after_show(self):
        """vm.show() then vm.edit() — diagnostic: does a no-op cap call
        'warm up' the cap before a struct-parameter call against it?
        """
        vm = self.client.vms.create(
            "edit-after-show",
            cpu_count=1,
            ram_mb=256,
            headless=True,
        )
        _ = vm.show()
        vm.edit(ram_mb=512)
        details = vm.show()
        assert details.ram_mb == 512
        vm.delete()

    def test_edit_via_get(self):
        """Create VM, then `get` a fresh Vm cap by name, then edit on it.

        Diagnostic: maybe the abort is specific to the cap returned by
        `vms.create`. Importing the same VM via `vms.get(name)` yields a
        different cap; if edit works on that, the bug is in
        create's-returned-cap path.
        """
        created = self.client.vms.create(
            "edit-via-get",
            cpu_count=1,
            ram_mb=256,
            headless=True,
        )
        name = created.show().name  # use created cap once
        fresh = self.client.vms.get(name)
        fresh.edit(ram_mb=512)
        assert fresh.show().ram_mb == 512
        fresh.delete()

    def test_edit_persists(self):
        """Edits via the inner daemon round-trip through its Postgres."""
        vm = self.client.vms.create(
            "edit-target",
            cpu_count=1,
            ram_mb=256,
            headless=True,
        )
        vm.edit(cpu_count=2, ram_mb=512, description="from python integration tests")
        details = vm.show()
        assert details.cpu_count == 2
        assert details.ram_mb == 512
        assert details.description == "from python integration tests"
        vm.delete()

    def test_vm(self):
        """Full inner-VM lifecycle with SSH-driven assertions.

        The default `InnerVmSsh` boots a **headless, BIOS** Alpine
        VM (the `headless=True` default and no OVMF drives attached
        — see `InnerVmUefi` for the UEFI variant and the
        `_GfxOn` subclass in `test_non_headless_vm_has_display_adapter`
        for the non-headless variant). `InnerVmSsh` handles the
        create/attach/start/wait-for-sshd sequence and the matching
        teardown; the body just runs commands and checks the guest
        is the image we expect.
        """
        with InnerVmSsh(self) as inner:
            r = inner.run("uname -s")
            assert r.exit_code == 0
            assert r.stdout.strip() == "Linux"

            r = inner.run("cat /etc/os-release")
            assert "Alpine Linux" in r.stdout

            r = inner.run("hostname")
            assert "corvus-test" in r.stdout

            # Multiplex sanity: many commands should reuse the
            # ControlMaster connection and complete quickly.
            for _ in range(5):
                assert inner.run("true").exit_code == 0

            # QGA exec mirrors the SSH path — same command, different
            # transport. Catches regressions where one path works
            # and the other doesn't.
            r = inner.vm.guest_exec("uname -s")
            assert r.exit_code == 0
            assert r.stdout.strip() == "Linux"

            # BIOS guest has no EFI variables; efibootmgr exits
            # non-zero with a "no EFI" diagnostic. The `; true`
            # forces a zero exit so the default check=True doesn't
            # raise; what we care about is no Boot#### entries.
            r = inner.run("efibootmgr -v 2>&1; true")
            assert "Boot0" not in r.stdout

            # Headless guest gets `-vga none -display none`; the
            # kernel sees no display device.
            r = inner.run("lshw -class display")
            assert r.exit_code == 0
            assert r.stdout.strip() == ""

    def test_start_async_with_guest_agent(self):
        """`vm.start(wait=False)` returns at status=`starting`; the
        VM flips to `running` once the daemon catches the first
        QGA ping. Mirror for stop."""

        class _AsyncStart(InnerVm):
            wait_for_qga = False  # uses vm.start(wait=False)

        with _AsyncStart(self) as inner:
            self._wait_status(inner.vm, "running", timeout_sec=90)
            inner.vm.stop(wait=False)
            self._wait_status(inner.vm, "stopped", timeout_sec=60)

    def test_start_async_without_guest_agent(self):
        """Without QGA the daemon transitions stopped → running
        immediately after qemu spawn — no `starting` step.

        Only the start half is verified explicitly. A graceful
        `vm.stop()` on a no-QGA VM falls back to ACPI
        `system_powerdown`, which the guest can only honour once
        userspace is up (`acpid` loaded, power-button handler
        registered) — racy if we send it within seconds of boot.
        Cleanup goes through `InnerVm.__exit__`'s `vm.reset()`
        which is a hard kill and always works.
        """

        class _AsyncStartNoQga(InnerVm):
            guest_agent = False
            wait_for_qga = False  # would be meaningless anyway

        with _AsyncStartNoQga(self) as inner:
            self._wait_status(inner.vm, "running", timeout_sec=30)

    def test_uefi_vm_lists_efi_boot_entries(self):
        """UEFI-booted Alpine guest exposes EFI variables and at
        least one BootXXXX entry."""
        with InnerVmUefi(self) as inner:
            r = inner.run("efibootmgr -v")
            assert r.exit_code == 0
            assert "Boot" in r.stdout

    def test_non_headless_vm_has_display_adapter(self):
        """Non-headless VMs get `-vga …` and the kernel sees a
        graphics adapter."""

        class _GfxOn(InnerVmSsh):
            headless = False

        with _GfxOn(self) as inner:
            r = inner.run("lshw -class display")
            assert r.exit_code == 0
            # Match any of qemu's standard adapters (std/qxl/virtio).
            out = r.stdout.lower()
            assert any(
                k in out for k in ("vga", "qxl", "virtio gpu", "display")
            ), f"no display adapter visible in lshw output: {r.stdout!r}"

    def test_cpu_and_ram_edit_round_trip(self):
        """Boot, read nproc + MemTotal, stop, edit cpu_count+ram_mb,
        boot again, re-read and confirm the values changed.

        Uses plain `InnerVm` (not `InnerVmSsh`) because
        `InnerVmSsh`'s single ControlMaster can't span a
        stop/start cycle; the test opens two `inner_ssh` sessions
        manually, one per boot.
        """

        class _Sized(InnerVm):
            cpu_count = 2
            ram_mb = 1024

        with _Sized(self) as inner:
            with self.inner_ssh(inner.vm) as shell:
                shell.wait_ready(timeout_sec=90)
                assert int(shell.run("nproc").stdout.strip()) == 2
                mem_kb = _mem_total_kb(shell)
                # MemTotal is consistently a bit below the configured
                # `-m` flag (kernel reserved + integer rounding).
                assert mem_kb >= 0.85 * 1024 * 1024

            inner.vm.stop(wait=True)
            inner.vm.edit(cpu_count=4, ram_mb=2048)
            inner.vm.start(wait=True)

            with self.inner_ssh(inner.vm) as shell:
                shell.wait_ready(timeout_sec=90)
                assert int(shell.run("nproc").stdout.strip()) == 4
                mem_kb = _mem_total_kb(shell)
                assert mem_kb >= 0.85 * 2 * 1024 * 1024

    def test_pause_resume_and_reset(self):
        """Combined exercise of `vm.pause()`, resume via
        `vm.start()`, and `vm.reset()`.

        Daemon semantics worth knowing:

          * `vm.pause()` calls QMP `stop` and flips status to
            `paused` synchronously.
          * `vm.start()` on a paused VM calls
            `resumeFromPaused` — QMP `cont` + DB
            `setVmStatus VmRunning`. We assert the post-resume
            status flipped back to `running`.
          * `vm.reset()` is a HARD reset — it kills QEMU and sets
            status to `stopped` (not a guest-level reboot). To
            verify the VM was actually torn down and rebuilt, we
            start it again and check the kernel's
            `/proc/sys/kernel/random/boot_id` (regenerated on
            every boot) differs from the pre-reset value.
        """
        with InnerVmSsh(self) as inner:
            with self.inner_ssh(inner.vm) as shell:
                shell.wait_ready(timeout_sec=90)
                boot_id_1 = shell.run(
                    "cat /proc/sys/kernel/random/boot_id"
                ).stdout.strip()
                assert boot_id_1, "empty boot_id on first read"

            # --- pause / resume -----------------------------------
            inner.vm.pause()
            assert inner.vm.show().status == "paused"

            inner.vm.start()
            assert inner.vm.show().status == "running"

            # --- reset (hard) -------------------------------------
            inner.vm.reset()
            self._wait_status(inner.vm, "stopped", timeout_sec=30)

            # Start fresh; the new boot_id must differ from the
            # one we captured before the reset.
            inner.vm.start()
            with self.inner_ssh(inner.vm) as shell:
                shell.wait_ready(timeout_sec=90)
                boot_id_2 = shell.run(
                    "cat /proc/sys/kernel/random/boot_id"
                ).stdout.strip()
                assert boot_id_2 and boot_id_2 != boot_id_1, (
                    f"boot_id unchanged across reset+restart: "
                    f"before={boot_id_1!r}, after={boot_id_2!r}"
                )
