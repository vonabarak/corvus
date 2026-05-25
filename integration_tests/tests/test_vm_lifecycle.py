"""End-to-end smoke test + VM lifecycle scenarios against the INNER daemon.

The tests in this file are grouped into several sibling `SingleNodeCase`
subclasses so xdist (`--dist=worksteal` in `pyproject.toml`) can spread
them across workers. Within a single worker each class's methods run
sequentially on one inner-daemon node, but classes run in parallel and
under worksteal a busy class's pending tests can be redistributed to
idle workers (at the cost of re-running the class fixture there).

Every method creates and deletes its own VMs under unique names, so
classes are mutually independent. If the class-scoped fixture itself
fails (node boot, cert deploy, first client open), the conftest hooks
in `integration_tests/conftest.py` skip every method in that class
with a coherent reason — no per-class daemon-reachability canary is
needed on top.
"""

from __future__ import annotations

import time

import pytest
from corvus_client import VmNotFound
from corvus_test_harness import (
    HOST_ALPINE_KEY_PATH,
    SingleNodeCase,
    Vm,
    VmSsh,
    VmUefi,
    probe_spice_link,
)


def _mem_total_kb(shell) -> int:
    """Parse /proc/meminfo's MemTotal in kB via the vm's SSH shell."""
    out = shell.run("awk '/^MemTotal:/{print $2}' /proc/meminfo").stdout
    return int(out.strip())


def _drain_serial_until(stream, needle: bytes, *, timeout: float) -> bytes:
    """Read from a `serial_console()` stream until `needle` appears.

    Mirrors `test_serial_console.py::_drain_until`. Kept inline
    here rather than promoted to a shared helper — one duplicate
    is cheaper than wiring a new harness symbol for a single
    consumer.
    """
    deadline = time.monotonic() + timeout
    buf = bytearray()
    while time.monotonic() < deadline:
        chunk = stream.read(timeout=1.0)
        if chunk is None:
            break  # EOF
        if chunk:
            buf.extend(chunk)
            if needle in buf:
                return bytes(buf)
    raise AssertionError(
        f"timed out waiting for {needle!r} after {timeout}s; tail={bytes(buf[-512:])!r}"
    )


class _VmLifecycleBase(SingleNodeCase):
    """Shared helpers for the VM-lifecycle test classes.

    Not collected by pytest (the leading underscore keeps it out of
    the `Test*` collection rule); each concrete subclass below
    inherits the wait/observe/pgrep helpers.
    """

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
            f"VM stuck at {last!r}, expected {target!r} (waited {timeout_sec}s)"
        )

    def _observe_transitions(
        self,
        vm,
        target: str,
        *,
        timeout_sec: float = 90.0,
        poll_sec: float = 0.1,
    ) -> list[str]:
        """Hot-poll vm.show().status and return the ordered list of
        DISTINCT statuses observed before reaching `target`.

        Caller is expected to have just issued an async transition
        (start/stop). Polling cadence is 100 ms so transient
        intermediate states (`starting`, `stopping`) are seen
        reliably even when the underlying RPC takes only a few
        hundred milliseconds."""
        observed: list[str] = []
        deadline = time.monotonic() + timeout_sec
        while time.monotonic() < deadline:
            s = vm.show().status
            if not observed or observed[-1] != s:
                observed.append(s)
            if s == target:
                return observed
            time.sleep(poll_sec)
        raise TimeoutError(
            f"VM never reached {target!r} within {timeout_sec}s; observed={observed!r}"
        )

    def _read_qemu_pid(self, vm_id: int) -> int | None:
        """Return the QEMU pid on the node whose argv contains
        ``corvus-vm-<vm_id>``, or ``None`` if no such process exists.

        Corvus launches QEMU with ``-name <name>,process=corvus-vm-<id>``
        (see ``src/Corvus/Node/Command.hs``), so the per-VM id is
        always embedded in the command line and makes the process
        directly grep-able.
        """
        r = self.nodes[0].run(
            f"pgrep -f 'corvus-vm-{vm_id}' || true",
            check=False,
            timeout_sec=10.0,
        )
        out = r.stdout.decode("utf-8", errors="replace").strip()
        if not out:
            return None
        # pgrep prints one pid per line; the first match is enough.
        # The shell that wraps this command runs `pgrep` directly via
        # exec, so it does not appear in the result set.
        return int(out.splitlines()[0].strip())


class TestVmSmokeAndCrud(_VmLifecycleBase):
    """Diagnostics + VM CRUD without booting QEMU.

    These finish in seconds; bundling them in one class amortizes
    the class-fixture cost (one nested-KVM node boot) across all
    eight cheap tests.
    """

    def test_inner_daemon_reachable(self):
        """Smoke test: the inner daemon answers `status()` after first boot.

        Implicitly verifies every layer of the harness:
          - outer Corvus created the VM from our template
          - virtiofs mounted the host stack-install dir at /opt/corvus/bin
          - systemd's corvus.service ExecStarted the inner daemon
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
        # Fresh node, no vms yet on first call. Subsequent methods may
        # leave their own vms; the class fixture is per-CLASS, not
        # per-method, so the inner DB is shared. Every method below
        # creates + deletes its own under a unique name to avoid
        # interference.
        assert isinstance(vms, list)

    def test_create_vm(self):
        """The inner daemon can create + list its own VMs.

        Sanity check that the inner Corvus's database (node-side
        Postgres) is fully wired up and the daemon's mutation path
        works. We do not `start` the vm here — that would exercise
        nested KVM, which has its own test module.
        """
        vm = self.client.vms.create(
            "doubly-nested",
            cpu_count=1,
            ram_mb=128,
            headless=True,
        )
        details = vm.show()
        assert details.name == "doubly-nested"
        assert details.cpu_count == 1
        assert details.ram_mb == 128
        vm.delete()
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


class TestVmBootBasics(_VmLifecycleBase):
    """Basic guest lifecycle: one boot per method."""

    def test_vm(self):
        """Full vm lifecycle with SSH-driven assertions.

        The default `VmSsh` boots a **headless, BIOS** Alpine
        VM (the `headless=True` default and no OVMF drives attached
        — see `VmUefi` for the UEFI variant and the `_GfxOn`
        subclass in `test_headless_swap_cycle` for the non-headless
        variant). `VmSsh` handles the create/attach/start/wait-for-sshd
        sequence and the matching teardown; the body just runs
        commands and checks the guest is the image we expect.

        A user-mode NIC is attached so the QGA-poller has a NIC
        to report guest IPs against — VSOCK alone doesn't surface
        in `guest-network-get-interfaces` output.
        """

        class _VmSshWithUserNic(VmSsh):
            def _net_ifs(self):
                return [{"type": "user"}]

        with _VmSshWithUserNic(self) as vm:
            r = vm.run("uname -s")
            assert r.exit_code == 0
            assert r.stdout.strip() == "Linux"

            r = vm.run("cat /etc/os-release")
            assert "Alpine Linux" in r.stdout

            r = vm.run("hostname")
            assert "corvus-test-vm" in r.stdout

            # Multiplex sanity: many commands should reuse the
            # ControlMaster connection and complete quickly.
            for _ in range(5):
                assert vm.run("true").exit_code == 0

            # QGA exec mirrors the SSH path — same command, different
            # transport. Catches regressions where one path works
            # and the other doesn't.
            r = vm.cap.guest_exec("uname -s")
            assert r.exit_code == 0
            assert r.stdout.strip() == "Linux"

            # BIOS guest has no EFI variables; efibootmgr exits
            # non-zero with a "no EFI" diagnostic. The `; true`
            # forces a zero exit so the default check=True doesn't
            # raise; what we care about is no Boot#### entries.
            r = vm.run("efibootmgr -v 2>&1; true")
            assert "Boot0" not in r.stdout

            # Headless guest gets `-vga none -display none`; the
            # kernel sees no display device.
            r = vm.run("lshw -class display")
            assert r.exit_code == 0
            assert r.stdout.strip() == ""

            # QGA healthcheck: `last_healthcheck` is set by the
            # poller after the first ping (the same ping that
            # flips status `starting` → `running`), and refreshed
            # every `qcHealthcheckInterval` seconds. We poll until
            # the timestamp advances; if it never does, the
            # steady-state poller never came up.
            hc0 = vm.cap.show().last_healthcheck
            assert hc0 is not None, (
                "last_healthcheck is None after wait_for_qga — "
                "first-ping path didn't update the DB"
            )
            deadline = time.monotonic() + 30.0
            hc1 = hc0
            while time.monotonic() < deadline:
                time.sleep(2.0)
                hc1 = vm.cap.show().last_healthcheck
                if hc1 is not None and hc1 > hc0:
                    break
            assert hc1 > hc0, (
                f"last_healthcheck didn't advance within 30s; "
                f"before={hc0!r}, after={hc1!r} — steady-state "
                f"poller isn't running"
            )

            # Guest-IP advertisement: the poller's secondary job.
            # The user NIC eth0 is set up by Alpine's DHCP client at
            # boot; the poller calls `guest-network-get-interfaces`
            # and propagates the CIDR address back to the daemon
            # within a couple of healthcheck cycles.
            deadline = time.monotonic() + 30.0
            guest_ips = None
            while time.monotonic() < deadline:
                for nif in vm.cap.list_net_ifs():
                    if nif.guest_ip_addresses and "/" in nif.guest_ip_addresses:
                        guest_ips = nif.guest_ip_addresses
                        break
                if guest_ips:
                    break
                time.sleep(2.0)
            assert guest_ips, (
                "QGA poller never reported guest_ip_addresses within "
                "30s — guest-network-get-interfaces path didn't reach "
                "the daemon"
            )

    def test_status_transitions_through_starting_and_stopping(self):
        """The DB-observable VM status must pass through `starting`
        before reaching `running`, and through `stopping` before
        reaching `stopped`.

        The stop path has always done this (the daemon sets
        ``VmStopping`` before dispatching ``vmStopGraceful`` to the
        agent and waits for it to complete). The start path was
        silently going ``stopped`` → ``running`` because
        ``launchVmViaAgent`` only ever set ``VmRunning`` after the
        agent returned — there was no symmetric pre-call
        ``VmStarting`` write. Users couldn't tell "spawning + first
        boot" apart from "fully up".

        Method:
          1. Create a VM with guestAgent enabled so the agent's
             ``vmStart`` blocks for the first QGA ping. The
             ``starting`` window is then comfortably long for the
             100ms poll cadence to catch.
          2. Issue ``start(wait=False)`` and hot-poll the status.
          3. Assert the observed sequence is exactly
             ``stopped → starting → running``.
          4. Issue ``stop(wait=False)`` and assert
             ``running → stopping → stopped``.
        """

        class _AsyncQga(Vm):
            wait_for_qga = False  # uses vm.start(wait=False)

        with _AsyncQga(self) as vm:
            # `Vm.__enter__` may have already run start() — wait
            # until we're back at a clean `stopped` for a known
            # baseline, then drive the transition ourselves.
            self._wait_status(vm.cap, "running", timeout_sec=90)
            vm.cap.stop(wait=True)
            self._wait_status(vm.cap, "stopped", timeout_sec=60)

            # --- start: stopped → starting → running ----------------
            vm.cap.start(wait=False)
            start_seq = self._observe_transitions(vm.cap, "running", timeout_sec=90)
            assert "starting" in start_seq, (
                "VM status never passed through 'starting' on async start. "
                f"Observed sequence: {start_seq!r}. "
                "The daemon should set VmStarting before invoking the "
                "agent's vmStart so external observers can tell a "
                "still-booting VM apart from a steady-state one."
            )
            # Order check: starting must come before running.
            assert start_seq.index("starting") < start_seq.index("running"), (
                f"'starting' should appear before 'running'. Got: {start_seq!r}"
            )

            # --- stop: running → stopping → stopped -----------------
            vm.cap.stop(wait=False)
            stop_seq = self._observe_transitions(vm.cap, "stopped", timeout_sec=60)
            assert "stopping" in stop_seq, (
                f"VM status never passed through 'stopping'. Got: {stop_seq!r}"
            )
            assert stop_seq.index("stopping") < stop_seq.index("stopped"), (
                f"'stopping' should appear before 'stopped'. Got: {stop_seq!r}"
            )

    def test_start_async_without_guest_agent(self):
        """Without QGA the daemon transitions stopped → running
        immediately after qemu spawn — no `starting` step.

        Only the start half is verified explicitly. A graceful
        `vm.stop()` on a no-QGA VM falls back to ACPI
        `system_powerdown`, which the guest can only honour once
        userspace is up (`acpid` loaded, power-button handler
        registered) — racy if we send it within seconds of boot.
        Cleanup goes through `Vm.__exit__`'s `vm.reset()`
        which is a hard kill and always works.
        """

        class _AsyncStartNoQga(Vm):
            guest_agent = False
            wait_for_qga = False  # would be meaningless anyway

        with _AsyncStartNoQga(self) as vm:
            self._wait_status(vm.cap, "running", timeout_sec=30)

    def test_uefi_vm_lists_efi_boot_entries(self):
        """UEFI-booted Alpine guest exposes EFI variables and at
        least one BootXXXX entry."""
        with VmUefi(self) as vm:
            r = vm.run("efibootmgr -v")
            assert r.exit_code == 0
            assert "Boot" in r.stdout


class TestVmEditWhileRunning(_VmLifecycleBase):
    """Edits that require a stop / edit / start cycle — two boots
    per method, so this class lands its own worker."""

    def test_headless_swap_cycle(self):
        """Headless ↔ non-headless edit cycle on the same VM.

        Non-headless half: VM gets `-vga …`, the kernel sees a
        graphics adapter (`lshw -class display`), and QEMU exposes
        a SPICE TCP listener on the node's @127.0.0.1@. We probe
        both: lshw inside the guest, and a SPICE link handshake
        against the listener from inside the node.

        Headless half: stop, edit `headless: true`, restart, and
        re-verify — no display adapter visible to the guest, no
        SPICE port allocated by the daemon, and the serial console
        buffer comes up (only headless VMs get a serial chardev —
        see `Handlers/Vm.hs:386-389`).
        """

        class _GfxOn(VmSsh):
            headless = False

        with _GfxOn(self) as vm:
            r = vm.run("lshw -class display")
            assert r.exit_code == 0
            # Match any of qemu's standard adapters (std/qxl/virtio).
            out = r.stdout.lower()
            assert any(k in out for k in ("vga", "qxl", "virtio gpu", "display")), (
                f"no display adapter visible in lshw output: {r.stdout!r}"
            )

            # SPICE liveness: a non-headless VM must have a
            # spice_port allocated, and qemu must be speaking the
            # SPICE link protocol on it. The probe runs from inside
            # the node via SSH (qemu binds 127.0.0.1 of the node,
            # unreachable from the host) and parses the 16-byte
            # SpiceLinkHeader the server returns.
            details = vm.cap.show()
            assert details.spice_port is not None and details.spice_port > 0, (
                f"non-headless VM has no SPICE port: {details!r}"
            )
            info = probe_spice_link(
                node_cid=self.nodes[0].cid,
                spice_port=details.spice_port,
                host_key_path=HOST_ALPINE_KEY_PATH,
            )
            # SPICE_MAGIC + version are mirrored from
            # spice-protocol's spice/protocol.h.
            assert info.magic == b"REDQ", info
            assert info.major == 2, info

            # --- swap to headless ---------------------------------
            vm.cap.stop(wait=True)
            vm.cap.edit(headless=True)
            vm.cap.start(wait=True)

            with self.vm_shell(vm.cap) as shell:
                shell.wait_ready(timeout_sec=90)
                r = shell.run("lshw -class display")
                assert r.exit_code == 0
                assert r.stdout.strip() == "", (
                    f"display adapter visible after headless swap: {r.stdout!r}"
                )
            details = vm.cap.show()
            assert details.spice_port is None, (
                f"headless VM should have no spice_port; got {details!r}"
            )
            # Serial console buffer is wired up only for headless
            # VMs; reach login: through the daemon's ring buffer.
            with vm.cap.serial_console() as stream:
                data = _drain_serial_until(stream, b"login:", timeout=60.0)
                assert b"login:" in data

    def test_cpu_and_ram_edit_round_trip(self):
        """Boot, read nproc + MemTotal, stop, edit cpu_count+ram_mb,
        boot again, re-read and confirm the values changed.

        Uses plain `Vm` (not `VmSsh`) because
        `VmSsh`'s single ControlMaster can't span a
        stop/start cycle; the test opens two `vm_shell` sessions
        manually, one per boot.
        """

        class _Sized(Vm):
            cpu_count = 2
            ram_mb = 1024

        with _Sized(self) as vm:
            with self.vm_shell(vm.cap) as shell:
                shell.wait_ready(timeout_sec=90)
                assert int(shell.run("nproc").stdout.strip()) == 2
                mem_kb = _mem_total_kb(shell)
                # MemTotal is consistently a bit below the configured
                # `-m` flag (kernel reserved + integer rounding).
                assert mem_kb >= 0.85 * 1024 * 1024

            vm.cap.stop(wait=True)
            vm.cap.edit(cpu_count=4, ram_mb=2048)
            vm.cap.start(wait=True)

            with self.vm_shell(vm.cap) as shell:
                shell.wait_ready(timeout_sec=90)
                assert int(shell.run("nproc").stdout.strip()) == 4
                mem_kb = _mem_total_kb(shell)
                assert mem_kb >= 0.85 * 2 * 1024 * 1024


class TestVmPauseResetPowerOff(_VmLifecycleBase):
    """Pause / resume / reset / guest-initiated poweroff scenarios."""

    def test_pause_resume_reset_stop(self):
        """Combined exercise of all four VM lifecycle actions:
        `vm.pause()`, resume via `vm.start()`, `vm.reset()`, and
        graceful `vm.stop(wait=True)`.

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
          * `vm.stop(wait=True)` is the GRACEFUL path — QMP
            `system_powerdown` sends ACPI to the guest, which
            Alpine's acpid handles by initiating a clean
            shutdown. Distinct from `vm.reset()`'s SIGKILL'd
            QEMU.
        """
        with VmSsh(self) as vm:
            with self.vm_shell(vm.cap) as shell:
                shell.wait_ready(timeout_sec=90)
                boot_id_1 = shell.run(
                    "cat /proc/sys/kernel/random/boot_id"
                ).stdout.strip()
                assert boot_id_1, "empty boot_id on first read"

            # --- pause / resume -----------------------------------
            # The pause / resume RPCs return when the daemon has
            # issued the QMP 'stop' / 'cont' command, but the
            # status column in the daemon's DB is bumped by the
            # background monitor thread (it processes the QMP
            # event-channel reply). Under load that bump can lag
            # the RPC return by a few ms. Poll with '_wait_status'
            # rather than assert the status immediately — the
            # original tight assert flaked on busy hosts.
            vm.cap.pause()
            self._wait_status(vm.cap, "paused", timeout_sec=10)

            vm.cap.start()
            self._wait_status(vm.cap, "running", timeout_sec=30)

            # --- reset (hard) -------------------------------------
            vm.cap.reset()
            self._wait_status(vm.cap, "stopped", timeout_sec=30)

            # Start fresh; the new boot_id must differ from the
            # one we captured before the reset.
            vm.cap.start()
            with self.vm_shell(vm.cap) as shell:
                shell.wait_ready(timeout_sec=90)
                boot_id_2 = shell.run(
                    "cat /proc/sys/kernel/random/boot_id"
                ).stdout.strip()
                assert boot_id_2 and boot_id_2 != boot_id_1, (
                    f"boot_id unchanged across reset+restart: "
                    f"before={boot_id_1!r}, after={boot_id_2!r}"
                )

            # --- graceful ACPI shutdown ---------------------------
            # `vm.stop(wait=True)` asks QEMU to send ACPI
            # power-down via QMP and blocks until the guest powers
            # itself off. Test image bakes both QGA and acpid so
            # this is reliable; on an image without acpid this
            # would hang until the daemon's 5-min timeout.
            #
            # The 'wait=True' RPC returns once QEMU has exited;
            # the DB's 'status' column is bumped by the monitor
            # thread reaping the exit, which can lag the RPC by
            # a few ms under load. Poll with '_wait_status'
            # rather than assert immediately.
            vm.cap.stop(wait=True)
            self._wait_status(vm.cap, "stopped", timeout_sec=10)

    def test_start_after_guest_initiated_poweroff(self):
        """``vm.start()`` works after the guest powers itself off.

        Regression for: the agent's @handleVmStart@ returned the
        existing ledger entry without checking @vlsLastExitCode@.
        After ``sudo poweroff`` inside the guest, the reaper
        recorded exit code 0 but the ledger row persisted; the
        next ``vm.start()`` matched that row and short-circuited
        to "already running" with the dead pid, never spawning a
        new QEMU. Daemon-side this surfaced as a task with
        result=success and message="State: stopped".

        Triggers via SSH so the guest powers off itself (no QMP
        path involved); then expects ``vm.start()`` to land a
        boot_id that differs from the pre-poweroff one.
        """

        with VmSsh(self) as vm:
            with self.vm_shell(vm.cap) as shell:
                shell.wait_ready(timeout_sec=90)
                boot_id_1 = shell.run(
                    "cat /proc/sys/kernel/random/boot_id"
                ).stdout.strip()
                assert boot_id_1

            # Trigger the guest's @poweroff@ via QGA's
            # @guest-exec@: equivalent to a logged-in user
            # running ``sudo poweroff``, but doesn't depend on
            # SSH staying up across the kernel halt. The agent
            # still routes through QGA (no QMP system_powerdown
            # involved) so this exercises the exact code path
            # the bug report came from — the agent records the
            # exit code but the ledger entry persists because
            # no stop RPC was issued.
            try:
                vm.cap.guest_exec("/sbin/poweroff -f")
            except Exception:
                # QGA's connection drops mid-exec when the
                # kernel halts. The bug is downstream of this
                # call; whether QGA returned cleanly is
                # immaterial.
                pass

            # Wait for the guest to finish powering off and the
            # daemon's monitor to reap the exit. 90s covers a
            # full Alpine shutdown sequence under nested KVM
            # plus one status-poller tick.
            self._wait_status(vm.cap, "stopped", timeout_sec=90)

            # The crux: start must spawn a fresh QEMU. Before
            # the fix the agent's @handleVmStart@ saw the still-
            # present ledger entry (the reaper wrote
            # @vlsLastExitCode = Just 0@ but never evicted the
            # row) and returned success with the dead pid. The
            # daemon then attached a monitor that immediately
            # observed @VmAgentStopped@ and slid the DB right
            # back to stopped — task message
            # @"State: stopped"@, result success, no new QEMU.
            vm.cap.start()
            self._wait_status(vm.cap, "running", timeout_sec=120)

            with self.vm_shell(vm.cap) as shell:
                shell.wait_ready(timeout_sec=120)
                boot_id_2 = shell.run(
                    "cat /proc/sys/kernel/random/boot_id"
                ).stdout.strip()
                assert boot_id_2 and boot_id_2 != boot_id_1, (
                    f"boot_id unchanged across guest poweroff + start: "
                    f"before={boot_id_1!r}, after={boot_id_2!r} — "
                    f"the agent likely returned the stale ledger entry "
                    f"and no new QEMU was spawned"
                )


class TestVmRebootQuirk(_VmLifecycleBase):
    """The four ``reboot_quirk`` scenarios share the
    ``_RebootQuirkVm`` / ``_NoQuirkVm`` mixin pattern and the
    QEMU-pid scrape helper, so they live together."""

    def test_reboot_quirk_restart_on_guest_reboot(self):
        """With ``reboot_quirk=True``, a guest-initiated reboot
        bounces QEMU transparently.

        Tianocore OVMF firmware hangs on second boot in some
        configurations (tianocore/edk2#12441). The quirk works
        around it by running QEMU with ``-no-reboot`` and having
        the agent re-spawn QEMU after each guest-initiated
        exit. The guest sees a normal reboot; firmware sees a
        cold boot.

        Verifies:

        * After ``reboot -f`` inside the guest, the daemon's
          status row stays at ``running`` (agent re-spawned in
          place; no transient ``stopped``).
        * The guest's ``boot_id`` differs after the bounce —
          proving an actual QEMU process change, not just a
          ledger-level idempotence.
        """

        class _RebootQuirkVm(VmSsh):
            reboot_quirk = True

        with _RebootQuirkVm(self) as vm:
            boot_id_1 = vm.run("cat /proc/sys/kernel/random/boot_id").stdout.strip()
            assert boot_id_1

            # Trigger a clean guest reboot via SSH. ``reboot -f``
            # bypasses init; the kernel halts and QEMU exits
            # (because we're running with @-no-reboot@). SSH
            # rather than ``vm.cap.guest_exec`` because the
            # daemon's QGA poll loop blocks on a dead chardev
            # for tens of seconds per retry when the guest
            # vanishes mid-exec.
            try:
                vm.run("doas /sbin/reboot -f", check=False, timeout_sec=5)
            except Exception:
                pass

            # Sample status periodically: the row must NEVER
            # show 'stopped' (the auto-restart should keep it
            # at 'running' throughout). A single 'stopped'
            # sighting would mean the agent's monitor thread
            # tripped before the re-spawn finished.
            saw_stopped = False
            deadline = time.monotonic() + 120.0
            settled_running = False
            while time.monotonic() < deadline:
                status = vm.cap.show().status
                if status == "stopped":
                    saw_stopped = True
                    break
                if status == "running":
                    # Try to ssh in; if it answers, the new QEMU
                    # is up and the guest agent finished its
                    # first ping. Bail out of the polling loop.
                    try:
                        with self.vm_shell(vm.cap) as shell:
                            shell.wait_ready(timeout_sec=5)
                            boot_id_2 = shell.run(
                                "cat /proc/sys/kernel/random/boot_id"
                            ).stdout.strip()
                        if boot_id_2 and boot_id_2 != boot_id_1:
                            settled_running = True
                            break
                    except Exception:
                        # ssh down for a few moments during boot
                        # is normal; keep polling.
                        pass
                time.sleep(1.0)

            assert not saw_stopped, (
                "VM transitioned to 'stopped' during a quirk-on "
                "reboot — the agent failed to re-spawn in place"
            )
            assert settled_running, (
                "VM did not finish rebooting back to 'running' "
                "within 120s, or boot_id did not change"
            )

    def test_reboot_quirk_does_not_restart_on_daemon_stop(self):
        """With ``reboot_quirk=True``, ``vm.stop()`` from the
        daemon still actually stops the VM. The auto-restart is
        suppressed when the agent observes a stop intent.

        Without this guarantee, the only way to power-off a
        quirk-enabled VM would be ``vm.reset()`` (SIGKILL +
        explicit ledger eviction) — surprising semantics. The
        agent's ``vlsStopRequested`` flag exists exactly to
        gate the reaper's auto-restart on this case.
        """

        class _RebootQuirkVm(VmSsh):
            reboot_quirk = True

        with _RebootQuirkVm(self) as vm:
            vm.cap.stop(wait=True)
            self._wait_status(vm.cap, "stopped", timeout_sec=30)

            # Hold for a few more seconds and assert it didn't
            # bounce back to running. If the quirk-suppression
            # is broken the agent would re-spawn QEMU here.
            for _ in range(10):
                time.sleep(1.0)
                status = vm.cap.show().status
                assert status == "stopped", (
                    f"VM bounced back to {status!r} after a daemon-"
                    f"initiated stop — quirk-suppression failed"
                )

    def test_reboot_quirk_qemu_pid_changes_on_reboot(self):
        """With ``reboot_quirk=True`` a guest-initiated reboot must
        replace the underlying QEMU process — the agent's reaper
        ``-no-reboot`` + re-spawn path is the whole point of the
        feature, and its only externally visible artefact is a new
        OS-level pid.

        Tests for ``status`` continuity and ``boot_id`` change
        already exist (see ``test_reboot_quirk_restart_on_guest_reboot``);
        this one nails down the lower-level claim that we really did
        kill and re-launch the qemu-system binary, not just bounce
        the guest in place.
        """

        class _RebootQuirkVm(VmSsh):
            reboot_quirk = True

        with _RebootQuirkVm(self) as vm:
            vm_id = vm.cap.show().id
            pid_before = self._read_qemu_pid(vm_id)
            assert pid_before is not None, (
                f"no QEMU process found for vm id {vm_id} before reboot"
            )

            # Trigger guest reboot via SSH. QEMU exits because we
            # launched it with ``-no-reboot``; the agent's reaper
            # re-spawns a fresh process under a new pid. SSH
            # rather than ``guest_exec``: the daemon's QGA poller
            # blocks on a silent chardev for tens of seconds per
            # retry when the guest vanishes mid-exec.
            try:
                vm.run("doas /sbin/reboot -f", check=False, timeout_sec=5)
            except Exception:
                pass

            deadline = time.monotonic() + 120.0
            pid_after: int | None = None
            while time.monotonic() < deadline:
                p = self._read_qemu_pid(vm_id)
                if p is not None and p != pid_before:
                    pid_after = p
                    break
                time.sleep(1.0)

            assert pid_after is not None, (
                f"QEMU pid for vm id {vm_id} did not change within 120s "
                f"after a guest-initiated reboot (still {pid_before}) — "
                f"the reboot-quirk reaper did not re-spawn"
            )

    def test_no_reboot_quirk_qemu_pid_unchanged_on_reboot(self):
        """Without ``reboot_quirk``, a guest-initiated reboot keeps
        the same QEMU process — QEMU resets the machine in place,
        so the host-level pid is preserved.

        Together with the quirk-on test, this pins down both halves
        of the contract: the flag exclusively controls whether we go
        through QEMU's in-process reset or the agent's exit-and-respawn
        loop.
        """

        class _NoQuirkVm(VmSsh):
            reboot_quirk = False

        with _NoQuirkVm(self) as vm:
            boot_id_1 = vm.run("cat /proc/sys/kernel/random/boot_id").stdout.strip()

            vm_id = vm.cap.show().id
            pid_before = self._read_qemu_pid(vm_id)
            assert pid_before is not None, (
                f"no QEMU process found for vm id {vm_id} before reboot"
            )

            # SSH rather than ``guest_exec``: the daemon's QGA
            # poller blocks on a silent chardev for tens of seconds
            # per retry when the guest vanishes mid-exec.
            try:
                vm.run("doas /sbin/reboot -f", check=False, timeout_sec=5)
            except Exception:
                pass

            # Wait for the guest to actually have rebooted (boot_id
            # changes), so we don't just sample before the reboot
            # took effect. Without this, a flat pid would prove
            # nothing.
            deadline = time.monotonic() + 120.0
            rebooted = False
            while time.monotonic() < deadline:
                try:
                    with self.vm_shell(vm.cap) as shell:
                        shell.wait_ready(timeout_sec=5)
                        boot_id_2 = shell.run(
                            "cat /proc/sys/kernel/random/boot_id"
                        ).stdout.strip()
                    if boot_id_2 and boot_id_2 != boot_id_1:
                        rebooted = True
                        break
                except Exception:
                    # ssh down for a few seconds during boot is
                    # expected — keep polling.
                    pass
                time.sleep(2.0)

            assert rebooted, (
                "guest did not finish rebooting within 120s — "
                "cannot validate pid stability"
            )

            pid_after = self._read_qemu_pid(vm_id)
            assert pid_after == pid_before, (
                f"QEMU pid for vm id {vm_id} changed across an "
                f"in-place reboot: before={pid_before}, after={pid_after}. "
                f"Without reboot_quirk the same qemu-system process should "
                f"survive the guest reset"
            )
