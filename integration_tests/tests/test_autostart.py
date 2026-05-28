"""Integration tests for VM + network autostart.

Autostart used to run inside the daemon-startup task, which raced
the per-node supervisor's first dial: by the time the autostart
loop asked the node agent to allocate a vsock CID, the registry
was empty and the call failed with `"node N not registered with
daemon"`. The fix moves autostart into the supervisor's per-node
`onConnect` callbacks (one for nodeagent, one for netd), gated by
a single-shot flag so an unrelated agent flap doesn't re-fire the
autostart on a VM the operator may have stopped post-startup.

These tests assert that:

1. A stopped VM with `autostart=True` is started after a daemon
   restart, end-to-end.
2. A saved VM with `autostart=True` is resumed after a daemon
   restart, and the guest's RAM survives the round trip.
3. The single-shot flag holds: stopping an autostart VM manually
   and restarting only the nodeagent (not the daemon) leaves the
   VM stopped.
4. The same trio for networks: cold autostart works, and stopping
   then bouncing only netd does not re-fire.

See `doc/vm-management.md` (Save/Resume section) for the operator-
level picture and `src/Corvus/NodeSupervisor.hs` for the hook
points (`autostartVmsOnNode` / `autostartNetworksOnNode`).
"""

from __future__ import annotations

import secrets
import shlex
import time

from corvus_test_harness import SingleNodeCase, Vm


def _uniq(stem: str) -> str:
    return f"{stem}-{secrets.token_hex(3)}"


def _qemu_count(node, vm_name: str) -> int:
    """Count qemu-system processes whose argv mentions `vm_name`.

    Mirrors the helper in test_vm_migration.py / test_vm_save_load.py;
    the `^qemu-system` anchor keeps the shell that wraps this pgrep
    call from matching its own argv.
    """
    r = node.run(
        f"pgrep -af '^qemu-system.*-name {vm_name},' || true",
        check=False,
        timeout_sec=10.0,
    )
    out = r.stdout.decode("utf-8", errors="replace").strip()
    return sum(1 for line in out.splitlines() if line.strip())


def _poll_until(cond, *, timeout_sec: float, msg: str, poll_sec: float = 0.5) -> None:
    deadline = time.monotonic() + timeout_sec
    while time.monotonic() < deadline:
        if cond():
            return
        time.sleep(poll_sec)
    raise AssertionError(f"{msg} (waited {timeout_sec}s)")


def _restart_unit(node, unit: str) -> None:
    """`systemctl restart` a unit and wait for it to come back active.
    Mirrors the pattern in test_component_restart.py; kept inline so
    this file is self-contained and doesn't import from another test
    module.
    """
    node.run(f"sudo systemctl restart {unit}", check=True)
    # systemctl returns once the unit has been re-queued, not once
    # ExecStart succeeded. Settle briefly, then poll for active.
    time.sleep(0.5)
    deadline = time.monotonic() + 15.0
    while time.monotonic() < deadline:
        r = node.run(f"systemctl is-active {unit}", check=False)
        if r.stdout.decode().strip() == "active":
            return
        time.sleep(0.5)
    raise AssertionError(f"{unit} did not return to active within 15 s")


def _drop_cached_client(node) -> None:
    """The harness lazily memoises one pycapnp Client per TestNode.
    After a daemon restart the old TCP socket is stale; drop the
    cache so the next `self.client` access re-dials."""
    if node._client is not None:
        try:
            node._client.close()
        except Exception:
            pass
        node._client = None


class TestVmAutostart(SingleNodeCase):
    """VM autostart fires per-node when the nodeagent connects, not
    at daemon startup. The tests exercise the cold-stopped case
    (fast: no QGA, non-bootable), the resume-from-saved case (real
    Alpine with QGA, round-tripping a RAM sentinel), and the
    no-refire-on-agent-flap invariant."""

    def _create_stopped_autostart_vm(self):
        """Build a non-bootable, no-QGA VM marked autostart=True on the
        single node. Returns `(vm_cap, vm_name, disk_name)`. The
        daemon will happily transition this through
        ``stopped → running`` because we asked for `guest_agent=False`
        (no first-ping wait); the QEMU process spawns + sits idle.
        Caller cleans up in a `finally` block."""
        vm_name = _uniq("auto-vm")
        disk_name = _uniq("auto-disk")
        self.client.disks.create(disk_name, size_mb=16, format="qcow2")
        vm = self.client.vms.create(
            vm_name,
            cpu_count=1,
            ram_mb=128,
            headless=True,
            guest_agent=False,
            cloud_init=False,
            autostart=True,
        )
        vm.attach_disk(disk_name, interface="virtio")
        return vm, vm_name, disk_name

    def _cleanup_silent(self, vm_name: str, disk_name: str) -> None:
        try:
            self.client.vms.get(vm_name).reset()
        except Exception:
            pass
        try:
            self.client.vms.get(vm_name).delete()
        except Exception:
            pass
        try:
            self.client.disks.get(disk_name).delete()
        except Exception:
            pass

    def test_vm_autostart_starts_stopped_vm_on_daemon_boot(self):
        """A stopped VM with autostart=True is started after a daemon
        restart. Pre-fix: the startup-time autostart loop raced the
        supervisor's first nodeagent dial and the VM landed in
        VmError. Post-fix: the supervisor's onConnect callback runs
        autostart after ssAgents is populated, so vsock allocation
        succeeds and the VM goes Running."""
        vm, vm_name, disk_name = self._create_stopped_autostart_vm()
        try:
            # Bring the VM to a known stopped state so the daemon
            # restart cleanly triggers the autostart pass — the
            # autostart hook fires on supervisor (re)spawn, not on
            # every reconnect, so the row's current state at the
            # moment of restart is what matters.
            _poll_until(
                lambda: vm.show().status == "stopped",
                timeout_sec=5.0,
                msg="VM did not settle at stopped before daemon restart",
            )

            _restart_unit(self.node, "corvus")
            _drop_cached_client(self.node)

            # 30 s budget: daemon comes up, runs persistent migrate
            # + startup task, the supervisor spawns and dials the
            # local nodeagent, autostart fires on the dial-success
            # callback, VmStart picks up the row. With guest_agent
            # off, "running" is committed the moment QEMU spawns.
            client = self.client
            _poll_until(
                lambda: client.vms.get(vm_name).show().status == "running",
                timeout_sec=30.0,
                msg=f"VM {vm_name!r} did not autostart after daemon restart",
            )
            _poll_until(
                lambda: _qemu_count(self.node, vm_name) == 1,
                timeout_sec=10.0,
                msg=f"qemu for {vm_name!r} not running after autostart",
            )
        finally:
            self._cleanup_silent(vm_name, disk_name)

    def test_vm_autostart_resumes_saved_vm(self):
        """A saved VM with autostart=True resumes after a daemon
        restart, and the guest's RAM (a sentinel in /tmp) survives.
        Uses the full Alpine + QGA fixture because we need the
        guest to be able to read /tmp/sentinel back out post-resume.

        This is the exact case the previously-skipped
        TestVmSaveAutostartResumes was guarding against — it's now
        the load-bearing test for the supervisor-onConnect
        autostart hook."""
        with Vm(self) as vm:
            # `Vm` defaults to autostart=False; we need to stop +
            # edit + start to flip the flag.
            vm.cap.reset()
            _poll_until(
                lambda: vm.cap.show().status == "stopped",
                timeout_sec=15.0,
                msg="reset did not land at stopped before autostart edit",
            )
            vm.cap.edit(autostart=True)
            vm.cap.start(wait=True)
            _poll_until(
                lambda: vm.cap.show().status == "running",
                timeout_sec=60.0,
                msg="VM did not return to running after autostart edit",
            )

            sentinel = f"corvus-autostart-{int(time.monotonic_ns())}"
            r = vm.cap.guest_exec(
                f"/bin/sh -c {shlex.quote(f'echo {sentinel} > /tmp/sentinel')}"
            )
            assert r.exit_code == 0, r

            vm.cap.save()
            _poll_until(
                lambda: vm.cap.show().status == "saved",
                timeout_sec=10.0,
                msg="vm.show().status did not become 'saved'",
            )

            _restart_unit(self.node, "corvus")
            _drop_cached_client(self.node)

            # 90 s budget: cold-boot autostart for a saved VM has
            # to (a) wait for the daemon to come back up, (b) dial
            # the nodeagent, (c) spawn QEMU with -incoming, (d)
            # stream the state file in, (e) QMP cont, (f) wait for
            # QGA. Slow on first boot of the test image; tight on
            # cached.
            client = self.client
            # The Vm context manager's `self.cap` AND `self.client`
            # were captured before the restart and now point at a
            # closed pycapnp runloop. Refresh both so subsequent
            # operations (and the __exit__ cleanup — reset, delete,
            # and the ephemeral-overlay sweep via `self.client`)
            # don't dangle as un-awaited coroutines
            # ("translate_async.<locals>.wrapped was never awaited"
            # warnings).
            vm.client = client
            vm.cap = client.vms.get(vm.name)
            _poll_until(
                lambda: vm.cap.show().status == "running",
                timeout_sec=90.0,
                msg=f"VM {vm.name!r} did not resume after daemon restart",
            )
            _poll_until(
                lambda: _qemu_count(self.node, vm.name) == 1,
                timeout_sec=10.0,
                msg=f"qemu for {vm.name!r} not running after autostart resume",
            )

            # Sentinel survived the autostart-driven save/load
            # round trip — proves the RAM image was actually
            # restored (not just a cold boot wearing a `saved` →
            # `running` label).
            r = vm.cap.guest_exec("/bin/cat /tmp/sentinel")
            assert r.exit_code == 0, r
            assert sentinel in r.stdout, (
                f"sentinel {sentinel!r} did not survive autostart resume; "
                f"guest stdout={r.stdout!r}"
            )

    def test_vm_autostart_does_not_refire_on_agent_reconnect(self):
        """The single-shot flag ('claimAutostartSlot' on NodeConns)
        ensures autostart fires once per supervisor lifetime, not
        on every nodeagent reconnect. Without the gate, an operator
        who stops an autostart VM by hand would see it come back
        the moment the nodeagent flaps.

        Sequence: create autostart=True VM, let it autostart,
        manually stop it, restart corvus-nodeagent (NOT corvus),
        confirm the VM stays stopped."""
        vm, vm_name, disk_name = self._create_stopped_autostart_vm()
        try:
            # Wait for the initial autostart-on-create (the VM was
            # created with autostart=True on a node whose
            # supervisor is already connected; per-node autostart
            # only fires on FRESH agent connect — i.e. on this
            # path it doesn't fire and the row stays stopped. So
            # explicitly start it once to put the VM through the
            # "running" state the operator-stop test needs).
            _poll_until(
                lambda: vm.show().status == "stopped",
                timeout_sec=10.0,
                msg="VM did not settle at stopped before manual start",
            )
            vm.start(wait=False)
            _poll_until(
                lambda: vm.show().status == "running",
                timeout_sec=15.0,
                msg="VM did not start before agent flap",
            )

            # Operator stops it. From here on, no autostart fire
            # should put it back up — including the nodeagent
            # bounce below. Use reset (hard-kill) rather than stop:
            # this VM has no QGA / OS, so ACPI shutdown would hang
            # to the default timeout. Reset commits stopped
            # synchronously.
            vm.reset()
            _poll_until(
                lambda: vm.show().status == "stopped",
                timeout_sec=15.0,
                msg="VM did not reach stopped after manual reset",
            )

            # Bounce the nodeagent only. The daemon stays up; the
            # supervisor reconnects on its existing NodeConns
            # entry; 'claimAutostartSlot' returns False (already
            # fired); autostartVmsOnNode is skipped.
            #
            # Capture last_node_agent_push_at before the bounce so
            # we can wait for it to advance — that's the cheapest
            # signal that the reconnect completed and the daemon's
            # status subscription was re-attached.
            before = self.client.nodes.get(self.node.short_name).show()
            before_push = before.last_node_agent_push_at

            _restart_unit(self.node, "corvus-nodeagent")

            _poll_until(
                lambda: (
                    self.client.nodes.get(self.node.short_name)
                    .show()
                    .last_node_agent_push_at
                    not in (None, before_push)
                ),
                timeout_sec=30.0,
                msg="nodeagent did not push fresh stats after restart",
            )

            # Generous window for any (incorrect) re-autostart to
            # fire — VmStart records a task in <1 s on a healthy
            # supervisor; 5 s is plenty.
            time.sleep(5.0)
            assert self.client.vms.get(vm_name).show().status == "stopped", (
                "autostart re-fired on a nodeagent reconnect; the single-shot "
                "flag is not holding"
            )
        finally:
            self._cleanup_silent(vm_name, disk_name)


class TestNetworkAutostart(SingleNodeCase):
    """Network autostart fires per-node when netd connects, not at
    daemon startup. Mirrors the VM tests but on the netd hook."""

    def _create_autostart_network(self) -> str:
        """Create a managed network marked autostart=True on the
        single node, return its name. Caller cleans up in
        ``finally``. The 10.255.0.0/24 subnet avoids collision with
        any nested-test-VM networks."""
        name = _uniq("auto-net")
        # subnet kept disjoint from harness-default networks.
        self.client.networks.create(
            name,
            subnet="10.255.0.0/24",
            dhcp=False,
            nat=False,
            autostart=True,
        )
        return name

    def _cleanup_silent_network(self, name: str) -> None:
        try:
            self.client.networks.get(name).stop(force=True)
        except Exception:
            pass
        try:
            self.client.networks.get(name).delete()
        except Exception:
            pass

    def test_network_autostart_starts_on_daemon_boot(self):
        """A network with autostart=True is started after a daemon
        restart. Pre-fix: the startup autostart loop fired before
        netd was registered. Post-fix: the netd onConnect callback
        runs autostart, so 'network start' reaches a live agent."""
        name = self._create_autostart_network()
        try:
            # Stop it so the daemon restart's autostart pass has
            # something to do. (.create() doesn't auto-start; the
            # autostart flag only fires on supervisor (re)spawn,
            # which a daemon restart triggers.)
            try:
                self.client.networks.get(name).stop(force=True)
            except Exception:
                pass
            _poll_until(
                lambda: self.client.networks.get(name).show().running is False,
                timeout_sec=10.0,
                msg="network did not settle at running=False before daemon restart",
            )

            _restart_unit(self.node, "corvus")
            _drop_cached_client(self.node)

            client = self.client
            _poll_until(
                lambda: client.networks.get(name).show().running is True,
                timeout_sec=30.0,
                msg=f"network {name!r} did not autostart after daemon restart",
            )
        finally:
            self._cleanup_silent_network(name)

    def test_network_autostart_does_not_refire_on_netd_reconnect(self):
        """Symmetric single-shot guard for networks: bouncing just
        corvus-netd (daemon stays up) must not re-fire autostart on
        a network the operator has stopped manually."""
        name = self._create_autostart_network()
        try:
            # Start it (autostart on create-only isn't a behaviour
            # we promise — the flag fires on supervisor (re)spawn).
            self.client.networks.get(name).start()
            _poll_until(
                lambda: self.client.networks.get(name).show().running is True,
                timeout_sec=15.0,
                msg=f"network {name!r} did not reach running",
            )

            self.client.networks.get(name).stop(force=True)
            _poll_until(
                lambda: self.client.networks.get(name).show().running is False,
                timeout_sec=15.0,
                msg=f"network {name!r} did not reach stopped after manual stop",
            )

            _restart_unit(self.node, "corvus-netd")

            # `netd_connected` flips False → True as the netd
            # supervisor's `withNetAgentClient` (re)establishes
            # the cap and re-enters `ncNetAgent = Just …` in
            # ssAgents. Polling it is the cheapest "netd is back"
            # signal — `last_net_agent_push_at` would be cleaner
            # but the daemon doesn't currently populate it (only
            # the nodeagent push path writes its equivalent).
            _poll_until(
                lambda: (
                    self.client.nodes.get(self.node.short_name).show().netd_connected
                ),
                timeout_sec=30.0,
                msg="netd did not reconnect after restart",
            )

            time.sleep(5.0)
            assert self.client.networks.get(name).show().running is False, (
                "network autostart re-fired on a netd reconnect; the "
                "single-shot flag is not holding"
            )
        finally:
            self._cleanup_silent_network(name)
