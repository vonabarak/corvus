"""End-to-end coverage for ``crv vm exec`` and the underlying
``vm.guest_exec`` RPC.

[doc/guest-exec.md](../../doc/guest-exec.md) describes guest-exec as
the single most-used probe in the rest of the suite, but no test
asserts the contract directly: exit-code propagation, ``--output
json`` shape, and the rejection paths when the VM is stopped or has
no guest agent.

What's covered:

* Success-vs-failure CLI exit code: ``crv vm exec`` returns 0
  when the guest exits 0, 1 when the guest exits non-zero. The
  full guest exit code rides on the ``--output json`` payload
  (see [src/Corvus/Client/Commands/GuestExec.hs:38] —
  ``pure (exitcode == 0)``).
* ``--output json`` returns ``{"status": "ok", "exitcode", "stdout",
  "stderr"}`` (see [src/Corvus/Client/Commands/GuestExec.hs:28-34]
  for the keys).
* ``vm.guest_exec`` on a stopped VM is refused with the daemon's
  guest-exec-specific message ``"VM is <status>; VM must be
  running for guest-exec"`` (see
  [src/Corvus/Rpc/Vm.hs:310-311] — distinct shape from the
  canonical ``invalid transition from <s>: <r>`` format, so
  surfaces as :class:`ServerError`, not
  :class:`InvalidTransition`).
* ``vm.guest_exec`` on a running VM with ``guest_agent=False`` is
  refused with ``RespGuestAgentNotEnabled``
  (:class:`GuestAgentNotEnabled`).

What's NOT covered here:

* Long-running command + Ctrl-C cancellation. Cancel sends a fresh
  task-cancel RPC; the daemon's QGA path doesn't yet honour it
  (the handler at ``src/Corvus/Handlers/GuestExec.hs:39`` passes
  ``vgeTimeoutSec=0`` and blocks). Bucket 9 covers task-cancel
  cascading separately; pinning a guest-exec cancellation here
  would need a deliberate timeout knob plus daemon-side coop.
* Windows PowerShell ``-EncodedCommand``. Already exercised by
  :mod:`test_windows`, plus our regular test image is Alpine. A
  dedicated Windows guest-exec test would just duplicate that
  coverage.
* "Guest agent enabled but not yet responsive". Racy — would need
  a synthetic delay in the QGA poller's first-ping path. The
  state-machine error matrix in :mod:`test_state_machine_errors`
  already pins the broader contract.
"""

from __future__ import annotations

import json
import secrets

import pytest
from corvus_client.exceptions import GuestAgentNotEnabled, ServerError
from corvus_test_harness import SingleNodeCase, Vm


def _crv(node, args: str, *, check: bool = False):
    """Run ``/opt/corvus/bin/crv <args>`` on the test node. Matches
    the helper used by :mod:`test_task_history`; ``check=False`` so
    we can assert on non-zero exit codes from ``crv vm exec``
    without raising at SSH layer."""
    return node.run(f"/opt/corvus/bin/crv {args}", check=check)


class TestGuestExecCli(SingleNodeCase):
    """``crv vm exec`` exit-code + JSON output via the inner CLI.

    Boots a single Alpine VM with QGA, runs three exec invocations
    in sequence, and one JSON round-trip. Shares one VM boot — the
    daemon's RPC surface here is read-only-stateful (the VM keeps
    running, no per-test mutation) so back-to-back execs are
    independent."""

    @pytest.fixture(scope="class", autouse=True)
    def _install_client_certs(self, _class_topology):
        """``crv`` on the test node needs the host-side client cert
        trio to dial the daemon over mTLS. Mirrors the pattern from
        :mod:`test_task_history`."""
        self.install_node_client_certs()

    def test_exit_code_and_json_output(self):
        with Vm(self) as vm:
            vm_id = vm.cap.show().id
            node = self.node

            # ── Phase A: CLI exit code mirrors the guest's
            # success/failure dichotomy. The CLI exits 0 iff the
            # guest exited 0; any non-zero guest exit collapses to
            # CLI exit 1 (see GuestExec.hs:38 — `pure (exitcode == 0)`).
            # The full guest exit code is only available via
            # `--output json` (Phase B/C).
            for guest_code, cli_code in [(0, 0), (1, 1), (77, 1), (42, 1)]:
                cp = _crv(node, f"vm exec {vm_id} 'exit {guest_code}'")
                assert cp.returncode == cli_code, (
                    f"crv vm exec 'exit {guest_code}' returned "
                    f"{cp.returncode}, expected {cli_code}; "
                    f"stdout={cp.stdout!r} stderr={cp.stderr!r}"
                )

            # ── Phase B: --output json shape. The flag is on the
            # top-level optionsParser (Parser.hs:194), so it goes
            # BEFORE the subcommand: `crv --output json vm exec …`.
            # Keys match src/Corvus/Client/Commands/GuestExec.hs:28-34
            # exactly.
            cp = _crv(
                node, f"--output json vm exec {vm_id} 'echo hi; echo err >&2'"
            )
            assert cp.returncode == 0, cp
            payload = json.loads(cp.stdout.decode("utf-8"))
            assert payload["status"] == "ok", payload
            assert payload["exitcode"] == 0, payload
            assert "hi" in payload["stdout"], payload
            assert "err" in payload["stderr"], payload

            # ── Phase C: --output json on a failing command. The
            # CLI exit code collapses to 1 (per Phase A), but the
            # JSON payload carries the real guest exit code so
            # operator scripts can recover it. Catches a future
            # refactor that, e.g., zeroes out the exitcode field in
            # json mode on failure.
            cp = _crv(node, f"--output json vm exec {vm_id} 'exit 42'")
            assert cp.returncode == 1, cp
            payload = json.loads(cp.stdout.decode("utf-8"))
            assert payload["exitcode"] == 42, payload


class TestGuestExecRejections(SingleNodeCase):
    """Refusal paths surfaced through the pycapnp client — typed
    exceptions, not CLI exit codes. Two small VMs, each in its own
    test for clarity; both finish in seconds (no boot needed)."""

    def test_stopped_vm_rejected_with_must_be_running(self):
        """A stopped VM (never started) has no QEMU process to
        carry the QGA channel; the daemon refuses at the FSM gate.

        Unlike the canonical ``invalid transition from <s>: <r>``
        shape used by stop/start/pause (see :mod:`test_state_machine_errors`),
        the guest-exec handler at [src/Corvus/Rpc/Vm.hs:310-311]
        emits ``"VM is <status>; VM must be running for guest-exec"``
        — which the client's regex translator (no match for
        ``_INVALID_TRANSITION_RE``) maps to :class:`ServerError`,
        not :class:`InvalidTransition`. We pin the message so a
        refactor that changes the wording (or unifies the two
        FSM-rejection shapes) flags here."""
        vm = self.client.vms.create(
            f"exec-stopped-{secrets.token_hex(3)}",
            cpu_count=1,
            ram_mb=64,
            headless=True,
            guest_agent=True,
        )
        try:
            with pytest.raises(ServerError) as exc_info:
                vm.guest_exec("echo hi")
            msg = str(exc_info.value).lower()
            assert "stopped" in msg, f"missing 'stopped': {msg!r}"
            assert "must be running" in msg, f"missing 'must be running': {msg!r}"
        finally:
            vm.delete()

    def test_running_vm_without_qga_rejected(self):
        """A running VM whose ``guest_agent`` flag was never enabled
        has no QGA chardev attached; the daemon refuses with
        :class:`GuestAgentNotEnabled` from the regex translation in
        ``python/corvus_client/exceptions.py:171``."""

        class _NoQgaVm(Vm):
            guest_agent = False
            wait_for_qga = False

        with _NoQgaVm(self) as vm:
            # Without QGA there's no `starting → running` window;
            # the daemon flips straight to running once QEMU spawns.
            # Vm.__enter__ already returned, so the row is at
            # 'running' — guest_exec should pass the FSM check then
            # fail at the QGA-enabled check.
            with pytest.raises(GuestAgentNotEnabled):
                vm.cap.guest_exec("echo hi")
