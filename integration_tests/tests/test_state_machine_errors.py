"""FSM rejection paths for VM operator actions.

Every *legal* transition in
:func:`Corvus.Model.VmState.validateTransition` is already implicitly
covered by the rest of the suite. Every *refused* transition is not:
this file pins each one down end-to-end so a refactor that silently
weakens the state machine (e.g. allows ``stop`` on a saved VM, or
forgets to reject ``start`` while saving) fails loudly here rather
than in production.

Refusals come back as :class:`corvus_client.InvalidTransition`
because the daemon emits ``"invalid transition from <status>:
<reason>"`` via ``statusOrThrow`` in ``src/Corvus/Rpc/Vm.hs:621``, and
the client's regex table at ``python/corvus_client/exceptions.py:210``
translates that shape to the typed exception.

What's covered:

* From ``stopped``: ``stop``, ``pause``, ``save`` all refused.
* From ``running``: ``start`` refused.
* From ``paused``: ``stop``, ``pause`` refused.
* From ``saved``: ``pause``, ``save`` refused. (``stop`` from saved
  lives in :mod:`test_vm_save_load` since it's part of the save/load
  contract.)
* ``reset`` is the universal escape hatch: lands at ``stopped`` from
  any of ``stopped`` (idempotent), ``running``, ``paused``, ``saved``.
  Reset from ``saved`` is already in :mod:`test_vm_save_load`; the
  others are here.

Not covered here (genuinely hard to reach from a client test):

* ``error`` state rejections — :mod:`test_vm_start_failures`
  exercises the error-state entry; the rejection matrix from there
  is well-defined by the FSM and would be cheap to add, but it
  needs an oversized-RAM-style failure to land in ``error`` first.
  Left as a follow-up.
* Transient state rejections (``starting``/``stopping``/``saving``/
  ``loading``/``migrating``) — :mod:`test_vm_save_load`'s
  ``test_async_save_surfaces_saving_state_and_blocks_concurrent_start``
  is the template; the other transient states are racy to hit
  reliably from a client test.

See ``src/Corvus/Model/VmState.hs`` for the canonical state-machine
table.
"""

from __future__ import annotations

import time

import pytest
from corvus_client import InvalidTransition
from corvus_test_harness import SingleNodeCase, Vm


def _poll_until(cond, *, timeout_sec: float, msg: str, poll_sec: float = 0.5) -> None:
    deadline = time.monotonic() + timeout_sec
    while time.monotonic() < deadline:
        if cond():
            return
        time.sleep(poll_sec)
    raise AssertionError(f"{msg} (waited {timeout_sec}s)")


def _assert_refused(
    action,
    *,
    from_status: str,
    keyword: str,
) -> InvalidTransition:
    """Run a no-arg `action()` callable, assert it raises
    `InvalidTransition` whose `status` matches `from_status` and
    whose `reason` contains `keyword` (case-insensitive).

    Returns the exception so callers can do extra assertions if they
    want. Wrap call sites with `pytest.raises` instead of using this
    helper when the daemon may also legitimately succeed (e.g. for
    race-prone transient-state rejections); for the deterministic
    refusals in this file, this helper centralises the boilerplate.
    """
    with pytest.raises(InvalidTransition) as exc_info:
        action()
    exc = exc_info.value
    assert exc.status == from_status, (
        f"expected refusal from {from_status!r}; daemon reported "
        f"from-status {exc.status!r}, reason={exc.reason!r}"
    )
    assert keyword.lower() in exc.reason.lower(), (
        f"refusal reason {exc.reason!r} missing expected keyword {keyword!r}"
    )
    return exc


class TestRefusedFromStopped(SingleNodeCase):
    """A freshly-created VM is in ``stopped``. ``stop``, ``pause``,
    and ``save`` must all be refused — no QEMU process to act on,
    and the FSM is the only thing standing between the daemon and a
    nonsensical state write.

    Each test creates + deletes its own minimal VM (no boot, no QGA,
    no disk attached) so they finish in seconds. The class doesn't
    need the heavyweight :class:`Vm` context manager — going through
    ``self.client.vms.create`` directly is enough.
    """

    def _make_stopped_vm(self, name: str):
        return self.client.vms.create(
            name,
            cpu_count=1,
            ram_mb=64,
            headless=True,
            guest_agent=False,
        )

    def test_stop_on_stopped_refused(self):
        vm = self._make_stopped_vm("fsm-stop-on-stopped")
        try:
            assert vm.show().status == "stopped"
            _assert_refused(
                lambda: vm.stop(wait=False),
                from_status="stopped",
                keyword="already stopped",
            )
            assert vm.show().status == "stopped", "status drifted after refused stop"
        finally:
            vm.delete()

    def test_pause_on_stopped_refused(self):
        vm = self._make_stopped_vm("fsm-pause-on-stopped")
        try:
            _assert_refused(
                lambda: vm.pause(),
                from_status="stopped",
                keyword="stopped",
            )
            assert vm.show().status == "stopped"
        finally:
            vm.delete()

    def test_save_on_stopped_refused(self):
        vm = self._make_stopped_vm("fsm-save-on-stopped")
        try:
            _assert_refused(
                lambda: vm.save(wait=False),
                from_status="stopped",
                keyword="stopped",
            )
            assert vm.show().status == "stopped"
        finally:
            vm.delete()


class TestRefusedFromRunning(SingleNodeCase):
    """A running VM rejects ``start``. (``stop``, ``pause``, and
    ``save`` from running are legal and exercised elsewhere.)"""

    def test_start_on_running_refused(self):
        with Vm(self) as vm:
            assert vm.cap.show().status == "running"
            _assert_refused(
                lambda: vm.cap.start(wait=False),
                from_status="running",
                keyword="already running",
            )
            assert vm.cap.show().status == "running", (
                "status drifted after refused start on running VM"
            )


class TestRefusedFromPaused(SingleNodeCase):
    """A paused VM rejects ``stop`` (operators are routed to ``reset``)
    and a second ``pause``.

    Note: ``start`` from ``paused`` is legal — the daemon picks
    ``ActionStartResumePaused`` and resumes via QMP ``cont`` — so
    that's not a refusal to test."""

    def test_stop_on_paused_refused(self):
        with Vm(self) as vm:
            vm.cap.pause()
            _poll_until(
                lambda: vm.cap.show().status == "paused",
                timeout_sec=10.0,
                msg="vm did not reach 'paused' after pause()",
            )
            exc = _assert_refused(
                lambda: vm.cap.stop(wait=False),
                from_status="paused",
                keyword="reset",
            )
            # The canonical message points operators at the right
            # verb. Pin both halves so a future rewording that drops
            # the "reset instead" hint flags here.
            assert "paused" in exc.reason.lower()
            assert vm.cap.show().status == "paused", (
                "status drifted after refused stop on paused VM"
            )

    def test_pause_on_paused_refused(self):
        with Vm(self) as vm:
            vm.cap.pause()
            _poll_until(
                lambda: vm.cap.show().status == "paused",
                timeout_sec=10.0,
                msg="vm did not reach 'paused' after pause()",
            )
            _assert_refused(
                lambda: vm.cap.pause(),
                from_status="paused",
                keyword="already paused",
            )
            assert vm.cap.show().status == "paused"


class TestRefusedFromSaved(SingleNodeCase):
    """A saved VM rejects ``pause`` and a second ``save``. (``stop``
    from saved is covered by :mod:`test_vm_save_load` since it's the
    canonical example of the FSM's "operators must explicitly choose
    discard or resume" pattern.)"""

    def test_pause_on_saved_refused(self):
        with Vm(self) as vm:
            vm.cap.save(wait=True)
            _poll_until(
                lambda: vm.cap.show().status == "saved",
                timeout_sec=10.0,
                msg="vm did not reach 'saved' after save()",
            )
            _assert_refused(
                lambda: vm.cap.pause(),
                from_status="saved",
                keyword="saved",
            )
            assert vm.cap.show().status == "saved", (
                "status drifted after refused pause on saved VM"
            )

    def test_save_on_saved_refused(self):
        with Vm(self) as vm:
            vm.cap.save(wait=True)
            _poll_until(
                lambda: vm.cap.show().status == "saved",
                timeout_sec=10.0,
                msg="vm did not reach 'saved' after save()",
            )
            _assert_refused(
                lambda: vm.cap.save(wait=False),
                from_status="saved",
                keyword="already saved",
            )
            assert vm.cap.show().status == "saved"


class TestResetUniversal(SingleNodeCase):
    """``reset`` is the universal escape hatch: legal from every
    state, always lands at ``stopped``. The FSM's catch-all rule
    ``(_, ActionReset) -> Right VmStopped`` makes this load-bearing —
    if a refactor ever turns that into a guarded clause, every
    "stuck VM" recovery path in production breaks. This class proves
    the universality by exercising reset from the three operator-
    reachable non-error states. (Reset from ``saved`` is in
    :mod:`test_vm_save_load`; ``error`` is in
    :mod:`test_vm_start_failures`.)"""

    def test_reset_from_stopped_is_idempotent(self):
        """Reset on an already-stopped VM is a no-op success — the
        VM stays stopped, no error. Without this, scripted recovery
        flows (`reset; start`) would need a `try/except` around the
        reset just for the not-running case."""
        vm = self.client.vms.create(
            "fsm-reset-stopped",
            cpu_count=1,
            ram_mb=64,
            headless=True,
            guest_agent=False,
        )
        try:
            assert vm.show().status == "stopped"
            vm.reset()
            assert vm.show().status == "stopped", (
                "reset from stopped should be a no-op; status drifted"
            )
            # Twice, to prove genuine idempotence (the second call
            # would fail with "VM is already stopped" if the FSM's
            # catch-all rule were dropped).
            vm.reset()
            assert vm.show().status == "stopped"
        finally:
            vm.delete()

    def test_reset_from_running_lands_stopped(self):
        with Vm(self) as vm:
            assert vm.cap.show().status == "running"
            vm.cap.reset()
            _poll_until(
                lambda: vm.cap.show().status == "stopped",
                timeout_sec=30.0,
                msg="vm did not reach 'stopped' after reset from running",
            )

    def test_reset_from_paused_lands_stopped(self):
        with Vm(self) as vm:
            vm.cap.pause()
            _poll_until(
                lambda: vm.cap.show().status == "paused",
                timeout_sec=10.0,
                msg="vm did not reach 'paused' after pause()",
            )
            vm.cap.reset()
            _poll_until(
                lambda: vm.cap.show().status == "stopped",
                timeout_sec=30.0,
                msg="vm did not reach 'stopped' after reset from paused",
            )
