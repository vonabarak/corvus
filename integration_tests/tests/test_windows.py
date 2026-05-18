"""Windows Server 2025 integration tests.

Ports `WindowsIntegrationSpec` from
[`doc/integration-tests-pre-capnp.md`](../../doc/integration-tests-pre-capnp.md).
Split into two methods because attaching a virtiofs shared dir to a
UEFI Windows VM at boot reproducibly hangs OVMF (the guest sits on
the TianoCore splash forever) — combining virtiofs with the
NoCloud datasource ISO + OVMF appears to be a firmware-side issue
that needs separate debugging. Until that's fixed:

  * `test_lifecycle_and_cloud_init` — cloud-init enabled, no shared
    dir. Covers properties 1a/1b (status + healthcheck renewal),
    2 (guest-exec) and 3 (`#ps1_sysnative` user-data executes).

  * `test_shared_directory` — virtiofs shared dir attached, cloud-init
    disabled. Covers property 4 (round-trip read/write through the
    auto-assigned viofs drive letter).

Both tests rely on:
  * QGA + cloudbase-init + WinFSP + the virtio-fs driver baked into
    `windows-server-2025-eval` (see `autounattend.xml` FirstLogon
    Order 2–11).
  * The harness's `VmWindows` wrapper, which boots the image UEFI
    with `guest_agent=True` so the daemon transitions to `running`
    after QGA's first ping. Drive the guest via
    `vm.cap.guest_exec(...)`; no SSH on Windows.
"""
from __future__ import annotations

import base64
import secrets
import threading
import time
from typing import Optional

import pytest

from corvus_client import ServerError
from corvus_test_harness import SingleNodeCase, VmWindows


def _ps(script: str) -> str:
    """Wrap a PowerShell snippet in `powershell -EncodedCommand <base64>`.

    The daemon auto-wraps non-prefixed commands in `cmd.exe /c …`, and
    cmd.exe interprets `|`, `&`, `<`, `>`, `^` inside the line even
    when they're nested inside PowerShell's own quotes. PowerShell's
    `-EncodedCommand` takes a base64-encoded UTF-16LE string, which
    contains none of those metacharacters, so cmd.exe passes it
    through intact and PowerShell decodes + executes the original.
    """
    blob = base64.b64encode(script.encode("utf-16-le")).decode("ascii")
    return f"powershell -EncodedCommand {blob}"


pytestmark = [pytest.mark.slow]


class TestWindows(SingleNodeCase):
    """Two boots: cloud-init path; virtiofs path."""

    # Windows-on-nested-KVM is glacial: 2-4 min boot + cloud-init,
    # then per-`guest_exec` round-trip ~5-10 s (QGA polls exec-status
    # asynchronously). 25 min ceiling covers either test method,
    # comfortably above the project-wide default `timeout = 600`.

    # ---- virtiofs helpers --------------------------------------------------

    def _find_share_drive(self, vm) -> Optional[str]:
        """Return the drive letter of the virtio-fs mount, or `None`.

        Restarts `VirtioFsSvc` first (auto-start can race the PCI
        device enumeration on first boot), waits briefly for the
        mount, then enumerates FileSystem PSDrives and probes each
        for the host-staged sentinel `from-host.txt`. Both probes
        bound by the daemon's 60 s QGA exec budget.

        The viofs Windows driver's default letter policy isn't
        deterministic across versions, so we don't hard-code a
        candidate range — whatever `Get-PSDrive` returns is what we
        check.
        """
        # `sc.exe stop/start` are fire-and-forget at the SCM level —
        # they return as soon as the service enters a transition
        # state, even if the service body is wedged. Avoid
        # PowerShell's `Restart-Service`, which blocks until the
        # service reaches `Stopped` and can eat the QGA budget.
        vm.cap.guest_exec("cmd.exe /c sc.exe stop VirtioFsSvc")
        time.sleep(2)
        vm.cap.guest_exec("cmd.exe /c sc.exe start VirtioFsSvc")
        time.sleep(10)

        list_r = vm.cap.guest_exec(_ps(
            "Get-PSDrive -PSProvider FileSystem | "
            "Select-Object -ExpandProperty Name"
        ))
        if list_r.exit_code != 0:
            return None
        candidate_letters = [
            ln.strip() for ln in list_r.stdout.splitlines()
            if ln.strip() and ln.strip().upper() != "C"
        ]
        for letter in candidate_letters:
            try:
                probe = vm.cap.guest_exec(
                    f"cmd.exe /c type {letter}:\\from-host.txt"
                )
            except ServerError:
                continue
            if probe.exit_code == 0 and "HOST-WROTE" in probe.stdout:
                return letter
        return None

    def _raise_share_diagnostic(self, vm) -> None:
        """Gather every diagnostic we have on virtiofs state in the
        guest and raise `AssertionError` with the lot. Called when
        `_find_share_drive` has returned `None` even after a reset.
        """

        def _probe(cmd: str) -> str:
            try:
                rr = vm.cap.guest_exec(cmd)
                return (
                    f"exit={rr.exit_code} "
                    f"stdout={rr.stdout!r} stderr={rr.stderr!r}"
                )
            except ServerError as e:
                return f"ServerError({e!r})"

        svc = _probe(_ps("(Get-Service VirtioFsSvc).Status"))
        winfsp = _probe(_ps(
            "(Get-Service WinFsp.Launcher -ErrorAction "
            "SilentlyContinue).Status"
        ))
        # Enumerate virtio PnP devices. `Status: OK` = driver bound,
        # `Error`/`Unknown` = ConfigManagerErrorCode tells us why.
        pnp = _probe(_ps(
            "Get-PnpDevice -Class System -ErrorAction "
            "SilentlyContinue | Where-Object "
            "{$_.FriendlyName -match 'VirtIO|virtio'} | "
            "Select-Object FriendlyName,Status,InstanceId | "
            "Format-List | Out-String"
        ))
        drives_seen = _probe(_ps(
            "Get-PSDrive -PSProvider FileSystem | "
            "Select-Object Name,Used,Free | "
            "Format-Table | Out-String"
        ))
        # `Win32_Volume` lists volumes that may have no drive letter
        # but otherwise look mounted — covers the "viofs presented a
        # volume but Windows refused to assign a letter" case.
        volumes = _probe(_ps(
            "Get-CimInstance Win32_Volume | "
            "Select-Object DriveLetter,Label,FileSystem,Capacity | "
            "Format-Table | Out-String"
        ))
        # Windows event log entries from VirtioFsSvc / WinFsp —
        # usually the most direct signal when the service started
        # but the mount didn't.
        events = _probe(_ps(
            "Get-WinEvent -LogName Application -MaxEvents 200 "
            "-ErrorAction SilentlyContinue | "
            "Where-Object {$_.ProviderName -match "
            "'virtio|VirtIO|WinFsp'} | "
            "Select-Object -First 10 TimeCreated,LevelDisplayName,"
            "Message | Format-List | Out-String"
        ))
        raise AssertionError(
            "virtiofs share never mounted (no FileSystem PSDrive "
            "contained from-host.txt), even after a VirtioFsSvc "
            "restart and a full guest reset\n"
            f"VirtioFsSvc status: {svc}\n"
            f"WinFsp.Launcher status: {winfsp}\n"
            f"virtio PnP devices: {pnp}\n"
            f"FileSystem PSDrives seen: {drives_seen}\n"
            f"Win32_Volume entries: {volumes}\n"
            f"VirtioFsSvc/WinFsp event log: {events}"
        )

    # ---- tests -------------------------------------------------------------

    @pytest.mark.timeout(1500)
    def test_lifecycle_and_cloud_init(self):
        """VM-lifecycle properties + cloud-init `#ps1_sysnative`.

        No shared dir attached — the UEFI + virtiofs combination
        currently hangs OVMF. `test_shared_directory` covers the
        virtiofs path separately, without cloud-init.
        """
        marker_token = secrets.token_hex(4)

        class _WinCI(VmWindows):
            def _cloud_init_config(_self):
                user_data = (
                    "#ps1_sysnative\r\n"
                    f"Set-Content -Path C:\\corvus-marker.txt "
                    f"-Value 'ci-ok-{marker_token}'\r\n"
                )
                return {"user_data": user_data}

        with _WinCI(self) as vm:
            # ---- (1a) VM status -------------------------------------
            # `__enter__` only returns once start(wait=True) sees the
            # first QGA ping (daemon transitions to `running`).
            # Belt-and-suspenders: explicitly observe.
            assert vm.cap.show().status == "running"

            # ---- (1b) Healthcheck renewal ---------------------------
            # Subscribe to the guest-agent push stream and collect
            # `last_healthcheck` timestamps; the daemon ticks every
            # ~10 s, so two distinct timestamps must arrive within
            # ~25 s. The callback runs on the runloop thread, so we
            # guard the list with a lock.
            hc_events: list = []
            hc_lock = threading.Lock()

            def _on_status(ev):
                with hc_lock:
                    hc_events.append(ev.last_healthcheck)

            sub = vm.cap.subscribe_guest_agent(_on_status)
            try:
                deadline = time.monotonic() + 25.0
                saw_renewal = False
                while time.monotonic() < deadline:
                    with hc_lock:
                        distinct = sorted(
                            {t for t in hc_events if t is not None}
                        )
                    if len(distinct) >= 2:
                        assert distinct[1] > distinct[0], distinct
                        saw_renewal = True
                        break
                    time.sleep(1.0)
                if not saw_renewal:
                    with hc_lock:
                        snapshot = list(hc_events)
                    raise AssertionError(
                        f"guest-agent healthcheck did not renew "
                        f"within 25 s; events={snapshot!r} — "
                        "Windows healthcheck-poller regression?"
                    )
            finally:
                try:
                    sub.close()
                except Exception:
                    pass

            # ---- (2) guest-exec works -------------------------------
            r = vm.cap.guest_exec("cmd.exe /c echo windows-test-ok")
            assert r.exit_code == 0, r
            assert "windows-test-ok" in r.stdout, r

            # ---- (3) Custom #ps1_sysnative user-data ran ------------
            # cloudbase-init's first-boot processing can outlast QGA's
            # first ping by minutes (it installs plugins, parses the
            # NoCloud datasource, then runs user-data). Each
            # `guest_exec` round-trip is itself several seconds on
            # Windows; while cloudbase-init is actively running, QGA's
            # process-spawning path can be slow enough that the
            # daemon's 60-s QGA timeout fires and `guest_exec` raises
            # ServerError with body `Connection failed: <<timeout>>`.
            # Treat those as "QGA-busy, try again later" — the
            # underlying healthcheck (`guest-ping`) is much lighter and
            # keeps working through the storm. Poll sparsely: 30 iters
            # × 10 s sleep + ~6 s exec = ~8 min budget.
            marker_path = "C:\\corvus-marker.txt"
            found = False
            last_r = None
            last_err: Optional[Exception] = None
            for _ in range(30):
                try:
                    last_r = vm.cap.guest_exec(
                        f"cmd.exe /c type {marker_path}"
                    )
                except ServerError as e:
                    last_err = e
                    time.sleep(10)
                    continue
                if (
                    last_r.exit_code == 0
                    and f"ci-ok-{marker_token}" in last_r.stdout
                ):
                    found = True
                    break
                time.sleep(10)
            if not found:
                # Gather diagnostics from the guest so the failure log
                # tells us whether cloudbase-init even ran. Each helper
                # is wrapped in its own try — if QGA is still flaky we
                # want partial info, not a swallowed ServerError.
                def _probe(cmd: str) -> str:
                    try:
                        rr = vm.cap.guest_exec(cmd)
                        return (
                            f"exit={rr.exit_code} "
                            f"stdout={rr.stdout!r} stderr={rr.stderr!r}"
                        )
                    except ServerError as e:
                        return f"ServerError({e!r})"

                svc = _probe(_ps(
                    "(Get-Service cloudbase-init).Status"
                ))
                log_tail = _probe(_ps(
                    "$p = 'C:\\Program Files\\Cloudbase Solutions"
                    "\\Cloudbase-Init\\log\\cloudbase-init.log'; "
                    "if (Test-Path $p) "
                    "{ Get-Content -Tail 40 -Path $p } "
                    "else { 'log not present' }"
                ))
                last_desc = (
                    f"last err: {last_err!r}" if last_err is not None
                    else (
                        f"last `type {marker_path}` "
                        f"exit={last_r.exit_code} "
                        f"stdout={last_r.stdout!r} "
                        f"stderr={last_r.stderr!r}"
                        if last_r is not None else "no exec attempted"
                    )
                )
                raise AssertionError(
                    f"cloud-init #ps1_sysnative didn't run; "
                    f"{last_desc}\n"
                    f"cloudbase-init service: {svc}\n"
                    f"cloudbase-init.log tail probe: {log_tail}"
                )

    @pytest.mark.skip(
        reason=(
            "Windows + virtio-fs is blocked by two firmware-side bugs: "
            "(1) the share doesn't bind on first boot under nested-KVM "
            "even though VirtIO FS Device shows Status=OK and "
            "VirtioFsSvc/WinFsp.Launcher are both Running (vhost-user-fs "
            "handshake between inner qemu and virtiofsd appears to fail "
            "silently); (2) the natural workaround — power-cycle the VM "
            "to re-do the handshake — trips TianoCore edk2#12441 "
            "(OVMF hangs on reboot with virtio-fs attached). Skip until "
            "either upstream fix lands — the test takes ~25 min and "
            "doesn't currently verify anything end-to-end."
        )
    )
    @pytest.mark.timeout(1500)
    def test_shared_directory(self):
        """Virtiofs shared dir round-trips through the guest.

        Currently xfail — see the marker above. cloud-init is
        disabled here because the earlier `cloud_init=True +
        shared_dir` configuration triggered a guest-initiated reboot
        (cloudbase-init hostname change), which then hit edk2#12441
        and hung OVMF mid-reboot. Even with cloud-init off the
        first-boot mount doesn't appear, but at least the test
        terminates cleanly with diagnostics instead of hanging the
        outer daemon.
        """
        share_path = f"/tmp/win-share-{secrets.token_hex(4)}"
        self.node.run(f"mkdir -p {share_path}")
        self.node.run(f"echo HOST-WROTE > {share_path}/from-host.txt")

        class _WinShare(VmWindows):
            # cloud_init disabled: enabling it lets cloudbase-init
            # reboot the guest mid-test, and that reboot hangs OVMF
            # due to edk2#12441 — the test would never terminate.
            cloud_init = False

            def _shared_dirs(_self):
                return [{"path": share_path, "tag": "winshare"}]

        try:
            with _WinShare(self) as vm:
                share_drive = self._find_share_drive(vm)
                if share_drive is None:
                    self._raise_share_diagnostic(vm)
                drive = share_drive

                # Read host-written content from inside the guest.
                r = vm.cap.guest_exec(
                    f"cmd.exe /c type {drive}:\\from-host.txt"
                )
                assert r.exit_code == 0, r
                assert "HOST-WROTE" in r.stdout, r

                # Write from the guest, observe on the host.
                guest_token = secrets.token_hex(4)
                w = vm.cap.guest_exec(
                    f"cmd.exe /c echo GUEST-WROTE-{guest_token} > "
                    f"{drive}:\\from-guest.txt"
                )
                assert w.exit_code == 0, w

                out = self.node.run(
                    f"cat {share_path}/from-guest.txt"
                ).stdout.decode()
                assert f"GUEST-WROTE-{guest_token}" in out
        finally:
            self.node.run(f"rm -rf {share_path}", check=False)
