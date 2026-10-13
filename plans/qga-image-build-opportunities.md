# Other QGA opportunities reviewed

The Windows 11 desktop work implements native `guest-ping` readiness and
`guest-shutdown` reboot/powerdown, and native file uploads for PowerShell
provisioner scripts. Remaining opportunities below are follow-ups.

| QGA capability | Proposed Corvus use | Constraints and disposition |
|---|---|---|
| `guest-info` | Discover enabled commands and improve unsupported-command errors before provisioning. | Check enabled flags and success-response metadata, rather than assuming support from OS/version. Follow-up. |
| `guest-exec` structured path, arguments and environment | Preserve executable arguments through internal RPC instead of joining them into shell text. | Keep shell execution explicit for script provisioners. Also review retries of mutating scripts, captured-output truncation and Windows output streaming. Follow-up. |
| `guest-file-open/write/flush/close` | Share file transfer and log helpers; remove base64 shell decoding for uploads. | Handle partial writes and always close handles. Directory creation and permissions still need OS-specific handling. Flush is not a filesystem durability guarantee. Implemented for PowerShell script staging; broader upload and log consolidation remain follow-ups. |
| `guest-fstrim` | Trim unused guest storage before final shutdown to help artifact compaction. | Check runtime support and drive discard configuration; complements host qcow2 compaction. Follow-up. |
| OS, network and filesystem queries | Generalize build diagnostics and record useful provenance. | Agent availability does not prove setup completion, a file's existence or a TCP listener's readiness. Follow-up. |
| `guest-sync-delimited` | Strengthen reconnect and timeout recovery. | Requires sentinel-aware framing and parser reset. This work instead makes existing `guest-sync` fail without a verified matching ID. Follow-up. |
| Filesystem freeze/thaw | Retain snapshot consistency support in existing workflows. | Always bracket thaw; Windows VSS freeze is time-limited. Offline image capture requires no freeze. No new build use. |
| `guest-set-time` | Retain restore-time clock synchronization. | Windows may require an explicit timestamp rather than the implicit RTC path. No additional build use. |

Corvus already has native guest-agent file operations for Linux streamed build
logs, filesystem freeze/thaw for snapshots, OS detection for guest execution,
network queries and restore-time clock synchronization. Prefer consolidating
these implementations before adding parallel build-only helpers.

References: [QGA protocol](https://www.qemu.org/docs/master/interop/qemu-ga-ref.html),
[QMP reboot events](https://www.qemu.org/docs/master/interop/qemu-qmp-ref.html#event-RESET),
[Windows QGA implementation](https://gitlab.com/qemu-project/qemu/-/blob/master/qga/commands-win32.c).
