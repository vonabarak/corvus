# Read-only readiness probe; Corvus encodes it at runtime for PowerShell.
# QGA and IMAGE_STATE_COMPLETE can precede the final specialize reboot.
# TickCount measures boot uptime without guest wall-clock synchronization jumps.
$ErrorActionPreference = 'Stop'
$setup = Get-ItemProperty 'HKLM:\SYSTEM\Setup'
$state = Get-ItemProperty 'HKLM:\SOFTWARE\Microsoft\Windows\CurrentVersion\Setup\State'
if (([Environment]::TickCount -ge 180000) -and
    ($setup.SystemSetupInProgress -eq 0) -and
    ($setup.OOBEInProgress -eq 0) -and
    ($state.ImageState -eq 'IMAGE_STATE_COMPLETE') -and
    (Get-LocalUser -Name corvus -ErrorAction SilentlyContinue)) {
    exit 0
}
exit 1
