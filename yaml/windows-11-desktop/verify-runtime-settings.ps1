$ErrorActionPreference = 'Stop'

# Read-only checks after reboot; also usable on fresh runtime clones after OOBE.
if ((Get-CimInstance Win32_ComputerSystem).AutomaticManagedPagefile) {
    throw 'Automatic pagefile management is still enabled'
}
if (@(Get-CimInstance Win32_PageFileSetting).Count -ne 0) {
    throw 'Paging files are still configured'
}
if (@(Get-CimInstance Win32_PageFileUsage).Count -ne 0) {
    throw 'Paging files are still active; reboot or inspect Windows paging configuration'
}
$memory = 'HKLM:\SYSTEM\CurrentControlSet\Control\Session Manager\Memory Management'
if (@((Get-ItemProperty -Path $memory -Name PagingFiles).PagingFiles |
      Where-Object { $_ }).Count -ne 0) {
    throw 'PagingFiles registry configuration is not empty'
}
foreach ($drive in Get-CimInstance Win32_LogicalDisk -Filter 'DriveType = 3') {
    foreach ($name in @('pagefile.sys', 'swapfile.sys', 'hiberfil.sys')) {
        $path = "$($drive.DeviceID)\$name"
        if (Test-Path -LiteralPath $path) { throw "Unexpected disk-backed memory file: $path" }
    }
}
$states = & "$env:SystemRoot\System32\powercfg.exe" /a 2>&1
if ($LASTEXITCODE -ne 0) { throw "powercfg /a failed: $states" }
$report = $states -join "`n"
Write-Output $report
# This recipe uses en-GB Windows. Fail closed if Windows changes/localizes the
# report instead of treating unrecognized output as proof that sleep is disabled.
if ($report -match 'The following sleep states are available on this system:\s*(?<available>[\s\S]*?)(?:The following sleep states are not available|\z)') {
    throw "Guest sleep states remain available: $($Matches.available.Trim())"
}
if ($report -notmatch 'The following sleep states are not available on this system:' -and
    $report -notmatch 'There are no sleep states available on this system') {
    throw 'Unrecognized powercfg /a report; cannot verify all sleep states are disabled'
}
Write-Output 'Verified: no guest sleep states, hibernation or disk-backed paging'
