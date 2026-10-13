$ErrorActionPreference = 'Stop'

# OOBE can run longer than Windows' default AC standby timeout. A sleeping
# guest stops QGA, so the build cannot run its readiness wait-for probe.
$power = Start-Process "$env:SystemRoot\System32\powercfg.exe" `
    -ArgumentList '/change', 'standby-timeout-ac', '0' -Wait -PassThru
if ($power.ExitCode -ne 0) {
    throw "Failed to disable AC standby (powercfg exit $($power.ExitCode))"
}
