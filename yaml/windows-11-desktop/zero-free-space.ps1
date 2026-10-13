$ErrorActionPreference = 'Stop'

# Run after installers, configuration and Sysprep have removed their files.
# Zero free space on the image's system volume so qcow2 compaction can omit it.
$executable = 'C:\Program Files\Sysinternals\SDelete\sdelete64.exe'
if (-not (Test-Path -LiteralPath $executable)) { throw "Missing SDelete executable: $executable" }
& $executable -accepteula -nobanner -z $env:SystemDrive
if ($LASTEXITCODE -ne 0) { throw "SDelete failed (exit $LASTEXITCODE)" }
Write-Output 'System volume free space zeroed; ready for image compaction'
