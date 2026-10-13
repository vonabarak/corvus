$answer = 'C:\Windows\Temp\corvus-runtime-unattend.xml'
if (-not (Test-Path -LiteralPath $answer)) { throw "Missing runtime answer file: $answer" }
$user = Get-LocalUser -Name corvus -ErrorAction SilentlyContinue
if ($user) {
    $sid = $user.SID.Value
    Get-CimInstance Win32_UserProfile | Where-Object SID -eq $sid | Remove-CimInstance
    Remove-LocalUser -Name corvus
}
# /quit leaves shutdown to Corvus: capture only follows confirmed graceful exit.
$process = Start-Process 'C:\Windows\System32\Sysprep\Sysprep.exe' -ArgumentList '/generalize', '/oobe', '/quit', '/quiet', "/unattend:$answer" -Wait -PassThru
if ($process.ExitCode -ne 0) { throw "Sysprep failed: $($process.ExitCode); see Sysprep\Panther logs" }
$state = (Get-ItemProperty 'HKLM:\SOFTWARE\Microsoft\Windows\CurrentVersion\Setup\State').ImageState
if ($state -ne 'IMAGE_STATE_GENERALIZE_RESEAL_TO_OOBE') { throw "Unexpected sealed image state: $state" }
Write-Output 'Image generalized for OOBE; ready for graceful poweroff'
