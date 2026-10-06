$ErrorActionPreference = 'Stop'
$ProgressPreference = 'SilentlyContinue'

$logPath = 'C:\Windows\Temp\corvus-build.log'
Start-Transcript -Path $logPath -Append

function Assert-DeviceDriver {
    param([string]$Name, [string]$DeviceIdPattern, [string]$Service)

    $devices = @(Get-CimInstance -ClassName Win32_PnPEntity | Where-Object { $_.DeviceID -match $DeviceIdPattern })
    if ($devices.Count -eq 0) {
        throw "$Name device was not found"
    }
    foreach ($device in $devices) {
        if ($device.ConfigManagerErrorCode -ne 0 -or [string]::IsNullOrEmpty($device.Service)) {
            throw "$Name driver is not ready: $($device.Name), error $($device.ConfigManagerErrorCode)"
        }
        if ($Service -and $device.Service -ne $Service) {
            throw "$Name requires the $Service driver, found $($device.Service)"
        }
    }
}

try {
    if ((Get-Tpm).TpmPresent) {
        throw 'The reusable image must be installed without TPM; check the installer template'
    }

    # Audit mode may open the Sysprep dialog. Close it before the unattended
    # sealing invocation; only one Sysprep instance can run at a time.
    Get-Process -Name sysprep -ErrorAction SilentlyContinue | Stop-Process -Force

    $bitLockerPath = 'HKLM:\SYSTEM\CurrentControlSet\Control\BitLocker'
    if (-not (Test-Path -Path $bitLockerPath)) {
        New-Item -Path $bitLockerPath | Out-Null
    }
    New-ItemProperty -Path $bitLockerPath -Name PreventDeviceEncryption -PropertyType DWord -Value 1 -Force | Out-Null

    $winFspMsi = 'C:\Windows\Temp\winfsp-2.2.26215.msi'
    Invoke-WebRequest -UseBasicParsing -Uri 'https://github.com/winfsp/winfsp/releases/download/v2.2B4/winfsp-2.2.26215.msi' -OutFile $winFspMsi

    $winFspHash = (Get-FileHash -Algorithm SHA256 -Path $winFspMsi).Hash
    if ($winFspHash -ne '2ECB5C89405488A95BBD8A01875E02C48534FD37BBDFD84488F7590464D65944') {
        throw "WinFSP checksum mismatch: $winFspHash"
    }

    $winFspInstall = Start-Process msiexec.exe -ArgumentList '/i', $winFspMsi, '/qn', '/norestart', '/l*v', 'C:\Windows\Temp\winfsp-install.log' -Wait -PassThru
    if ($winFspInstall.ExitCode -notin @(0, 3010)) {
        throw "WinFSP installer exited with code $($winFspInstall.ExitCode)"
    }

    # The MSI alone installs VirtIO drivers. The bundle also installs QGA,
    # the QXL display driver and the SPICE agent, all needed by runtime VMs.
    $virtioInstall = Start-Process 'E:\virtio-win-guest-tools.exe' -ArgumentList '/quiet', '/norestart', '/log', 'C:\Windows\Temp\virtio-win-install.log' -Wait -PassThru
    if ($virtioInstall.ExitCode -notin @(0, 3010)) {
        throw "VirtIO guest tools installer exited with code $($virtioInstall.ExitCode)"
    }

    Set-Service -Name QEMU-GA -StartupType Automatic
    Start-Service -Name QEMU-GA

    Set-Service -Name spice-agent -StartupType Automatic
    Start-Service -Name spice-agent

    $virtioFsService = Get-Service -Name VirtioFsSvc -ErrorAction SilentlyContinue
    if ($null -eq $virtioFsService) {
        $vioFsDriver = Start-Process pnputil.exe -ArgumentList '/add-driver', 'E:\viofs\w11\amd64\viofs.inf', '/install' -Wait -PassThru
        if ($vioFsDriver.ExitCode -notin @(0, 3010)) {
            throw "VirtIO-FS driver installation exited with code $($vioFsDriver.ExitCode)"
        }

        $virtioFsDir = 'C:\Program Files\Virtio-FS'
        New-Item -Path $virtioFsDir -ItemType Directory -Force | Out-Null
        Copy-Item -Path 'E:\viofs\w11\amd64\virtiofs.exe' -Destination "$virtioFsDir\virtiofs.exe" -Force
        New-Service -Name VirtioFsSvc -BinaryPathName '"C:\Program Files\Virtio-FS\virtiofs.exe"' -StartupType Automatic -DisplayName 'VirtIO-FS Service'
    }
    else {
        Set-Service -Name VirtioFsSvc -StartupType Automatic
    }

    if ((Get-Service -Name QEMU-GA).Status -ne 'Running') {
        throw 'QEMU Guest Agent did not reach the Running state'
    }
    if ((Get-Service -Name spice-agent).Status -ne 'Running') {
        throw 'SPICE guest agent did not reach the Running state'
    }
    if ($null -eq (Get-Service -Name VirtioFsSvc -ErrorAction SilentlyContinue)) {
        throw 'VirtIO-FS service was not installed'
    }

    Assert-DeviceDriver -Name 'QXL' -DeviceIdPattern '^PCI\\VEN_1B36&DEV_0100' -Service QxlDod
    Assert-DeviceDriver -Name 'VirtIO balloon' -DeviceIdPattern '^PCI\\VEN_1AF4&DEV_(1002|1045)' -Service balloon
    Assert-DeviceDriver -Name 'HDA audio' -DeviceIdPattern '^HDAUDIO\\'

    # Protection Off alone is insufficient: a suspended BitLocker volume is
    # still encrypted. Capture only a fully decrypted system volume.
    $systemVolume = Get-BitLockerVolume -MountPoint $env:SystemDrive
    if ($systemVolume.VolumeStatus -ne 'FullyDecrypted' -or $systemVolume.EncryptionPercentage -ne 0 -or $systemVolume.ProtectionStatus -ne 'Off') {
        throw 'System volume is not fully decrypted with BitLocker protection off; refusing to capture it'
    }

    # Sysprep caches this answer file for specialize/OOBE on each clone.
    # It must never reuse the installation answer file or require its CDs.
    $runtimeAnswer = 'C:\Windows\Temp\corvus-runtime-unattend.xml'
    Copy-Item -Path 'F:\runtime-unattend.xml' -Destination $runtimeAnswer -Force
    Remove-Item -Path $winFspMsi -Force

    Write-Output 'Guest integration verified and system volume decrypted; generalizing the image'
    Stop-Transcript
    $sysprep = Start-Process 'C:\Windows\System32\Sysprep\Sysprep.exe' -ArgumentList '/generalize', '/oobe', '/shutdown', "/unattend:$runtimeAnswer" -Wait -PassThru
    if ($sysprep.ExitCode -ne 0) {
        throw "Sysprep exited with code $($sysprep.ExitCode); see C:\Windows\System32\Sysprep\Panther\ logs"
    }
}
catch {
    $failure = $_ | Out-String
    Stop-Transcript -ErrorAction SilentlyContinue
    Add-Content -Path $logPath -Value $failure
    exit 1
}
