$ErrorActionPreference = 'Stop'
$ProgressPreference = 'SilentlyContinue'

$logPath = 'C:\Windows\Temp\corvus-build.log'
Start-Transcript -Path $logPath -Append

try {
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

    $virtioInstall = Start-Process msiexec.exe -ArgumentList '/i', 'E:\virtio-win-gt-x64.msi', '/qn', '/norestart', '/l*v', 'C:\Windows\Temp\virtio-win-install.log' -Wait -PassThru
    if ($virtioInstall.ExitCode -notin @(0, 3010)) {
        throw "VirtIO guest tools installer exited with code $($virtioInstall.ExitCode)"
    }

    Set-Service -Name QEMU-GA -StartupType Automatic
    Start-Service -Name QEMU-GA

    $spiceService = Get-Service -Name vdservice -ErrorAction SilentlyContinue
    if ($null -ne $spiceService) {
        Set-Service -Name vdservice -StartupType Automatic
        Start-Service -Name vdservice
    }

    $virtioFsService = Get-Service -Name VirtioFsSvc -ErrorAction SilentlyContinue
    if ($null -eq $virtioFsService) {
        $vioFsDriver = Start-Process pnputil.exe -ArgumentList '/add-driver', 'E:\viofs\w11\amd64\viofs.inf', '/install' -Wait -PassThru
        if ($vioFsDriver.ExitCode -ne 0) {
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
    if ($null -eq (Get-Service -Name VirtioFsSvc -ErrorAction SilentlyContinue)) {
        throw 'VirtIO-FS service was not installed'
    }

    Remove-Item -Path $winFspMsi -Force
    $winlogonPath = 'HKLM:\SOFTWARE\Microsoft\Windows NT\CurrentVersion\Winlogon'
    Remove-ItemProperty -Path $winlogonPath -Name AutoAdminLogon, DefaultUserName, DefaultPassword, AutoLogonCount -ErrorAction SilentlyContinue

    Stop-Transcript
    shutdown.exe /s /t 30 /f
}
catch {
    $_ | Out-String | Add-Content -Path $logPath
    Stop-Transcript -ErrorAction SilentlyContinue
    exit 1
}
