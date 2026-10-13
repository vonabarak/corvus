# Resolve stable vendor releases at bake time; unchanged builds never run this.
$logDir = 'C:\ProgramData\Corvus\DesktopBuild'
$downloadDir = Join-Path $env:TEMP ('corvus-desktop-' + [Guid]::NewGuid())
New-Item -ItemType Directory -Path $logDir, $downloadDir -Force | Out-Null
$manifest = [System.Collections.Generic.List[object]]::new()
Start-Transcript -Path (Join-Path $logDir 'install.log') -Force | Out-Null

function Get-Installer($name, $uri, $filename, $expectedHash = '') {
    $destination = Join-Path $downloadDir $filename
    Write-Output "Downloading $name from $uri" | Out-Host
    $response = Invoke-WebRequest -UseBasicParsing -Uri $uri -OutFile $destination -PassThru
    $hash = (Get-FileHash -LiteralPath $destination -Algorithm SHA256).Hash
    if ($expectedHash -and $hash -ne $expectedHash) { throw "$name SHA256 mismatch" }
    $manifest.Add([ordered]@{
        software = $name; requestedUrl = $uri
        resolvedUrl = $response.BaseResponse.ResponseUri.AbsoluteUri
        sha256 = $hash; downloadedAt = (Get-Date).ToUniversalTime().ToString('o')
    })
    $manifest | ConvertTo-Json -Depth 5 | Set-Content -LiteralPath (Join-Path $logDir 'downloads.json') -Encoding UTF8
    return $destination
}

function Install-Msi($name, $path, $properties) {
    $log = Join-Path $logDir ($name + '-msi.log')
    $arguments = '/i "{0}" /qn /norestart /l*v "{1}" {2}' -f $path, $log, $properties
    $process = Start-Process msiexec.exe -ArgumentList $arguments -Wait -PassThru
    if ($process.ExitCode -notin 0, 3010) { throw "$name MSI failed: $($process.ExitCode); see $log" }
    Write-Output "$name installed (exit $($process.ExitCode))"
}

function Install-TotalCommander {
    $tcPage = (Invoke-WebRequest -UseBasicParsing 'https://www.ghisler.com/download.htm').Content
    $tcMatch = [regex]::Match($tcPage, 'href="(https://totalcommander\.ch/[0-9]+/tcmd[0-9]+x64\.exe)"', 'IgnoreCase')
    if (-not $tcMatch.Success) { throw 'Cannot resolve stable Total Commander x64 installer' }
    $tc = Get-Installer 'Total Commander' $tcMatch.Groups[1].Value 'totalcommander.exe'
    $tcDir = 'C:\Program Files\Total Commander'
    # The interactive installer fails in QGA's SYSTEM session. Deploy its
    # official cabinet payload directly; Program Files keeps binaries protected
    # and Total Commander uses each runtime user's AppData for configuration.
    Add-Type -AssemblyName System.IO.Compression.FileSystem
    $tcPayload = Join-Path $downloadDir 'totalcommander-payload'
    [IO.Compression.ZipFile]::ExtractToDirectory($tc, $tcPayload)
    New-Item -ItemType Directory -Path $tcDir -Force | Out-Null
    & expand.exe '-F:*' (Join-Path $tcPayload 'INSTALL.CAB') $tcDir | Out-Null
    if ($LASTEXITCODE -ne 0) { throw "Total Commander cabinet extraction failed: $LASTEXITCODE" }
    $tcShortcut = (New-Object -ComObject WScript.Shell).CreateShortcut('C:\ProgramData\Microsoft\Windows\Start Menu\Programs\Total Commander.lnk')
    $tcShortcut.TargetPath = Join-Path $tcDir 'TOTALCMD64.EXE'
    $tcShortcut.WorkingDirectory = $tcDir
    $tcShortcut.Save()
}

function Install-Firefox {
    $firefox = Get-Installer 'Firefox' 'https://download.mozilla.org/?product=firefox-msi-latest-ssl&os=win64&lang=en-GB' 'firefox.msi'
    Install-Msi 'firefox' $firefox 'DESKTOP_SHORTCUT=false START_MENU_SHORTCUT=true TASKBAR_SHORTCUT=false'
}

function Install-PdfXChangeEditor {
    $pdfPage = (Invoke-WebRequest -UseBasicParsing 'https://www.pdf-xchange.com/product/pdf-xchange-editor').Content
    # The first x64 dropdown belongs to Editor, before the Editor Plus dropdown.
    $pdfMatch = [regex]::Match($pdfPage, '<a\b[^>]*data-defaultfor64bit="1"[^>]*href="([^"]+)"', 'IgnoreCase')
    if (-not $pdfMatch.Success) { throw 'Cannot resolve PDF-XChange Editor x64 MSI landing page' }
    $landingUri = [Uri]::new([Uri]'https://www.pdf-xchange.com', [Net.WebUtility]::HtmlDecode($pdfMatch.Groups[1].Value))
    $landingPage = (Invoke-WebRequest -UseBasicParsing $landingUri.AbsoluteUri).Content
    $msiMatch = [regex]::Match($landingPage, 'href="(https://www\.pdf-xchange\.com/downloads/[^"?]+\.x64\.msi[^" ]*)"', 'IgnoreCase')
    if (-not $msiMatch.Success) { throw 'Cannot resolve PDF-XChange Editor MSI download' }
    $hashMatch = [regex]::Match($landingPage, 'class="checksum-value">([a-f0-9]{64})<', 'IgnoreCase')
    $pdf = Get-Installer 'PDF-XChange Editor' ([Net.WebUtility]::HtmlDecode($msiMatch.Groups[1].Value)) 'pdf-editor.msi' $hashMatch.Groups[1].Value
    Install-Msi 'pdf-editor' $pdf 'DESKTOP_SHORTCUTS=0 PROGRAMSMENU_SHORTCUTS=1 SET_AS_DEFAULT=0 VIEW_IN_BROWSERS=0'
}

function Install-ProcessExplorer {
    $zip = Get-Installer 'Process Explorer' 'https://download.sysinternals.com/files/ProcessExplorer.zip' 'process-explorer.zip'
    $peDir = 'C:\Program Files\Sysinternals\Process Explorer'
    New-Item -ItemType Directory -Path $peDir -Force | Out-Null
    Expand-Archive -LiteralPath $zip -DestinationPath $peDir -Force
    $peExe = Join-Path $peDir 'procexp64.exe'
    $shortcut = (New-Object -ComObject WScript.Shell).CreateShortcut('C:\ProgramData\Microsoft\Windows\Start Menu\Programs\Process Explorer.lnk')
    $shortcut.TargetPath = $peExe
    $shortcut.WorkingDirectory = $peDir
    $shortcut.Save()
}

try {
    Install-TotalCommander
    Install-Firefox
    Install-PdfXChangeEditor
    Install-ProcessExplorer

    $executables = [ordered]@{
        'Total Commander' = 'C:\Program Files\Total Commander\TOTALCMD64.EXE'
        Firefox = 'C:\Program Files\Mozilla Firefox\firefox.exe'
        'PDF-XChange Editor' = 'C:\Program Files\PDF-XChange\PDF Editor\PXCEditor.exe'
        'Process Explorer' = 'C:\Program Files\Sysinternals\Process Explorer\procexp64.exe'
    }
    $versions = foreach ($entry in $executables.GetEnumerator()) {
        if (-not (Test-Path -LiteralPath $entry.Value)) { throw "Missing installed executable: $($entry.Value)" }
        [ordered]@{ software = $entry.Key; path = $entry.Value; version = (Get-Item -LiteralPath $entry.Value).VersionInfo.ProductVersion }
    }
    $versions | ConvertTo-Json | Set-Content -LiteralPath (Join-Path $logDir 'versions.json') -Encoding UTF8
    Write-Output 'All desktop applications installed; restart handled by Corvus'
} finally {
    Stop-Transcript | Out-Null
    Remove-Item -LiteralPath $downloadDir -Recurse -Force -ErrorAction SilentlyContinue
}
