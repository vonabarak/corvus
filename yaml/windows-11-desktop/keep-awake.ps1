$ErrorActionPreference = 'Stop'

# Apply before OOBE finishes: sleeping would stop QGA and the readiness probe.
# Reapply after the post-install reboot, since setup can restore power defaults.
# Machine policies persist in the published image, including after plan changes.
function Invoke-PowerCfg {
    param([string[]]$PowerArguments)
    & "$env:SystemRoot\System32\powercfg.exe" @PowerArguments
    if ($LASTEXITCODE -ne 0) {
        throw "powercfg $PowerArguments failed (exit $LASTEXITCODE)"
    }
}

# GUIDs are shared by powercfg and the Windows Power.admx machine policies.
$settings = @{
    '29f6c1db-86da-48c5-9fdb-f2b67b1f44da' = 0 # Idle sleep timeout
    '7bc4a2f9-d8fc-4469-b07b-33eb785aaca0' = 0 # Unattended sleep timeout
    '9d7815a6-7ee4-497e-8888-515a05f02364' = 0 # Hibernate timeout
    '94ac6d29-73ce-41a6-809f-6363ba21b47e' = 0 # Hybrid sleep
    'abfc2519-3608-4c2a-94ea-171b0ed546ab' = 0 # Allow standby S1-S3
}
$buttons = @{
    '96996bc0-ad50-47ec-923b-6f41874dd9eb' = 0 # Sleep button: do nothing
    '5ca83367-6e45-459f-a27b-476b1d01c936' = 0 # Lid close: do nothing
}
foreach ($group in @(@{ Subgroup = 'SUB_SLEEP'; Settings = $settings },
                    @{ Subgroup = 'SUB_BUTTONS'; Settings = $buttons })) {
    foreach ($setting in $group.Settings.GetEnumerator()) {
        $policy = "HKLM:\SOFTWARE\Policies\Microsoft\Power\PowerSettings\$($setting.Key)"
        if (-not (Test-Path -LiteralPath $policy)) {
            New-Item -Path $policy -Force | Out-Null
        }
        # Applied policies override the plan and powercfg refuses to change
        # those settings. Leave matching overrides in place on later passes.
        $existing = Get-ItemProperty -Path $policy
        foreach ($supply in @('AC', 'DC')) {
            if ($existing."${supply}SettingIndex" -ne $setting.Value) {
                Invoke-PowerCfg @("/set$($supply.ToLower())valueindex", 'SCHEME_CURRENT',
                                  $group.Subgroup, $setting.Key, "$($setting.Value)")
            }
            New-ItemProperty -Path $policy -Name "${supply}SettingIndex" `
                -PropertyType DWord -Value $setting.Value -Force | Out-Null
        }
    }
}
Invoke-PowerCfg @('/setactive', 'SCHEME_CURRENT')
# Disables suspend-to-disk, hybrid sleep and Fast Startup; removes hiberfil.sys.
Invoke-PowerCfg @('/hibernate', 'off')
# A generalized source image can leave a hibernation file while Windows already
# reports hibernation disabled. Re-enable it to reconcile the inherited file,
# then disable it again so Windows removes it through the supported API.
if (Test-Path -LiteralPath "$env:SystemDrive\hiberfil.sys") {
    # Windows rejects /hibernate on during specialize. The recipe runs this
    # script again after setup and reboot, then verifies that no file remains.
    $setup = Get-ItemProperty 'HKLM:\SYSTEM\Setup'
    $state = Get-ItemProperty 'HKLM:\SOFTWARE\Microsoft\Windows\CurrentVersion\Setup\State'
    if (($setup.SystemSetupInProgress -ne 0) -or
        ($setup.OOBEInProgress -ne 0) -or
        ($state.ImageState -ne 'IMAGE_STATE_COMPLETE')) {
        Write-Output 'Hibernation disabled; inherited file cleanup deferred until setup completes'
        return
    }
    Invoke-PowerCfg @('/hibernate', 'on')
    Invoke-PowerCfg @('/hibernate', 'off')
}
if (Test-Path -LiteralPath "$env:SystemDrive\hiberfil.sys") {
    throw 'powercfg /hibernate off succeeded but hiberfil.sys remains'
}
Write-Output 'Persistent power policies applied; hibernation disabled'
