$ErrorActionPreference = 'Stop'

# Apply only after setup readiness. Existing paging files remain active until
# the recipe's post-install reboot; never try to delete an active paging file.
Get-CimInstance Win32_ComputerSystem |
    Set-CimInstance -Property @{ AutomaticManagedPagefile = $false }
Get-CimInstance Win32_PageFileSetting | Remove-CimInstance
$memory = 'HKLM:\SYSTEM\CurrentControlSet\Control\Session Manager\Memory Management'
New-ItemProperty -Path $memory -Name PagingFiles -PropertyType MultiString `
    -Value ([string[]]@()) -Force | Out-Null
Write-Output 'Disk-backed paging disabled; takes effect after reboot'
