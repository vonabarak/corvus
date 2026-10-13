# Windows 11 desktop

This recipe consumes `windows-11-pro-base`, owned by
[the Windows 11 recipe](../windows-11/README.md), and publishes the disk image
and runtime template `windows-11-desktop`. It installs the latest stable x64
Total Commander, Firefox (en-GB), PDF-XChange Editor and Process Explorer at
bake time. Shared Start Menu shortcuts are available to runtime users;
licensing and first-launch prompts retain vendor defaults. Total Commander is
installed by extracting the official cabinet payload and creating a shared
shortcut, because its interactive installer fails in QGA's SYSTEM session.
It uses per-user configuration and has no installer-created uninstall entry.

```sh
make image IMAGE=windows-11-desktop
```

The Makefile prepares the upstream recipe and propagates `IMAGE_POLICY`.
Normal `update` builds reuse matching artifacts; remote installer releases do
not invalidate that identity automatically. `ensure` only fills missing outputs.
To refresh applications without rebuilding the base:

```sh
make -C yaml/windows-11 ensure
make -C yaml/windows-11-desktop publish IMAGE_POLICY=overwrite
```

To rebuild the base and desktop together:

```sh
make image-rebuild IMAGE=windows-11-desktop
```

The bake runs without TPM and first disables sleep and hibernation so Windows cannot
suspend while specialize/OOBE is still running. Machine policies prohibit standby
on AC and battery, with idle and unattended sleep timers disabled, hybrid sleep
disabled, and sleep-button/lid actions set to do nothing. Hibernation is disabled,
including Fast Startup. These settings persist in runtime VMs after sealing.
It then waits for Windows setup
and the initial `corvus` account using repeated short read-only probes. The probe also requires cleared
setup flags and at least three minutes of uptime: QGA and `IMAGE_STATE_COMPLETE`
can precede Windows’ final specialize reboot. The recipe references `wait-for-setup.ps1` through `wait-for.script`.
Corvus reads that source and generates PowerShell’s `-EncodedCommand` at runtime
to avoid `cmd.exe` quoting.
After setup completes, the bake disables automatic pagefile management and removes
all configured pagefiles. It installs software and reboots using QGA to activate
the paging changes. It reapplies power settings after that reboot because the
early pass runs during specialize/OOBE, which can restore power defaults.
If an inherited `hiberfil.sys` survives disabling hibernation, the post-setup
pass briefly enables and disables hibernation to let Windows remove the file.
This recovery waits until setup finishes, since Windows rejects enabling
hibernation during specialize.
A read-only check then rejects available sleep states (including
Modern Standby), configured/active pagefiles, and remaining `pagefile.sys`,
`swapfile.sys` or `hiberfil.sys` on fixed drives before sealing. Run
`verify-runtime-settings.ps1` in a fresh clone after OOBE and again after reboot
to check persistence. Windows uses RAM without disk-backed paging; allocate
enough RAM for the intended workload. Host-controlled VM pause/save is unaffected.
The bake then removes that account/profile,
and seals with Sysprep `/generalize /oobe /quit`. Corvus then requests graceful
poweroff before capturing a compact qcow2 image. Failed builds, including final
shutdown timeouts, preserve the bake VM for inspection and do not publish it.

Runtime VMs use independent TPM state and recreate the base recipe's `corvus`
account during OOBE. Encryption remains opt-in. See the upstream guide for
credentials and Windows activation. Check two fresh clones after a new bake:
all four applications, recreated account, independent machine identities/TPMs,
working QGA and a decrypted system drive.

Downloads, resolved URLs, SHA256 hashes, installed versions and installer logs
are recorded under `C:\ProgramData\Corvus\DesktopBuild`. Temporary downloads
are removed. The recipe needs internet access through the `corvus` network.
Vendor page layout changes fail the bake rather than selecting an unknown file.

Vendor references: [Total Commander](https://www.ghisler.com/download.htm),
[Firefox MSI deployment](https://support.mozilla.org/en-US/kb/deploy-firefox-msi-installers),
[PDF-XChange Editor](https://www.pdf-xchange.com/product/pdf-xchange-editor),
[Process Explorer](https://learn.microsoft.com/en-us/sysinternals/downloads/process-explorer).
