# Windows 11 overlay base and template

Manually download the Windows 11 x64 multi-edition installation ISO from
Microsoft and register it in Corvus as `windows-11-iso`. Microsoft generates
temporary download links, which makes automating the download difficult; the
build does not download the Windows installation ISO.

To get the download link:

1. Open Microsoft's [Windows 11 download page](https://www.microsoft.com/en-us/software-download/windows11)
   in a browser and find the section for downloading an x64 ISO.
2. Select the Windows 11 multi-edition ISO for x64 devices and submit the
   selection.
3. Select **English International** and confirm. This matches the `en-GB`
   language configured in the bundled answer files.
4. Use the **64-bit Download** button to save the ISO, or right-click it and
   copy the link address. The generated link expires after 24 hours; generate
   a new one if it has expired.

If you copied the link, Corvus can download it directly on the node and import
it into managed storage. Replace the quoted placeholder with the complete URL;
keep the quotes because the URL may contain shell metacharacters:

```sh
crv disk import windows-11-iso 'PASTE_MICROSOFT_DOWNLOAD_URL_HERE' \
  --format raw --path ISOs/Windows11/ --wait
```

If you downloaded the ISO in your browser, upload it from the machine running
`crv` instead. Replace the example path with your downloaded file:

```sh
crv disk upload windows-11-iso "$HOME/Downloads/windows-11.iso" \
  --format raw --path ISOs/Windows11/
```

For an ISO already present on the Corvus node, use `crv disk import` with its
absolute node-side path instead of the URL. To register that file in place
without copying it into managed storage, use:

```sh
crv disk register windows-11-iso /absolute/path/on/node/windows-11.iso --format raw
```

Choose one of these methods. In a multi-node setup, add `--node <node-name>`
to select the node that will run the build. Confirm the exact disk name before
building:

```sh
crv disk show windows-11-iso
```

The build automatically downloads the pinned VirtIO-Win 0.1.302 guest-tools ISO
from Fedora, verifies its SHA-256 checksum, and registers it as `virtio-win-iso`
under `ISOs/VirtIO/` if that disk
is not already registered. An existing `virtio-win-iso` is reused without
downloading or replacing it. The node needs internet access for this download.
The managed `corvus` network must provide internet access for the pinned WinFSP
download. The host must have the OVMF firmware paths used in
[windows-11.yml](windows-11.yml).
From the repository root, run:

```sh
make image IMAGE=windows-11
```

The installer uses no TPM and provisions Windows in audit mode. It installs
VirtIO drivers, QEMU Guest Agent, SPICE guest tools, WinFSP, and VirtIO-FS,
checks the QXL, balloon, and HDA drivers, and refuses to seal an encrypted
system volume. Sysprep generalizes Windows and shuts it down for capture as
`windows-11-pro-base` under `BaseImages/Windows11/`. Successful bake VMs are
removed; failed bake VMs remain available for inspection. Provisioning logs
are in `C:\Windows\Temp\`; Sysprep logs are in
`C:\Windows\System32\Sysprep\Panther\`.

The final `windows-11-pro` template uses a writable overlay of this base,
independently cloned pristine UEFI variables, QXL video, Intel HDA audio with
a SPICE backend, a VirtIO memory balloon, and TPM. Installation media is not
attached to runtime VMs. Create as many VMs as needed:

```sh
crv template instantiate windows-11-pro win11-a
crv template instantiate windows-11-pro win11-b
crv vm start win11-a
crv vm start win11-b
```

Each clone completes specialize/OOBE unattended, generates its own Windows
computer name (independent of the Corvus VM name), and creates the local
administrator `corvus` with password `corvus`. There is no automatic login.
Locale is `en-GB` and timezone is UTC. Change the password after first login
and activate each VM with an appropriate Windows license.

TPM state is created separately for each VM on first start and persists across
its subsequent starts. No TPM state is copied from the installer. Automatic
device encryption remains disabled even with TPM present. BitLocker can be
enabled explicitly within an individual VM; save its recovery key separately
and preserve that VM's TPM state. Keep the shared base immutable and decrypted.

## Rebuilds and failures

An existing artifact or template is skipped, so rerunning the build does not
upgrade it. The pipeline still registers a missing runtime template when the
base already exists. An older image built with TPM or without Sysprep must be
rebuilt from installation media before use as this overlay base:

```sh
make image-rebuild IMAGE=windows-11
```

Rebuild cleanup cannot remove a base referenced by existing VMs. Preserve
those VMs and their disks; retire their dependencies deliberately or build
under separate artifact/template names. Do not repurpose a retained installation
VM containing user data as a generalized base. After investigating a failed
bake, remove that failed VM before retrying if it still uses the answer ISO.

## Runtime verification

After a fresh bake, start two clones and confirm that both reach the login
screen without installation media, have distinct computer names and TPM
identities, and report a decrypted system volume with BitLocker protection
off. Check QEMU Guest Agent, QXL display, SPICE clipboard/audio, and balloon
operation. Write a file in one clone and confirm it is absent in the other.
Restart each VM and confirm its TPM identity persists. Test explicit BitLocker
enablement and reboot recovery only on a disposable clone with its recovery
key saved.

Validated on 2026-10-06 with Windows 11 25H2 English International x64 media.
The fresh bake completed in 12 minutes 20 seconds, Sysprep recorded successful
generalization and shutdown, and the compacted QCOW2 passed `qemu-img check`.
Two overlays completed unattended setup with distinct computer names and TPM
endorsement keys, healthy QXL/balloon/HDA drivers and guest services, and fully
decrypted system volumes. Overlay writes remained isolated and TPM identities
persisted across stop/start cycles.

One test clone reached the Windows lock screen while Corvus still reported
`starting` without guest-agent health. A stop/start restored the connection;
subsequent starts passed. Guest-agent availability can precede completion of
specialize/OOBE, so also verify that Windows setup has completed before running
checks that require the `corvus` account.
