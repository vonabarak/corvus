# Gentoo rescue USB image

Build and publish the 2 GiB raw disk and its runtime template:

```sh
make image IMAGE=gentoo-rescue
make image-check IMAGE=gentoo-rescue
```

The build reports the compressed root filesystem size and available ext4 space.
First writable boot expands storage to the USB stick's capacity.

Normal rescue builds default to `IMAGE_POLICY=update`, skipping unchanged recipes
and dependencies. When a build runs, it synchronizes the Gentoo Portage tree and
selects current unmasked **stable amd64** packages with the stable systemd profile.
Publication creates a new version and moves `latest`, preserving existing versions
and users. Remote Portage changes alone do not trigger a build; use
`IMAGE_POLICY=overwrite` to force a refresh. Explicit policy overrides still work.
`make image-ensure IMAGE=gentoo-rescue` only fills missing outputs; it does not
refresh an existing image. `make image-rebuild IMAGE=gentoo-rescue` forces the
whole dependency chain. Compatible package caches speed up installation; packages
may be compiled from source when the selected version is unavailable as a binary.

Rescue uses the shared Gentoo Portage verification library for signed repository
snapshots, repository metadata, and downloaded binary packages. The common
`GENTOO_KEYSERVER` option selects the HKPS repository key refresh endpoint;
see [Portage verification](README.md#portage-verification). Unsigned locally built
packages remain usable in the trusted shared cache.

The registered glibc/systemd builder provides native tools and caches. Rescue
uses the Gentoo distribution binary kernel, its complete modules, linux-firmware,
and a generic, non-hostonly dracut initramfs. It does not use the VM-only kernel
used by other Corvus Gentoo images. The recipe checks required hardware options,
boot drivers, and squashfs/OverlayFS support before publication.

`build.py` composes the stages in `rescue_stages.py` with the shared builder
stages. The bundle includes the recipe-local module. The YAML uploads
`assets/` as a directory. Files under `assets/root/` mirror their destinations
in the target root filesystem. The other asset directories hold shared
systemd drop-ins, Portage policy, and the GRUB template. The build fills in
the carrier UUID in that template and generates the payload checksum manifest.

## Rescue utilities

The image includes BIOS and UEFI GRUB repair tools, FSArchiver, Borg, rclone,
XFS dump and restore, and the QEMU disk utilities `qemu-img` and `qemu-nbd`.
Network tools include `socat`, `iperf3`, `nmap`, `dig`, `smbclient`, and
`mount.cifs`. Hardware diagnostics include `dmidecode`, `lsscsi`, `sg_inq`,
`hdparm`, `lshw`, `hwinfo`, `ipmitool`, `memtester`, and `picocom`.

To enable multipath for the current session, configure `/etc/multipath.conf`
for the intended disks, then run:

```sh
rm /etc/udev/rules.d/56-multipath.rules
udevadm control --reload
systemctl unmask multipathd.service multipathd.socket
systemctl start multipathd.service
multipath -ll
```

## Disk and runtime

| GPT partition | Size | Format | Use |
| --- | --- | --- | --- |
| 1 | 1 MiB | BIOS boot | BIOS GRUB |
| 2 | 64 MiB | FAT32 | Removable UEFI GRUB; normally unmounted |
| 3 | Remaining space | ext4, `rescue-data` | Kernel, initramfs, compressed root and persistent home |

The ESP holds `/EFI/BOOT/BOOTX64.EFI`, `/grub/grub.cfg` and both GRUB
module sets. Its menu waits 300 seconds, reports kernel/initramfs loading and
searches the ext4 UUID. Select **Gentoo rescue (verify boot files)**
to check every file listed in `SHA256SUMS` before GRUB loads Linux. The list
covers the kernel, initramfs and SquashFS. A failed check cancels that boot entry. The ext4 partition contains:

```text
vmlinuz
initramfs.img
root.squashfs
SHA256SUMS
```

A custom generic dracut module mounts this partition under
`/run/initramfs/rescue-media`, mounts the squashfs file read-only through a loop
device, and supplies a tmpfs OverlayFS for `/`. These mounts survive switch-root.
A small, versioned shutdown initramfs inside the squashfs lets dracut release
them in dependency order without unpacking the boot archive's hardware firmware
into RAM again. The carrier is mounted
read-write when possible, otherwise read-only with journal replay disabled.
`/root` binds the ext4 partition root only if writable,
with free blocks/inodes and a successful synchronized write probe. Otherwise it
is an empty tmpfs; fallback never unmounts the carrier backing the running root.
Changes elsewhere, including installed packages and configuration, vanish on reboot.
Carrier corruption that prevents reading the kernel or squashfs can prevent boot.

On writable boot, `rescue-grow` validates the boot UUID, ext4 label, parent disk
and exact three-partition GPT layout before extending partition 3 and its ext4
filesystem. It retries filesystem growth on later boots, including after partial
growth. Failures do not prevent home fallback or console login. Other disks are
never selected for expansion. Read-only media skip expansion and persistent home.

The bake template has **32 GiB RAM and 8 CPUs**. Installation happens entirely in
a **16 GiB tmpfs** at `/mnt/sysroot`, followed by XZ SquashFS compression
(1 MiB blocks, 2 GiB compressor memory limit). No staging system is written
to the target ext4 partition. The runtime image needs much less memory.

Local consoles log in automatically as root; the root password is locked. Use
only where unrestricted local root access is appropriate. SSH starts automatically;
install a public key in `/root/.ssh/authorized_keys` for root login. SSH host keys
persist under `/root/.ssh/host-keys` and are generated on first boot.
The default locale is `C.UTF-8`, and `en_US.UTF-8` is available for SSH clients
that forward their locale settings.

NetworkManager provides wired DHCP and automatically reconnects saved Wi-Fi
profiles. To connect and save a Wi-Fi network without a GUI:

```sh
nmcli --ask device wifi connect "YOUR_SSID"
```

Profiles and their passwords are stored in plain text in root-only files under
`/root/.config/NetworkManager/system-connections`. When persistent home is
unavailable, profiles, authorized keys, and host keys are temporary or absent;
the SSH host identity changes on each such boot.

QEMU guest-agent is enabled for VM testing.
Recovery disks are not mounted automatically; swap, RAID assembly and LVM
activation require explicit commands. Filesystem repair, partitioning, data
recovery, encryption, RAID/LVM, device diagnostics, networking and archive tools
are installed, including `mc`, `htop`, `strace`, `jq`, `yq`, `tmux`, `rar`,
`ncdu`, `lsof`, `exfatprogs`, and `mtr`. The `yq` command is the jq-based
YAML processor. No graphical desktop or ZFS is included.

Requires x86-64, BIOS or UEFI with Secure Boot disabled. Start with 4 GiB RAM;
2 GiB is the minimum target and available RAM limits volatile writes. There is
no persistent system overlay. Initial installation replaces the stick; subsequent
updates replace the three payload files and their checksums offline.

## Export rescue payloads

Run this command with the Corvus Python client installed in the selected Python
environment and with access to the Corvus daemon.

```sh
make -C yaml/gentoo export IMAGE=rescue
```

Use `PYTHON=/absolute/path/to/python` to select an environment.
Set `CORVUS_SOCKET` for a Unix socket or `CORVUS_HOST` and `CORVUS_PORT` for
TCP. TCP uses the Python client's default TLS configuration.

The first Corvus node must have the published `gentoo-rescue:latest` image
and be able to instantiate the `gentoo-musl-openrc-headless` template.
The exporter attaches the rescue image read-only to a temporary VM, downloads
and verifies the four files, and deletes the VM. It writes `vmlinuz`,
`initramfs.img`, `root.squashfs`, and `SHA256SUMS` to
`yaml/gentoo/build/gentoo-rescue/`. Set `OUTPUT_DIR=/absolute/path` to
override the destination.

## Initial USB installation

Use the registered raw image for a new USB stick. Obtain its path on the node
from `crv disk show gentoo-rescue:latest`. Run these commands with access to
that raw file. Replace `/path/to/registered-rescue.raw` with its path and
`/dev/sdX` with the **whole unmounted USB device**. Writing destroys the
contents of the USB device.

```sh
image=/path/to/registered-rescue.raw
sudo dd if="$image" of=/dev/sdX bs=4M conv=fsync status=progress
sudo cmp -n "$(stat -c %s "$image")" "$image" /dev/sdX
```

The first writable boot expands storage to use the remaining device capacity.
Back up home and reflash once when switching from an older layout. Restore
the contents of the old home directory directly into the new ext4 partition root.

## Offline payload updates

If the existing GRUB menu checks a separate SquashFS checksum, back up home and
reflash with the registered raw image to install the updated menu.

Boot another OS, unmount the rescue system, and mount its ext4 partition at
`/mnt/rescue`. Do not update the squashfs backing a running rescue session.
Keep a backup of the previous payloads. Verify the exported files on another
filesystem, then stage them and the checksum manifest before renaming them.

```sh
update=yaml/gentoo/build/gentoo-rescue
(cd "$update" && sha256sum -c SHA256SUMS) || exit 1
for path in vmlinuz initramfs.img root.squashfs SHA256SUMS; do
  sudo cp "$update/$path" "/mnt/rescue/$path.new" || exit 1
done
# Check the copies before replacing any active filename.
for path in vmlinuz initramfs.img root.squashfs SHA256SUMS; do
  sudo cmp "$update/$path" "/mnt/rescue/$path.new" || exit 1
done
sync
for path in vmlinuz initramfs.img root.squashfs SHA256SUMS; do
  sudo mv "/mnt/rescue/$path.new" "/mnt/rescue/$path" || exit 1
done
sync
sudo umount /mnt/rescue
```

Have enough free space for the staged files. Each rename is atomic, but the
renames together are not: do not interrupt this step, and restore a complete
matching set if interrupted. Existing home, filesystem UUID and ESP remain in
place. New payloads do not embed the exporting stick's UUID; the existing ESP
configuration supplies the destination UUID at boot.

## Validation

```sh
make yaml-schema-check
make -C yaml/gentoo test IMAGE=rescue
```

The runtime regression checks simulate healthy, absent, read-only, full,
inode-exhausted and I/O-failing storage, plus failed binds/remounts/unmounts.
They check empty home fallback without unmounting the carrier, and validate GRUB
and initramfs generator output. They do not substitute for boot tests.

The squashfs layout has passed QEMU boots with BIOS/virtio, BIOS/SATA,
UEFI/NVMe and UEFI/USB. Checks covered expansion of a compact image to 16 GiB,
idempotent reboots, persistent home, volatile `/etc`, and clean shutdown through
dracut's exit environment. A read-only USB boot with 2 GiB RAM used an empty
temporary home and left the complete disk hash unchanged.
An offline update from a separately built image preserved the destination's
different filesystem UUID, ESP hash and existing home files; payload checksums
and an offline ext4 check passed afterward. The updated disk booted through BIOS
and read-only UEFI/USB with 2 GiB RAM.

For each release, build through the real recipe and check tmpfs capacity and bake
memory. Boot BIOS and UEFI with virtio, SATA, NVMe and USB storage. Verify squashfs
is read-only, `/` is writable OverlayFS, `/root` is the ext4 partition root,
the ESP stays unmounted, `/etc` changes vanish and home persists. Boot whole-media
read-only with 2 GiB RAM and compare the complete disk hash before/after.

Flash a compact export onto a larger disposable disk; verify GPT/partition and
filesystem expansion, persistent home, idempotent reboots, and offline payload
updates preserving UUID, ESP and home. Check full/inode-exhausted/read-only/I/O
fallback and interrupted growth/update recovery. Check clean shutdown unmounts
before hashing writable media. Confirm unrelated swap, RAID and LVM disks stay
inactive. Physical USB, keyboard, Ethernet and Wi-Fi tests must be recorded
separately from QEMU checks; the earlier layout was tested on physical hardware.
