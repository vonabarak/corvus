# Gentoo rescue USB image

Build and publish the 2 GiB raw disk and its runtime template:

```sh
make image IMAGE=gentoo-rescue
make image-check IMAGE=gentoo-rescue
```

The current payload fits this target with approximately 353 MiB free on ext4.
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

Repository sync verifies Gentoo's signed manifests and refreshes the trusted
release keys through HKPS. If Gentoo's keyserver is unreachable, select another
public keyserver with `GENTOO_KEYSERVER=hkps://keyserver.ubuntu.com`; this changes
the refresh endpoint, not the trusted keys or signature checks.

The registered glibc/systemd builder provides native tools and caches. Rescue
uses the Gentoo distribution binary kernel, its complete modules, linux-firmware,
and a generic, non-hostonly dracut initramfs. It does not use the VM-only kernel
used by other Corvus Gentoo images. The recipe checks required hardware options,
boot drivers, and squashfs/OverlayFS support before publication.

## Disk and runtime

| GPT partition | Size | Format | Use |
| --- | --- | --- | --- |
| 1 | 1 MiB | BIOS boot | BIOS GRUB |
| 2 | 64 MiB | FAT32 | Removable UEFI GRUB; normally unmounted |
| 3 | Remaining space | ext4, `rescue-data` | Kernel, initramfs, compressed root and persistent home |

The ESP holds `/EFI/BOOT/BOOTX64.EFI`, `/grub/grub.cfg` and both GRUB
module sets. Its menu waits 300 seconds, reports kernel/initramfs loading and
searches the ext4 UUID. The ext4 partition contains:

```text
boot/vmlinuz
boot/initramfs.img
rescue/root.squashfs
rescue/SHA256SUMS
home/root/
```

A custom generic dracut module mounts this partition under
`/run/initramfs/rescue-media`, mounts the squashfs file read-only through a loop
device, and supplies a tmpfs OverlayFS for `/`. These mounts survive switch-root.
A small, versioned shutdown initramfs inside the squashfs lets dracut release
them in dependency order without unpacking the boot archive's hardware firmware
into RAM again. The carrier is mounted
read-write when possible, otherwise read-only with journal replay disabled.
`/boot` binds its `boot/` directory. `/root` binds `home/root/` only if writable,
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
a **16 GiB tmpfs** at `/mnt/sysroot`, followed by zstd squashfs compression
(level 15, 1 MiB blocks, 2 GiB compressor memory). No staging system is written
to the target ext4 partition. The runtime image needs much less memory.

Local consoles log in automatically as root; the root password is locked. Use
only where unrestricted local root access is appropriate. SSH is disabled,
including socket activation. To enable key-based SSH manually:

```sh
install -d -m 700 /root/.ssh
# Put your public key in /root/.ssh/authorized_keys, then:
chmod 600 /root/.ssh/authorized_keys
ssh-keygen -A
systemctl unmask sshd.service
systemctl start sshd.service
```

Wired Ethernet uses DHCP through systemd-networkd. Configure Wi-Fi manually with
`iw` and `wpa_supplicant`; DHCP can be enabled for the selected wireless interface
with a networkd `.network` file. QEMU guest-agent is enabled for VM testing.
Recovery disks are not mounted automatically; swap, RAID assembly and LVM
activation require explicit commands. Filesystem repair, partitioning, data
recovery, encryption, RAID/LVM, device diagnostics, networking and archive tools
are installed. No graphical desktop or ZFS is included.

Requires x86-64, BIOS or UEFI with Secure Boot disabled. Start with 4 GiB RAM;
2 GiB is the minimum target and available RAM limits volatile writes. There is
no persistent system overlay. Initial installation replaces the stick; subsequent
updates replace only the three payload files offline.

## Export and write a USB stick

Find the published disk placement with `crv disk show gentoo-rescue -o json`.
On its node, export a compact copy and update archive (the registered disk remains
2 GiB). Root is needed for loop mounts and resizing; install util-linux, e2fsprogs,
gptfdisk, coreutils, tar and udev on the export host:

```sh
sudo make -C yaml/gentoo export IMAGE=gentoo-rescue \
  SOURCE_IMAGE=/path/to/published-image.raw
```

Outputs are `yaml/gentoo/build/gentoo-rescue/gentoo-rescue.img` and
`gentoo-rescue-update.tar`. The export checks the payload hashes, shrinks ext4
leaving only 64 MiB above its minimum size, and relocates the backup GPT after truncation.
Shrinking repeats until ext4's minimum size stops decreasing.
The archive contains only the three payloads and `rescue/SHA256SUMS`; it excludes
home, GRUB and the ESP. `OUTPUT_DIR=/absolute/path` overrides the output directory.

Use an unmounted USB stick larger than the compact image. Replace `/dev/sdX`
with the **whole USB device**; this destroys its contents:

```sh
sudo dd if=yaml/gentoo/build/gentoo-rescue/gentoo-rescue.img of=/dev/sdX bs=4M conv=fsync status=progress
sync
sudo cmp -n "$(stat -c %s yaml/gentoo/build/gentoo-rescue/gentoo-rescue.img)" \
  yaml/gentoo/build/gentoo-rescue/gentoo-rescue.img /dev/sdX
```

The first writable boot expands storage to use the remaining device capacity.
No conversion of the old four-partition layout is provided: back up home and
reflash once to adopt this layout.

## Offline payload updates

Boot another OS, unmount the rescue system, and mount its ext4 partition at
`/mnt/rescue`. Do not update the squashfs backing a running rescue session.
Keep a backup of the previous payloads. Extract and verify on another filesystem,
then stage all three files before renaming them:

```sh
mkdir update
tar -xf gentoo-rescue-update.tar -C update
(cd update && sha256sum -c rescue/SHA256SUMS)
sudo cp update/boot/vmlinuz /mnt/rescue/boot/vmlinuz.new
sudo cp update/boot/initramfs.img /mnt/rescue/boot/initramfs.img.new
sudo cp update/rescue/root.squashfs /mnt/rescue/rescue/root.squashfs.new
# Check the copies before replacing any active filename.
for path in boot/vmlinuz boot/initramfs.img rescue/root.squashfs; do
  sudo cmp "update/$path" "/mnt/rescue/$path.new" || exit 1
done
sync
for path in boot/vmlinuz boot/initramfs.img rescue/root.squashfs; do
  sudo mv "/mnt/rescue/$path.new" "/mnt/rescue/$path"
done
sudo cp update/rescue/SHA256SUMS /mnt/rescue/rescue/SHA256SUMS
sync
sudo umount /mnt/rescue
```

Have enough free space for the staged files. Each rename is atomic, but the three
renames together are not: do not interrupt this step, and restore a complete
matching set if interrupted. Existing home, filesystem UUID and ESP remain in
place. New payloads do not embed the exporting stick's UUID; the existing ESP
configuration supplies the destination UUID at boot.

## Validation

```sh
make yaml-schema-check
make -C yaml/gentoo test IMAGE=gentoo-rescue
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
is read-only, `/` is writable OverlayFS, `/boot` and `/root` share the carrier,
the ESP stays unmounted, `/etc` changes vanish and home persists. Boot whole-media
read-only with 2 GiB RAM and compare the complete disk hash before/after.

Flash a compact export onto a larger disposable disk; verify GPT/partition and
filesystem expansion, persistent home, idempotent reboots, and offline payload
updates preserving UUID, ESP and home. Check full/inode-exhausted/read-only/I/O
fallback and interrupted growth/update recovery. Check clean shutdown unmounts
before hashing writable media. Confirm unrelated swap, RAID and LVM disks stay
inactive. Physical USB, keyboard, Ethernet and Wi-Fi tests must be recorded
separately from QEMU checks; the earlier layout was tested on physical hardware.
