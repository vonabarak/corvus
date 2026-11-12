# Gentoo rescue USB image

Build and publish the 12 GiB raw disk and its BIOS/UEFI templates:

```sh
make image IMAGE=gentoo-rescue
make image-check IMAGE=gentoo-rescue
```

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
boot drivers, and systemd volatile-root support before publication.

## Disk and runtime

| GPT partition | Size | Format | Use |
| --- | --- | --- | --- |
| 1 | 1 MiB | BIOS boot | BIOS GRUB |
| 2 | 64 MiB | FAT32 | Removable UEFI GRUB; normally unmounted |
| 3 | 8 GiB | ext4 | System, kernel, initramfs, GRUB configuration |
| 4 | Remaining space (about 3.9 GiB) | ext4 | Persistent `/root` |

The original system filesystem stays read-only with journal replay disabled
(`ro,noload`). A RAM OverlayFS provides writable system state. Changes outside
`/root`, including installed packages, logs, configuration, and SSH host keys,
disappear after reboot. The separate home partition is mounted read-write only
when usable. If it is missing, damaged, full, or read-only, `/root` is an empty
RAM filesystem: saved home files are hidden and new changes disappear on reboot.
Home setup has bounded device/mount waits and never formats or repairs storage.
The home filesystem is initially empty apart from ext4's `lost+found` directory.

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
no persistent system overlay, automatic partition expansion, or in-place upgrade.
**Back up `/root` before reflashing: writing the whole image replaces home too.**

## Export and write a USB stick

Inspect the published disk and find its placement's `file_path`:

```sh
crv disk show gentoo-rescue -o json
```

On the node holding that placement, compress the raw disk without changing the
registered file. Substitute its actual path for `/path/to/published-image.raw`:

```sh
xz -T0 -c /path/to/published-image.raw > gentoo-rescue.img.xz
```

Use an unmounted USB stick of at least 12 GiB (a normal 16 GB stick suffices).
Replace `/dev/sdX` with the **whole USB device**; this destroys its contents:

```sh
xz -dc gentoo-rescue.img.xz | sudo dd of=/dev/sdX bs=4M conv=fsync status=progress
sync
xz -dc gentoo-rescue.img.xz | sudo cmp -n 12884901888 - /dev/sdX
```

## Validation

```sh
make yaml-schema-check
make -C yaml/gentoo test IMAGE=gentoo-rescue
```

The home regression checks cover healthy, absent, read-only, damaged, full,
inode-exhausted and I/O-failing storage, including failed unmounts. They simulate
OS/device boundaries and do not substitute for boot tests.

For each built release, boot both runtime templates and test whole-medium
read-only operation. Exercise virtio, SATA, NVMe and USB-backed boot devices;
verify the lower ext4 mount is `ro,noload`, `/` is writable OverlayFS, the ESP
stays unmounted, `/etc` changes vanish on reboot and `/root` changes persist only
with writable home. Reboot against missing, corrupt and full home partitions and
check that RAM fallback is empty. Check system/ESP hashes before and after boot.
Attach disposable swap, RAID and LVM disks and confirm none are activated; use
separate scratch disks for partition, filesystem, encryption and recovery tools.

Physical USB boot, keyboard, Ethernet and Wi-Fi tests must be recorded separately
from QEMU checks. VM success alone does not establish physical hardware coverage.

Implementation validation on 2026-11-11 used stable kernel
`6.18.63-gentoo-dist-bin` and QEMU with 4 GiB RAM. BIOS/virtio, BIOS/SATA,
UEFI/NVMe and UEFI/read-only USB boots passed. Writable boots preserved `/root`
and discarded `/etc` changes; the read-only USB boot provided an empty tmpfs
home and writable volatile system. All four boots reported no failed systemd
units. System and ESP hashes matched the unbooted image after writable boots.
A BIOS/virtio read-only boot also passed with 2 GiB RAM.
Physical hardware checks remain pending.
