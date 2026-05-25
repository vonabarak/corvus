#!/bin/bash
# Build the synthetic-installer ISO used by
# `integration_tests/tests/test_build_installer.py`.
#
# What the ISO does: BIOS-boots via isolinux, the kernel hands
# control to /init (the cpio-packed busybox shell script from
# yaml/corvus-test-installer/init.sh), which mounts the floppy
# supplied by `crv build`, copies the marker payload onto
# /dev/vda, and powers off. The bake template's drives list a
# CD-ROM, and corvus's command builder now emits `-boot
# order=dc` for any VM with a CD-ROM attached, so seabios picks
# the synthetic-installer ISO before the (blank) target HD.
#
# Why a script rather than a `crv build` pipeline: producing the
# tiny ISO from scratch only needs a kernel + busybox + isolinux,
# none of which the corvus daemon needs to know about. Baking it
# in a VM would emerge several packages and run for minutes;
# this finishes in ~30 s on a cold cache.
#
# Output: `~/VMs/BaseImages/SyntheticInstaller/corvus-test-installer-iso.raw`
# (~13 MB), registered with the outer daemon as
# `corvus-test-installer-iso`. The path is chosen so the
# integration harness's `register_base_images()` (which walks
# `~/VMs/BaseImages/<dir>/*.raw|.qcow2|.img`) picks it up
# automatically for the inner daemon inside the test-node VM —
# no second registration step in the test.
#
# Tools required on host: curl, tar, gzip, cpio, mkisofs (or
# genisoimage), and the `crv` CLI on PATH.

set -euo pipefail

DISK_NAME=corvus-test-installer-iso

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
YAML_DIR="$REPO_ROOT/yaml/corvus-test-installer"
BUILD_DIR="$REPO_ROOT/build/synthetic-installer"
CACHE_DIR="$REPO_ROOT/build/synthetic-installer-cache"

# The host-resident path of the assembled ISO. Lives under
# ~/VMs/BaseImages/ so the integration harness's
# `register_base_images()` (which walks that tree) registers it
# with the inner daemon automatically — the test then references
# it by name in the build YAML without any explicit `disk
# register` step.
BASE_IMAGES_DIR=${CORVUS_BASE_IMAGES_DIR:-$HOME/VMs/BaseImages}
ISO_DIR="$BASE_IMAGES_DIR/SyntheticInstaller"
ISO_PATH="$ISO_DIR/$DISK_NAME.raw"

# Alpine v3.21 packages. The kernel is `linux-virt` (KVM-optimised,
# ~10 MB); busybox-static gives us /bin/busybox; syslinux gives
# us isolinux.bin + ldlinux.c32. Pinned versions for
# reproducibility — bump when 404s start happening and Alpine's
# archive moves on.
ALPINE_MIRROR=${ALPINE_MIRROR:-https://dl-cdn.alpinelinux.org/alpine}
ALPINE_VERSION=${ALPINE_VERSION:-v3.21}
ALPINE_ARCH=${ALPINE_ARCH:-x86_64}
ALPINE_KERNEL_PKG=${ALPINE_KERNEL_PKG:-linux-virt-6.12.91-r0.apk}
ALPINE_BUSYBOX_PKG=${ALPINE_BUSYBOX_PKG:-busybox-static-1.37.0-r14.apk}
ALPINE_SYSLINUX_PKG=${ALPINE_SYSLINUX_PKG:-syslinux-6.04_pre1-r16.apk}

log() { printf '[build-synthetic-installer] %s\n' "$*" >&2; }
die() { log "error: $*"; exit 1; }

need() {
  command -v "$1" >/dev/null 2>&1 || die "missing tool: $1 (install on host before re-running)"
}

# Pick mkisofs or genisoimage (Debian/Ubuntu ships only the latter).
if command -v mkisofs >/dev/null 2>&1; then
  MKISOFS=mkisofs
elif command -v genisoimage >/dev/null 2>&1; then
  MKISOFS=genisoimage
else
  die "missing tool: install mkisofs (cdrtools) or genisoimage"
fi

need curl
need tar
need gzip
need cpio
need crv

# ── 1. Fetch apks into cache (idempotent) ─────────────────────────────────
mkdir -p "$CACHE_DIR"

fetch() {
  local pkg=$1
  local out=$CACHE_DIR/$pkg
  if [ -f "$out" ]; then
    log "cache hit: $pkg"
    return
  fi
  local url=$ALPINE_MIRROR/$ALPINE_VERSION/main/$ALPINE_ARCH/$pkg
  log "fetch: $url"
  curl -fsSL -o "$out.tmp" "$url" \
    || die "download failed: $url (check ALPINE_*_PKG envs or browse $ALPINE_MIRROR/$ALPINE_VERSION/main/$ALPINE_ARCH/)"
  mv "$out.tmp" "$out"
}

fetch "$ALPINE_KERNEL_PKG"
fetch "$ALPINE_BUSYBOX_PKG"
fetch "$ALPINE_SYSLINUX_PKG"

# ── 2. Extract apks. Alpine .apk is a gzip-compressed tarball with a
# few control-only sub-streams concatenated in front; GNU tar reads
# straight through and gives us the data members. ─────────────────────────
EXTRACT_DIR=$BUILD_DIR/extracted
rm -rf "$BUILD_DIR"
mkdir -p "$EXTRACT_DIR/kernel" "$EXTRACT_DIR/busybox" "$EXTRACT_DIR/syslinux"
tar -xzf "$CACHE_DIR/$ALPINE_KERNEL_PKG"    -C "$EXTRACT_DIR/kernel"   2>/dev/null || true
tar -xzf "$CACHE_DIR/$ALPINE_BUSYBOX_PKG"   -C "$EXTRACT_DIR/busybox"  2>/dev/null || true
tar -xzf "$CACHE_DIR/$ALPINE_SYSLINUX_PKG"  -C "$EXTRACT_DIR/syslinux" 2>/dev/null || true

# Locate the actual artifacts. `find -print -quit` returns the first match.
VMLINUZ=$(find "$EXTRACT_DIR/kernel" -name 'vmlinuz-*' -type f -print -quit)
BUSYBOX=$(find "$EXTRACT_DIR/busybox" -name 'busybox.static' -type f -print -quit)
ISOLINUX_BIN=$(find "$EXTRACT_DIR/syslinux" -name 'isolinux.bin' -type f -print -quit)
LDLINUX_C32=$(find "$EXTRACT_DIR/syslinux" -name 'ldlinux.c32' -type f -print -quit)
# Alpine's linux-virt is trimmed for virtualisation — floppy +
# fat/vfat aren't compiled in, they're modules under
# /lib/modules/<ver>/. Stage the three we need into the initramfs
# so /init can insmod them.
FLOPPY_KO=$(find "$EXTRACT_DIR/kernel" -name 'floppy.ko*' -print -quit)
VIRTIO_BLK_KO=$(find "$EXTRACT_DIR/kernel" -name 'virtio_blk.ko*' -print -quit)
FAT_KO=$(find "$EXTRACT_DIR/kernel" -name 'fat.ko*' -print -quit)
VFAT_KO=$(find "$EXTRACT_DIR/kernel" -name 'vfat.ko*' -print -quit)
NLS_UTF8_KO=$(find "$EXTRACT_DIR/kernel" -name 'nls_utf8.ko*' -print -quit)
NLS_CP437_KO=$(find "$EXTRACT_DIR/kernel" -name 'nls_cp437.ko*' -print -quit)

[ -n "$VMLINUZ" ]      || die "vmlinuz not found inside $ALPINE_KERNEL_PKG"
[ -n "$BUSYBOX" ]      || die "busybox.static not found inside $ALPINE_BUSYBOX_PKG"
[ -n "$ISOLINUX_BIN" ] || die "isolinux.bin not found inside $ALPINE_SYSLINUX_PKG"
[ -n "$LDLINUX_C32" ]  || die "ldlinux.c32 not found inside $ALPINE_SYSLINUX_PKG"
[ -n "$FLOPPY_KO" ]    || die "floppy.ko not found inside $ALPINE_KERNEL_PKG"
[ -n "$VIRTIO_BLK_KO" ] || die "virtio_blk.ko not found inside $ALPINE_KERNEL_PKG"
[ -n "$FAT_KO" ]       || die "fat.ko not found inside $ALPINE_KERNEL_PKG"
[ -n "$VFAT_KO" ]      || die "vfat.ko not found inside $ALPINE_KERNEL_PKG"
[ -n "$NLS_UTF8_KO" ]  || die "nls_utf8.ko not found inside $ALPINE_KERNEL_PKG"
[ -n "$NLS_CP437_KO" ] || die "nls_cp437.ko not found inside $ALPINE_KERNEL_PKG"

# ── 3. Assemble the initramfs ────────────────────────────────────────────
INITRAMFS=$BUILD_DIR/initramfs
# Pre-create the mount points + /tmp so `mount -t proc /proc`
# etc. don't fail on a missing target directory in init.sh.
mkdir -p \
  "$INITRAMFS/bin" \
  "$INITRAMFS/proc" \
  "$INITRAMFS/sys" \
  "$INITRAMFS/dev" \
  "$INITRAMFS/tmp" \
  "$INITRAMFS/mnt/floppy"
cp "$BUSYBOX" "$INITRAMFS/bin/busybox"
chmod +x "$INITRAMFS/bin/busybox"
# Symlink the applets the init script calls. We don't run
# `busybox --install -s` at boot to keep init.sh small and obvious.
for applet in sh mount dd sync poweroff cat mkdir printf ls sleep insmod; do
  ln -sf busybox "$INITRAMFS/bin/$applet"
done
# Stage the floppy + fat/vfat + nls_utf8 kernel modules so
# init.sh can insmod them. The trimmed Alpine virt kernel
# doesn't compile them in. Alpine ships them gzipped; busybox
# insmod doesn't handle .ko.gz reliably, so unzip them into the
# initramfs as plain .ko files.
mkdir -p "$INITRAMFS/lib/modules"
stage_module() {
  local src=$1
  local base
  base=$(basename "$src")
  base=${base%.gz}
  if [ "$src" != "${src%.gz}" ]; then
    gzip -dc "$src" > "$INITRAMFS/lib/modules/$base"
  else
    cp "$src" "$INITRAMFS/lib/modules/$base"
  fi
}
stage_module "$NLS_UTF8_KO"
stage_module "$NLS_CP437_KO"
stage_module "$FAT_KO"
stage_module "$VFAT_KO"
stage_module "$FLOPPY_KO"
stage_module "$VIRTIO_BLK_KO"
cp "$YAML_DIR/init.sh" "$INITRAMFS/init"
chmod +x "$INITRAMFS/init"

INITRD=$BUILD_DIR/initrd.img
( cd "$INITRAMFS" && find . | cpio -o -H newc 2>/dev/null | gzip -9 > "$INITRD" )

# ── 4. Assemble the ISO tree ─────────────────────────────────────────────
ISO_TREE=$BUILD_DIR/iso
mkdir -p "$ISO_TREE/isolinux"
cp "$VMLINUZ"      "$ISO_TREE/vmlinuz"
cp "$INITRD"       "$ISO_TREE/initrd.img"
cp "$ISOLINUX_BIN" "$ISO_TREE/isolinux/isolinux.bin"
cp "$LDLINUX_C32"  "$ISO_TREE/isolinux/ldlinux.c32"
cp "$YAML_DIR/isolinux.cfg" "$ISO_TREE/isolinux/isolinux.cfg"

# isolinux.bin needs the boot-info-table patched in to know its own
# load offset. -no-emul-boot + -boot-load-size 4 is the standard
# El-Torito recipe for isolinux. -R (Rock Ridge) + -J (Joliet)
# preserve the lowercase /vmlinuz, /initrd.img, /isolinux/...
# paths the isolinux config references; without them the files get
# mangled to 8.3 uppercase and isolinux can't find the kernel.
mkdir -p "$ISO_DIR"
$MKISOFS \
  -o "$ISO_PATH" \
  -V CORVUS-INSTALLER \
  -R -J \
  -b isolinux/isolinux.bin \
  -c isolinux/boot.cat \
  -no-emul-boot \
  -boot-load-size 4 \
  -boot-info-table \
  -quiet \
  "$ISO_TREE"

log "built $ISO_PATH ($(du -h "$ISO_PATH" | cut -f1))"

# ── 5. Register with the outer daemon (idempotent) ───────────────────────
# mkisofs overwrote $ISO_PATH in place, so an existing
# registration (which is just a DB row pointing at this file)
# still resolves to the fresh content — no need to delete +
# re-register, which would also delete the file we just built
# (`crv disk delete` calls the agent's image-delete RPC).
if crv -o json disk show "$DISK_NAME" >/dev/null 2>&1; then
  log "disk $DISK_NAME already registered; skipping register"
else
  crv disk register "$DISK_NAME" "$ISO_PATH" --format raw
  log "registered $DISK_NAME"
fi
