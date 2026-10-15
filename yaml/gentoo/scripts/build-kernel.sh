#!/bin/sh
KSRC=$(ls -1d /usr/src/linux-*-gentoo* 2>/dev/null | sort -V | tail -1)
test -n "$KSRC" -a -d "$KSRC" \
  || { echo "no gentoo-sources install at /usr/src/linux-*-gentoo"; ls /usr/src/; exit 1; }

install -m 0644 /tmp/gentoo-kernel-config "$KSRC/.config"
make -C "$KSRC" -j"$(nproc)" \
  ARCH=x86_64 olddefconfig

make -C "$KSRC" -j"$(nproc)" ARCH=x86_64 bzImage modules
make -C "$KSRC" -j"$(nproc)" ARCH=x86_64 \
  INSTALL_MOD_PATH="$SYSROOT" modules_install

KVER=$(make -C "$KSRC" -s ARCH=x86_64 kernelrelease)
test -n "$KVER"
install -d "$SYSROOT/boot"
install -m 0644 "$KSRC/arch/x86/boot/bzImage" \
  "$SYSROOT/boot/vmlinuz-$KVER"
install -m 0644 "$KSRC/System.map" \
  "$SYSROOT/boot/System.map-$KVER"
install -m 0644 "$KSRC/.config" \
  "$SYSROOT/boot/config-$KVER"
