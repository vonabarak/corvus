#!/bin/sh
grub-install --target=i386-pc --boot-directory="$SYSROOT/boot" "$DEV"

KERNEL=$(cd "$SYSROOT/boot" && ls -1t vmlinuz-* 2>/dev/null | grep -v '\.old$' | head -1)
test -n "$KERNEL"
ln -sf "$KERNEL" "$SYSROOT/boot/vmlinuz"

install -d "$SYSROOT/boot/grub"
cat > "$SYSROOT/boot/grub/grub.cfg" <<EOF
set timeout=0
set default=0

insmod part_gpt
insmod ext2
insmod search_label

serial --unit=0 --speed=115200
terminal_input serial console
terminal_output serial console

menuentry 'Gentoo Linux (custom kernel)' {
    search --set=root --label corvus-root
    linux /boot/$KERNEL root=/dev/vda2 rootfstype=ext4 console=tty0 console=ttyS0,115200n8
}
EOF
