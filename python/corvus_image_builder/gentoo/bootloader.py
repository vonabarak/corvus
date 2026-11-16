"""Install BIOS and UEFI bootloaders for the target image."""

from ..core import BuildContext
from ._files import link, write
from ._validation import validate_disk
from .base import GentooStage


class BootloaderStage(GentooStage):
    """Install BIOS and removable UEFI GRUB with a serial-console kernel entry.

    The generated entry expects the runtime root on the first virtio disk's
    third partition, /dev/vda3.
    """

    requires = frozenset({"configured"})
    produces = frozenset({"bootloader"})

    def validate(self, context: BuildContext) -> None:
        validate_disk(self.settings.disk)

    def execute(self, context: BuildContext) -> None:
        root = context.root
        self.command(
            context,
            "grub-install",
            "--target=i386-pc",
            f"--boot-directory={root}/boot",
            str(self.settings.disk),
        )
        self.command(
            context,
            "grub-install",
            "--target=x86_64-efi",
            f"--boot-directory={root}/boot",
            f"--efi-directory={root}/boot/efi",
            "--removable",
            "--no-nvram",
        )
        kernel = context.artifacts["kernel"]
        link(root / "boot/vmlinuz", str(kernel))
        kernel_args = " ".join(self.settings.kernel_args)
        write(
            root / "boot/grub/grub.cfg",
            (
                f"set timeout=0\n"
                f"set default=0\n"
                f"insmod part_gpt\n"
                f"insmod ext2\n"
                f"insmod search_label\n"
                f"serial --unit=0 --speed=115200\n"
                f"terminal_input serial console\n"
                f"terminal_output serial console\n"
                f"menuentry 'Gentoo Linux' {{\n"
                f"    search --set=root --label corvus-root\n"
                f"    linux /boot/{kernel} root=/dev/vda3 rootfstype=ext4 console=tty0 console=ttyS0,115200n8 {kernel_args}\n"
                f"}}\n"
            ),
        )
        context.artifacts["bootloader"] = root / "boot/grub/grub.cfg"
