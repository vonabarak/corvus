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
            f"""set timeout=0
set default=0
insmod part_gpt
insmod ext2
insmod search_label
serial --unit=0 --speed=115200
terminal_input serial console
terminal_output serial console
menuentry 'Gentoo Linux' {{
    search --set=root --label corvus-root
    linux /boot/{kernel} root=/dev/vda3 rootfstype=ext4 console=tty0 console=ttyS0,115200n8 {kernel_args}
}}
""",
        )
        context.artifacts["bootloader"] = root / "boot/grub/grub.cfg"
