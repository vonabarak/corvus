"""Settings for native Gentoo image flavors."""

from dataclasses import dataclass
from pathlib import Path


@dataclass(frozen=True)
class Settings:
    """Configure target disk, shared cache, kernel config, and native Gentoo flavor.

    Supported libc/init pairs are glibc/systemd and musl/OpenRC. Binary package
    preferences apply to both target and native builder package installations.
    """

    disk: Path
    cache: Path
    kernel_config: Path | None
    use_binary: bool = True
    fetch_binary: bool = True
    name: str = "gentoo-image"
    libc: str = "glibc"
    init: str = "systemd"
    esp_size_mib: int = 512
    kernel_args: tuple[str, ...] = ()

    def __post_init__(self) -> None:
        if self.esp_size_mib <= 0:
            raise ValueError("EFI partition size must be positive")
        if any(
            not arg or any(char.isspace() or char in "\"'{};\\" for char in arg)
            for arg in self.kernel_args
        ):
            raise ValueError("Kernel arguments must be individual literal tokens")
        if (self.libc, self.init) not in (("glibc", "systemd"), ("musl", "openrc")):
            raise ValueError("Unsupported libc/init combination")

    @property
    def variant(self) -> str:
        return "x86-64_musl" if self.libc == "musl" else "x86-64"

    @property
    def system_atoms(self) -> tuple[str, ...]:
        return (
            ("sys-libs/musl", "sys-apps/openrc", "sys-apps/sysvinit")
            if self.libc == "musl"
            else ("sys-libs/glibc", "sys-apps/systemd")
        )
