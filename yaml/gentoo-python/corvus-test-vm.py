"""Build the small immutable musl/OpenRC guest used by the integration harness."""

import os
import shutil
import subprocess
from pathlib import Path

from corvus_image_builder.core import BuildContext, BuildManager, ImageRecipe
from corvus_image_builder.gentoo import (
    BootloaderStage,
    ChrootEnvironmentStage,
    DiskStage,
    FinalizeStage,
    KernelStage,
    OpenRCConfigurationStage,
    PackageInstallStage,
    RepositoryStage,
    Settings,
    WorkspaceStage,
    link,
    remove,
    write,
)
from corvus_image_builder.portage import PortageBackend


class TestVmRepository(RepositoryStage):
    """Apply the minimal guest's feature policy before resolving any packages."""

    def execute(self, context: BuildContext) -> None:
        write(
            Path("/etc/portage/package.use/corvus-test-vm"),
            """sys-apps/lshw -gtk
app-crypt/tpm2-tss -fapi -policy
app-crypt/tpm2-tools -fapi
app-admin/doas -pam
sys-apps/busybox -make-symlinks -pam -mdev -syslog
""",
        )
        super().execute(context)


class TestVmPackages(PackageInstallStage):
    """Install explicit musl runtime atoms, never @system or a target toolchain."""

    requires = frozenset({"disk", "interfaces"})
    base_atoms = (
        "sys-apps/baselayout",
        "sys-libs/musl",
        "sys-apps/openrc",
        "sys-apps/sysvinit",
        "virtual/udev",
        "app-shells/bash",
        "sys-apps/coreutils",
        "sys-apps/findutils",
        "sys-apps/grep",
        "sys-apps/sed",
        "sys-apps/gawk",
        "app-alternatives/awk",
        "sys-apps/util-linux",
        "sys-apps/kbd",
        "sys-apps/shadow",
        "sys-process/procps",
        "sys-apps/iproute2",
        "net-misc/dhcpcd",
        "net-misc/openssh",
        "net-misc/socat",
        "app-admin/sysklogd",
        "app-misc/ca-certificates",
        "app-emulation/qemu-guest-agent",
        "sys-fs/e2fsprogs",
        "sys-fs/dosfstools",
    )
    atoms = (
        "app-admin/doas",
        "app-crypt/tpm2-tools",
        "app-crypt/tpm2-tss",
        "sys-power/acpid",
        "sys-apps/diffutils",
        "sys-apps/pciutils",
        "sys-apps/lshw",
        "sys-boot/efibootmgr",
        "net-misc/iputils",
        "sys-apps/busybox",
    )

    def install_system(self, context: BuildContext) -> None:
        self.packages(context, self.base_atoms)

    def execute(self, context: BuildContext) -> None:
        super().execute(context)
        for library in ("libstdc++.so.6", "libgcc_s.so.1"):
            source = Path(
                subprocess.check_output(
                    ["gcc", f"-print-file-name={library}"], text=True
                ).strip()
            )
            if not source.is_file():
                raise ValueError(f"Native compiler runtime is missing: {library}")
            destination = context.root / "usr/lib" / library
            shutil.copyfile(source, destination)
            destination.chmod(0o755)
        self.chroot_string(context, "/usr/sbin/lshw -version")
        applets = subprocess.check_output(
            ["chroot", str(context.root), "/bin/busybox", "--list"], text=True
        ).splitlines()
        if "nslookup" not in applets:
            raise ValueError("BusyBox lacks nslookup")
        link(context.root / "bin/nslookup", "busybox")

    def verify(self, context: BuildContext) -> None:
        super().verify(context)
        backend = PortageBackend(context.runner, context.root)
        missing = [atom for atom in self.base_atoms if not backend.installed(atom)]
        if missing:
            raise RuntimeError(f"Minimal runtime packages are missing: {missing}")


class TestVmConfiguration(OpenRCConfigurationStage):
    """Add harness credentials, DHCPv4 boot, doas and ACPI power-button support."""

    def validate(self, context: BuildContext) -> None:
        if not Path("/run/corvus-test-key.pub").is_file():
            raise ValueError("Missing integration harness SSH public key")

    def execute(self, context: BuildContext) -> None:
        super().execute(context)
        root = context.root
        self.chroot_string(context, "useradd --create-home --shell /bin/sh corvus")
        self.chroot_string(context, "chpasswd", input=b"corvus:corvus\n")
        (root / "home/corvus/.ssh").mkdir(parents=True, exist_ok=True)
        (root / "home/corvus/.ssh").chmod(0o700)
        shutil.copyfile(
            "/run/corvus-test-key.pub", root / "home/corvus/.ssh/authorized_keys"
        )
        (root / "home/corvus/.ssh/authorized_keys").chmod(0o600)
        self.chroot_string(context, "chown -R corvus:corvus /home/corvus")
        write(
            root / "etc/doas.conf",
            "permit nopass setenv { PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin } corvus\n",
        )
        (root / "etc/doas.conf").chmod(0o400)
        self.chroot_string(context, "doas -C /etc/doas.conf")
        ssh = root / "etc/ssh/sshd_config"
        write(
            ssh,
            "PasswordAuthentication yes\nPermitRootLogin no\nSetEnv PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin\n"
            + ssh.read_text(),
        )
        write(
            root / "etc/profile.d/corvus-test.sh",
            "export PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin\n",
        )
        write(
            root / "etc/dhcpcd.conf",
            """ipv4only
noipv4ll
waitip 4
timeout 20
clientid
hostname
option domain_name_servers, domain_name, domain_search
option routers, subnet_mask, classless_static_routes
""",
        )
        with (root / "etc/hosts").open("a") as stream:
            stream.write("127.0.0.1 corvus-test-vm\n")
        write(
            root / "etc/acpi/events/corvus-power-button",
            "event=button/power.*\naction=/etc/acpi/actions/corvus-power-button.sh\n",
        )
        write(
            root / "etc/acpi/actions/corvus-power-button.sh",
            "#!/bin/sh\nexec /sbin/poweroff\n",
        )
        (root / "etc/acpi/actions/corvus-power-button.sh").chmod(0o755)
        (root / "etc/acpi/events/default").unlink(missing_ok=True)
        self.chroot_string(context, "rc-update add acpid default")


class TestVmFinalize(FinalizeStage):
    """Reject unwanted runtimes before stripping package records and build files."""

    forbidden = (
        "sys-devel/gcc",
        "sys-devel/binutils",
        "llvm-core/clang",
        "dev-lang/rust",
        "dev-lang/rust-bin",
        "dev-lang/go",
        "sys-apps/portage",
        "sys-libs/glibc",
        "sys-apps/systemd",
        "app-admin/sudo",
        "dev-lang/python",
    )

    def clean(self, context: BuildContext) -> None:
        backend = PortageBackend(context.runner, context.root)
        unexpected = [atom for atom in self.forbidden if backend.installed(atom)]
        if unexpected:
            raise RuntimeError(f"Unexpected runtime packages: {unexpected}")
        super().clean(context)
        for relative in (
            "usr/share/doc",
            "usr/share/man",
            "usr/share/info",
            "usr/src",
            "usr/include",
            "etc/portage",
            "var/db/pkg",
            "var/lib/portage",
        ):
            remove(context.root, relative)
        for directory in ("usr/lib", "usr/lib64"):
            for path in (context.root / directory).rglob("*"):
                if path.is_file() and path.suffix in (".a", ".la", ".pc"):
                    remove(context.root, str(path.relative_to(context.root)))
        for directory in ("usr/share/locale", "usr/lib/locale"):
            base = context.root / directory
            if not base.exists():
                continue
            for path in base.iterdir():
                if path.name not in (
                    "en",
                    "C",
                    "C.utf8",
                    "C.UTF-8",
                ) and not path.name.startswith(("en_", "en@")):
                    remove(context.root, str(path.relative_to(context.root)))


def build_recipe(settings: Settings) -> ImageRecipe:
    """Compose a minimal from-scratch image without requiring Gentoo for planning."""
    return ImageRecipe(
        [
            stage(settings)
            for stage in (
                WorkspaceStage,
                TestVmRepository,
                DiskStage,
                ChrootEnvironmentStage,
                TestVmPackages,
                KernelStage,
                TestVmConfiguration,
                BootloaderStage,
                TestVmFinalize,
            )
        ]
    )


if __name__ == "__main__":
    settings = Settings(
        name="corvus-test-vm",
        libc="musl",
        init="openrc",
        disk=Path(os.environ.get("TARGET_DISK", "/dev/vdb")),
        cache=Path(os.environ.get("IMAGE_CACHE", "/mnt/corvus-cache")),
        kernel_config=Path(
            os.environ.get("KERNEL_CONFIG", "/run/corvus-kernel-config")
        ),
        esp_size_mib=64,
        kernel_args=("net.ifnames=0",),
        use_binary=os.environ.get("USEPKG", "true") == "true",
        fetch_binary=os.environ.get("GETBINPKG", "true") == "true",
    )
    context = BuildContext(Path(os.environ.get("IMAGE_ROOT", "/mnt/sysroot")).resolve())
    manager = BuildManager()
    recipe = build_recipe(settings)
    if os.environ.get("IMAGE_BUILD_PLAN") == "1":
        print("\n".join(manager.plan(recipe, context)))
    else:
        manager.run(recipe, context)
