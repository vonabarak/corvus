# Corvus image builder

Standalone Python library for native Gentoo image construction inside a disposable
amd64 glibc/systemd builder VM. It has no Corvus RPC or pip runtime dependencies;
the VM supplies Portage, compiler, partitioning, filesystem and GRUB tools.

Run the parallel managed pipeline from `yaml/gentoo-python`:

```sh
make plan
make build
```

The Makefile ensures the registered Gentoo builder dependency through its owning
recipe and bundles this package before submission. Output is named
`gentoo-glibc-systemd-headless-python`; canonical images are unaffected. BIOS and
UEFI boot files are installed; the recipe registers a template for each firmware.
The disk is GPT with BIOS, EFI and ext4 partitions.
The recipe configures DHCP, resolved, SSH, QGA, serial console and UTF-8 locales.
Root password login is locked, and machine identity and SSH host keys are reset.

For execution in an already provisioned builder VM:

```sh
python3 image-builder.pyz run corvus_image_builder.gentoo:headless \
  --root /mnt/sysroot --disk /dev/vdb --kernel-config /run/corvus-kernel-config
```

This formats the specified disk. The library rejects mounted disks and requires
an empty mount directory. Run only in a disposable builder VM: native Portage
configuration is intentionally changed. The VM needs the `corvus-cache` virtiofs
share, or an existing mount at `--cache`. A shared build lock serializes cache
updates. Completed builds unmount filesystems; failures preserve them for diagnosis
and release the lock. Logs go to the Corvus shell task, outside the image.

`Stage` exposes validate/execute/verify hooks, requires/produces artifact names,
and owned cleanup callbacks through `BuildContext`. `ImageRecipe` orders stage
instances. Subclass `PackageInstallStage` and override `atoms`, or replace a stage
in the list returned by `headless(Settings(...))`. Custom recipe factories take no
arguments and return `ImageRecipe`; the built-in headless CLI accepts explicit
settings. `plan` validates ordering without requiring root or importing Portage.

The hybrid backend uses system Portage Python APIs for installed package and
metadata queries, and subprocess `emerge` for complete installation. Kernel
selection retains Portage's `best_visible` query. Signed repository and remote
binary verification are required; unsigned locally compiled packages are allowed.
Kernel archives are published atomically and keyed by ebuild revision and config.

This pilot implements the headless runtime; builder preparation still uses the
existing registered builder, and cloud-init variants are not migrated. Keep the
canonical shell and Catalyst recipes until live boot parity has been established.

For use as a library, install with `pip install ./image_builder`. For example,
a custom recipe module can extend the package set while retaining the other stages:

```python
from pathlib import Path
from corvus_image_builder.gentoo import PackageInstallStage, Settings, headless


class MonitoringPackages(PackageInstallStage):
    atoms = (*PackageInstallStage.atoms, "sys-process/htop")


def build():
    settings = Settings(
        disk=Path("/dev/vdb"),
        cache=Path("/mnt/corvus-cache"),
        kernel_config=Path("/run/corvus-kernel-config"),
    )
    recipe = headless(settings)
    recipe.stages = [
        MonitoringPackages(settings) if type(stage) is PackageInstallStage else stage
        for stage in recipe.stages
    ]
    return recipe
```

Run it with `corvus-image-builder run my_recipe:build --root /mnt/sysroot` in
the builder VM, with the module installed or available on `PYTHONPATH`.
