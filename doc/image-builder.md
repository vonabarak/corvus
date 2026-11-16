# Python image builder

[`corvus_image_builder`](../python/corvus_image_builder/) is a reusable Python
library for building disk images from composable stages. Common work such as
partitioning, package installation, kernel compilation, and bootloader setup
lives in the library. Each image flavor owns a Python script that chooses its
packages, settings, and stage order. The package has no CLI or `main()` function.

The current implementation builds native amd64 Gentoo glibc/systemd and
musl/OpenRC images inside disposable Gentoo builder VMs. It uses Portage's Python
API for package queries and isolated `emerge` subprocesses for installation.
It does not use Catalyst. Corvus handles builder VM orchestration, input uploads,
target disk allocation, compaction, and image publication through build YAML.
See [image builds](image-builds.md) for the Corvus pipeline format and
[managed image recipes](test-images.md#import-ownership) for dependency ownership.

## Architecture

The implementation separates execution, Portage access, and Gentoo stages:

| Module | Responsibility |
|---|---|
| [`core.py`](../python/corvus_image_builder/core.py) | Stage contract, ordered recipes, execution, shared artifacts, subprocesses, and resource cleanup |
| [`portage.py`](../python/corvus_image_builder/portage.py) | Installed-package queries, metadata, and package installation requests |
| [`gentoo/`](../python/corvus_image_builder/gentoo/) | Native builder validation and concrete Gentoo image stages |

The Gentoo package exports its public classes through
[`gentoo/__init__.py`](../python/corvus_image_builder/gentoo/__init__.py), so flavor
scripts can import them from `corvus_image_builder.gentoo`. Its implementation is
split by responsibility:

| Module | Responsibility |
|---|---|
| [`settings.py`](../python/corvus_image_builder/gentoo/settings.py) | Native flavor and build settings |
| [`base.py`](../python/corvus_image_builder/gentoo/base.py) | Shared Gentoo stage operations |
| [`workspace.py`](../python/corvus_image_builder/gentoo/workspace.py) | Builder validation, cache mounts, and locking |
| [`repository.py`](../python/corvus_image_builder/gentoo/repository.py) | Signed Portage repositories and synchronization |
| [`disk.py`](../python/corvus_image_builder/gentoo/disk.py) | Target partitioning, formatting, and mounts |
| [`mounts.py`](../python/corvus_image_builder/gentoo/mounts.py) | Owned mount groups and private target runtime interfaces |
| [`packages.py`](../python/corvus_image_builder/gentoo/packages.py) | Common package stage and glibc/musl bootstraps |
| [`kernel.py`](../python/corvus_image_builder/gentoo/kernel.py) | Kernel builds and caching |
| [`systemd.py`](../python/corvus_image_builder/gentoo/systemd.py) | glibc/systemd image configuration |
| [`openrc.py`](../python/corvus_image_builder/gentoo/openrc.py) | musl/OpenRC image configuration |
| [`bootloader.py`](../python/corvus_image_builder/gentoo/bootloader.py) | BIOS and UEFI GRUB installation |
| [`finalize.py`](../python/corvus_image_builder/gentoo/finalize.py) | Identity removal and image cleanup |

The exported file-writing, symlink, and guarded removal helpers live in
[`_files.py`](../python/corvus_image_builder/gentoo/_files.py).

Flavor scripts and their YAML recipes live in
[`yaml/gentoo-python`](../yaml/gentoo-python/). Each flavor uses matching `.py`,
`.yml`, and generated `.pyz` basenames. The bundle helper places the chosen
script at `__main__.py` alongside the library, including its subpackages. Python
runs that archive inside the builder VM without installing the library there.

`BuildContext` carries the target root, `CommandRunner`, an artifact dictionary,
and owned resource callbacks. `ImageRecipe` holds stage instances in execution
order. `BuildManager.plan()` checks that earlier stages declare every required
artifact and returns the stage names. Planning checks ordering only; it does
not run stage validation or inspect disks.

`BuildManager.run()` checks the plan, calls every stage's `validate()` before
executing any stage, then runs `execute()` and `verify()` for each stage in order.
The default verification checks that declared outputs exist in
`context.artifacts`. Package installation also verifies the installed atoms.
The manager does not sort stages, resume builds, or persist artifact state.
An artifact is an in-memory value, such as a path or kernel filename, shared
between stages; it is separate from a Corvus published disk image.

`WorkspaceStage` uses umask `022` during execution so newly created runtime
directories remain accessible to non-root users even when the provisioning
transport starts with umask `077`. Cleanup restores the caller's original umask.

Resources registered through `context.own()` are released in reverse order.
Cleanup runs on success, failure, and interruption. Gentoo mount callbacks use
`preserve_on_failure=True` to leave the failed builder available for diagnosis;
the shared cache lock is always released. `CommandRunner` runs each command in
its own process group and terminates that group on timeout or interruption.
The YAML recipes retain failed builder VMs with `cleanup: onSuccess`.

## Gentoo stages

Both headless recipes use this sequence, choosing the matching package and
configuration stages for the flavor:

| Stage | Output artifact | Work |
|---|---|---|
| `WorkspaceStage` | `workspace` | Validate root privileges, native profile, tools, and unused target disk; mount and lock the shared cache |
| `RepositoryStage` | `repository` | Configure builder Portage, initialize trust, and sync signed repositories |
| `DiskStage` | `disk` | Partition, format, and mount the target disk |
| `ChrootEnvironmentStage` | `interfaces` | Mount target runtime interfaces and package inputs without sharing the builder's `/run` |
| `GlibcPackageInstallStage` or `MuslPackageInstallStage` | `system` | Bootstrap the base system, install flavor atoms, and write target Portage configuration |
| `KernelStage` | `kernel` | Reuse or build the kernel and modules |
| `SystemdConfigurationStage` or `OpenRCConfigurationStage` | `configured` | Configure filesystems, locale, networking, and boot services |
| `BootloaderStage` | `bootloader` | Install BIOS and UEFI GRUB |
| `FinalizeStage` | `image` | Remove identities and residue, lock root, and record metadata |

The full dependency chain is:

```text
workspace -> repository -> disk -> interfaces -> system -> kernel -> configured -> bootloader -> image
```

`DiskStage` destroys the target disk's contents and creates GPT partitions for
BIOS boot, a FAT EFI system partition, and an ext4 root. It mounts the root at
`context.root` and the EFI partition below `boot/efi`.

`PackageInstallStage` is an abstract base that creates a merged `/usr` layout,
installs a subclass's `atoms` tuple, retains the builder profile in the target's
Portage configuration, and verifies installed packages. Its concrete subclasses
implement the `install_system()` hook: `GlibcPackageInstallStage` bootstraps
glibc/systemd with temporary package USE overrides needed for an empty target
root; `MuslPackageInstallStage` bootstraps native musl/OpenRC system packages.

`KernelStage` installs kernel sources into the builder, compiles using the
provided configuration, and installs the kernel and modules into the target.
`SystemdConfigurationStage` configures glibc locales and systemd networking,
DNS, SSH, QGA, and serial login. `OpenRCConfigurationStage` configures the
corresponding OpenRC services, dhcpcd, logging, C.UTF-8, and VSOCK forwarding to SSH.

`BootloaderStage` installs BIOS and removable UEFI GRUB. Its kernel entry expects
the runtime root at `/dev/vda3`, so the image must be the first virtio disk.
The kernel configuration must support that boot path without an initramfs.
`FinalizeStage` removes SSH host keys, clears machine identity and build residue,
locks root's password, records `/etc/corvus-image-info`, and flushes writes.
The manager then unmounts owned filesystems; Corvus publishes the completed disk.

`PortageBackend` imports Portage lazily, allowing planning on a non-Gentoo host.
It queries installed-package databases and metadata through the Python API,
validates requested atoms, and delegates installation to `emerge` with the
requested `ROOT`. `InstallRequest` specifies atoms, root, and binary package
preferences. Installation invalidates cached query trees even on failure.
Dependency resolution, hooks, compilation, and merges remain owned by `emerge`.

## Build an existing flavor

Run these commands from the repository root:

```sh
make -C yaml/gentoo-python plan IMAGE=glibc-systemd-headless
make -C yaml/gentoo-python build IMAGE=glibc-systemd-headless

make -C yaml/gentoo-python plan IMAGE=musl-openrc-headless
make -C yaml/gentoo-python build IMAGE=musl-openrc-headless
```

`glibc-systemd-headless` is the default `IMAGE`. `plan` bundles the script and
prints its stage order without requiring Gentoo, root, or a running Corvus daemon.
`build` bundles it, ensures the registered builder dependency through the owning
`yaml/gentoo` recipe, and invokes `crv build` on the selected YAML. A working
Corvus installation and the resources named in the YAML, including the `corvus`
network and OVMF images, are required for actual builds. These targets remain
local to `yaml/gentoo-python`; the top-level Makefile does not integrate them.

| Flavor | Registered builder dependency | Published image |
|---|---|---|
| `glibc-systemd-headless` | `gentoo-glibc-systemd-builder-cloudinit` | `gentoo-glibc-systemd-headless` |
| `musl-openrc-headless` | `gentoo-musl-openrc-builder` | `gentoo-musl-openrc-headless` |
| `corvus-test-node` | Python `glibc-systemd-headless` build | `corvus-test-node` |
| `corvus-test-vm` | `gentoo-musl-openrc-builder` | `corvus-test-vm` |

Each headless YAML also registers BIOS and UEFI runtime templates named after its
published image, with `-uefi` appended for UEFI. The musl target disk is 4 GiB;
the glibc target is 20 GiB. Both are headless without cloud-init. The shell
recipes and their variants remain available under `yaml/gentoo`.

Local Makefile variables include `CACHE_DIR` (default `/data/corvus-cache` on the
Corvus node), `IMAGE_POLICY` (default `update`), `USEPKG`, and `GETBINPKG` (both
default `true`). Binary preference permits source builds when needed. For example:

```sh
make -C yaml/gentoo-python build IMAGE=musl-openrc-headless USEPKG=false GETBINPKG=false
```

`GENTOO_KEYSERVER` selects the repository keyserver (default
`hkps://keys.gentoo.org`) for both the image and its builder dependencies.
Repository snapshot fetch and repository sync each get up to three attempts,
with five seconds between failed attempts. Key refresh and signature verification
remain enabled on every attempt; persistent failures stop the build. If a
keyserver cannot refresh a release key, select another server explicitly:

```sh
make -C yaml/gentoo-python build IMAGE=musl-openrc-headless GENTOO_KEYSERVER=hkps://keyserver.ubuntu.com
```

The node cache is shared into the builder as virtiofs tag `corvus-cache`, mounted
by default at `/mnt/corvus-cache`. Distfiles are shared across flavors; package
caches and binhosts are separated by Portage variant. Kernel archives live under
`kernels/<libc>-<init>/`, keyed by source revision and configuration SHA-256.
A shared `python-build.lock` serializes Python builds using the same cache.

## Use the library from a flavor script

The package is importable after `pip install -e .` from the repository root;
`PYTHONPATH=python` also permits imports directly from the checkout. In the VM,
the `.pyz` supplies the library while Gentoo supplies Portage and native tools.
The supplied flavor scripts are complete examples; this smaller glibc example
shows the same composition interface:

```python
from pathlib import Path

from corvus_image_builder.core import BuildContext, BuildManager, ImageRecipe
from corvus_image_builder.gentoo import (
    BootloaderStage,
    ChrootEnvironmentStage,
    DiskStage,
    FinalizeStage,
    GlibcPackageInstallStage,
    KernelStage,
    RepositoryStage,
    Settings,
    SystemdConfigurationStage,
    WorkspaceStage,
)


class ServerPackages(GlibcPackageInstallStage):
    """Install the base system and packages for a small server flavor."""

    atoms = ("net-misc/openssh", "app-emulation/qemu-guest-agent")


settings = Settings(
    name="my-gentoo-server",
    disk=Path("/dev/vdb"),
    cache=Path("/mnt/corvus-cache"),
    kernel_config=Path("/run/corvus-kernel-config"),
)
context = BuildContext(root=Path("/mnt/sysroot").resolve())
recipe = ImageRecipe(
    stages=[
        stage(settings)
        for stage in (
            WorkspaceStage,
            RepositoryStage,
            DiskStage,
            ChrootEnvironmentStage,
            ServerPackages,
            KernelStage,
            SystemdConfigurationStage,
            BootloaderStage,
            FinalizeStage,
        )
    ]
)
manager = BuildManager()
print("\n".join(manager.plan(recipe, context)))
# Execute only as root inside the disposable native Gentoo builder VM:
# manager.run(recipe, context)
```

For musl, use `Settings(libc="musl", init="openrc", ...)`, subclass
`MuslPackageInstallStage`, and substitute `OpenRCConfigurationStage`.
The VM must match the flavor: amd64 musl/OpenRC or amd64 no-multilib systemd/glibc.
This is a native build, not a cross-compilation interface. Execution changes
builder Portage configuration and erases the selected target disk, so it belongs
inside the disposable VM. The mount directory must initially be empty, and the
target disk and its descendants must have no mounted filesystems.

Existing scripts read `IMAGE_ROOT`, `TARGET_DISK`, `IMAGE_CACHE`, `KERNEL_CONFIG`,
`USEPKG`, and `GETBINPKG` environment overrides. The boolean overrides use the
literal string `true`. `IMAGE_BUILD_PLAN=1` selects planning. `PORTAGE_KEYSERVER`
overrides the repository keyserver; YAML exposes it as `portage_keyserver`.
These environment conventions belong to the scripts and repository stage;
constructing `Settings` directly lets a new script choose its own interface.

## Extend a recipe

Subclass `Stage` for generic work or `GentooStage` for access to `settings`,
`command()`, and `packages()`. Declare `requires` and `produces`, implement
`execute(context)`, and publish each declared output in `context.artifacts`.
Put prerequisite checks in `validate()` that do not depend on outputs from
earlier stages, since all validation runs before execution. Override `verify()`
for stronger output checks and call `super().verify(context)` to retain the
artifact check. Register owned resources as soon as they are acquired.

For example, insert this stage between configuration and bootloader installation:

```python
from corvus_image_builder.core import BuildContext
from corvus_image_builder.gentoo import GentooStage


class SiteConfiguration(GentooStage):
    """Add a site-specific issue banner after the base configuration."""

    requires = frozenset({"configured"})
    produces = frozenset({"site-configured"})

    def execute(self, context: BuildContext) -> None:
        banner = context.root / "etc/issue"
        banner.write_text("Managed Gentoo server\n")
        context.artifacts["site-configured"] = banner
```

Explicit stage order determines execution. If a later custom stage must depend
on this output, declare `site-configured` in that stage's `requires`.
Subclass the abstract `PackageInstallStage` and implement `install_system()` for
a new bootstrap. To extend an existing bootstrap, subclass
`GlibcPackageInstallStage` or `MuslPackageInstallStage` and call its implementation
before adding behavior.

To add a flavor, create matching Python and YAML files, select the proper native
builder in the YAML, upload the generated archive and kernel configuration, and
invoke the archive with Python. Add its name and builder mapping to the local
Makefile's `IMAGES` and `BUILDER_<flavor>` definitions. Keep dependency imports in
their owning recipes. The archive can also be produced directly:

```sh
python3 yaml/gentoo-python/bundle.py yaml/gentoo-python/my-flavor.py yaml/gentoo-python/my-flavor.pyz
```

Here `my-flavor.py` is the new script you create. Bundle creation is deterministic
for identical source files; complete image builds depend on repository state,
packages, and build timestamps and are not guaranteed byte-for-byte reproducible.


## Integration-test images

The standalone Python directory also builds the outer test node and minimal
nested guest. The original recipes under `yaml/gentoo` remain unchanged.

```sh
make -C yaml/gentoo-python plan IMAGE=corvus-test-node
make -C yaml/gentoo-python build IMAGE=corvus-test-node
make -C yaml/gentoo-python plan IMAGE=corvus-test-vm
make -C yaml/gentoo-python build IMAGE=corvus-test-vm
```

Both builds prepare the existing harness SSH key without rotating it. Planning
only bundles the selected script and checks stage dependencies; it does not
require the key, root, Gentoo, or a Corvus connection.

All four Python flavors publish canonical names shared with the shell recipes,
including headless runtime templates and their `-uefi` variants. Whichever
implementation is built under the selected image policy becomes the published
version; the integration harness needs no image-name changes. Existing resources
with older `-python` names are not deleted or redirected. Root image commands
still select the shell recipes; explicitly use the commands above for Python.

### Outer node: customize, do not bootstrap

`corvus-test-node.py` defines an ordered overlay recipe:

1. Prepare the native glibc workspace, configure the node's Git overlays and
   package policy, and synchronize repositories.
2. `ExistingImageStage` validates and mounts the inherited root and ESP without
   repartitioning, formatting, compiling a kernel, or reinstalling GRUB.
3. `ChrootEnvironmentStage` supplies private runtime interfaces and package
   inputs. `NodePackages` extends the existing system, then rebuilds pycapnp
   from source against the installed capnproto library.
4. `NodeDatabase` initializes PostgreSQL 18, creates the role/database using a
   Unix-socket-only server, and stops that server on success or failure.
5. `NodeConfiguration` installs VDE/DHCP/NAT, harness access and static units;
   Corvus daemon services remain disabled until certificate deployment.
6. `NodeFinalize` resets first-boot identity, cleans residue and records tool
   versions while retaining runtime Portage policy and package records.

The Makefile first evaluates the **local Python glibc headless build**, forwarding
`IMAGE_POLICY`, `CACHE_DIR`, binary preferences and the keyserver. It does not
satisfy this dependency merely by finding a shell-built headless image. The node
uses `offline-overlay` and retains the headless base's disk size and boot files.
Static systemd units are uploaded from their existing shared location; no old
build shell script is executed.

### Nested guest: an explicit minimal runtime

`corvus-test-vm.py` assembles the normal fresh-disk, package, kernel, OpenRC and
bootloader stages with test-specific subclasses. Its package bootstrap uses an
explicit atom list, **not `@system`**, so the artifact contains no compiler,
Python, Portage, glibc, systemd or sudo. Required C++ runtime DSOs are copied from
the native musl toolchain without installing GCC into the target.

The image is 1 GiB with a 64 MiB ESP and `net.ifnames=0`. It preserves the harness
user/password, SSH key, passwordless `doas`, DHCPv4 boot, BusyBox DNS lookup, TPM
utilities, VSOCK forwarding, QGA and ACPI shutdown contracts. The finalizer checks
for forbidden packages **before** deleting package records, development files,
documentation and non-English locales. Runtime shared libraries and licenses
remain.

### Reusable extension points

- `Settings.esp_size_mib` defaults to 512; `kernel_args` defaults to an empty
  tuple of literal tokens. Overlay recipes can set `kernel_config=None` because
  only `KernelStage` requires a source configuration.
- `Mounts` owns successful mount acquisitions and releases them in reverse order.
  `ChrootEnvironmentStage` never binds the builder's `/run`; target commands do
  not access its init-system sockets. Build scratch stays on the helper disk.
- `GentooStage.chroot()` runs individual commands with builder Portage variables
  removed. It accepts stdin bytes for SQL and password configuration, using the
  same timeout and process-group handling as other commands.
- `InstallRequest` supports `noreplace`, `usepkg_exclude`, `reinstall_atoms` and
  a helper `tmpdir`. Package queries still use Portage's Python API; installs
  still run through isolated `emerge` subprocesses.
- `PackageInstallStage.configure_portage()` copies package policy and normalizes
  target cache locations. Overlay subclasses can call it without bootstrapping.
- `FinalizeStage.reset_identity()`, `clean()` and `metadata()` are customization
  hooks. Finalization retires chroot mounts before cleanup; deletion rejects
  symlink ancestors and mounted subtrees so shared cache contents survive.
  `write`, `link` and `remove` are available from the Gentoo package for flavor
  scripts. Root/ESP mounts retain the existing failure-preservation policy.

Every new stage should declare meaningful `requires` and `produces` artifacts;
planning checks the supplied order without executing stages. `validate()` runs
for all stages before execution, so checks involving a root populated by earlier
stages belong in `execute()` or `verify()`, not in preflight validation.

### Corvus development overlay

Run `make -C yaml/gentoo-python plan IMAGE=corvus-test` to inspect the
stages and `make -C yaml/gentoo-python build IMAGE=corvus-test` to build.
The recipe first evaluates the Python glibc headless base, installs cloud-init
and live Corvus with build dependencies, and initializes PostgreSQL and VDE.
It publishes the canonical `gentoo-corvus-test` image and retains the development
VM's NoCloud configuration and optional virtiofs package caches. Legacy recipes
remain available; both workflows publish the same image name.

`PostgreSQLInitializationStage` owns and stops its socket-only temporary server
before runtime mounts are released. `VdeNetworkConfigurationStage` provides the
shared isolated subnet, DHCP, and outbound NAT used by development and node images.
